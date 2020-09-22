;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;; The purpose of IR conversion is to turn a data flow graph, whose nodes
;;; are lazy arrays, into an analogous graph, whose nodes are buffers and
;;; kernels.  Kernels and buffers alternate, such that the inputs and
;;; outputs of a kernel are always buffers, and such that the inputs and
;;; outputs of a buffer are always kernels.
;;;
;;; The IR conversion algorithm proceeds as such:
;;;
;;; - One dendrite is created for each supplied graph root.
;;;
;;; - We maintain a priority queue of dendrites, sorted by depth.
;;;
;;; - The dendrite with the highest depth is advanced until it reaches
;;;   either a lazy array with multiple inputs, or a lazy array that has
;;;   more than one static reference.  In the case of multiple inputs, a
;;;   new dendrite is created for all but the first input.  The new
;;;   dendrites are placed in the priority queue, and the first one is
;;;   processed further.  If the lazy array has more then one static
;;;   reference, the dendrite is also inserted into the priority queue.

(defstruct ir-converter
  ;; A priority queue of clusters, sorted by the depth of the corresponding
  ;; lazy arrays.
  (pqueue (priority-queue:make-pqueue #'>))
  ;; A hash table, mapping from lazy arrays to clusters.
  (cluster-table (make-hash-table :test #'eq) :type hash-table)
  ;; A hash table, mapping from Common Lisp arrays to buffers.
  (array-table (make-hash-table :test #'eq) :type hash-table)
  ;; A hash table, mapping from Common Lisp scalars to buffers of rank zero
  ;; containing those scalars.
  (scalar-table (make-hash-table :test #'eql) :type hash-table))

(defun ir-converter-next-cluster (ir-converter)
  (priority-queue:pqueue-pop
   (ir-converter-pqueue ir-converter)))

(defun ir-converter-empty-p (ir-converter)
  (priority-queue:pqueue-empty-p
   (ir-converter-pqueue ir-converter)))

(declaim (ir-converter *ir-converter*))
(defvar *ir-converter*)

(defstruct (cluster
            (:constructor make-cluster (lazy-array)))
  ;; The cluster's lazy array.
  (lazy-array nil :type lazy-array)
  ;; A list of dendrites that have reached this cluster.
  (dendrites '() :type list))

(defstruct (stem
            (:constructor make-stem (cluster kernel)))
  ;; The cluster in which the stem is rooted.
  (cluster nil :type cluster)
  ;; The kernel that is grown from that stem.
  (kernel nil :type kernel)
  ;; A list of dendrites that originated from this stem.  This list is used
  ;; to invalidate dendrites whenever one of them reaches more than one
  ;; input of a fusion node.
  (dendrites '() :type list))

(defun ensure-cluster (lazy-array)
  (alexandria:ensure-gethash
   lazy-array
   (ir-converter-cluster-table *ir-converter*)
   (let ((cluster (make-cluster lazy-array)))
     (priority-queue:pqueue-push
      cluster
      (lazy-array-depth lazy-array)
      (ir-converter-pqueue *ir-converter*))
     cluster)))

(defun cluster-ntype (cluster)
  (declare (cluster cluster))
  (element-ntype (cluster-lazy-array cluster)))

(defun cluster-shape (cluster)
  (declare (cluster cluster))
  (lazy-array-shape (cluster-lazy-array cluster)))

(defstruct (dendrite
            (:constructor %make-dendrite)
            (:copier %copy-dendrite))
  ;; The stem from which this dendrite originated.
  (stem nil :type stem)
  ;; The shape of the iteration space referenced by the dendrite.
  (shape nil :type shape)
  ;; A transformation from the dendrite's shape to the iteration space of
  ;; the dendrite's kernel.
  (transformation nil :type transformation)
  ;; The depth of the cluster most recently visited by this dendrite.
  (depth nil :type unsigned-byte)
  ;; The cons cell whose car is the value of the current lazy array being
  ;; referenced, and whose cdr is to be filled with the result of growing
  ;; the dendrite.
  (cons nil :type cons)
  ;; Whether the dendrite is to be considered when converting a cluster.
  (validp t :type boolean))

(defun dendrite-kernel (dendrite)
  (declare (dendrite dendrite))
  (stem-kernel (dendrite-stem dendrite)))

(defun dendrite-cluster (dendrite)
  (declare (dendrite dendrite))
  (stem-cluster (dendrite-stem dendrite)))

(defun make-dendrite
    (cluster shape &optional (buffer (make-buffer :shape shape :ntype (cluster-ntype cluster))))
  (declare (cluster cluster) (shape shape) (buffer buffer))
  (let* ((cons (cons 0 nil))
         (transformation (identity-transformation (shape-rank shape)))
         (store-instruction (make-store-instruction cons buffer transformation))
         (kernel (make-kernel :iteration-space shape))
         (stem (make-stem cluster kernel))
         (dendrite (%make-dendrite
                    :stem stem
                    :shape shape
                    :transformation transformation
                    :depth (lazy-array-depth (cluster-lazy-array cluster))
                    :cons cons)))
    (push store-instruction (alexandria:assoc-value (kernel-targets kernel) buffer))
    (push store-instruction (alexandria:assoc-value (buffer-writers buffer) kernel))
    (push dendrite (stem-dendrites stem))
    dendrite))

(defun copy-dendrite (dendrite)
  (declare (dendrite dendrite))
  (let ((stem (dendrite-stem dendrite))
        (copy (%copy-dendrite dendrite)))
    (push copy (stem-dendrites stem))
    copy))

(defun ir-from-lazy-arrays (lazy-arrays)
  (let ((*ir-converter* (make-ir-converter))
        (root-buffers '()))
    ;; Create and grow one dendrite for each root array.
    (loop for lazy-array in lazy-arrays do
      (let* ((cluster (make-cluster lazy-array))
             (dendrite (make-dendrite cluster (lazy-array-shape lazy-array))))
        (push (caar (kernel-targets (stem-kernel (dendrite-stem dendrite))))
              root-buffers)
        (grow-dendrite dendrite lazy-array)))
    ;; Successively convert all clusters.
    (loop until (ir-converter-empty-p *ir-converter*)
          for cluster = (ir-converter-next-cluster *ir-converter*)
          do (convert-cluster cluster (cluster-lazy-array cluster)))
    ;; Assign instruction numbers.
    (map-kernels #'assign-instruction-numbers root-buffers)
    (normalize-ir root-buffers)
    (nreverse root-buffers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cluster Conversion

(defgeneric convert-cluster (cluster lazy-array))

(defmethod convert-cluster :around
    ((cluster cluster)
     (non-immediate non-immediate))
  (remhash non-immediate (ir-converter-cluster-table *ir-converter*))
  (let ((valid-dendrites (delete-if-not #'dendrite-validp (cluster-dendrites cluster))))
    (setf (cluster-dendrites cluster) valid-dendrites)
    (cond
      ;; If there are zero valid dendrites, the cluster can be ignored
      ((null valid-dendrites)
       (values))
      ;; If we have only a single valid dendrite, and that dendrite is
      ;; also invertible, we need not consider this cluster at all.
      ;; The dendrite can simply continue its growth.
      ((and (null (rest valid-dendrites))
            (transformation-invertiblep
             (dendrite-transformation (first valid-dendrites))))
       (setf (dendrite-depth (first valid-dendrites))
             (lazy-array-depth non-immediate))
       (grow-dendrite (first valid-dendrites) non-immediate))
      ;; Otherwise, actually convert the cluster.
      (t (call-next-method)))))

(defmethod convert-cluster
    ((cluster cluster)
     (non-immediate non-immediate))
  (let ((dendrites (cluster-dendrites cluster))
        (alist '())
        (buffers '()))
    ;; Compute an alist from shapes to dendrites that will write into a
    ;; buffer of that shape.
    (loop for dendrite in dendrites do
      (block convert-one-dendrite
        (let ((dshape (dendrite-shape dendrite)))
          (loop for entry in alist do
            (let* ((eshape (car entry))
                   (cover (fuse-shapes eshape dshape)))
              (when (<= (- (shape-size cover)
                           (shape-rank cover))
                        (+ (shape-size dshape)
                           (shape-size eshape)))
                (setf (car entry) cover)
                (push dendrite (cdr entry))
                (return-from convert-one-dendrite)))
                finally (push `(,dshape ,dendrite) alist)))))
    ;; Create one buffer per alist entry and insert the corresponding load
    ;; instructions.
    (loop for (shape . mergeable-dendrites) in alist do
      (let ((buffer (make-buffer :shape shape :ntype (cluster-ntype cluster))))
        (push buffer buffers)
        (loop for dendrite in mergeable-dendrites do
          (with-accessors ((cons dendrite-cons)
                           (kernel dendrite-kernel)
                           (transformation dendrite-transformation)) dendrite
            (let ((load-instruction (make-load-instruction buffer transformation)))
              (setf (cdr cons) load-instruction)
              (push load-instruction (alexandria:assoc-value (kernel-sources kernel) buffer))
              (push load-instruction (alexandria:assoc-value (buffer-readers buffer) kernel)))))))
    (setf buffers (nreverse buffers))
    ;; Now subdivide the space of all buffers and emit one kernel per
    ;; resulting fragment, plus some copy kernels if the fragment is part
    ;; of the shapes of several buffers.
    (let ((fragments (subdivide-shapes (mapcar #'first alist))))
      (loop for (shape . bitmask) in fragments do
        (let ((target-buffers
                (loop for buffer in buffers
                      for index from 0
                      when (logbitp index bitmask)
                        collect buffer)))
          (let ((main-buffer (first target-buffers)))
            (grow-dendrite (make-dendrite cluster shape main-buffer) non-immediate)
            ;; Finally, emit copy kernels from the main buffer to all the
            ;; other buffers.
            (loop for target-buffer in (rest target-buffers) do
              (insert-copy-kernel shape target-buffer main-buffer))))))))

(defun insert-copy-kernel (iteration-space target-buffer source-buffer)
  (let* ((rank (shape-rank iteration-space))
         (transformation (identity-transformation rank))
         (load (make-load-instruction source-buffer transformation))
         (store (make-store-instruction
                 (cons 0 load)
                 target-buffer
                 (identity-transformation (shape-rank iteration-space))))
         (kernel
           (make-kernel
            :iteration-space iteration-space
            :sources `((,source-buffer ,load))
            :targets `((,target-buffer ,store)))))
    (push `(,kernel ,load) (buffer-readers source-buffer))
    (push `(,kernel ,store) (buffer-writers target-buffer))
    kernel))

(defmethod convert-cluster
    ((cluster cluster)
     (lazy-multiple-value-map lazy-multiple-value-map))
  (convert-lazy-multiple-value-map
   lazy-multiple-value-map
   (cluster-dendrites cluster)))

(defun convert-lazy-multiple-value-map
    (lazy-multiple-value-map dendrites)
  (let ((inputs (inputs lazy-multiple-value-map))
        (mergeable-dendrites-list '()))
    (loop for dendrite in dendrites do
      (let ((entry (find dendrite mergeable-dendrites-list
                         :key #'car
                         :test
                         (lambda (d1 d2)
                           (and (eq
                                 (dendrite-kernel d1)
                                 (dendrite-kernel d2))
                                (shape-equal
                                 (dendrite-shape d1)
                                 (dendrite-shape d2))
                                (transformation-equal
                                 (dendrite-transformation d1)
                                 (dendrite-transformation d2)))))))
        (if (not entry)
            (push (list dendrite) mergeable-dendrites-list)
            (push dendrite (cdr entry)))))
    (loop for mergeable-dendrites in mergeable-dendrites-list do
      (let* ((input-conses (loop for input in inputs collect (cons 0 input)))
             (instruction
               (make-multiple-value-call-instruction
                (number-of-values lazy-multiple-value-map)
                (operator lazy-multiple-value-map)
                input-conses)))
        (loop for dendrite in mergeable-dendrites do
          (setf (cdr (dendrite-cons dendrite))
                instruction))
        (loop for input in inputs
              for input-cons in input-conses
              for input-dendrite = (copy-dendrite (first mergeable-dendrites))
              for depth = (lazy-array-depth lazy-multiple-value-map)
              do (setf (dendrite-cons input-dendrite) input-cons)
              do (grow-dendrite input-dendrite input))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dendrite Growing

(defgeneric grow-dendrite (dendrite lazy-array))

(defmethod grow-dendrite :around
    ((dendrite dendrite)
     (non-immediate non-immediate))
  (if (and (< (lazy-array-depth non-immediate)
              (dendrite-depth dendrite))
           (> (lazy-array-refcount non-immediate) 1))
      (enqueue-dendrite dendrite non-immediate)
      (call-next-method)))

(defun enqueue-dendrite (dendrite lazy-array)
  (declare (dendrite dendrite))
  (push dendrite (cluster-dendrites (ensure-cluster lazy-array)))
  (values))

(defmethod grow-dendrite
    ((dendrite dendrite)
     (lazy-fuse lazy-fuse))
  (with-accessors ((shape dendrite-shape)
                   (transformation dendrite-transformation)
                   (stem dendrite-stem)
                   (cons dendrite-cons)) dendrite
    (let* ((inputs (inputs lazy-fuse))
           (intersections
             (loop for input in inputs
                   for intersection = (shape-intersection shape (lazy-array-shape input))
                   collect intersection)))
      (case (count-if-not #'empty-shape-p intersections)
        (0 (error "Erroneous fusion."))
        (1 (let ((input (nth (position-if-not #'empty-shape-p intersections) inputs)))
             (grow-dendrite dendrite input)))
        (otherwise
         (let* ((kernel (dendrite-kernel dendrite))
                (buffer (caar (kernel-targets kernel))))
           ;; Invalidate the current kernel and its dendrites.
           (delete-kernel kernel)
           (loop for dendrite in (stem-dendrites stem) do
             (setf (dendrite-validp dendrite) nil))
           ;; Try growing from the cluster again, but with one stem for
           ;; each reachable fusion input.
           (loop for input in inputs
                 for intersection in intersections
                 unless (empty-shape-p intersection)
                   do (grow-dendrite
                       (make-dendrite
                        (stem-cluster stem)
                        (transform intersection (invert-transformation transformation))
                        buffer)
                       (cluster-lazy-array (stem-cluster stem))))))))))

(defmethod grow-dendrite
    ((dendrite dendrite)
     (lazy-reshape lazy-reshape))
  (with-accessors ((shape dendrite-shape)
                   (transformation dendrite-transformation)) dendrite
    (setf shape (transform
                 (shape-intersection shape (lazy-array-shape lazy-reshape))
                 (transformation lazy-reshape)))
    (setf transformation (compose-transformations
                          (transformation lazy-reshape)
                          transformation))
    (grow-dendrite dendrite (input lazy-reshape))))

(defmethod grow-dendrite
    ((dendrite dendrite)
     (lazy-map lazy-map))
  (with-accessors ((shape dendrite-shape)
                   (transformation dendrite-transformation)
                   (cons dendrite-cons)) dendrite
    (let* ((inputs (inputs lazy-map))
           (input-conses (loop for input in inputs collect (cons 0 input))))
      (setf (cdr cons)
            (make-call-instruction
             :inputs input-conses
             :operator (operator lazy-map)))
      ;; If our function has zero inputs, we are done.  Otherwise we create
      ;; one dendrite for each input (except the first one, for which we
      ;; can reuse the current dendrite) and continue growing.
      (unless (null inputs)
        (loop for input in inputs
              for input-cons in input-conses do
                (let ((new-dendrite (copy-dendrite dendrite)))
                  (setf (dendrite-cons new-dendrite) input-cons)
                  (grow-dendrite new-dendrite input)))))))

(defmethod grow-dendrite
    ((dendrite dendrite)
     (lazy-multiple-value-map lazy-multiple-value-map))
  (convert-lazy-multiple-value-map lazy-multiple-value-map (list dendrite)))

(defmethod grow-dendrite
    ((dendrite dendrite)
     (lazy-multiple-value-ref lazy-multiple-value-ref))
  (with-accessors ((cons dendrite-cons)) dendrite
    (setf (car cons)
          (value-n lazy-multiple-value-ref)))
  (grow-dendrite dendrite (input lazy-multiple-value-ref)))

(defmethod grow-dendrite
    ((dendrite dendrite)
     (array-immediate array-immediate))
  (with-accessors ((shape dendrite-shape)
                   (transformation dendrite-transformation)
                   (stem dendrite-stem)
                   (cons dendrite-cons)) dendrite
    (let* ((kernel (stem-kernel stem))
           (shape (lazy-array-shape array-immediate))
           (buffer
             (if (zerop (shape-rank shape))
                 (alexandria:ensure-gethash
                  (aref (storage array-immediate))
                  (ir-converter-scalar-table *ir-converter*)
                  (make-buffer :shape (make-shape '())
                               :ntype (element-ntype array-immediate)
                               :storage (storage array-immediate)))
                 (alexandria:ensure-gethash
                  (storage array-immediate)
                  (ir-converter-array-table *ir-converter*)
                  (make-buffer :shape (lazy-array-shape array-immediate)
                               :ntype (element-ntype array-immediate)
                               :storage (storage array-immediate)))))
           (load-instruction (make-load-instruction buffer transformation)))
      (push load-instruction (alexandria:assoc-value (kernel-sources kernel) buffer))
      (push load-instruction (alexandria:assoc-value (buffer-readers buffer) kernel))
      (setf (cdr cons) load-instruction))))

(defmethod grow-dendrite
    ((dendrite dendrite)
     (range-immediate range-immediate))
  (with-accessors ((cons dendrite-cons)
                   (transformation dendrite-transformation)) dendrite
    (setf (cdr cons)
          (make-iref-instruction transformation))))
