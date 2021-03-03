;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;; The purpose of IR conversion is to turn a data flow graph, whose nodes
;;; are lazy arrays, into an analogous graph, whose nodes are buffers and
;;; kernels.  Kernels and buffers alternate, such that the inputs and
;;; outputs of a kernel are always buffers, and such that the inputs and
;;; outputs of a buffer are always kernels.
;;;
;;; The main data structure IR conversion algorithm are so called dendrites
;;; that grow from the lazy arrays that are the graph roots and along their
;;; inputs.  When a dendrite reaches an immediate, its growth stops.  While
;;; growing, dendrites create the instructions of a particular kernel,
;;; while keeping track of the current transformation and shape.  When a
;;; dendrite grows over a lazy array has more than one input, the dendrite
;;; branches out into multiple dendrites.  Each dendrite has a stem that
;;; tracks the kernel being generated.  All dendrites that are the result
;;; of branching out this way share the same stem.
;;;
;;; Whenever a dendrite reaches a lazy array with a refcount larger than
;;; one, or a lazy array that follows a broadcasting reshape operation, its
;;; growth is suspended and we record that this dendrite has reached that
;;; lazy array.  The data structure that tracks which dendrites have
;;; reached a particular lazy array is called a cluster.  Once the growth
;;; of all dendrites has been suspended or stopped, we pick the cluster
;;; whose lazy array has the highest depth from a priority queue.  This
;;; cluster is now turned into one or more buffers, and each buffer is the
;;; root of a stem with a single dendrite that is grown further.  Once
;;; there are no further clusters, the IR conversion is complete.
;;;
;;; A special case occurs when a dendrite reaches a fusion node with
;;; multiple inputs that intersect with the dendrite's shape.  In such a
;;; case, we want to replace the current kernel by multiple kernels, while
;;; choosing the iteration space of each kernel such that it reaches only a
;;; single input of the fusion node.  We achieve this by deleting both the
;;; stem's kernel, and all dendrites that originate from that stem.  Then
;;; we restart with one stem and kernel for each suitable subspace of the
;;; original stem.  In doing so, we eliminate fusion nodes altogether from
;;; the IR.

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
  (scalar-table (make-hash-table :test #'eql) :type hash-table)
  ;; A list of lists of conses that need to be updated by writing the value
  ;; of the cdr of the first cons to the cdr of each remaining cons.
  (cons-updates '() :type list)
  ;; The maximum size we allow a kernel to grow during buffer pruning.
  (kernel-size-threshold nil :type unsigned-byte :read-only t)
  ;; An list of potentially superfluous buffers, i.e., buffer where all
  ;; dendrites are disjoint and cover the entire buffer.  Cluster
  ;; conversion also makes sure that the data slot of each potentially
  ;; superfluous buffer contains the list of dendrites reaching it.
  (potentially-superfluous-buffers '() :type list))

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
  (petalisp.type-inference:generalize-ntype
   (element-ntype
    (cluster-lazy-array cluster))))

(defun cluster-shape (cluster)
  (declare (cluster cluster))
  (lazy-array-shape (cluster-lazy-array cluster)))

(defstruct stem
  ;; The cluster in which the stem is rooted.
  (cluster nil :type cluster)
  ;; The kernel that is grown from that stem.
  (kernel nil :type kernel)
  ;; A list whose entries are the buffers for each value produced by the
  ;; root instruction of that stem, or NIL if that value is never
  ;; referenced.
  (buffers nil :type list)
  ;;A stem is turned invalid when one of its dendrites reaches more than
  ;; one input of a lazy fuse node.
  (validp t :type boolean))

(defstruct (dendrite
            (:constructor %make-dendrite))
  ;; The stem from which this dendrite originated.
  (stem nil :type stem)
  ;; The shape of the iteration space referenced by the dendrite.
  (shape nil :type shape)
  ;; A transformation from the dendrite's shape to the iteration space of
  ;; the dendrite's kernel.
  (transformation nil :type transformation)
  ;; The depth of the cluster most recently visited by this dendrite.
  (depth nil :type unsigned-byte)
  ;; The cons cell whose car is to be filled with a cons cell whose cdr is
  ;; the next instruction, and whose car is an integer denoting which of
  ;; the multiple values of the cdr is being referenced.
  (cons nil :type cons))

(defun dendrite-kernel (dendrite)
  (declare (dendrite dendrite))
  (stem-kernel (dendrite-stem dendrite)))

(defun dendrite-cluster (dendrite)
  (declare (dendrite dendrite))
  (stem-cluster (dendrite-stem dendrite)))

(defun dendrite-validp (dendrite)
  (declare (dendrite dendrite))
  (stem-validp (dendrite-stem dendrite)))

(defun dendrite-value-n (dendrite)
  (car (dendrite-cons dendrite)))

(defun make-dendrite (cluster shape buffers)
  (declare (cluster cluster) (shape shape) (list buffers))
  (let* ((transformation (identity-transformation (shape-rank shape)))
         (kernel (make-kernel :iteration-space shape))
         (stem (make-stem
                :cluster cluster
                :kernel kernel
                :buffers buffers))
         (store-instructions
           (loop for buffer in buffers
                 for index from 0
                 unless (null buffer)
                   collect
                   (make-store-instruction kernel (cons index nil) buffer transformation)))
         (dendrite (%make-dendrite
                    :stem stem
                    :shape shape
                    :transformation transformation
                    :depth (lazy-array-depth (cluster-lazy-array cluster))
                    :cons (store-instruction-input (first store-instructions)))))
    (unless (null (rest store-instructions))
      (push (mapcar #'store-instruction-input store-instructions)
            (ir-converter-cons-updates *ir-converter*)))
    dendrite))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IR Conversion

(defun ir-from-lazy-arrays (lazy-arrays &key (kernel-size-threshold 32))
  (let ((*ir-converter* (make-ir-converter :kernel-size-threshold kernel-size-threshold))
        (root-buffers '()))
    ;; Create and grow one dendrite for each root array.
    (loop for lazy-array in lazy-arrays do
      (let* ((cluster (make-cluster lazy-array))
             (shape (lazy-array-shape lazy-array))
             (buffer (make-buffer
                      :shape shape
                      :ntype (petalisp.type-inference:generalize-ntype
                              (element-ntype lazy-array))))
             (dendrite (make-dendrite cluster shape (list buffer))))
        (push buffer root-buffers)
        (grow-dendrite dendrite lazy-array)))
    ;; Successively convert all clusters.
    (loop until (ir-converter-empty-p *ir-converter*)
          for cluster = (ir-converter-next-cluster *ir-converter*)
          do (convert-cluster cluster (cluster-lazy-array cluster)))
    ;; Update all cons cells whose instruction couldn't be determined
    ;; immediately at cluster conversion time.
    (loop for (cons . other-conses) in (ir-converter-cons-updates *ir-converter*) do
      (let ((instruction (cdr cons)))
        ;; The cdr might not contain an instruction if the corresponding
        ;; dendrite has been invalidated in the meantime.
        (when (instructionp instruction)
          (loop for other-cons in other-conses do
            (setf (cdr other-cons) instruction)))))
    (prune-superfluous-buffers
     (ir-converter-potentially-superfluous-buffers *ir-converter*))
    ;; Collapse each buffer's shape, remove all ranges with size one from
    ;; interior buffers, and ensure that each kernel has an instruction
    ;; vector, and that each instruction has a number that is an index into
    ;; that vector.
    (finalize-ir root-buffers)
    (nreverse root-buffers)))

(defun finalize-ir (root-buffers)
  (map-buffers-and-kernels
   #'finalize-buffer
   #'finalize-kernel
   root-buffers))

(defun finalize-buffer (buffer)
  (setf (buffer-data buffer) nil)
  (if (interior-buffer-p buffer)
      (transform-buffer buffer (normalizing-transformation (buffer-shape buffer)))
      (transform-buffer buffer (collapsing-transformation (buffer-shape buffer)))))

(defun finalize-kernel (kernel)
  (setf (kernel-data kernel) nil)
  ;; We use this opportunity to compute the kernel instruction vector,
  ;; knowing it will be cached for all future invocations.
  (setf (kernel-instruction-vector kernel)
        (let ((counter 0))
          (labels ((clear-instruction-number (instruction)
                     (unless (= -1 (instruction-number instruction))
                       (incf counter)
                       (setf (instruction-number instruction) -1)
                       (map-instruction-inputs #'clear-instruction-number instruction))))
            (map-kernel-store-instructions #'clear-instruction-number kernel))
          (let ((vector (make-array counter))
                (index 0))
            (labels ((assign-instruction-number (instruction)
                       (when (= -1 (instruction-number instruction))
                         (setf (instruction-number instruction) -2)
                         (map-instruction-inputs #'assign-instruction-number instruction)
                         (setf (instruction-number instruction) index)
                         (setf (svref vector index) instruction)
                         (incf index))))
              (map-kernel-store-instructions #'assign-instruction-number kernel))
            (setf (kernel-instruction-vector kernel) vector))))
  (transform-kernel
   kernel
   (normalizing-transformation (kernel-iteration-space kernel))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cluster Conversion

(defgeneric convert-cluster (cluster lazy-array))

;;; Under certain circumstances, there is no need to convert the current
;;; cluster at all.  This around method handles these cases.  It also
;;; removes all dendrites that have been invalidated from the cluster.
(defmethod convert-cluster :around
    ((cluster cluster)
     (non-immediate non-immediate))
  ;; Since clusters are converted in depth-first order, and since all
  ;; further lazy arrays have a depth less than the current cluster, there
  ;; is no need to keep the current cluster in the cluster table.  No new
  ;; dendrites will ever reach it.
  (remhash non-immediate (ir-converter-cluster-table *ir-converter*))
  ;; Remove all dendrites that have become invalid because their stem has
  ;; been abandoned after encountering a lazy-fuse node.
  (setf (cluster-dendrites cluster)
        (remove-if-not #'dendrite-validp (cluster-dendrites cluster)))
  ;; Don't try anything smart if there is at least one dendrite with a
  ;; non-invertible transformation.
  (if (find-if-not #'transformation-invertiblep (cluster-dendrites cluster)
                     :key #'dendrite-transformation)
      (call-next-method)
      ;; If every transformation is invertible, we can try to merge
      ;; dendrites with the same stem, transformation and referenced value
      ;; number.
      (let ((mergeable-dendrites '()))
        (loop for dendrite in (cluster-dendrites cluster) do
          (let ((entry (find dendrite mergeable-dendrites
                             :key #'first
                             :test #'mergeable-dendrites-p)))
            (if (consp entry)
                (push dendrite (cdr entry))
                (push (list dendrite) mergeable-dendrites))))
        (trivia:match mergeable-dendrites
          ;; If there are zero mergeable dendrites left, we do nothing.
          ((list))
          ;; If there is exactly one list of mergeable dendrites remaining,
          ;; we don't even have to call the next method.  Instead, we can
          ;; simply continue growing the first one, knowing that the others
          ;; will be patched up later.
          ((list (list* dendrite _))
           (setf (dendrite-depth dendrite)
                 (lazy-array-depth non-immediate))
           (grow-dendrite dendrite non-immediate))
          ;; If there is more than one list of mergeable dendrites, we have
          ;; to go through the regular cluster conversion.  However, we can
          ;; still pull one trick: We can hide all but one dendrite of each
          ;; list of mergeable dendrites, knowing that the others will be
          ;; patched up later.
          (_
           (setf (cluster-dendrites cluster)
                 (mapcar #'first mergeable-dendrites))
           (call-next-method)))
        ;; Patch up the remaining dendrites.
        (loop for (dendrite . other-dendrites) in mergeable-dendrites do
          (let ((cons (dendrite-cons dendrite)))
            ;; If we know the instruction corresponding to the first
            ;; dendrite, we copy it right away.  Otherwise, we delay that
            ;; copying till later.
            (if (instructionp (cdr cons))
                (loop for other-dendrite in other-dendrites do
                  (setf (cdr (dendrite-cons other-dendrite))
                        (cdr cons)))
                (push (list* cons (mapcar #'dendrite-cons other-dendrites))
                      (ir-converter-cons-updates *ir-converter*))))))))

(defun mergeable-dendrites-p (d1 d2)
  (and (eq
        (dendrite-stem d1)
        (dendrite-stem d2))
       (eql
        (car (dendrite-cons d1))
        (car (dendrite-cons d2)))
       (transformation-equal
        (dendrite-transformation d1)
        (dendrite-transformation d2))))

(defmethod convert-cluster
    ((cluster cluster)
     (non-immediate non-immediate))
  (let* ((shape-dendrites-alist (partition-dendrites (cluster-dendrites cluster)))
         ;; Create one buffer for each shape-dendrites-alist entry.
         (buffer-dendrites-alist
           (loop for (shape . dendrites) in shape-dendrites-alist
                 collect
                 (cons
                  (make-buffer
                   :shape shape
                   :ntype (cluster-ntype cluster))
                  dendrites))))
    ;; Emit one load instruction for each dendrite.
    (loop for (buffer . dendrites) in buffer-dendrites-alist do
      (loop for dendrite in dendrites do
        (with-accessors ((cons dendrite-cons)
                         (kernel dendrite-kernel)
                         (transformation dendrite-transformation)) dendrite
          (setf (cdr cons) (make-load-instruction kernel buffer transformation)))))
    ;; Now subdivide the space of all buffers and emit one kernel per
    ;; resulting fragment, plus some copy kernels if the fragment is part
    ;; of the shapes of several buffers.
    (let ((fragments (subdivide-shapes (mapcar #'first shape-dendrites-alist))))
      (loop for (shape . bitmask) in fragments do
        (trivia:ematch (select-masked-elements buffer-dendrites-alist bitmask)
          ((list* (list* main-buffer dendrites) other-entries)
           ;; Check whether the main buffer is potentially superfluous.  If
           ;; so, track this buffer for the later pruning phase.  Also,
           ;; (ab)use the data slot of the buffer to record all its
           ;; dendrites.
           (when (and (null other-entries)
                      (potentially-superfluous-buffer-p main-buffer dendrites))
             (setf (buffer-data main-buffer) dendrites)
             (push main-buffer (ir-converter-potentially-superfluous-buffers *ir-converter*)))
           ;; Ensure that the elements of the main buffer are being computed.
           (grow-dendrite (make-dendrite cluster shape (list main-buffer)) non-immediate)
           ;; Emit copy kernels from the main buffer to all the other
           ;; buffers.
           (loop for (other-buffer . nil) in other-entries do
             (insert-copy-kernel shape other-buffer main-buffer))))))))

;;; This method is quite similar to the one for general non-immediate
;;; arrays, but the differences have proven to be too large to combine both
;;; methods.  The main difference is that we need to allocate buffers for
;;; each of the resulting values of the instruction.
(defmethod convert-cluster
    ((cluster cluster)
     (lazy-multiple-value-map lazy-multiple-value-map))
  ;; During partitioning, we treat all dendrites as if they'd write into
  ;; the same buffer, although in practice the buffer they write to is
  ;; determined by their value number.  The reason is that we want 
  (let* ((shape-dendrites-alist (partition-dendrites (cluster-dendrites cluster)))
         ;; A list of buffer-dendrites alists, one for each value returned
         ;; by the lazy multiple value map instruction.  Each
         ;; buffer-dendrites alist has the same length as the shape
         ;; dendrites alist.  Entries of a buffer-dendrites alist are NIL
         ;; in case the corresponding buffer is never referenced.
         (list-of-buffer-dendrites-alists
           ;; Remember that we abuse the ntype slot of a lazy multiple
           ;; value map to store a list of ntypes - one for each of its
           ;; multiple values.
           (loop for element-ntype in (element-ntype lazy-multiple-value-map)
                 for buffer-ntype = (petalisp.type-inference:generalize-ntype element-ntype)
                 for value-n from 0
                 collect
                 (loop for entry in shape-dendrites-alist
                       for dendrites = (remove value-n (cdr entry)
                                               :key #'dendrite-value-n
                                               :test #'/=)
                       collect
                       (if (null dendrites)
                           nil
                           (cons (make-buffer
                                  :shape (car entry)
                                  :ntype buffer-ntype)
                                 dendrites))))))
    ;; Emit one load instruction for each dendrite.
    (loop for buffer-dendrites-alist in list-of-buffer-dendrites-alists do
      (loop for entry in buffer-dendrites-alist do
        (when (consp entry)
          (loop for dendrite in (cdr entry) do
            (with-accessors ((cons dendrite-cons)
                             (kernel dendrite-kernel)
                             (transformation dendrite-transformation)) dendrite
              (setf (car cons) 0)
              (setf (cdr cons) (make-load-instruction kernel (car entry) transformation)))))))
    ;; Now subdivide the space of all buffers and emit one kernel per
    ;; resulting fragment, plus some copy kernels if the fragment is part
    ;; of the shapes of several buffers.
    (loop for (shape . bitmask) in (subdivide-shapes (mapcar #'first shape-dendrites-alist)) do
      (let* ((list-of-filtered-buffer-dendrites-alists
               (loop for buffer-dendrites-alist in list-of-buffer-dendrites-alists
                     collect
                     (remove nil (select-masked-elements buffer-dendrites-alist bitmask))))
             (main-buffers (mapcar #'caar list-of-filtered-buffer-dendrites-alists))
             (dendrite (make-dendrite cluster shape main-buffers)))
        ;; Check whether the main buffers are potentially superfluous.  If
        ;; so, track them for the later pruning phase.  Also, (ab)use the
        ;; data slot of each main buffer to record all its dendrites.
        (when (loop for buffer-dendrites-alist in list-of-filtered-buffer-dendrites-alists
                    always (and (= 1 (length buffer-dendrites-alist))
                                (potentially-superfluous-buffer-p
                                 (car (first buffer-dendrites-alist))
                                 (cdr (first buffer-dendrites-alist)))))
          (loop for ((main-buffer . dendrites)) in list-of-filtered-buffer-dendrites-alists do
            (setf (buffer-data main-buffer) dendrites)
            (push main-buffer (ir-converter-potentially-superfluous-buffers *ir-converter*))))
        ;; Ensure that the elements of the main buffers are being computed.
        (grow-dendrite dendrite lazy-multiple-value-map)
        ;; Emit copy kernels from the main buffers to all the other
        ;; buffers.
        (loop for buffer-dendrites-alist in list-of-filtered-buffer-dendrites-alists do
          (let ((main-buffer (car (first buffer-dendrites-alist))))
            (loop for (other-buffer . nil) in (rest buffer-dendrites-alist) do
              (insert-copy-kernel shape other-buffer main-buffer))))))))

(defun select-masked-elements (list bitmask)
  (loop for element in list
        for index from 0
        when (logbitp index bitmask)
          collect element))

(defun potentially-superfluous-buffer-p (buffer dendrites)
  (declare (buffer buffer) (list dendrites))
  (and (<= (loop for dendrite in dendrites sum (shape-size (dendrite-shape dendrite)))
           (buffer-size buffer))
       (loop for (d1 . rest) on dendrites
             never
             (loop for d2 in rest
                     thereis
                     (shape-intersectionp (dendrite-shape d1) (dendrite-shape d2))))))

(defun insert-copy-kernel (iteration-space target-buffer source-buffer)
  (let* ((rank (shape-rank iteration-space))
         (transformation (identity-transformation rank))
         (kernel (make-kernel :iteration-space iteration-space)))
    (make-store-instruction
     kernel
     (cons 0 (make-load-instruction kernel source-buffer transformation))
     target-buffer
     transformation)
    kernel))

;;; Compute an alist whose keys are shapes and whose values are dendrites
;;; that are covered by that shape.  Ensure that the dendrites of each
;;; entry cover a reasonable portion of that shape.  We use an empirically
;;; determined heuristic to decide which dendrites are to be grouped in one
;;; entry.
(defun partition-dendrites (dendrites)
  (let ((alist '()))
    (loop for dendrite in dendrites do
      (block convert-one-dendrite
        (let ((dshape (dendrite-shape dendrite)))
          (loop for entry in alist do
            (let* ((eshape (car entry))
                   (cover (fuse-shapes eshape dshape)))
              (when (<= (* (shape-size cover) 0.75)
                        (+ (shape-size dshape)
                           (shape-size eshape)))
                (setf (car entry) cover)
                (push dendrite (cdr entry))
                (return-from convert-one-dendrite)))
                finally (push `(,dshape ,dendrite) alist)))))
    alist))

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
      ;; We employ a trick here - if the multiply referenced non-immediate
      ;; is a lazy multiple value ref, we don't create a cluster for it but
      ;; for its parent.
      (if (typep non-immediate 'lazy-multiple-value-ref)
          (with-accessors ((cons dendrite-cons)) dendrite
            (setf (car cons)
                  (lazy-multiple-value-ref-value-n non-immediate))
            (push dendrite (cluster-dendrites (ensure-cluster (lazy-array-input non-immediate)))))
          (push dendrite (cluster-dendrites (ensure-cluster non-immediate))))
      (call-next-method)))

(defmethod grow-dendrite
    ((dendrite dendrite)
     (lazy-fuse lazy-fuse))
  (with-accessors ((shape dendrite-shape)
                   (transformation dendrite-transformation)
                   (stem dendrite-stem)
                   (kernel dendrite-kernel)
                   (cons dendrite-cons)) dendrite
    (let* ((inputs (lazy-array-inputs lazy-fuse))
           (intersections
             (loop for input in inputs
                   for intersection = (shape-intersection shape (lazy-array-shape input))
                   collect intersection)))
      (case (count-if-not #'empty-shape-p intersections)
        (0 (error "Erroneous fusion."))
        (1 (let ((input (nth (position-if-not #'empty-shape-p intersections) inputs)))
             (grow-dendrite dendrite input)))
        (otherwise
         ;; Invalidate the current kernel and its dendrites.
         (setf (stem-validp stem) nil)
         (delete-kernel kernel)
         ;; Try growing from the cluster again, but with one stem for each
         ;; reachable fusion input.
         (loop for input in inputs
               for intersection in intersections
               unless (empty-shape-p intersection)
                 do (grow-dendrite
                     (make-dendrite
                      (stem-cluster stem)
                      (transform intersection (invert-transformation transformation))
                      (stem-buffers stem))
                     (cluster-lazy-array (stem-cluster stem)))))))))

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
    (grow-dendrite dendrite (lazy-array-input lazy-reshape))))

(defmethod grow-dendrite
    ((dendrite dendrite)
     (lazy-map lazy-map))
  (with-accessors ((shape dendrite-shape)
                   (transformation dendrite-transformation)
                   (cons dendrite-cons)) dendrite
    (let* ((inputs (lazy-array-inputs lazy-map))
           (input-conses (loop for input in inputs collect (cons 0 input))))
      (setf (cdr cons)
            (make-call-instruction
             (lazy-map-number-of-values lazy-map)
             (lazy-map-operator lazy-map)
             input-conses))
      ;; If our function has zero inputs, we are done.  Otherwise we create
      ;; one dendrite for each input and continue growing.
      (loop for input in inputs
            for input-cons in input-conses do
              (let ((new-dendrite (copy-dendrite dendrite)))
                (setf (dendrite-cons new-dendrite) input-cons)
                (grow-dendrite new-dendrite input))))))

(defmethod grow-dendrite
    ((dendrite dendrite)
     (lazy-multiple-value-ref lazy-multiple-value-ref))
  (with-accessors ((cons dendrite-cons)) dendrite
    (setf (car cons)
          (lazy-multiple-value-ref-value-n lazy-multiple-value-ref)))
  (grow-dendrite dendrite (lazy-array-input lazy-multiple-value-ref)))

(defmethod grow-dendrite
    ((dendrite dendrite)
     (array-immediate array-immediate))
  (with-accessors ((shape dendrite-shape)
                   (transformation dendrite-transformation)
                   (stem dendrite-stem)
                   (cons dendrite-cons)) dendrite
    (let* ((kernel (stem-kernel stem))
           (shape (lazy-array-shape array-immediate))
           (storage (array-immediate-storage array-immediate))
           (ntype (petalisp.type-inference:generalize-ntype
                   (element-ntype array-immediate)))
           (buffer
             (if (zerop (shape-rank shape))
                 (alexandria:ensure-gethash
                  (aref (array-immediate-storage array-immediate))
                  (ir-converter-scalar-table *ir-converter*)
                  (make-buffer
                   :shape shape
                   :ntype ntype
                   :storage storage))
                 (alexandria:ensure-gethash
                  (array-immediate-storage array-immediate)
                  (ir-converter-array-table *ir-converter*)
                  (make-buffer
                   :shape shape
                   :ntype ntype
                   :storage storage)))))
      (setf (cdr cons)
            (make-load-instruction kernel buffer transformation)))))

(defmethod grow-dendrite
    ((dendrite dendrite)
     (range-immediate range-immediate))
  (with-accessors ((cons dendrite-cons)
                   (transformation dendrite-transformation)) dendrite
    (setf (cdr cons)
          (make-iref-instruction transformation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffer Pruning
;;;
;;; The IR conversion occasionally inserts superfluous buffers,
;;; i.e. buffers that could be removed altogether by substituting all loads
;;; to it with copies of one of the kernels storing to it.
;;;
;;; We could perform this optimization during cluster conversion, but then
;;; there would be no limit on the size of the generated kernels.  So
;;; instead we delay this pruning step until after the entire graph is
;;; converted, and we stop pruning before kernels grow too large.  To make
;;; most of the limited number of consecutive prunes, we perform the
;;; pruning from the largest prunes to the smallest ones.

;;; Each individual prune describes how a copy of one writer kernel can be
;;; incorporated into one reader kernel.
(defstruct prune
  ;; The number of elements that would be removed by applying that prune.
  (size nil :type unsigned-byte :read-only t)
  ;; An alist where each key is a kernel, and where each value is a list of
  ;; entries of the form (dendrite writer store-instruction).
  (buffers '() :type list :read-only t))

(defun prune-superfluous-buffers (potentially-superfluous-buffers)
  ;; Determine the list of valid prunes, and sort them by size in
  ;; descending order.  This way, later steps automatically start with the
  ;; largest and most promising prunes.
  (mapc
   #'maybe-prune
   (sort (loop for buffer in potentially-superfluous-buffers
               when (find-prune buffer) collect it)
         #'> :key #'prune-size)))

(defvar *prune*)

(defun maybe-prune (prune)
  (let ((*prune* prune)
        (readers '())
        (writers '())
        (substitutes '()))
    ;; Determine all kernels reading from any of the prune's buffers.
    (loop for buffer in (prune-buffers prune) do
      (map-buffer-outputs
       (lambda (reader) (pushnew reader readers))
       buffer))
    ;; Determine all kernels writing to any of the prune's buffers.
    (loop for buffer in (prune-buffers prune) do
      (map-buffer-inputs
       (lambda (writer) (pushnew writer writers))
       buffer))
    ;; For each reader that is not also a writer, we compute a new kernel
    ;; where any reference to a superfluous buffer is replaced by a copy of
    ;; the kernel writing to the referenced part of the superfluous buffer.
    (loop for reader in (set-difference readers writers) do
      (multiple-value-bind (substitute substitute-size)
          (make-substitute-kernel reader)
        (push substitute substitutes)
        ;; If any of the new kernels has grown too large, we give up.
        (when (> substitute-size (ir-converter-kernel-size-threshold *ir-converter*))
          (mapc #'delete-kernel substitutes)
          (return-from maybe-prune))))
    ;; Remove all readers and writers.
    (mapc #'delete-kernel readers)
    (mapc #'delete-kernel writers)))

;;; Returns a prune involving the supplied buffer, or NIL, if there is no
;;; valid prune associated with that buffer.
(defun find-prune (buffer)
  (let (;; An alist where each key is a buffer that may be rendered
        ;; obsolete by the prune, and where the corresponding value is a
        ;; list of all dendrites that have reached that buffer.
        (buffer-dendrites-alist '())
        ;; The sum of the number of elements of all the buffers that are
        ;; rendered obsolete by this prune.
        (size 0))
    ;; Populate the buffer dendrites alist and clear each buffer's data
    ;; slot.
    (labels ((scan-buffer (buffer)
               (when (buffer-data buffer)
                 (push (list* buffer (shiftf (buffer-data buffer) nil))
                       buffer-dendrites-alist)
                 (map-buffer-outputs
                  (lambda (reader)
                    (map-kernel-inputs
                     (lambda (buffer)
                       (scan-buffer buffer))
                     reader))
                  buffer)
                 (map-buffer-inputs
                  (lambda (writer)
                    (map-kernel-outputs
                     (lambda (buffer)
                       (scan-buffer buffer))
                     writer))
                  buffer))))
      (scan-buffer buffer))
    (when (null buffer-dendrites-alist)
      (return-from find-prune nil))
    ;; Ensure that each dendrite has a unique associated writer.
    (loop for (buffer . dendrites) in buffer-dendrites-alist do
      (incf size (buffer-size buffer))
      (loop for dendrite in dendrites do
        (assert (eq buffer (load-instruction-buffer (cdr (dendrite-cons dendrite)))))
        (block check-one-dendrite
          (map-buffer-inputs
           (lambda (writer)
             (map-kernel-store-instructions
              (lambda (store-instruction)
                (when (and (eq (store-instruction-buffer store-instruction) buffer)
                           (subshapep (dendrite-shape dendrite)
                                      (transform (kernel-iteration-space writer)
                                                 (store-instruction-transformation store-instruction))))
                  (return-from check-one-dendrite)))
              writer))
           buffer)
          ;; If we reach this point, it means we haven't found a unique
          ;; writer for this particular dendrite.  Give up.
          (return-from find-prune nil))))
    (make-prune
     :size size
     :buffers
     ;; Mark each buffer that is part of a prune as superfluous.
     (mapcar #'car buffer-dendrites-alist))))

;;; An alist whose keys are of the form (kernel . transformation), and
;;; whose values are hash tables mapping from old instructions to their
;;; copies.
(defvar *instruction-tables*)

;;; A hash table mapping from old instructions to their copies.
(defvar *instruction-table*)

(defun ensure-instruction-table (kernel transformation)
  (loop for (entry . table) in *instruction-tables* do
    (when (and (eq (car entry) kernel)
               (transformation-equal (cdr entry) transformation))
      (return-from ensure-instruction-table table)))
  (let ((instruction-table (make-hash-table :test #'eq)))
    (push (cons (cons kernel transformation) instruction-table)
          *instruction-tables*)
    instruction-table))

(defmacro ensure-instruction-clone (instruction clone-form)
  (alexandria:once-only (instruction)
    `(or (gethash ,instruction *instruction-table*)
         (setf (gethash ,instruction *instruction-table*)
               ,clone-form))))

;;; Create a copy of KERNEL, but where each reference to a superfluous
;;; buffer is replaced by a copy of the instructions of the kernel
;;; producing the elements of that buffer.  Returns the size of the
;;; resulting kernel as a second argument.
(defun make-substitute-kernel (kernel)
  (let* ((*instruction-tables* '())
         (*instruction-table* (make-hash-table :test #'eq))
         (iteration-space (kernel-iteration-space kernel))
         (substitute-kernel (make-kernel :iteration-space iteration-space)))
    (loop for (buffer . store-instructions) in (kernel-targets kernel) do
      (loop for store-instruction in store-instructions do
        (let* ((input (store-instruction-input store-instruction))
               (buffer (store-instruction-buffer store-instruction))
               (transformation (identity-transformation (shape-rank iteration-space))))
          (make-store-instruction
           substitute-kernel
           (clone-reference (car input) (cdr input) substitute-kernel transformation)
           buffer
           (store-instruction-transformation store-instruction)))))
    (values
     substitute-kernel
     (+ (hash-table-count *instruction-table*)
        (loop for (entry . instruction-table) in *instruction-tables*
              sum (hash-table-count instruction-table))))))

;;; Copy an reference to an instruction from another kernel into KERNEL,
;;; using the supplied instruction table to avoid cloning an instruction
;;; twice.
(defgeneric clone-reference (value-n instruction kernel transformation))

(defmethod clone-reference
    ((value-n integer)
     (call-instruction call-instruction)
     (kernel kernel)
     (transformation transformation))
  (cons
   value-n
   (ensure-instruction-clone
    call-instruction
    (make-call-instruction
     (call-instruction-number-of-values call-instruction)
     (call-instruction-operator call-instruction)
     (loop for (value-n . instruction) in (call-instruction-inputs call-instruction)
           collect
           (clone-reference value-n instruction kernel transformation))))))

(defmethod clone-reference
    ((value-n (eql 0))
     (iref-instruction iref-instruction)
     (kernel kernel)
     (transformation transformation))
  (cons
   value-n
   (ensure-instruction-clone
    iref-instruction
    (make-iref-instruction
     (compose-transformations
      (iref-instruction-transformation iref-instruction)
      transformation)))))

(defmethod clone-reference
    ((value-n (eql 0))
     (load-instruction load-instruction)
     (kernel kernel)
     (transformation transformation))
  (let* ((buffer (load-instruction-buffer load-instruction))
         (transformation
           (compose-transformations
            (load-instruction-transformation load-instruction)
            transformation)))
    (if (member buffer (prune-buffers *prune*))
        ;; If the buffer is to be pruned, we replace the load instruction
        ;; by a clone of the matching store instruction.
        (let ((reader-shape (transform (kernel-iteration-space kernel) transformation)))
          (map-buffer-inputs
           (lambda (writer)
             (loop for (store-buffer . store-instructions) in (kernel-targets writer) do
               (when (eq store-buffer buffer)
                 (loop for store-instruction in store-instructions do
                   (when (subshapep
                          reader-shape
                          (transform (kernel-iteration-space writer)
                                     (store-instruction-transformation store-instruction)))
                     (let* ((input (store-instruction-input store-instruction))
                            (transformation
                              (compose-transformations
                               (invert-transformation
                                (store-instruction-transformation store-instruction))
                               transformation))
                            (*instruction-table* (ensure-instruction-table writer transformation)))
                       (return-from clone-reference
                         (clone-reference (car input) (cdr input) kernel transformation))))))))
           buffer)
          (error "Invalid prune:~%~S" *prune*))
        ;; If the buffer is not to be pruned, simply create a new load
        ;; instruction and register it with the kernel and the buffer.
        (cons
         value-n
         (ensure-instruction-clone
          load-instruction
          (make-load-instruction kernel buffer transformation))))))
