;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

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
  ;; A hash table, mapping from unknowns to buffers.
  (unknown-table (make-hash-table :test #'eq) :type hash-table)
  ;; An alist whose entries are conses of leaf buffers and their
  ;; corresponding lazy arrays.
  (leaf-alist '() :type list)
  ;; A list of lists of conses that need to be updated by writing the value
  ;; of the cdr of the first cons to the cdr of each remaining cons.
  (cons-updates '() :type list)
  ;; The maximum size we allow a kernel to grow during buffer pruning.
  (kernel-size-threshold nil :type unsigned-byte :read-only t)
  ;; An list of potentially superfluous buffers, i.e., buffer where all
  ;; dendrites are disjoint and cover the entire buffer.  Cluster
  ;; conversion also makes sure that the data slot of each potentially
  ;; superfluous buffer contains the list of dendrites reaching it.
  (potentially-superfluous-buffers '() :type list)
  ;; A hash table, mapping from potentially superfluous buffers to
  ;; dendrites.
  (buffer-dendrites-table (make-hash-table :test #'eq) :type hash-table))

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
  (typo:ntype-primitive-ntype
   (lazy-array-ntype
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

(defmacro buffer-dendrites (buffer)
  `(values (gethash ,buffer (ir-converter-buffer-dendrites-table *ir-converter*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IR Conversion

(defun ir-from-lazy-arrays (lazy-arrays &key (kernel-size-threshold 32) (debug nil))
  (let ((*ir-converter* (make-ir-converter :kernel-size-threshold kernel-size-threshold))
        (reversed-root-buffers '()))
    ;; Create and grow one dendrite for each root array.
    (loop for lazy-array in lazy-arrays do
      (let* ((cluster (make-cluster lazy-array))
             (shape (lazy-array-shape lazy-array))
             (buffer (make-buffer
                      :shape shape
                      :depth (lazy-array-depth lazy-array)
                      :ntype (typo:ntype-primitive-ntype
                              (lazy-array-ntype lazy-array))))
             (dendrite (make-dendrite cluster shape (list buffer))))
        (push buffer reversed-root-buffers)
        (grow-dendrite dendrite lazy-array)))
    ;; Successively convert all clusters.
    (loop until (ir-converter-empty-p *ir-converter*)
          for cluster = (ir-converter-next-cluster *ir-converter*)
          for lazy-array = (cluster-lazy-array cluster)
          for delayed-action = (lazy-array-delayed-action lazy-array)
          do (convert-cluster cluster lazy-array delayed-action))
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
    (let ((root-buffers (nreverse reversed-root-buffers)))
      (when debug (check-ir root-buffers))
      (finalize-ir root-buffers)
      root-buffers)))

(defun program-from-lazy-arrays (lazy-arrays &key (kernel-size-threshold 32) (debug nil))
  (if (null lazy-arrays)
      (error "Cannot create empty IR programs.")
      (buffer-program
       (first
        (ir-from-lazy-arrays
         lazy-arrays
         :kernel-size-threshold kernel-size-threshold
         :debug debug)))))

(defun finalize-ir (root-buffers)
  (ensure-tasks root-buffers)
  (map-buffers #'finalize-buffer root-buffers)
  (map-kernels #'finalize-kernel root-buffers)
  root-buffers)

(defun finalize-buffer (buffer)
  (setf (buffer-number buffer)
        (program-number-of-buffers (buffer-program buffer)))
  (incf (program-number-of-buffers (buffer-program buffer)))
  (if (interior-buffer-p buffer)
      (let* ((reuse-potential (buffer-reuse-potential buffer))
             (t1 (reuse-optimizing-transformation reuse-potential))
             (t2 (normalizing-transformation (transform-shape (buffer-shape buffer) t1))))
        (transform-buffer buffer (compose-transformations t2 t1)))
      (transform-buffer buffer (collapsing-transformation (buffer-shape buffer)))))

(defun finalize-kernel (kernel)
  (setf (kernel-number kernel)
        (program-number-of-kernels (kernel-program kernel)))
  (incf (program-number-of-kernels (kernel-program kernel)))
  (recompute-kernel-instruction-vector kernel)
  (let* ((reuse-potential (kernel-reuse-potential kernel))
         (t1 (reuse-optimizing-transformation reuse-potential))
         (t2 (normalizing-transformation (transform-shape (kernel-iteration-space kernel) t1))))
    (transform-kernel kernel (compose-transformations t2 t1))))

(defun recompute-kernel-instruction-vector (kernel)
  (let ((magic-fixnum (- most-positive-fixnum 17))
        (size 0))
    (labels ((clear-instruction-number (instruction)
               (unless (eql (instruction-number instruction) magic-fixnum)
                 (setf (instruction-number instruction) magic-fixnum)
                 (incf size)
                 (map-instruction-inputs #'clear-instruction-number instruction))))
      (map-kernel-store-instructions #'clear-instruction-number kernel))
    (let ((vector (make-array size))
          (index 0))
      (labels ((assign-instruction-number (instruction)
                 (when (eq (instruction-number instruction) magic-fixnum)
                   (setf (instruction-number instruction) size)
                   (map-instruction-inputs #'assign-instruction-number instruction)
                   (setf (instruction-number instruction) index)
                   (setf (svref vector index) instruction)
                   (incf index))))
        (map-kernel-store-instructions #'assign-instruction-number kernel))
      (setf (kernel-instruction-vector kernel) vector)
      kernel)))

(defun recompute-program-task-vector (program)
  (let ((magic-fixnum (- most-positive-fixnum 19))
        (size 0))
    (labels ((clear-task-number (task)
               (unless (eql (task-number task) magic-fixnum)
                 (setf (task-number task) magic-fixnum)
                 (incf size)
                 (map-task-predecessors #'clear-task-number task))))
      (clear-task-number (program-final-task program)))
    (let ((vector (make-array size))
          (index 0))
      (labels ((assign-task-number (task)
                 (when (eql (task-number task) magic-fixnum)
                   (setf (task-number task) size)
                   (map-task-predecessors #'assign-task-number task)
                   (setf (task-number task) index)
                   (setf (svref vector index) task)
                   (incf index))))
        (assign-task-number (program-final-task program)))
      (setf (program-task-vector program) vector)
      program)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cluster Conversion

(defgeneric convert-cluster (cluster lazy-array delayed-action))

;;; Under certain circumstances, there is no need to convert the current
;;; cluster at all.  This around method handles these cases.  It also
;;; removes all dendrites that have been invalidated from the cluster.
(defmethod convert-cluster :around
    ((cluster cluster)
     (lazy-array lazy-array)
     (delayed-action delayed-action))
  ;; Since clusters are converted in depth-first order, and since all
  ;; further lazy arrays have a depth less than the current cluster, there
  ;; is no need to keep the current cluster in the cluster table.  No new
  ;; dendrites will ever reach it.
  (remhash lazy-array (ir-converter-cluster-table *ir-converter*))
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
                 (lazy-array-depth lazy-array))
           (grow-dendrite dendrite lazy-array))
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
       (transformation=
        (dendrite-transformation d1)
        (dendrite-transformation d2))))

(defmethod convert-cluster
    ((cluster cluster)
     (lazy-array lazy-array)
     (delayed-action delayed-action))
  (let* ((shape-dendrites-alist (partition-dendrites (cluster-dendrites cluster)))
         (depth (lazy-array-depth lazy-array))
         ;; Create one buffer for each shape-dendrites-alist entry.
         (buffer-dendrites-alist
           (loop for (shape . dendrites) in shape-dendrites-alist
                 collect
                 (cons
                  (make-buffer
                   :shape shape
                   :depth depth
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
             (setf (buffer-dendrites main-buffer) dendrites)
             (push main-buffer (ir-converter-potentially-superfluous-buffers *ir-converter*)))
           ;; Ensure that the elements of the main buffer are being computed.
           (grow-dendrite (make-dendrite cluster shape (list main-buffer)) lazy-array)
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
     (lazy-array lazy-array)
     (delayed-multiple-value-map delayed-multiple-value-map))
  ;; During partitioning, we treat all dendrites as if they'd write into
  ;; the same buffer, although in practice the buffer they write to is
  ;; determined by their value number.  The reason is that we don't want to
  ;; introduce additional fragmentation.  The price is slightly inefficient
  ;; code when some return values of a multiple valued function are used in
  ;; a wildly different way than others.
  (let* ((shape-dendrites-alist (partition-dendrites (cluster-dendrites cluster)))
         (depth (lazy-array-depth lazy-array))
         (values-ntype (delayed-multiple-value-map-values-ntype delayed-multiple-value-map))
         (refbits (delayed-multiple-value-map-refbits delayed-multiple-value-map))
         ;; A list of buffer-dendrites alists, one for each value returned
         ;; by the lazy multiple value map instruction.  Each
         ;; buffer-dendrites alist has the same length as the shape
         ;; dendrites alist.  Entries of a buffer-dendrites alist are NIL
         ;; in case the corresponding buffer is never referenced.
         (list-of-buffer-dendrites-alists
           (loop for value-n below (integer-length refbits)
                 for element-ntype = (typo:values-ntype-nth-value-ntype value-n values-ntype)
                 for buffer-ntype = (typo:ntype-primitive-ntype element-ntype)
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
                                  :depth depth
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
            (setf (buffer-dendrites main-buffer) dendrites)
            (push main-buffer (ir-converter-potentially-superfluous-buffers *ir-converter*))))
        ;; Ensure that the elements of the main buffers are being computed.
        (grow-dendrite dendrite lazy-array)
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

(defun grow-dendrite (dendrite lazy-array)
  (grow-dendrite-aux dendrite lazy-array (lazy-array-delayed-action lazy-array)))

(defgeneric grow-dendrite-aux (dendrite lazy-array delayed-action))

(defmethod grow-dendrite-aux :around
    ((dendrite dendrite)
     (lazy-array lazy-array)
     (delayed-action delayed-action))
  (if (and (< (lazy-array-depth lazy-array)
              (dendrite-depth dendrite))
           (> (lazy-array-refcount lazy-array) 1))
      ;; We employ a trick here - if the multiply referenced lazy-array is
      ;; an nth-value reference, we don't create a cluster for it but for
      ;; its parent.
      (if (delayed-nth-value-p delayed-action)
          (with-accessors ((cons dendrite-cons)) dendrite
            (setf (car cons)
                  (delayed-nth-value-number delayed-action))
            (push dendrite (cluster-dendrites (ensure-cluster (delayed-nth-value-input delayed-action)))))
          (push dendrite (cluster-dendrites (ensure-cluster lazy-array))))
      (call-next-method)))

(defmethod grow-dendrite-aux
    ((dendrite dendrite)
     (lazy-array lazy-array)
     (delayed-fuse delayed-fuse))
  (with-accessors ((shape dendrite-shape)
                   (transformation dendrite-transformation)
                   (stem dendrite-stem)
                   (kernel dendrite-kernel)
                   (cons dendrite-cons)) dendrite
    (let* ((inputs (delayed-fuse-inputs delayed-fuse))
           (intersections
             (loop for input in inputs
                   for intersection = (shape-intersection shape (lazy-array-shape input))
                   collect intersection)))
      (case (count-if-not #'empty-shape-p intersections)
        (0 (error "Erroneous fusion."))
        ;; If only a single input intersects with the dendrite's shape, we
        ;; can continue growing along that input.
        (1 (let ((input (nth (position-if-not #'empty-shape-p intersections) inputs)))
             (grow-dendrite dendrite input)))
        ;; In case the dendrite's shape intersects with more than one
        ;; input, we project each of these intersections back to the stem
        ;; and retry with this projected shape.  By construction, this new
        ;; shape will only reach a single input of this fusion.
        (otherwise
         ;; Invalidate the current kernel and its dendrites.
         (setf (stem-validp stem) nil)
         (delete-kernel kernel)
         ;; Try growing from the cluster again, but with one stem for each
         ;; reachable fusion input.
         (let* ((cluster (stem-cluster stem))
                (lazy-array (cluster-lazy-array cluster)))
           (loop for input in inputs
                 for intersection in intersections
                 unless (empty-shape-p intersection)
                   do (grow-dendrite
                       (make-dendrite
                        cluster
                        (transform-shape intersection (invert-transformation transformation))
                        (stem-buffers stem))
                       lazy-array))))))))

(defmethod grow-dendrite-aux
    ((dendrite dendrite)
     (lazy-array lazy-array)
     (delayed-reshape delayed-reshape))
  (with-accessors ((shape dendrite-shape)
                   (transformation dendrite-transformation)) dendrite
    (setf shape (transform-shape
                 (shape-intersection shape (lazy-array-shape lazy-array))
                 (delayed-reshape-transformation delayed-reshape)))
    (setf transformation (compose-transformations
                          (delayed-reshape-transformation delayed-reshape)
                          transformation))
    (grow-dendrite dendrite (delayed-reshape-input delayed-reshape))))

(defmethod grow-dendrite-aux
    ((dendrite dendrite)
     (lazy-array lazy-array)
     (delayed-map delayed-map))
  (with-accessors ((shape dendrite-shape)
                   (transformation dendrite-transformation)
                   (cons dendrite-cons)) dendrite
    (let* ((inputs (delayed-map-inputs delayed-map))
           (input-conses (loop for input in inputs collect (cons 0 input))))
      (setf (cdr cons)
            (make-call-instruction
             (delayed-map-number-of-values delayed-map)
             (delayed-map-fnrecord delayed-map)
             input-conses))
      ;; If our function has zero inputs, we are done.  Otherwise we create
      ;; one dendrite for each input and continue growing.
      (loop for input in inputs
            for input-cons in input-conses do
              (let ((new-dendrite (copy-dendrite dendrite)))
                (setf (dendrite-cons new-dendrite) input-cons)
                (grow-dendrite new-dendrite input))))))

(defmethod grow-dendrite-aux
    ((dendrite dendrite)
     (lazy-array lazy-array)
     (delayed-nth-value delayed-nth-value))
  (with-accessors ((cons dendrite-cons)) dendrite
    (with-accessors ((input delayed-nth-value-input)
                     (number delayed-nth-value-number)) delayed-nth-value
      (setf (car cons) number)
      (grow-dendrite dendrite input))))

(defmethod grow-dendrite-aux
    ((dendrite dendrite)
     (lazy-array lazy-array)
     (delayed-array delayed-array))
  (with-accessors ((shape dendrite-shape)
                   (transformation dendrite-transformation)
                   (stem dendrite-stem)
                   (cons dendrite-cons)) dendrite
    (let* ((kernel (stem-kernel stem))
           (shape (lazy-array-shape lazy-array))
           (storage (delayed-array-storage delayed-array))
           (depth (lazy-array-depth lazy-array))
           (ntype (typo:ntype-primitive-ntype
                   (lazy-array-ntype lazy-array))))
      (multiple-value-bind (buffer reusedp)
          (if (zerop (shape-rank shape))
              (alexandria:ensure-gethash
               (aref (delayed-array-storage delayed-array))
               (ir-converter-scalar-table *ir-converter*)
               (make-buffer
                :shape shape
                :ntype ntype
                :depth depth
                :storage storage))
              (alexandria:ensure-gethash
               (delayed-array-storage delayed-array)
               (ir-converter-array-table *ir-converter*)
               (make-buffer
                :shape shape
                :ntype ntype
                :depth depth
                :storage storage)))
        (when (not reusedp)
          (push (cons buffer lazy-array)
                (ir-converter-leaf-alist *ir-converter*)))
        (setf (cdr cons)
              (make-load-instruction kernel buffer transformation))))))

(defmethod grow-dendrite-aux
    ((dendrite dendrite)
     (lazy-array lazy-array)
     (delayed-range delayed-range))
  (with-accessors ((cons dendrite-cons)
                   (transformation dendrite-transformation)) dendrite
    (setf (cdr cons)
          (make-iref-instruction transformation))))

(defmethod grow-dendrite-aux
    ((dendrite dendrite)
     (lazy-array lazy-array)
     (delayed-unknown delayed-unknown))
  (with-accessors ((shape dendrite-shape)
                   (transformation dendrite-transformation)
                   (stem dendrite-stem)
                   (cons dendrite-cons)) dendrite
    (let* ((kernel (stem-kernel stem))
           (shape (lazy-array-shape lazy-array))
           (depth (lazy-array-depth lazy-array))
           (ntype (typo:ntype-primitive-ntype
                   (lazy-array-ntype lazy-array))))
      (multiple-value-bind (buffer reusedp)
          (alexandria:ensure-gethash
           lazy-array
           (ir-converter-unknown-table *ir-converter*)
           (make-buffer
            :shape shape
            :ntype ntype
            :depth depth))
        (when (not reusedp)
          (push (cons buffer lazy-array)
                (ir-converter-leaf-alist *ir-converter*)))
        (setf (cdr cons)
              (make-load-instruction kernel buffer transformation))))))

(defun reference-to-delayed-unknown ()
  (error "Attempt to evaluate an unknown lazy array."))

(defmethod grow-dendrite-aux
    ((dendrite dendrite)
     (lazy-array lazy-array)
     (delayed-wait delayed-wait))
  (grow-dendrite-aux dendrite lazy-array (delayed-wait-delayed-action delayed-wait)))

(defmethod grow-dendrite-aux
    ((dendrite dendrite)
     (lazy-array lazy-array)
     (delayed-failure delayed-failure))
  (error (delayed-failure-condition delayed-failure)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Task Partitioning
;;;
;;; This pass takes an IR and ensures that each buffer and kernel therein
;;; has an associated task, and that the successors and predecessors of
;;; each task are set up correctly.

(defun ensure-tasks (root-buffers)
  ;; Ensure that each kernel and buffer has a task.
  (let* ((program (make-program :leaf-alist (ir-converter-leaf-alist *ir-converter*)
                                :root-buffers root-buffers))
         (initial-task (make-task :program program))
         (final-task (make-task :program program))
         (root-tasks '()))
    (setf (program-initial-task program) initial-task)
    (setf (program-final-task program) final-task)
    (let ((worklist root-buffers))
      ;; Assign tasks to buffers.
      (loop until (null worklist) for buffer = (pop worklist) do
        (when (null (buffer-task buffer))
          (if (leaf-buffer-p buffer)
              (unless (buffer-task buffer)
                (setf (buffer-task buffer) initial-task)
                (push buffer (task-defined-buffers initial-task)))
              (do-task-kernels (kernel (ensure-buffer-task buffer program))
                (do-kernel-inputs (buffer kernel)
                  (when (null (buffer-task buffer))
                    (push buffer worklist))))))))
    ;; Determie the root tasks.
    (loop for root-buffer in root-buffers do
      (pushnew (buffer-task root-buffer) root-tasks))
    ;; Determine the predecessors and successors of each task.
    (let ((worklist root-tasks))
      (loop until (null worklist) for task = (pop worklist) do
        ;; A task with successors has already been processed.
        (unless (task-successors task)
          ;; Determine all successors.
          (do-task-defined-buffers (buffer task)
            (do-buffer-outputs (kernel buffer)
              (let ((successor (kernel-task kernel)))
                (unless (eq successor task)
                  (pushnew successor (task-successors task))))))
          ;; A task with zero successors is a predecessor of the final
          ;; task.
          (when (null (task-successors task))
            (push task (task-predecessors final-task))
            (push final-task (task-successors task)))
          ;; Determine all predecessors.
          (do-task-kernels (kernel task)
            (do-kernel-inputs (buffer kernel)
              (let ((predecessor (buffer-task buffer)))
                (unless (eq predecessor task)
                  (pushnew predecessor (task-predecessors task))))))
          ;; Enqueue all predecessors that haven't been processed so far.
          (loop for predecessor in (task-predecessors task) do
            (unless (task-successors predecessor)
              (push predecessor worklist))))))
    ;; Finish by computing the task vector.
    (recompute-program-task-vector program)
    ;; Populate the leaf buffer alist.
    program))

(defun ensure-buffer-task (buffer program)
  (let ((task (make-task :program program))
        (max-depth (buffer-depth buffer))
        (kernel-worklist '())
        (buffer-worklist (list buffer)))
    (loop until (and (null buffer-worklist) (null kernel-worklist)) do
      (loop until (and (null buffer-worklist) (null kernel-worklist)) do
        (loop until (null buffer-worklist) for buffer = (pop buffer-worklist) do
          (unless (eq (buffer-task buffer) task)
            (unless (null (buffer-task buffer))
              (error "Attempt to assign a task to a buffer that already has a task."))
            (setf (buffer-task buffer) task)
            (push buffer (task-defined-buffers task))
            (setf max-depth (max max-depth (buffer-depth buffer)))
            (do-buffer-inputs (kernel buffer)
              (pushnew kernel kernel-worklist))))
        (loop until (null kernel-worklist) for kernel = (pop kernel-worklist) do
          (unless (eq (kernel-task kernel) task)
            (unless (null (kernel-task kernel))
              (error "Attempt to assign a task to a kernel that already has a task."))
            (setf (kernel-task kernel) task)
            (push kernel (task-kernels task))
            (do-kernel-outputs (buffer kernel)
              (pushnew buffer buffer-worklist)))))
      ;; Compute the event horizon of all buffers in this task, and ensure
      ;; that any buffer in the event horizon that also appears as an input
      ;; of any of the task's kernels is also added to the task.
      (let ((event-horizon (event-horizon (task-defined-buffers task) max-depth)))
        (loop for kernel in (task-kernels task) do
          (do-kernel-inputs (buffer kernel)
            (when (member buffer event-horizon)
               (push buffer buffer-worklist))))))
    task))

;;; The event horizon of a set of buffers are all buffers that depend,
;;; directly or indirectly, on the contents of those buffers and that have
;;; less than the supplied depth.
(defun event-horizon (buffers depth)
  (let ((result '())
        (worklist buffers))
    (loop until (null worklist) for buffer = (pop worklist) do
      (when (< (buffer-depth buffer) depth)
        (unless (member buffer result)
          (push buffer result)
          (do-buffer-outputs (kernel buffer)
            (do-kernel-outputs (buffer kernel)
              (unless (< (buffer-depth buffer) depth)
                (pushnew buffer worklist)))))))
    result))

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
  ;; The number of bits of memory that are freed by applying this prune.
  (bits nil :type unsigned-byte :read-only t)
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
         #'> :key #'prune-bits)))

(defvar *prune*)

(defun maybe-prune (prune)
  (let ((*prune* prune)
        (readers '())
        (writers '())
        (substitutes '()))
    ;; Determine all kernels writing to any of the prune's buffers.
    (loop for buffer in (prune-buffers prune) do
      (do-buffer-inputs (writer buffer)
        ;; Abort if a writer is writing to more than one of the prune's
        ;; buffers.  Without this step, we'd introduce redundant calculations
        ;; by copying all the instructions of one kernel into multiple other
        ;; kernels.
        (unless (member writer writers)
          (push writer writers)
          (let ((n-targets 0))
            (do-kernel-outputs (buffer writer)
              (when (member buffer (prune-buffers prune))
                (when (< 1 (incf n-targets))
                  (return-from maybe-prune))))))))
    ;; Determine all kernels reading from any of the prune's buffers.
    (loop for buffer in (prune-buffers prune) do
      (do-buffer-outputs (reader buffer)
        (pushnew reader readers)))
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
        (buffer-dendrites-alist (collect-prune-buffer-dendrites-alist buffer))
        ;; The amount of bits of memory freed by applying this prune.
        (bits 0))
    (when (null buffer-dendrites-alist)
      (return-from find-prune nil))
    ;; Ensure that each dendrite has a unique associated writer.
    (loop for (buffer . dendrites) in buffer-dendrites-alist do
      (incf bits (buffer-bits buffer))
      (loop for dendrite in dendrites do
        (assert (eq buffer (load-instruction-buffer (cdr (dendrite-cons dendrite)))))
        (block check-one-dendrite
          (do-buffer-inputs (writer buffer)
            (do-kernel-store-instructions (store-instruction writer)
              (when (and (eq (store-instruction-buffer store-instruction) buffer)
                         (subshapep (dendrite-shape dendrite)
                                    (transform-shape
                                     (kernel-iteration-space writer)
                                     (store-instruction-transformation store-instruction))))
                (return-from check-one-dendrite))))
          ;; If we reach this point, it means we haven't found a unique
          ;; writer for this particular dendrite.  Give up.
          (return-from find-prune nil))))
    (make-prune
     :bits bits
     :buffers
     ;; Mark each buffer that is part of a prune as superfluous.
     (mapcar #'car buffer-dendrites-alist))))

;; Collect the buffer dendrites alist for the current buffer and all
;; potentially superfluous buffers that have a chain of neighbors
;; connecting them to the current buffer.  While collecting, clear the data
;; slot of each scanned buffer.  (Recall that the data slot stores all
;; dendrites into that buffer if and only if that buffer is potentially
;; superfluous.)
(defun collect-prune-buffer-dendrites-alist (buffer)
  (let ((buffer-dendrites-alist '()))
    (labels ((scan-buffer (buffer)
               (when (buffer-dendrites buffer)
                 (push (list* buffer (shiftf (buffer-dendrites buffer) nil))
                       buffer-dendrites-alist)
                 (scan-neighbors buffer)))
             (scan-neighbors (buffer)
               ;; Scan all buffers that are read by a kernel that also
               ;; reads the current buffer.
               (do-buffer-outputs (reader buffer)
                 (map-kernel-inputs #'scan-buffer reader))
               ;; Scan all buffers that are written to by a kernel that
               ;; also writes to the current buffer.
               (do-buffer-inputs (writer buffer)
                 (map-kernel-outputs #'scan-buffer writer))))
      (scan-buffer buffer))
    buffer-dendrites-alist))

;;; An alist whose keys are of the form (kernel . transformation), and
;;; whose values are hash tables mapping from old instructions to their
;;; copies.
(defvar *instruction-tables*)

;;; A hash table mapping from old instructions to their copies.
(defvar *instruction-table*)

(defun ensure-instruction-table (kernel transformation)
  (loop for (entry . table) in *instruction-tables* do
    (when (and (eq (car entry) kernel)
               (transformation= (cdr entry) transformation))
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
    (loop for (buffer . stencils) in (kernel-targets kernel) do
      (loop for stencil in stencils do
        (loop for store-instruction in (stencil-instructions stencil) do
          (let* ((input (store-instruction-input store-instruction))
                 (buffer (store-instruction-buffer store-instruction))
                 (transformation (identity-transformation (shape-rank iteration-space))))
            (make-store-instruction
             substitute-kernel
             (clone-reference (car input) (cdr input) substitute-kernel transformation)
             buffer
             (store-instruction-transformation store-instruction))))))
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
     (call-instruction-fnrecord call-instruction)
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
        (let ((reader-shape (transform-shape (kernel-iteration-space kernel) transformation)))
          (do-buffer-inputs (writer buffer)
            (loop for (store-buffer . stencils) in (kernel-targets writer) do
              (when (eq store-buffer buffer)
                (loop for stencil in stencils do
                  (loop for store-instruction in (stencil-instructions stencil) do
                    (when (subshapep
                           reader-shape
                           (transform-shape (kernel-iteration-space writer)
                                            (store-instruction-transformation store-instruction)))
                      (let* ((input (store-instruction-input store-instruction))
                             (transformation
                               (compose-transformations
                                (invert-transformation
                                 (store-instruction-transformation store-instruction))
                                transformation))
                             (*instruction-table* (ensure-instruction-table writer transformation)))
                        (return-from clone-reference
                          (clone-reference (car input) (cdr input) kernel transformation)))))))))
          (error "Invalid prune:~%~S" *prune*))
        ;; If the buffer is not to be pruned, simply create a new load
        ;; instruction and register it with the kernel and the buffer.
        (cons
         value-n
         (ensure-instruction-clone
          load-instruction
          (make-load-instruction kernel buffer transformation))))))
