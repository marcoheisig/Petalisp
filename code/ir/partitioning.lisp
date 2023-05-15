;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;; In this file, we partition buffers and kernels into shards.  The goal is
;;; that the cost of each shard is below a certain user-supplied threshold.  By
;;; definition, kernels have no data dependencies and can be split however we
;;; like, so the main challenge is to partition buffers in a way that respects
;;; all access patterns, maximizes data locality, and minimizes the amount of
;;; ghost layers.  There are also be cases where buffers cannot be split at
;;; all, e.g., when a vector appears twice as input of a particular kernel, and
;;; the Ith iteration of that kernel accesses both the Ith element of the
;;; vector and the END-Ith element.
;;;
;;; Initially, the partitioning algorithm annotates each buffer with a
;;; specification that expresses along which axes it can be split, and what
;;; ghost layers would have to be introduced so that the same split can later
;;; be used to split kernels with stencils that reference this buffer.  Then,
;;; also during initialization, each kernel and buffer is assigned a
;;; corresponding initial kernel shard and buffer shard, and all buffer shards
;;; whose cost is above a threshold are placed in a priority queue.  Then,
;;; until that priority queue is empty, the partitioning algorithm picks the
;;; costliest buffer shard from the queue, splits it in the most suitable axis
;;; and position, and then propagates that split.
;;;
;;; A split is propagated by placing it in another priority queue that
;;; prioritizes each split by the cost of the buffer shard it splits. Then,
;;; until that queue of splits is empty, one split is taken and transformed to
;;; the iteration space of any kernel shard reading and writing to that split's
;;; buffer shard, and from there to the buffer shards being referenced by those
;;; kernel shards.  Any buffer shard that is being reached this way and that
;;; fulfills certain user-supplied criteria is split, and the new split is then
;;; placed in the priority queue of splits so that it will be propagated
;;; further.
;;;
;;; The purpose of propagating splits this way is that subsequent buffer and
;;; kernel shards have good data locality and qualify for later being scheduled
;;; on the same worker.
;;;
;;; The final phase of the partitioning is to assign memory layouts to all
;;; buffer shards that are being referenced by at least one kernel shard.  Each
;;; layout also has a reference to some storage, which is handed out such that
;;; each buffer shard that is being referenced and that has an ancestor that is
;;; also being referenced has the same storage as that ancestor.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reasoning about Memory Layouts

(defstruct (storage
            (:predicate storagep)
            (:constructor make-storage))
  ;; The ntype of the elements of the storage.
  (ntype nil :type typo:ntype :read-only t)
  ;; The number of elements being stored.
  (size nil :type unsigned-byte :read-only t)
  ;; An opaque object that encodes how the storage is being allocated.
  (allocation nil))

(defstruct (layout
            (:predicate layoutp)
            (:constructor make-layout))
  ;; The index of the first storage element referenced by this layout.
  (offset 0 :type unsigned-byte :read-only t)
  ;; A vector whose Ith entry is the number of elements to skip when changing
  ;; the Ith index component of an index by one.
  (strides nil :type (simple-array unsigned-byte (*)) :read-only t)
  ;; The storage being accessed.
  (storage nil :type storage :read-only t))

(defun layout-rank (layout)
  (declare (layout layout))
  (length (layout-strides layout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Kernel and Buffer Shards

(defstruct (kernel-shard (:constructor %make-kernel-shard))
  ;; The kernel being partitioned by this kernel shard.
  (kernel nil :type kernel :read-only t)
  ;; The part of the kernel iteration space assigned to this shard.
  (iteration-space nil :type shape :read-only t)
  ;; One buffer shard for each of the kernel's target buffers.
  (targets '() :type list)
  ;; One buffer shard for each of the kernel's source buffers.
  (sources '() :type list))

(defmethod print-object ((kernel-shard kernel-shard) stream)
  (format stream "~@<#<~;~S ~_~@{~S ~:_~S~^ ~_~}~;>~:>"
          (class-name (class-of kernel-shard))
          :kernel (kernel-shard-kernel kernel-shard)
          :iteration-space (kernel-shard-iteration-space kernel-shard)))

;; This comparison function is used to sort the readers and writers slot of
;; each buffer shard, so that more important kernels are considered first when
;; propagating a split.
(defun kernel-shard-more-important-p (kernel-shard-1 kernel-shard-2)
  (> (shape-size (kernel-shard-iteration-space kernel-shard-1))
     (shape-size (kernel-shard-iteration-space kernel-shard-2))))

(defstruct buffer-shard
  ;; The buffer being partitioned by this buffer shard.
  (buffer nil :type buffer :read-only t)
  ;; The buffer shard that was split to create this one.
  (parent nil :type (or null buffer-shard) :read-only t)
  ;; The shard of the buffer-shard's buffer's shape that is exclusively owned by
  ;; that buffer-shard and its children.
  (domain nil :type shape :read-only t)
  ;; The buffer-shard's domain, padded with ghost layers.
  (shape nil :type shape :read-only t)
  ;; A list of kernel shards that read from this buffer shard.
  (readers '() :type list)
  ;; A list of kernel shards that write into this buffer shard.
  (writers '() :type list)
  ;; A cache for the buffer-shard's split priority.
  (split-priority-cache nil :type (or null unsigned-byte))
  ;; The split operation in case this buffer-shard has been split, or NIL, if it
  ;; hasn't been split so far.
  (split nil :type (or null structure-object))
  ;; The memory layout of the buffer shard.
  (layout nil :type (or null layout)))

(defmethod print-object ((buffer-shard buffer-shard) stream)
  (format stream "~@<#<~;~S ~_~@{~S ~:_~S~^ ~_~}~;>~:>"
          (class-name (class-of buffer-shard))
          :domain (buffer-shard-domain buffer-shard)
          :buffer (buffer-shard-buffer buffer-shard)
          :split (buffer-shard-split buffer-shard)))

(defun buffer-shard-bits (buffer-shard)
  (declare (buffer-shard buffer-shard))
  (* (typo:ntype-bits (buffer-ntype (buffer-shard-buffer buffer-shard)))
     (shape-size (buffer-shard-shape buffer-shard))))

(defstruct (split (:predicate splitp))
  ;; The axis at which the buffer-shard is split.
  (axis nil :type axis :read-only t)
  ;; The lowest element of the right child's range in the axis being split.
  (position nil :type integer :read-only t)
  ;; The left child of the split.
  (left-child nil :type buffer-shard :read-only t)
  ;; The right child of the split.
  (right-child nil :type buffer-shard :read-only t))

(defmethod print-object ((split split) stream)
  (format stream "~@<#<~;~S ~_~@{~S ~:_~S~^ ~_~}~;>~:>"
          (class-name (class-of split))
          :axis (split-axis split)
          :position (split-position split)
          :left-child (split-left-child split)
          :right-child (split-right-child split)))

(defun split-parent (split)
  (declare (split split))
  (the (values buffer-shard &optional)
       (buffer-shard-parent (split-left-child split))))

(defun buffer-shard-add-reader (buffer-shard kernel-shard)
  ;; TODO sort
  (push kernel-shard (buffer-shard-readers buffer-shard)))

(defun buffer-shard-add-writer (buffer-shard kernel-shard)
  ;; TODO sort
  (push kernel-shard (buffer-shard-writers buffer-shard)))

(defun make-kernel-shard
    (&key kernel iteration-space targets sources)
  "Returns a new kernel shard from the supplied keyword arguments.  Ensure that
this kernel shard appears in the list of writers of all of its target buffer
shards, and in the list of readers of all its source buffer shards."
  (let ((kernel-shard
          (%make-kernel-shard
           :kernel kernel
           :iteration-space iteration-space
           :targets targets
           :sources sources)))
    (dolist (buffer-shard targets)
      (buffer-shard-add-writer buffer-shard kernel-shard))
    (dolist (buffer-shard sources)
      (buffer-shard-add-reader buffer-shard kernel-shard))
    kernel-shard))

(defun compute-program-primogenitor-buffer-shard-vector (program)
  "Returns a vector whose Nth entry is the primogenitor buffer shard
corresponding to the buffer with number N."
  (let* ((nbuffers (program-number-of-buffers program))
         (nkernels (program-number-of-kernels program))
         (buffer-shards (make-array nbuffers :initial-element nil))
         (kernel-shards (make-array nkernels :initial-element nil)))
    (flet ((buffer-shard (buffer)
             (svref buffer-shards (buffer-number buffer)))
           ((setf buffer-shard) (value buffer)
             (setf (svref buffer-shards (buffer-number buffer)) value))
           ((setf kernel-shard) (value kernel)
             (setf (svref kernel-shards (kernel-number kernel)) value)))
      ;; Create the primogenitor buffer shard of each buffer.
      (do-program-buffers (buffer program)
        (setf (buffer-shard buffer)
              (make-buffer-shard
               :buffer buffer
               :domain (buffer-shape buffer)
               :shape (buffer-shape buffer))))
      ;; Create the initial kernel shard of each kernel.
      (do-program-kernels (kernel program)
        (setf (kernel-shard kernel)
              (make-kernel-shard
               :kernel kernel
               :iteration-space (kernel-iteration-space kernel)
               :targets
               (petalisp.utilities:with-collectors ((targets collect-target))
                 (do-kernel-outputs (target kernel (targets))
                   (collect-target (buffer-shard target))))
               :sources
               (petalisp.utilities:with-collectors ((sources collect-source))
                 (do-kernel-inputs (source kernel (sources))
                   (collect-source (buffer-shard source)))))))
      buffer-shards)))

(defun buffer-shard-infants (buffer-shard)
  "An infant is a buffer-shard without a split.  This function returns the list
of infants whose line of ancestry contains the supplied buffer shard."
  (let ((split (buffer-shard-split buffer-shard)))
    (if (not split)
        (list buffer-shard)
        (append
         (buffer-shard-infants (split-left-child split))
         (buffer-shard-infants (split-right-child split))))))

(defun buffer-shard-chains (buffer-shard)
  "A list of all chains of buffer shards writing to buffer shard and its
children.  Useful for debugging."
  (declare (buffer-shard buffer-shard))
  (labels ((chain-elements (buffer-shard)
             (list*
              buffer-shard
              (let ((kernel-shards (buffer-shard-writers buffer-shard)))
                (if (not kernel-shards)
                    '()
                     (chain-elements
                      ;; Only follow the largest buffer-shard.
                      (let* ((kernel-shard (first kernel-shards))
                             (buffer-shards (kernel-shard-sources kernel-shard)))
                        (first (sort (copy-list buffer-shards) #'>
                                     :key #'buffer-shard-bits)))))))))
    (list*
     (chain-elements buffer-shard)
     (if (not (buffer-shard-split buffer-shard))
         '()
          (append
           (buffer-shard-chains (split-left-child (buffer-shard-split buffer-shard)))
           (buffer-shard-chains (split-right-child (buffer-shard-split buffer-shard))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reasoning About Ghost Layers

(defstruct (ghostspec
            (:predicate ghostspecp)
            (:constructor make-ghostspec))
  ;; A vector with one entry per axis, denoting how many layers must be copied
  ;; on the left of the right child of a split buffer-shard.  An entry of NIL
  ;; means there is no way to split that axis.
  (left-padding-vec nil :type simple-vector :read-only t)
  ;; A vector with the same semantics as the left padding, except that it
  ;; denotes the number of layers on the right of the left child.
  (right-padding-vec nil :type simple-vector :read-only t))

(defun ghostspec-left-padding (ghostspec axis)
  (svref (ghostspec-left-padding-vec ghostspec) axis))

(defun ghostspec-right-padding (ghostspec axis)
  (svref (ghostspec-right-padding-vec ghostspec) axis))

(define-modify-macro maxf (&rest numbers) max)

(defun compute-buffer-ghostspec (buffer)
  (let* ((shape (buffer-shape buffer))
         (rank (shape-rank shape))
         (ranges (shape-ranges shape))
         (left-padding-vec (make-array rank :initial-element 0))
         (right-padding-vec (make-array rank :initial-element 0)))
    (loop for axis below rank for range in ranges do
      (symbol-macrolet ((left-padding (aref left-padding-vec axis))
                        (right-padding (aref left-padding-vec axis)))
        (block check-axis
          (flet ((give-up ()
                   (setf left-padding nil)
                   (setf right-padding nil)
                   (return-from check-axis)))
            (macrolet ((update (v1 v2)
                         (alexandria:once-only (v2)
                           `(if (eql ,v1 :initial)
                                (setf ,v1 ,v2)
                                (unless (eql ,v1 ,v2) (give-up))))))
              ;; Don't split axes with less than two elements.
              (when (< (range-size range) 2) (give-up))
              ;; Check whether each kernel writing to BUFFER can be split at
              ;; the current AXIS.  The only time where this is not the case is
              ;; when a kernel has multiple writes to that buffer and they
              ;; differ in their permutation, scaling, or offset.
              (do-buffer-inputs (kernel buffer)
                (let ((p :initial) (s :initial) (o :initial))
                  (do-kernel-store-instructions (store-instruction kernel)
                    (let ((transformation (store-instruction-transformation store-instruction)))
                      (update p (aref (transformation-output-mask transformation) axis))
                      (update s (aref (transformation-scalings transformation) axis))
                      (update o (aref (transformation-offsets transformation) axis))))))
              ;; Now check whether each kernel reading from BUFFER can be split
              ;; at the current axis, assuming a sufficient number of ghost
              ;; layers is introduced.
              (do-buffer-outputs (kernel buffer)
                (let ((p :initial) (s :initial) (c :initial))
                  (dolist (stencil (kernel-stencils kernel buffer))
                    (update p (aref (stencil-output-mask stencil) axis))
                    (update s (aref (stencil-scalings stencil) axis))
                    (update c (aref (stencil-center stencil) axis))
                    (do-stencil-load-instructions (load-instruction stencil)
                      (let* ((transformation (load-instruction-transformation load-instruction))
                             (delta (- (aref (transformation-offsets transformation) axis) c)))
                        (cond ((plusp delta)
                               (maxf right-padding delta))
                              ((minusp delta)
                               (maxf left-padding (- delta))))))))))))))
    (make-ghostspec
     :left-padding-vec left-padding-vec
     :right-padding-vec right-padding-vec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reasoning About Buffer Shard Neighbors

(defstruct (vicinity
            (:predicate vicinityp))
  (left-neighbors-vec nil :type simple-vector :read-only t)
  (right-neighbors-vec nil :type simple-vector :read-only t))

(defun vicinity-left-neighbors (vicinity axis)
  (svref (vicinity-left-neighbors-vec vicinity) axis))

(defun vicinity-right-neighbors (vicinity axis)
  (svref (vicinity-right-neighbors-vec vicinity) axis))

(defun compute-buffer-shard-vicinity (buffer-shard)
  (declare (buffer-shard buffer-shard))
  (let* ((shape (buffer-shard-shape buffer-shard))
         (rank (shape-rank shape))
         (left-neighbors-vec (make-array rank :initial-element '()))
         (right-neighbors-vec (make-array rank :initial-element '())))
    (labels (;; The other child of the closest ancestor that has a split at
             ;; AXIS with BUFFER-SHARD as a child of the supplied SIDE.
             (sibling (buffer-shard axis side)
               (let ((parent (buffer-shard-parent buffer-shard)))
                 (if (not parent)
                     nil
                     (let* ((split (buffer-shard-split parent))
                            (left-child (split-left-child split))
                            (right-child (split-right-child split)))
                       (if (and (= axis (split-axis split))
                                (eq buffer-shard
                                    (ecase side
                                      (:left right-child)
                                      (:right left-child))))
                           (ecase side
                             (:left left-child)
                             (:right right-child))
                           (sibling parent axis side))))))
             ;; Find all children of ANCESTOR that appear on the specified
             ;; SIDE of each split at AXIS, and that intersect with SHAPE
             ;; in every other axis.
             (relevant-children (ancestor axis side)
               (if (not ancestor)
                   '()
                   (let ((split (buffer-shard-split ancestor)))
                     (if (not split)
                         (list ancestor)
                         (let ((split-axis (split-axis split))
                               (left-child (split-left-child split))
                               (right-child (split-right-child split)))
                           (if (= split-axis axis)
                               (relevant-children
                                (ecase side
                                  (:left left-child)
                                  (:right right-child))
                                axis
                                side)
                               (append
                                (when (relevant-child-p left-child split-axis)
                                  (relevant-children left-child axis side))
                                (when (relevant-child-p right-child split-axis)
                                  (relevant-children right-child axis side)))))))))
             ;; Returns whether CHILD's range in an AXIS coincides with any
             ;; shard of the shape of the buffer-shard whose vicinity we're
             ;; computing.
             (relevant-child-p (child axis)
               (range-intersectionp
                (shape-range shape axis)
                (shape-range (buffer-shard-domain child) axis))))
      (loop for axis below rank do
        (setf (svref left-neighbors-vec axis)
              (relevant-children (sibling buffer-shard axis :left) axis :right))
        (setf (svref right-neighbors-vec axis)
              (relevant-children (sibling buffer-shard axis :right) axis :left)))
      (make-vicinity
       :left-neighbors-vec left-neighbors-vec
       :right-neighbors-vec right-neighbors-vec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Partitioning

;;; The vector that contains the primogenitor shard of each buffer.
(defvar *primogenitor-buffer-shard-vector*)

(defun primogenitor-buffer-shard (buffer)
  (declare (buffer buffer))
  (the (values buffer-shard &optional)
       (svref *primogenitor-buffer-shard-vector* (buffer-number buffer))))

;;; The function for computing the split priority of a buffer-shard that was
;;; supplied to the partitioning operation.
(defvar *buffer-shard-split-priority*)

(defun buffer-shard-split-priority (buffer-shard)
  (declare (buffer-shard buffer-shard))
  (or (buffer-shard-split-priority-cache buffer-shard)
      (setf (buffer-shard-split-priority-cache buffer-shard)
            (funcall *buffer-shard-split-priority* buffer-shard))))

(defun buffer-shard-more-important-p (buffer-shard-1 buffer-shard-2)
  (> (buffer-shard-split-priority buffer-shard-1)
     (buffer-shard-split-priority buffer-shard-2)))

(defun split-more-important-p (split-1 split-2)
  (> (buffer-shard-split-priority (split-parent split-1))
     (buffer-shard-split-priority (split-parent split-2))))

;;; The minimum priority that a buffer-shard must have in order to be considered
;;; for splitting.
(declaim (type (real 0 *) *buffer-shard-split-min-priority*))
(defvar *buffer-shard-split-min-priority*)

;;; A real number that is the maximum permissible ratio of ghost points to
;;; interior points for a split.
(declaim (type (real 0 1) *buffer-shard-split-max-redundancy*))
(defvar *buffer-shard-split-max-redundancy*)

;;; A vector for looking up each buffer's ghostspec.
(defvar *buffer-ghostspec-vector*)

(defun buffer-ghostspec (buffer)
  (declare (buffer buffer))
  (let ((position (buffer-number buffer)))
    (symbol-macrolet ((place (svref *buffer-ghostspec-vector* position)))
      (the (values ghostspec &optional)
           (or place (setf place (compute-buffer-ghostspec buffer)))))))

;;; A priority queue of buffer-shards that have yet to be considered for being
;;; split.
(defvar *buffer-shard-queue*)

(defun make-buffer-shard-queue ()
  (queues:make-queue :priority-queue :compare #'buffer-shard-more-important-p))

(defun buffer-shard-queue-maybe-push (buffer-shard)
  (let ((priority (buffer-shard-split-priority buffer-shard)))
    ;; Only enqueue buffer shards whose priority exceeds the threshold...
    (when (>= priority *buffer-shard-split-min-priority*)
      ;; ... and which have at least one axis that can be split.
      (let ((ghostspec (buffer-ghostspec (buffer-shard-buffer buffer-shard))))
        (when (loop for axis below (shape-rank (buffer-shard-domain buffer-shard))
                      thereis (and (ghostspec-left-padding ghostspec axis)
                                   (ghostspec-left-padding ghostspec axis)))
          (queues:qpush *buffer-shard-queue* buffer-shard))))
    buffer-shard))

(defun buffer-shard-queue-cleanup ()
  (loop while (plusp (queues:qsize *buffer-shard-queue*))
        while (buffer-shard-split (queues:qtop *buffer-shard-queue*))
        do (queues:qpop *buffer-shard-queue*)))

(defun buffer-shard-queue-pop ()
  (buffer-shard-queue-cleanup)
  (queues:qpop *buffer-shard-queue*))

(defun buffer-shard-queue-emptyp ()
  (buffer-shard-queue-cleanup)
  (zerop (queues:qsize *buffer-shard-queue*)))

;;; A priority queue of splits that have yet to be propagated to all adjacent
;;; kernel shards and buffer shards.
(defvar *split-queue*)

(defun make-split-queue ()
  (queues:make-queue :priority-queue :compare #'split-more-important-p))

(defun split-queue-push (split)
  (queues:qpush *split-queue* split))

(defun split-queue-pop ()
  (queues:qpop *split-queue*))

(defun split-queue-emptyp ()
  (zerop (queues:qsize *split-queue*)))

(defun partition-program
    (program
     &key
       (split-priority 'buffer-shard-bits)
       (split-min-priority (* 30 1024 8)) ; Aim for L1 cache sized shards.
       (split-max-redundancy 0.125))
  "Partition all buffers and kernels in the supplied program into shards.
Returns, as multiple values, a vector mapping each buffer to its corresponding
primogenitor buffer shard, a vector mapping each kernel to its corresponding
primogenitor kernel shard, and a vector mapping each buffer to its
corresponding ghostspec.

SPLIT-PRIORITY - A function that takes a buffer shard and returns an unsigned
integer that is the priority when considering whether to split that buffer
shard.  Buffer shards with higher priority are split first, and buffer shards
whose priority is below a certain minimum priority are not split at all.

SPLIT-MIN-PRIORITY - An unsigned integer that is the priority a buffer shard
must exceed to be considered for splitting.

SPLIT-MAX-REDUNDANCY - A real number that is the maximum permissible ratio of
ghost points to interior points for a split."
  (declare (program program))
  (let ((*buffer-shard-split-priority* split-priority)
        (*buffer-shard-split-min-priority* split-min-priority)
        (*buffer-shard-split-max-redundancy* split-max-redundancy)
        (*buffer-shard-queue* (make-buffer-shard-queue))
        (*split-queue* (make-split-queue))
        (*primogenitor-buffer-shard-vector*
          (compute-program-primogenitor-buffer-shard-vector program))
        (*buffer-ghostspec-vector*
          (make-array (program-number-of-buffers program) :initial-element nil)))
    ;; Enqueue all qualifying primogenitor buffer shards.
    (map nil #'buffer-shard-queue-maybe-push *primogenitor-buffer-shard-vector*)
    ;; Split buffer shards until the queue is empty.
    (loop until (buffer-shard-queue-emptyp) for buffer-shard = (buffer-shard-queue-pop) do
      ;; TODO use binary search to find the optimal split position.
      (attempt-split buffer-shard (find-optimal-buffer-shard-split-axis buffer-shard))
      ;; For each split that hasn't been propagated yet, compute the
      ;; kernel-shards of its left and right child, which happens to
      ;; propagate the split as a side-effect.
      (loop until (split-queue-emptyp) do
        (propagate-split (split-queue-pop))))
    (map nil #'assign-layouts *primogenitor-buffer-shard-vector*)
    (values *primogenitor-buffer-shard-vector* *buffer-ghostspec-vector*)))

(defun attempt-split (buffer-shard axis &optional position)
  "Attempt to split buffer shard at the supplied axis and position.  If the split
could be introduced successfully, or if an equivalent split already existed,
return that split.  Otherwise, return NIL."
  (flet ((give-up () (return-from attempt-split nil)))
    ;; Ensure AXIS is not NIL.
    (unless axis (give-up))
    ;; Ensure the supplied AXIS and POSITION denote a split within the
    ;; buffer-shard's shape.
    (let* ((domain (buffer-shard-domain buffer-shard))
           (range (shape-range domain axis)))
      (if (not position)
          (unless (<= 2 (range-size range))
            (give-up))
          (unless (and (< (range-start range) position)
                       (<= position (range-last range)))
            (give-up))))
    ;; Attempt to reuse an existing split.  Give up if there is already a split
    ;; but it cannot be reused.
    (when (buffer-shard-split buffer-shard)
      (let* ((split (buffer-shard-split buffer-shard))
             (split-axis (split-axis split))
             (split-position (split-position split))
             (split-range (shape-range (buffer-shard-shape buffer-shard) split-axis)))
        (if (and (= split-axis axis)
                 (< (abs (- split-position position))
                    (range-step split-range)))
            (return-from attempt-split split)
            (give-up))))
    ;; Ensure that the buffer shard has a priority that is sufficiently high.
    (unless (>= (buffer-shard-split-priority buffer-shard) *buffer-shard-split-min-priority*)
      (give-up))
    ;; Ensure that the ghostspec permits splitting at that axis.
    (let* ((buffer (buffer-shard-buffer buffer-shard))
           (ghostspec (buffer-ghostspec buffer))
           (left-padding (ghostspec-left-padding ghostspec axis))
           (right-padding (ghostspec-right-padding ghostspec axis)))
      (unless (and left-padding right-padding)
        (give-up))
      ;; Plan the two domains that would result from a split.
      (multiple-value-bind (left-domain right-domain)
          (split-shape (buffer-shard-domain buffer-shard) axis position)
        ;; Ensure the split won't introduce too many ghost layers.
        (unless (<= right-padding
                    (* *buffer-shard-split-max-redundancy*
                       (range-size (shape-range left-domain axis))))
          (give-up))
        (unless (<= left-padding
                    (* *buffer-shard-split-max-redundancy*
                       (range-size (shape-range right-domain axis))))
          (give-up))
        ;; Success!  Create the split and push the newly created split and its
        ;; two children to the appropriate queues.
        (let* ((left-child
                 (make-buffer-shard
                  :buffer buffer
                  :domain left-domain
                  :parent buffer-shard
                  :shape (compute-buffer-shard-shape left-domain buffer)))
               (right-child
                 (make-buffer-shard
                  :buffer buffer
                  :domain right-domain
                  :parent buffer-shard
                  :shape (compute-buffer-shard-shape right-domain buffer)))
               (split
                 (make-split
                  :axis axis
                  :position (range-start (shape-range right-domain axis))
                  :left-child left-child
                  :right-child right-child)))
          (setf (buffer-shard-split buffer-shard) split)
          (buffer-shard-queue-maybe-push left-child)
          (buffer-shard-queue-maybe-push right-child)
          (split-queue-push split)
          split)))))

(defun compute-buffer-shard-shape (domain buffer)
  "Extend the domain of a buffer shard with suitable ghost layers."
  (declare (shape domain) (buffer buffer))
  (let ((ghostspec (buffer-ghostspec buffer)))
    (make-shape
     (loop for axis from 0
           for buffer-shard-range in (shape-ranges domain)
           for buffer-range in (shape-ranges (buffer-shape buffer))
           collect
           (if (range= buffer-shard-range buffer-range)
               buffer-range
               (let* ((step (range-step buffer-range))
                      (left-padding (ghostspec-left-padding ghostspec axis))
                      (right-padding (ghostspec-right-padding ghostspec axis)))
                 (range
                  (max (- (range-start buffer-shard-range) (* left-padding step))
                       (range-start buffer-range))
                  (min (+ (range-end buffer-shard-range) (* right-padding step))
                       (range-end buffer-range)))))))))

(defun find-optimal-buffer-shard-split-axis (buffer-shard)
  "Returns the axis where buffer shard can be split while introducing the minimum
number of ghost points, or NIL, if the buffer-shard cannot be split further.
If there are multiple axes with the same number of ghost points, pick the
lowest axis."
  (let* ((domain (buffer-shard-domain buffer-shard))
         (buffer (buffer-shard-buffer buffer-shard))
         (ghostspec (buffer-ghostspec buffer))
         (minimum-number-of-ghost-points (* *buffer-shard-split-max-redundancy* (shape-size domain)))
         (best-axis nil))
    (dotimes (axis (shape-rank domain) best-axis)
      (let ((left (ghostspec-left-padding ghostspec axis))
            (right (ghostspec-right-padding ghostspec axis)))
        (when (and left right)
          (let ((number-of-ghost-points
                  (* (+ left right)
                     (/ (shape-size domain)
                        (range-size (shape-range domain axis))))))
            (when (< number-of-ghost-points minimum-number-of-ghost-points)
              (setf minimum-number-of-ghost-points number-of-ghost-points)
              (setf best-axis axis))))))))

(defun propagate-split (split)
  (let* ((parent (split-parent split))
         (axis (split-axis split))
         (position (split-position split)))
    ;; Iterate over all adjacent kernel shards.
    (propagate-split/kmap
     (lambda (kernel-shard kaxis kposition)
       ;; Attempt a split of each adjacent buffer shard.
       (propagate-split/bmap #'attempt-split kernel-shard kaxis kposition)
       ;; Replace the kernel shard by one or two more refined kernel shards.
       (let ((iteration-space (kernel-shard-iteration-space kernel-shard)))
         (if (not kaxis)
             (refine-kernel-shard kernel-shard)
             (let* ((range (shape-range iteration-space kaxis))
                    (start (range-start range))
                    (last (range-last range)))
               (if (not (and (< start kposition) (<= kposition last)))
                   (refine-kernel-shard kernel-shard)
                   (multiple-value-bind (left right)
                       (split-shape iteration-space kaxis kposition)
                     (unlink-kernel-shard kernel-shard)
                     (add-child-kernel-shard kernel-shard left)
                     (add-child-kernel-shard kernel-shard right)))))))
     parent axis position)))

(defun propagate-split/kmap (function buffer-shard axis position)
  "Map the supplied function over all kernel shards neighboring the supplied
buffer shard.  At the same time, change the coordinate system from the supplied
axis and position from to that of each kernel shard being mapped over and pass
the resulting axis and position as additional arguments to the supplied
function."
  ;; We shift the position by -1/2, so that it will still be the
  ;; correct place to split even if there is a flip along that axis.
  (decf position 1/2)
  (let ((buffer (buffer-shard-buffer buffer-shard)))
    ;; Call function for each writer.
    (dolist (kernel-shard (buffer-shard-writers buffer-shard))
      (let* ((kernel (kernel-shard-kernel kernel-shard))
             (transformoid (second (find buffer (kernel-targets kernel) :key #'first))))
        (multiple-value-bind (kaxis kposition)
            (reverse-transform-axis-and-position transformoid axis position)
          (funcall function kernel-shard kaxis kposition))))
    ;; Call function for each reader.
    (dolist (kernel-shard (buffer-shard-readers buffer-shard))
      (let* ((kernel (kernel-shard-kernel kernel-shard))
             (transformoid (second (find buffer (kernel-sources kernel) :key #'first))))
        (multiple-value-bind (kaxis kposition)
            (reverse-transform-axis-and-position transformoid axis position)
          (funcall function kernel-shard kaxis kposition))))))

(defun propagate-split/bmap (function kernel-shard axis position)
  "Map the supplied function over all split-worthy buffer shards neighboring the
supplied kernel shard.  At the same time, change the coordinate system from the
supplied axis and position from to that of each buffer shard being mapped over
and pass the resulting axis and position as additional arguments to the
supplied function."
  ;; Only proceed if that kernel shard's iteration space is actually being
  ;; split.
  (when (and (integerp axis)
             (let* ((shape (kernel-shard-iteration-space kernel-shard))
                    (range (shape-range shape axis)))
               (and (<= (range-start range) position)
                    (< position (range-end range)))))
    (let ((kernel (kernel-shard-kernel kernel-shard)))
      ;; Call the function for each target.
      (dolist (buffer-shard (kernel-shard-targets kernel-shard))
        (unless (buffer-shard-split buffer-shard)
          (let* ((buffer (buffer-shard-buffer buffer-shard))
                 (transformoid (second (find kernel (buffer-writers buffer) :key #'first))))
            (multiple-value-bind (baxis bposition)
                (transform-axis-and-position transformoid axis position)
              (funcall function buffer-shard baxis bposition)))))
      ;; Call the function for each source.
      (dolist (buffer-shard (kernel-shard-sources kernel-shard))
        (unless (buffer-shard-split buffer-shard)
          (let* ((buffer (buffer-shard-buffer buffer-shard))
                 (transformoid (second (find kernel (buffer-readers buffer) :key #'first))))
            (multiple-value-bind (baxis bposition)
                (transform-axis-and-position transformoid axis position)
              (funcall function buffer-shard baxis bposition))))))))

(defun refine-kernel-shard (kernel-shard)
  "Replace any of the kernel shard's sources or targets that have one or more
splits with the most specific buffer shard in that tree of splits that still
provides all the data referenced by the kernel shard.  Update the writers and
readers slot of all affected buffers accordingly."
  (declare (kernel-shard kernel-shard))
  (with-slots (kernel iteration-space targets sources) kernel-shard
    (setf targets
          (loop for target in targets
                collect (refine-target kernel-shard iteration-space target)))
    (setf sources
          (loop for source in sources
                collect (refine-source kernel-shard iteration-space source)))))

(defun refine-target (kernel-or-kernel-shard iteration-space target)
  (multiple-value-bind (kernel kernel-shard)
      (etypecase kernel-or-kernel-shard
        (kernel (values kernel-or-kernel-shard nil))
        (kernel-shard (values (kernel-shard-kernel kernel-or-kernel-shard) kernel-or-kernel-shard)))
    (let* ((buffer (buffer-shard-buffer target))
           (transformoid (second (find buffer (kernel-targets kernel) :key #'first)))
           (new-target (refine-buffer-shard iteration-space transformoid target)))
      (when kernel-shard
        (unless (eq target new-target)
          (setf (buffer-shard-writers target)
                (remove kernel-shard (buffer-shard-writers target) :count 1))
          (buffer-shard-add-writer new-target kernel-shard)))
      new-target)))

(defun refine-source (kernel-or-kernel-shard iteration-space source)
  (multiple-value-bind (kernel kernel-shard)
      (etypecase kernel-or-kernel-shard
        (kernel (values kernel-or-kernel-shard nil))
        (kernel-shard (values (kernel-shard-kernel kernel-or-kernel-shard) kernel-or-kernel-shard)))
    (let* ((buffer (buffer-shard-buffer source))
           (transformoid (second (find buffer (kernel-sources kernel) :key #'first)))
           (new-source (refine-buffer-shard iteration-space transformoid source)))
      (when kernel-shard
        (unless (eq source new-source)
          (setf (buffer-shard-readers source)
                (remove kernel-shard (buffer-shard-readers source) :count 1))
          (buffer-shard-add-reader new-source kernel-shard)))
      new-source)))

(defun refine-buffer-shard (iteration-space transformoid buffer-shard)
  (declare (shape iteration-space) (buffer-shard buffer-shard))
  (with-slots (split domain) buffer-shard
    (if (not split)
        buffer-shard
        (with-slots (left-child right-child axis position) split
          (ecase (relative-position iteration-space transformoid axis position)
            (:both buffer-shard)
            (:left (refine-buffer-shard iteration-space transformoid left-child))
            (:right (refine-buffer-shard iteration-space transformoid right-child)))))))

(defun unlink-kernel-shard (kernel-shard)
  "Ensure that no buffer shards reference this kernel shard."
  (declare (kernel-shard kernel-shard))
  (dolist (buffer-shard (kernel-shard-sources kernel-shard))
    (setf (buffer-shard-readers buffer-shard)
          (remove kernel-shard (buffer-shard-readers buffer-shard) :count 1)))
  (dolist (buffer-shard (kernel-shard-targets kernel-shard))
    (setf (buffer-shard-writers buffer-shard)
          (remove kernel-shard (buffer-shard-writers buffer-shard) :count 1))))

(defun add-child-kernel-shard (kernel-shard new-iteration-space)
  (declare (kernel-shard kernel-shard) (shape new-iteration-space))
  (with-slots (kernel targets sources) kernel-shard
    (make-kernel-shard
     :kernel kernel
     :iteration-space new-iteration-space
     :targets
     (loop for target in targets
           collect (refine-target kernel new-iteration-space target))
     :sources
     (loop for source in sources
           collect (refine-source kernel new-iteration-space source)))))

(defun transform-axis-and-position (transformoid axis position)
  (multiple-value-bind (output-mask scalings offsets)
      (destructure-transformoid transformoid)
    (let* ((new-axis (or (position axis output-mask)
                         (return-from transform-axis-and-position
                           (values nil nil nil))))
           (scaling (aref scalings new-axis))
           (offset (aref offsets new-axis))
           (new-position (+ (* position scaling) offset))
           (flip (minusp scaling)))
      (values new-axis new-position flip))))

(defun reverse-transform-axis-and-position (transformoid axis position)
  (multiple-value-bind (output-mask scalings offsets)
      (destructure-transformoid transformoid)
    (let* ((new-axis (or (aref output-mask axis)
                         (return-from reverse-transform-axis-and-position
                           (values nil nil nil))))
           (scaling (aref scalings axis))
           (offset (aref offsets axis))
           (new-position (/ (- position offset) scaling))
           (flip (minusp scaling)))
      (values new-axis new-position flip))))

(defun relative-position (iteration-space transformoid axis position)
  "Returns either :LEFT, :RIGHT, or :BOTH, depending how the iteration space
relates to the supplied transformoid."
  (multiple-value-bind (output-mask scalings offsets)
      (destructure-transformoid transformoid)
    (let* ((new-axis (aref output-mask axis))
           (scaling (aref scalings axis))
           (offset (aref offsets axis)))
      (if (not new-axis)
          (if (< offset position)
              :left
              :right)
          (let* ((range (shape-range iteration-space new-axis))
                 (first (range-start range))
                 (last (range-last range))
                 (p1 (+ (* scaling first) offset))
                 (p2 (+ (* scaling last) offset))
                 (b1 (< p1 position))
                 (b2 (< p2 position)))
            (cond ((and b1 b2) :left)
                  (b1 :both)
                  (b2 :both)
                  (t :right)))))))

(defun destructure-transformoid (transformoid)
  (etypecase transformoid
    (transformation
     (values (transformation-output-mask transformoid)
             (transformation-scalings transformoid)
             (transformation-offsets transformoid)))
    (stencil
     (values (stencil-output-mask transformoid)
             (stencil-scalings transformoid)
             (stencil-center transformoid)))
    (iterating-instruction
     (destructure-transformoid (instruction-transformation transformoid)))))

(defun assign-layouts (primogenitor-buffer-shard)
  (declare (buffer-shard primogenitor-buffer-shard))
  (labels
      ((assign-layout (buffer-shard referenced-ancestor)
         (declare (buffer-shard buffer-shard)
                  (type (or null buffer-shard) referenced-ancestor))
         (with-slots (buffer split domain shape layout) buffer-shard
           ;; Check whether the buffer shard is being referenced.
           (when (or (buffer-shard-readers buffer-shard)
                     (buffer-shard-writers buffer-shard)
                     (not (interior-buffer-p buffer)))
             (if (not referenced-ancestor)
                 ;; If there is no referenced ancestor, create both a new
                 ;; layout and storage for all the shards's elements.
                 (let* ((ntype (buffer-ntype buffer))
                        (size (shape-size shape))
                        (storage (make-storage :ntype ntype :size size))
                        (strides (shape-strides shape)))
                   (setf layout (make-layout :storage storage :strides strides))
                   (setf referenced-ancestor buffer-shard))
                 ;; If there is already an ancestor layout, reuse its storage
                 ;; and strides and only compute a suitable offset.
                 (let* ((ancestor-shape (buffer-shard-shape referenced-ancestor))
                        (ancestor-layout (buffer-shard-layout referenced-ancestor))
                        (storage (layout-storage ancestor-layout))
                        (strides (layout-strides ancestor-layout))
                        (offset
                          (loop for stride across strides
                                for range1 in (shape-ranges ancestor-shape)
                                for range2 in (shape-ranges shape)
                                sum (* stride
                                       (- (range-start range2)
                                          (range-start range1))))))
                   (setf layout
                         (make-layout
                          :offset offset
                          :strides strides
                          :storage storage)))))
           ;; If there is a split, descend into each child.
           (when (splitp split)
             (assign-layout (split-left-child split) referenced-ancestor)
             (assign-layout (split-right-child split) referenced-ancestor)))))
    (assign-layout primogenitor-buffer-shard nil)))

(defun shape-strides (shape)
  (declare (shape shape))
  (let* ((rank (shape-rank shape))
         (strides (make-array rank)))
    (unless (zerop rank)
      (setf (aref strides (- rank 1)) 1)
      (loop for axis from (- rank 2) downto 0 do
        (setf (aref strides axis)
              (* (aref strides (1+ axis))
                 (range-size (shape-range shape (1+ axis)))))))
    strides))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sanity Checks

(defvar *check-shards-table*)

(defvar *check-shards-worklist*)

(defvar *check-shards-buffer-shards*)

(defvar *check-shards-kernel-shards*)

(defun check-shards ()
  "Raise an error if any shards are malformed.  Useful for debugging."
  (let ((*check-shards-worklist* (coerce *primogenitor-buffer-shard-vector* 'list))
        (*check-shards-table* (make-hash-table))
        (*check-shards-buffer-shards* (make-hash-table))
        (*check-shards-kernel-shards* (make-hash-table)))
    (loop until (null *check-shards-worklist*) do
      (check-shard (pop *check-shards-worklist*)))
    (maphash
     (lambda (buffer buffer-shards)
       (assert (shape= (buffer-shape buffer)
                       (apply #'fuse-shapes
                              (mapcar #'buffer-shard-domain buffer-shards)))))
     *check-shards-buffer-shards*)
    (maphash
     (lambda (kernel kernel-shards)
       (assert (shape= (kernel-iteration-space kernel)
                       (apply #'fuse-shapes
                              (mapcar #'kernel-shard-iteration-space kernel-shards)))))
     *check-shards-kernel-shards*)))

(defun check-shard-eventually (shard)
  (unless (gethash shard *check-shards-table*)
    (setf (gethash shard *check-shards-table*) t)
    (push shard *check-shards-worklist*)))

(defgeneric check-shard (shard))

(defmethod check-shard ((buffer-shard buffer-shard))
  (with-slots (buffer parent domain shape readers writers split) buffer-shard
    (assert (subshapep domain shape))
    (dolist (writer writers)
      (assert (member buffer-shard (kernel-shard-targets writer)))
      (check-shard-eventually writer))
    (dolist (reader readers)
      (assert (member buffer-shard (kernel-shard-sources reader)))
      (check-shard-eventually reader))
    (when parent
      (assert (buffer-shard-split parent))
      (with-slots (left-child right-child) (buffer-shard-split parent)
        (assert (or (eq buffer-shard right-child)
                    (eq buffer-shard left-child)))))
    (when split
      (with-slots (left-child right-child) split
        (check-shard-eventually left-child)
        (check-shard-eventually right-child)))
    (push buffer-shard (gethash buffer *check-shards-buffer-shards* '()))))

(defmethod check-shard ((kernel-shard kernel-shard))
  (with-slots (kernel iteration-space targets sources) kernel-shard
    (assert (= (length (kernel-targets kernel))
               (length targets)))
    (loop for target in targets
          for (buffer) in (kernel-targets kernel)
          do (assert (eq buffer (buffer-shard-buffer target)))
          do (assert (member kernel-shard (buffer-shard-writers target)))
          do (check-shard-eventually target))
    (assert (= (length (kernel-sources kernel))
               (length sources)))
    (loop for source in sources
          for (buffer) in (kernel-sources kernel)
          do (assert (eq buffer (buffer-shard-buffer source)))
          do (assert (member kernel-shard (buffer-shard-readers source)))
          do (check-shard-eventually source))
    (push kernel-shard (gethash kernel *check-shards-kernel-shards* '()))))
