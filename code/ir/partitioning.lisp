;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;; In this file, we partition buffers into chunks.  Each chunk describes
;;; the allocation of a part of that buffer, plus the ghost layers
;;; necessary to exchange information with neighboring chunks.
;;;
;;; The algorithm for partitioning buffers into chunks initially assigns
;;; each buffer a single chunk.  Then, each large chunk is split repeatedly
;;; into two chunks until a certain stopping criterion is reached.  Also,
;;; whenever a chunk is split, the splitting is propagated to chunks that
;;; use or produce this chunk's data until another stopping criterion is
;;; reached, so that all chunks that result from a single splitting step
;;; have good data locality and qualify for later being scheduled on the
;;; same worker.

(defstruct (action
            (:predicate actionp)
            (:constructor make-action))
  (iteration-space nil :type shape :read-only t)
  (kernel nil :type kernel :read-only t)
  (source-chunks '() :type list :read-only t))

;; This comparison function is used to sort the list of actions that write into
;; a particular chunk.
(defun action> (action1 action2)
  (> (shape-size (action-iteration-space action1))
     (shape-size (action-iteration-space action2))))

(defstruct (chunk
            (:predicate chunkp)
            (:constructor make-chunk))
  ;; The buffer being partitioned by this chunk.
  (buffer nil :type buffer :read-only t)
  ;; The part of the chunk's buffer's shape that is exclusively owned by
  ;; that chunk and its children.
  (domain nil :type shape :read-only t)
  ;; The chunk's domain, padded with ghost layers.
  (shape nil :type shape :read-only t)
  ;; The chunk that was split to create this one.
  (parent nil :type (or null chunk) :read-only t)
  ;; A list of actions that write into this chunk.
  (actions '() :type list)
  ;; A cache for the chunk's split priority.
  (split-priority-cache nil :type (or null unsigned-byte))
  ;; The split operation in case this chunk has been split, or NIL, if it
  ;; hasn't been split so far.
  (split nil :type (or null structure-object)))

(defmethod print-object ((chunk chunk) stream)
  (format stream "~@<#<~;~S ~_~@{~S ~:_~S~^ ~_~}~;>~:>"
          (class-name (class-of chunk))
          :domain (chunk-domain chunk)
          :buffer (chunk-buffer chunk)
          :split (chunk-split chunk)))

(defun chunk-bits (chunk)
  (declare (chunk chunk))
  (* (typo:ntype-bits (buffer-ntype (chunk-buffer chunk)))
     (shape-size (chunk-shape chunk))))

(defun chunk-primogenitor (chunk)
  (declare (chunk chunk))
  (if (not (chunk-parent chunk))
      chunk
      (the (values chunk &optional)
           (chunk-primogenitor (chunk-parent chunk)))))

(defun compute-program-primogenitor-chunk-vector (program)
  "Returns a vector whose Nth entry is the primogenitor chunk corresponding
to the buffer with number N."
  (let* ((size (program-number-of-buffers program))
         (result (make-array size :initial-element nil)))
    ;; Create the initial chunk for each buffer.
    (do-program-buffers (buffer program)
      (setf (svref result (buffer-number buffer))
            (make-chunk
             :buffer buffer
             :domain (buffer-shape buffer)
             :shape (buffer-shape buffer))))
    ;; Initialize the ACTIONS slot of each initial chunk.
    (do-program-buffers (buffer program result)
      (let ((actions '()))
        (do-buffer-inputs (kernel buffer)
          (let* ((iteration-space (kernel-iteration-space kernel))
                 (chunks '()))
            (do-kernel-inputs (buffer kernel)
              (push (svref result (buffer-number buffer))
                    chunks))
            (push (make-action :iteration-space iteration-space
                               :kernel kernel
                               :source-chunks (nreverse chunks))
                  actions)))
        ;; Sort actions such that large iteration spaces come first.
        (setf (chunk-actions (svref result (buffer-number buffer)))
              (sort actions #'action>))))))

(defstruct (split
            (:predicate splitp)
            (:constructor make-split))
  ;; The chunk being split.
  (parent nil :type chunk :read-only t)
  ;; The axis at which the chunk is split.
  (axis nil :type unsigned-byte :read-only t)
  ;; The left child of the split chunk.
  (left-child nil :type chunk :read-only t)
  ;; The right child of the split chunk.
  (right-child nil :type chunk :read-only t))

(defun split-position (split)
  (range-start
   (shape-range
    (chunk-domain
     (split-right-child split))
    (split-axis split))))

(defmethod print-object ((split split) stream)
  (format stream "~@<#<~;~S ~_~@{~S ~:_~S~^ ~_~}~;>~:>"
          (class-name (class-of split))
          :axis (split-axis split)
          :position (split-position split)
          :left-child (split-left-child split)
          :right-child (split-right-child split)))

(defun chunk-infants (chunk)
  "An infant is a chunk without a split.  This function returns the list of
infants whose line of ancestry contains the supplied CHUNK."
  (let ((split (chunk-split chunk)))
    (if (not split)
        (list chunk)
        (append
         (chunk-infants (split-left-child split))
         (chunk-infants (split-right-child split))))))

(defun chunk-chains (chunk)
  "A list of all chains of chunks writing to CHUNK and its children.  Useful
for debugging."
  (labels ((chain-elements (chunk)
             (list*
              chunk
              (let ((actions (chunk-actions chunk)))
                (if (not actions)
                    '()
                    (chain-elements
                     ;; Only follow the largest chunk.
                     (first (sort (copy-list (cddr (first actions))) #'>
                                  :key #'chunk-bits))))))))
    (list*
     (chain-elements chunk)
     (if (not (chunk-split chunk))
         '()
         (append
          (chunk-chains (split-left-child (chunk-split chunk)))
          (chunk-chains (split-right-child (chunk-split chunk))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reasoning About Ghost Layers

(defstruct (ghostspec
            (:predicate ghostspecp)
            (:constructor make-ghostspec))
  ;; A vector with one entry per axis, denoting how many layers must be
  ;; copied on the left of the right child of a split chunk.  An entry of
  ;; NIL means there is no way to split that axis.
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

(defun compute-chunk-shape (chunk-domain buffer)
  (declare (shape chunk-domain) (buffer buffer))
  (let ((ghostspec (buffer-ghostspec buffer)))
    (make-shape
     (loop for axis from 0
           for chunk-range in (shape-ranges chunk-domain)
           for buffer-range in (shape-ranges (buffer-shape buffer))
           collect
           (if (range= chunk-range buffer-range)
               buffer-range
               (let* ((step (range-step buffer-range))
                      (left-padding (ghostspec-left-padding ghostspec axis))
                      (right-padding (ghostspec-right-padding ghostspec axis)))
                 (range
                  (max (- (range-start chunk-range) (* left-padding step))
                       (range-start buffer-range))
                  (min (+ (range-end chunk-range) (* right-padding step))
                       (range-end buffer-range)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reasoning About Chunk Neighbors

(defstruct (vicinity
            (:predicate vicinityp))
  (left-neighbors-vec nil :type simple-vector :read-only t)
  (right-neighbors-vec nil :type simple-vector :read-only t))

(defun vicinity-left-neighbors (vicinity axis)
  (svref (vicinity-left-neighbors-vec vicinity) axis))

(defun vicinity-right-neighbors (vicinity axis)
  (svref (vicinity-right-neighbors-vec vicinity) axis))

(defun compute-chunk-vicinity (chunk)
  (declare (chunk chunk))
  (let* ((shape (chunk-shape chunk))
         (rank (shape-rank shape))
         (left-neighbors-vec (make-array rank :initial-element '()))
         (right-neighbors-vec (make-array rank :initial-element '())))
    (labels (;; The other child of the closest ancestor that has a split at
             ;; AXIS with CHUNK as a child of the supplied SIDE.
             (sibling (chunk axis side)
               (let ((parent (chunk-parent chunk)))
                 (if (not parent)
                     nil
                     (let* ((split (chunk-split parent))
                            (left-child (split-left-child split))
                            (right-child (split-right-child split)))
                       (if (and (= axis (split-axis split))
                                (eq chunk
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
                    (let ((split (chunk-split ancestor)))
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
             ;; part of the shape of the chunk whose vicinity we're
             ;; computing.
             (relevant-child-p (child axis)
               (range-intersectionp
                (shape-range shape axis)
                (shape-range (chunk-domain child) axis))))
      (loop for axis below rank do
        (setf (svref left-neighbors-vec axis)
              (relevant-children (sibling chunk axis :left) axis :right))
        (setf (svref right-neighbors-vec axis)
              (relevant-children (sibling chunk axis :right) axis :left)))
      (make-vicinity
       :left-neighbors-vec left-neighbors-vec
       :right-neighbors-vec right-neighbors-vec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Partitioning

;;; The function for computing the split priority of a chunk that was
;;; supplied to the partitioning operation.
(defvar *chunk-split-priority*)

(defun chunk-split-priority (chunk)
  (or (chunk-split-priority-cache chunk)
      (setf (chunk-split-priority-cache chunk)
            (funcall *chunk-split-priority* chunk))))

;;; The minimum priority that a chunk must have in order to be considered
;;; for splitting.
(declaim (type (real 0 *) *chunk-split-min-priority*))
(defvar *chunk-split-min-priority*)

;;; A real number that is the maximum permissible ratio of ghost points to
;;; interior points for a split.
(declaim (type (real 0 1) *chunk-split-max-redundancy*))
(defvar *chunk-split-max-redundancy*)

;;; A vector for looking up each buffer's initial chunk.
(declaim (ftype (function (buffer) (values chunk &optional)) *buffer-chunk*))
(defvar *buffer-chunk*)

(defun buffer-chunk (buffer)
  (funcall *buffer-chunk* buffer))

;;; A hash table for looking up each buffer's ghostspec.
(declaim (type hash-table *buffer-ghostspec-table*))
(defvar *buffer-ghostspec-table*)

(defun buffer-ghostspec (buffer)
  (declare (buffer buffer))
  (symbol-macrolet ((place (gethash buffer *buffer-ghostspec-table*)))
    (the (values ghostspec &optional)
         (or place (setf place (compute-buffer-ghostspec buffer))))))

;;; A list of splits that have yet to be propagated to nearby chunks.
(declaim (type list *unpropagated-splits*))
(defvar *unpropagated-splits*)

;;; A priority queue of chunks that have yet to be considered for being
;;; split.
(defvar *chunk-pqueue*)

(defun enqueue-chunk (chunk)
  (let ((priority (chunk-split-priority chunk)))
    ;; Immediately discard chunks whose priority doesn't match
    ;; the minimum priority.
    (when (>= priority *chunk-split-min-priority*)
      (priority-queue:pqueue-push chunk priority *chunk-pqueue*))
    chunk))

(defun next-chunk ()
  (priority-queue:pqueue-pop *chunk-pqueue*))

(defun next-chunk-p ()
  (not (priority-queue:pqueue-empty-p *chunk-pqueue*)))

(defun partition-chunks
    (chunks
     &key
       (buffer-chunk (alexandria:required-argument :buffer-chunk))
       (split-priority 'chunk-bits)
       (split-min-priority (* 30 1024))
       (split-max-redundancy 0.125))
  "Attempt to split the supplied sequence of chunks based on the supplied
keyword arguments.

BUFFER-CHUNK - A function for looking up the root chunk of each buffer that
is encountered during partitioning.

SPLIT-PRIORITY - A function that takes a chunk and returns an unsigned
integer that is the priority when considering whether to split the chunk.
Chunks with higher priority are split first, and chunks whose priority is
below a certain minimum priority are not split at all.

SPLIT-MIN-PRIORITY - An unsigned integer that is the priority a chunk must
exceed to be considered for splitting.

SPLIT-MAX-REDUNDANCY - A real number that is the maximum permissible ratio
of ghost points to interior points for a split.
"
  (let ((*buffer-chunk* buffer-chunk)
        (*chunk-split-priority* split-priority)
        (*chunk-split-min-priority* split-min-priority)
        (*chunk-split-max-redundancy* split-max-redundancy)
        (*buffer-ghostspec-table* (make-hash-table :test #'eq))
        (*chunk-pqueue* (priority-queue:make-pqueue #'>))
        (*unpropagated-splits* '()))
    ;; Enqueue all supplied chunks.
    (map nil #'enqueue-chunk chunks)
    ;; Split chunks until the queue is empty.
    (loop while (next-chunk-p) for chunk = (next-chunk) do
      ;; Skip chunks that have already been split.
      (unless (chunk-split chunk)
        (attempt-split chunk (find-chunk-split-axis chunk))
        ;; For each split that hasn't been propagated yet, compute the
        ;; actions of its left and right child, which happens to propagate
        ;; the split as a side-effect.
        (loop until (null *unpropagated-splits*) do
          (propagate-split/determine-child-actions (pop *unpropagated-splits*)))))))

(defun attempt-split (chunk axis &optional position)
  "Attempt to split CHUNK at the supplied AXIS and POSITION.  If the split
could be introduced successfully, or if an equivalent split already
existed, return that split.  Otherwise, return NIL."
  (flet ((give-up () (return-from attempt-split nil)))
    ;; Ensure AXIS is not NIL.
    (unless axis (give-up))
    ;; Ensure the supplied AXIS and POSITION denote a split within the chunk's shape
    (let* ((domain (chunk-domain chunk))
           (range (shape-range domain axis)))
      (if (not position)
          (unless (<= 2 (range-size range))
            (give-up))
          (unless (and (< (range-start range) position)
                       (<= position (range-last range)))
            (give-up))))
    ;; Attempt to reuse an existing split.
    (when (chunk-split chunk)
      (let* ((split (chunk-split chunk))
             (split-axis (split-axis split))
             (split-position (split-position split))
             (split-range (shape-range (chunk-shape chunk) split-axis)))
        (if (and (= split-axis axis)
                 (< (abs (- split-position position))
                    (range-step split-range)))
            (return-from attempt-split split)
            (give-up))))
    ;; Ensure that the CHUNK has a priority that is sufficiently high.
    (unless (>= (chunk-split-priority chunk) *chunk-split-min-priority*)
      (give-up))
    (let* ((buffer (chunk-buffer chunk))
           (ghostspec (buffer-ghostspec buffer))
           (left-padding (ghostspec-left-padding ghostspec axis))
           (right-padding (ghostspec-right-padding ghostspec axis)))
      ;; Ensure that the ghostspec permits splitting at that axis.
      (unless (and left-padding right-padding)
        (give-up))
      (multiple-value-bind (left-domain right-domain)
          (split-shape (chunk-domain chunk) axis position)
        ;; Ensure the split won't introduce too many ghost layers.
        (unless (<= right-padding
                    (* *chunk-split-max-redundancy*
                       (range-size (shape-range left-domain axis))))
          (give-up))
        (unless (<= left-padding
                    (* *chunk-split-max-redundancy*
                       (range-size (shape-range right-domain axis))))
          (give-up))
        ;; Success!
        (let* ((left-child
                 (make-chunk
                  :buffer buffer
                  :domain left-domain
                  :parent chunk
                  :shape (compute-chunk-shape left-domain buffer)))
               (right-child
                 (make-chunk
                  :buffer buffer
                  :domain right-domain
                  :parent chunk
                  :shape (compute-chunk-shape right-domain buffer)))
               (split
                 (make-split
                  :parent chunk
                  :axis axis
                  :left-child left-child
                  :right-child right-child)))
          (enqueue-chunk left-child)
          (enqueue-chunk right-child)
          (push split *unpropagated-splits*)
          (setf (chunk-split chunk) split)
          split)))))

(defun find-chunk-split-axis (chunk)
  "Returns the axis where chunk can be split while introducing the minimum
number of ghost points, or NIL, if the chunk cannot be split further."
  (let* ((domain (chunk-domain chunk))
         (buffer (chunk-buffer chunk))
         (ghostspec (buffer-ghostspec buffer))
         (minimum-number-of-ghost-points (* *chunk-split-max-redundancy* (shape-size domain)))
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

(defun propagate-split/determine-child-actions (split)
  ;; This function is called only for successful splits, meaning that the
  ;; ghostspec entries of the chunk's buffer at the split axis are non-NIL.
  ;; The ghostspec for an axis is non-NIL only if all writes to that buffer
  ;; have the same permutation, scaling, and offset, and if all stencils of
  ;; the reading kernels have the same permutation, scaling, and center.
  ;; Hence it is sufficient to only look at the first store instruction and
  ;; the first stencil when propagating the split.
  (let* ((axis (split-axis split))
         (position (split-position split))
         (parent (split-parent split))
         (left-child (split-left-child split))
         (right-child (split-right-child split))
         (parent-buffer (chunk-buffer parent)))
    ;; Propagate the split to all kernels reading from this buffer.
    (do-buffer-outputs (kernel parent-buffer)
      (multiple-value-bind (axis1 position1)
          (reverse-transform-axis-and-position
           (first (kernel-stencils kernel parent-buffer))
           axis
           ;; We shift the position by -1/2, so that it will still be the
           ;; correct place to split even if there is a flip along that
           ;; axis.
           (- position 1/2))
        (when axis1
          (loop for (target-buffer store-instruction) in (kernel-targets kernel) do
            (let ((target-chunk (find-most-specific-target-chunk target-buffer kernel parent)))
              (unless (chunk-split target-chunk)
                (multiple-value-bind (axis2 position2)
                    (transform-axis-and-position store-instruction axis1 position1)
                  (attempt-split target-chunk axis2 position2))))))))
    ;; Propagate the split to all kernels writing to this buffer, and
    ;; compute the child actions.
    (let ((left-actions '())
          (right-actions '())
          ;; An action is boring if it writes only to one half of the split.
          ;; The boring part is that this also means there is no way to
          ;; propagate the split across it.  We collect these actions and
          ;; process them last, because often another propagated split makes it
          ;; possible to narrow down their chunks, too.
          (boring-actions '()))
      (dolist (action (chunk-actions parent))
        (with-slots (iteration-space kernel (chunks source-chunks)) action
          (multiple-value-bind (axis1 position1 flip1)
              (reverse-transform-axis-and-position
               (second (assoc parent-buffer (kernel-targets kernel)))
               axis
               (- position 1/2))
            (if (or (not axis1)
                    (let ((range (shape-range iteration-space axis1)))
                      (not (and (< (range-start range) position1)
                                (<= position1 (range-last range))))))
                (push action boring-actions)
                (let ((left-chunks '())
                      (right-chunks '()))
                  (loop for (buffer stencil) in (kernel-sources kernel)
                        for chunk in chunks
                        do (multiple-value-bind (axis2 position2 flip2)
                               (transform-axis-and-position stencil axis1 position1)
                             (let ((split (attempt-split chunk axis2 position2)))
                               (cond ((not split)
                                      (push chunk left-chunks)
                                      (push chunk right-chunks))
                                     ((alexandria:xor flip1 flip2)
                                      (push (split-left-child split) right-chunks)
                                      (push (split-right-child split) left-chunks))
                                     (t
                                      (push (split-left-child split) left-chunks)
                                      (push (split-right-child split) right-chunks))))))
                  (multiple-value-bind (left-iteration-space right-iteration-space)
                      (split-shape iteration-space axis1 position1)
                    (when flip1 (rotatef left-iteration-space right-iteration-space))
                    (push (make-action :iteration-space left-iteration-space
                                       :kernel kernel
                                       :source-chunks (nreverse left-chunks))
                          left-actions)
                    (push (make-action :iteration-space right-iteration-space
                                       :kernel kernel
                                       :source-chunks (nreverse right-chunks))
                          right-actions)))))))
      ;; Now refine the boring actions.
      (dolist (action boring-actions)
        (with-slots (iteration-space kernel (chunks source-chunks)) action
          (let* ((child-chunks
                   (loop for (buffer stencil) in (kernel-sources kernel)
                         for source-chunk in chunks
                         for source-split = (chunk-split source-chunk)
                         collect
                         (if (not source-split)
                             source-chunk
                             (ecase (relative-position iteration-space stencil axis position)
                               (:left (split-left-child source-split))
                               (:right (split-right-child source-split))
                               (:both source-chunk)))))
                 (child-action (make-action :iteration-space iteration-space
                                            :kernel kernel
                                            :source-chunks child-chunks)))
            (ecase (relative-position
                    iteration-space
                    (second (assoc parent-buffer (kernel-targets kernel)))
                    axis
                    position)
              (:left (push child-action left-actions))
              (:right (push child-action right-actions))
              (:both (error "Not a boring action: ~S" action))))))
      ;; Finalize the child actions.
      (setf (chunk-actions left-child)
            (sort left-actions #'action>))
      (setf (chunk-actions right-child)
            (sort right-actions #'action>))
      split)))

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

(defun find-most-specific-target-chunk (buffer kernel source-chunk)
  "Returns the most specific child chunk of BUFFER whose actions from kernel
only reference the SOURCE-CHUNK part of the corresponding source buffer."
  (let* ((source-buffer (chunk-buffer source-chunk))
         (position (or (position source-buffer (kernel-sources kernel) :key #'car)
                       (error "Could not find a valid target chunk."))))
    (labels ((refine-search (target source)
               (if (not (chunk-parent source))
                   target
                   (let ((target (refine-search target (chunk-parent source))))
                     (if (not (chunk-split target))
                         target
                         (let* ((split (chunk-split target))
                                (left-child (split-left-child split))
                                (right-child (split-right-child split))
                                (left-action (find kernel (chunk-actions left-child) :key #'action-kernel))
                                (right-action (find kernel (chunk-actions right-child) :key #'action-kernel)))
                           (cond ((and left-action
                                       (eq source (nth position (action-source-chunks left-action))))
                                  left-child)
                                 ((and right-action
                                       (eq source (nth position (action-source-chunks right-action))))
                                  right-child)
                                 (t
                                  (return-from find-most-specific-target-chunk
                                    target)))))))))
      (refine-search (buffer-chunk buffer) source-chunk))))
