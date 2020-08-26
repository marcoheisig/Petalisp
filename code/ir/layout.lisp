;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;; During IR conversion, we decide which lazy arrays can be integrated
;;; directly into one or more kernels, and which are actually turned into
;;; memory locations.  For the latter, we have to figure out in which way
;;; the data will be laid out in memory.  To do so, we create a hash table
;;; that assigns each of these lazy arrays a LAYOUT - an object that tracks
;;; all load and store instructions and decides on the size and number of
;;; temporary buffers.
;;;
;;; The general rule is that each kernel will have a single, homogeneous
;;; output buffer for each store instruction, and one homogeneous output
;;; buffer per load instructions.  Under certain circumstances, it is
;;; possible to merge several of these buffers to reduce the memory
;;; footprint.  For example, load instructions with similar transformations
;;; can use the same buffer, and store instructions can often use the same
;;; buffer as the largest target of future load instructions.

(defgeneric make-layout (lazy-array))

(defgeneric layout-load (layout shape transformation))

(defgeneric layout-store (layout value shape transformation))

(defgeneric finalize-layout (layout))

(defstruct (layout
            (:constructor nil))
  (lazy-array nil :type lazy-array :read-only t)
  (ntype nil :read-only t))

(defstruct (range-immediate-layout
            (:include layout)
            (:constructor make-range-immediate-layout
                (range-immediate
                 &aux
                   (lazy-array range-immediate)
                   (ntype
                    (petalisp.type-inference:generalize-ntype
                     (element-ntype range-immediate)))))))

(defstruct (array-immediate-layout
            (:include layout)
            (:conc-name layout-)
            (:constructor make-array-immediate-layout
                (array-immediate storage
                 &aux
                   (lazy-array array-immediate)
                   (ntype
                    (petalisp.type-inference:generalize-ntype
                     (element-ntype array-immediate)))
                   (buffer
                    (make-buffer
                     :shape (shape array-immediate)
                     :ntype ntype
                     :storage storage)))))
  (buffer nil :type buffer))

(defstruct (lazy-array-layout
            (:include layout)
            (:conc-name layout-)
            (:constructor make-lazy-array-layout
                (lazy-array
                 &aux
                   (ntype
                    (petalisp.type-inference:generalize-ntype
                     (element-ntype lazy-array))))))
  ;; An alist whose keys are buffers and whose values are the corresponding
  ;; load instructions.
  (buffer-loads '())
  ;; An alist whose keys are buffers and whose values are the corresponding
  ;; store instructions.
  (buffer-stores '()))

(defun layout-depth (layout)
  (depth (layout-lazy-array layout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constructing a Layout

(defmethod make-layout ((range-immediate range-immediate))
  (make-range-immediate-layout range-immediate))

(defmethod make-layout ((array-immediate array-immediate))
  (make-array-immediate-layout array-immediate (storage array-immediate)))

(defmethod make-layout ((lazy-array lazy-array))
  (make-lazy-array-layout lazy-array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Loading from a Layout

;;; Loading from a range immediate is trivial.  It is defined as the
;;; identity on the iteration space and can be represented as an iref
;;; instruction.
(defmethod layout-load
    ((layout range-immediate-layout)
     (shape shape)
     (transformation transformation))
  (declare (ignore layout shape))
  (make-iref-instruction transformation))

;;; Loading from an array immediate is also straightforward.  It is assumed
;;; that the entire array is present in some kind of shared memory, which
;;; is represented by a single buffer.
(defmethod layout-load
    ((layout array-immediate-layout)
     (shape shape)
     (transformation transformation))
  (let ((layout-entry (assoc layout *layout-buffer-loads*)))
    (unless layout-entry
      (push (setf layout-entry `(,layout))
            *layout-buffer-loads*))
    (let ((buffer-entry (cadr layout-entry)))
      (unless buffer-entry
        (push (setf buffer-entry `(,(layout-buffer layout)))
              (cdr layout-entry)))
      (let ((load (make-load-instruction (car buffer-entry) transformation)))
        (push load (cdr buffer-entry))
        load))))

;;; Loading from a lazy array that is not an immediate is more involved.
;;; Before creating and using a new buffer, we attempt to reuse an existing
;;; buffer.  This is possible if all reads from that buffer are similar.
(defmethod layout-load
    ((layout lazy-array-layout)
     (shape shape)
     (transformation transformation))
  ;; Ensure a layout entry.
  (let ((layout-entry (assoc layout *layout-buffer-loads*)))
    (unless layout-entry
      (push (setf layout-entry `(,layout))
            *layout-buffer-loads*))
    ;; Attempt to reuse an existing buffer.
    (loop for buffer-entry in (cdr layout-entry) do
      (when (loop for load-instruction in (cdr buffer-entry)
                  always
                  (transformation-similar
                   transformation
                   (load-instruction-transformation load-instruction)
                   ;; TODO: Use some smarter heuristic than a hard-coded
                   ;; constant for when to merge load instruction buffers.
                   12))
        (let* ((buffer (car buffer-entry))
               (old-shape (buffer-shape (car buffer-entry)))
               (new-shape (fuse-shapes shape old-shape))
               (growth (- (shape-size new-shape)
                          (shape-size old-shape))))
          (when (and (< growth (floor (shape-size old-shape) 2))
                     (< growth (floor (shape-size shape) 2)))
            (setf (buffer-shape buffer) new-shape)
            (let ((load (make-load-instruction (car buffer-entry) transformation)))
              (push load (cdr buffer-entry))
              (return-from layout-load load))))))
    ;; If no buffer can be reused, create a now one, load from it, and
    ;; register both entities.
    (let* ((buffer (make-buffer
                    :shape shape
                    :reusablep t
                    :ntype (layout-ntype layout)))
           (load (make-load-instruction buffer transformation))
           (new-entry `(,buffer ,load)))
      (push new-entry (cdr layout-entry))
      (push new-entry (layout-buffer-loads layout))
      load)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Storing to a Layout

(defmethod layout-store
    ((layout array-immediate-layout)
     (value t)
     (shape shape)
     (transformation transformation))
  (make-store-instruction value (layout-buffer layout) transformation))

;;; We always store into a new buffer, relying on later optimization passes
;;; to merge buffers of certain load and store instructions.
(defmethod layout-store
    ((layout lazy-array-layout)
     (value t)
     (shape shape)
     (transformation transformation))
  (let* ((buffer (make-buffer
                  :shape shape
                  :reusablep t
                  :ntype (layout-ntype layout)))
         (store (make-store-instruction value buffer transformation)))
    (push `(,buffer ,store) (layout-buffer-stores layout))
    store))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Layout Finalization
;;;
;;; During layout finalization, all loads and store instructions related to
;;; a particular layout have already been created, but source and target
;;; buffers into this layout are not yet connected.  This step introduces
;;; the necessary copy kernels to move the data from each store instruction
;;; buffer to all relevant load instruction buffers.

(defmethod finalize-layout ((layout range-immediate-layout))
  (values))

(defmethod finalize-layout ((layout array-immediate-layout))
  (values))

(defmethod finalize-layout ((layout lazy-array-layout))
  (loop for (store-buffer store) in (layout-buffer-stores layout) do
    ;; As an optimization, we attempt to find a load instruction buffer
    ;; that is large enough to hold all elements that will be written to by
    ;; STORE (or almost large enough, in which case the buffer is suitably
    ;; resized).  If such a buffer exists, we can eliminate one copy kernel
    ;; by having the store instruction write to that buffer directly.
    (let ((store-shape (buffer-shape store-buffer))
          (best-fit-buffer nil)
          (best-fit-intersection nil)
          (best-fit-cover nil)
          (best-fit-growth nil))
      (declare (type (or null buffer) best-fit-buffer))
      (declare (type (or null shape) best-fit-cover))
      (declare (type (or null unsigned-byte) best-fit-growth))
      (loop for (load-buffer . nil) in (layout-buffer-loads layout) do
        (let* ((load-shape (buffer-shape load-buffer))
               (intersection (shape-intersection load-shape store-shape)))
          ;; If the load buffer's shape doesn't intersect with the store
          ;; buffer's shape, do nothing.
          (unless (empty-shape-p intersection)
            ;; If there is an intersection, the buffer may qualify for
            ;; being optimized away, in case it intersects with more than
            ;; 50 percent of the store buffer.
            (when (> (shape-size intersection) (ceiling (shape-size store-shape) 2))
              ;; For merging a load and the store buffer, the load buffer
              ;; needs to be resized to a space that covers both shapes.
              (let* ((cover (fuse-shapes store-shape load-shape))
                     (growth (- (shape-size cover) (shape-size load-shape))))
                (when (and
                       ;; Make sure that we don't accidentally resize a
                       ;; buffer that is already allocated somewhere.
                       (buffer-reusablep load-buffer)
                       ;; Only resize if it doesn't increase the size of
                       ;; the load buffer by more than the entire size of
                       ;; the store buffer.
                       (< growth (shape-size store-shape))
                       ;; Only accept the new 'best fit' if it has a
                       ;; smaller cover than any of the previous ones.
                       (or (null best-fit-growth)
                           (< growth best-fit-growth)
                           ;; When the growth is equal to that of the best
                           ;; fit, accept the new entry if it has a smaller
                           ;; load buffer.
                           (and (= growth best-fit-growth)
                                (< (shape-size load-shape)
                                   (shape-size (buffer-shape best-fit-buffer))))))
                  (rotatef best-fit-buffer load-buffer)
                  (rotatef best-fit-intersection intersection)
                  (rotatef best-fit-cover cover)
                  (rotatef best-fit-growth growth))))
            (when load-buffer
              (make-copy-kernel
               intersection
               load-buffer
               store-buffer
               (identity-transformation (shape-rank intersection)))))))
      (when best-fit-buffer
        (setf (buffer-shape best-fit-buffer) best-fit-cover)
        (substitute-buffer best-fit-buffer store-buffer)))))

(defun make-copy-kernel (iteration-space target-buffer source-buffer transformation)
  ;; When the source buffer has a single kernel writing to it, and when
  ;; that kernel is a copy kernel, the new copy kernel can skip past the
  ;; existing one by composing the respective transformations.
  (trivia:when-match (list (list kernel _))
      (buffer-writers source-buffer)
    (when (copy-kernel-p kernel)
      (return-from make-copy-kernel
        (make-copy-kernel
         iteration-space
         target-buffer
         (load-instruction-buffer
          (copy-kernel-load-instruction kernel))
         (compose-transformations
          (copy-kernel-transformation kernel)
          transformation)))))
  (let* ((load
           (make-load-instruction source-buffer transformation))
         (store
           (make-store-instruction
            (cons 0 load)
            target-buffer
            (identity-transformation (shape-rank iteration-space))))
         (kernel
           (make-kernel
            :iteration-space iteration-space
            :sources `((,source-buffer ,load))
            :targets `((,target-buffer ,store)))))
    (assign-instruction-numbers kernel)
    (push `(,kernel ,load) (buffer-readers source-buffer))
    (push `(,kernel ,store) (buffer-writers target-buffer))
    kernel))

(defun copy-kernel-p (kernel)
  (trivia:when-match (list (list _ load-instruction))
      (kernel-sources kernel)
    (trivia:when-match (list (list _ store-instruction))
        (kernel-targets kernel)
      (trivia:when-match (list (cons _ input))
          (instruction-inputs store-instruction)
        (when (eq input load-instruction)
          (return-from copy-kernel-p t))))))

(defun copy-kernel-transformation (kernel)
  (trivia:when-match (list (list _ load-instruction))
      (kernel-sources kernel)
    (trivia:when-match (list (list _ store-instruction))
        (kernel-targets kernel)
      (return-from copy-kernel-transformation
        (compose-transformations
         (load-instruction-transformation load-instruction)
         (store-instruction-transformation store-instruction)))))
  (error "Not a copy kernel: ~S." kernel))

(defun copy-kernel-load-instruction (kernel)
  (trivia:if-match (list (list _ load-instruction))
      (kernel-sources kernel)
    load-instruction
    (error "Not a copy kernel: ~S." kernel)))
