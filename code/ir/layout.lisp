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
  (lazy-array nil :type lazy-array :read-only t))

(defstruct (range-immediate-layout
            (:include layout)
            (:constructor make-range-immediate-layout
                (range-immediate &aux (lazy-array range-immediate)))))

(defstruct (array-immediate-layout
            (:include layout)
            (:conc-name layout-)
            (:constructor make-array-immediate-layout
                (array-immediate storage
                 &aux
                   (lazy-array array-immediate)
                   (buffer
                    (make-buffer
                     :shape (shape array-immediate)
                     :ntype (element-ntype array-immediate)
                     :storage storage)))))
  (buffer nil :type buffer))

(defstruct (lazy-array-layout
            (:include layout)
            (:conc-name layout-)
            (:constructor make-lazy-array-layout
                (lazy-array)))
  (buffer-loads '())
  (buffer-stores '()))

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
    (let* ((lazy-array (layout-lazy-array layout))
           (buffer (make-buffer
                    :shape shape
                    :reusablep t
                    :ntype (petalisp.type-inference:generalize-ntype
                            (element-ntype lazy-array))))
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
  (let* ((lazy-array (layout-lazy-array layout))
         (buffer (make-buffer
                  :shape shape
                  :reusablep t
                  :ntype (petalisp.type-inference:generalize-ntype
                          (element-ntype lazy-array))))
         (store (make-store-instruction value buffer transformation)))
    (push `(,buffer ,store) (layout-buffer-stores layout))
    store))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Layout Finalization

(defmethod finalize-layout ((layout range-immediate-layout))
  (values))

(defmethod finalize-layout ((layout array-immediate-layout))
  (values))

;;; At this point, we know all loads and store instructions related to a
;;; particular layout, but none of the target buffers of a store
;;; instruction is used.  During finalization, we introduce the necessary
;;; copy kernels to move the data from each store instruction buffer to all
;;; relevant load instruction buffers.
(defmethod finalize-layout ((layout lazy-array-layout))
  (loop for (store-buffer store) in (layout-buffer-stores layout) do
    (loop for (load-buffer . nil) in (layout-buffer-loads layout) do
      (let ((intersection
              (shape-intersection
               (buffer-shape store-buffer)
               (buffer-shape load-buffer))))
        (when intersection
          (let* ((transformation
                   (identity-transformation (shape-rank intersection)))
                 (load
                   (make-load-instruction store-buffer transformation))
                 (store
                   (make-store-instruction (cons 0 load) load-buffer transformation))
                 (kernel
                   (make-kernel
                    :iteration-space intersection
                    :sources `((,store-buffer ,load))
                    :targets `((,load-buffer ,store)))))
            (assign-instruction-numbers kernel)
            (push `(,kernel ,load) (buffer-readers store-buffer))
            (push `(,kernel ,store) (buffer-writers load-buffer))
            (values)))))))
