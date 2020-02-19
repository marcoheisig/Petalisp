;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;; The purpose of IR conversion is to turn a data flow graph, whose nodes
;;; are strided arrays, into an analogous graph, whose nodes are buffers
;;; and kernels.  Kernels and buffers alternate, such that the inputs and
;;; outputs of a kernel are always buffers, and such that the inputs and
;;; outputs of a buffer are always kernels.
;;;
;;; The IR conversion algorithm proceeds along the following steps:
;;;
;;; 1. A hash table is created that maps certain strided arrays to buffers
;;;    of the same size and element type.  This table is constructed such
;;;    that any subgraph without these nodes is a tree and contains no
;;;    reduction nodes.
;;;
;;; 2. Each root of a subtree from step 1 is turned into one or more
;;;    kernels.  All fusion nodes in the tree are eliminated by choosing
;;;    the iteration space of the kernels appropriately.
;;;
;;; 3. All buffers are updated to contain a list of kernels that read to
;;;    them or write from them.

(defun ir-from-lazy-arrays (lazy-arrays)
  (let ((*buffer-table* (compute-buffer-table lazy-arrays)))
    ;; Now create a list of kernels for each entry in the buffer table.
    (maphash
     (lambda (lazy-array buffer)
       (unless (or (eq buffer '.range-immediate.)
                   (immediatep lazy-array))
         (create-kernels lazy-array)))
     *buffer-table*)
    ;; Finally, return the buffers corresponding to the root and leaf nodes
    ;; and mark them as non-reusable, to avoid that their memory is
    ;; reclaimed.
    (loop for lazy-array in lazy-arrays
          collect
          (let ((buffer (gethash lazy-array *buffer-table*)))
            (setf (buffer-reusablep buffer) nil)
            buffer))))

(defvar *root*)

;;; We compute a partitioning of the shape of the root into multiple
;;; iteration spaces.  These spaces are chosen such that their union is the
;;; shape of the root, and such that each iteration space selects only a
;;; single input of each encountered fusion node.  Each such iteration
;;; space is used to create one kernel.
(defun create-kernels (root)
  (let ((*root* root))
    (map-iteration-spaces
     (lambda (iteration-space)
       (let ((kernel (compute-kernel root iteration-space)))
         (assign-instruction-numbers kernel)
         ;; Update the inputs and outputs of all buffers to match the
         ;; inputs and outputs of the corresponding kernels.
         (map-kernel-inputs
          (lambda (buffer)
            (pushnew kernel (buffer-outputs buffer)))
          kernel)
         (map-kernel-outputs
          (lambda (buffer)
            (pushnew kernel (buffer-inputs buffer)))
          kernel)))
     root)))

;;; Create one kernel from the given root and iteration space.
(defun compute-kernel (root iteration-space)
  (multiple-value-bind (store-instruction load-instructions)
      (compute-kernel-body root iteration-space)
    (make-kernel
     :iteration-space iteration-space
     :load-instructions load-instructions
     :store-instructions (list store-instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing the Kernel Body

(defvar *loads*)

(defun compute-kernel-body (root iteration-space)
  (let* ((*loads* '())
         (rank (shape-rank iteration-space))
         (inner-transformation (identity-transformation rank))
         (outer-transformation (outer-transformation rank)))
    (values
     (make-store-instruction
      (if (typep root 'reduction)
          (compute-value root iteration-space inner-transformation)
          (cons 0 (make-reduce-instruction
                   'values
                   (list
                    (compute-value root (shrink-shape iteration-space) outer-transformation)))))
      (gethash root *buffer-table*)
      outer-transformation)
     *loads*)))

(defun outer-transformation (rank)
  (petalisp.utilities:with-vector-memoization (rank)
    (make-transformation
     :input-rank rank
     :output-mask (loop for index from 1 below rank collect index))))

;;; Return the 'value' of ROOT for a given point in the iteration space of
;;; the kernel, i.e., return a cons cell whose cdr is an instruction and
;;; whose car is an integer denoting which of the N values of the
;;; instructions is referenced.
(defgeneric compute-value (node iteration-space transformation))

;; Check whether we are dealing with a leaf, i.e., a node that has a
;; corresponding entry in the buffer table and is not the root node.  If
;; so, return a reference to that buffer.
(defmethod compute-value :around
    ((node lazy-array)
     (iteration-space shape)
     (transformation transformation))
  ;; The root node always has an entry in the buffer table, yet we do not
  ;; want to treat it as a leaf node.
  (if (eq node *root*)
      (call-next-method)
      (multiple-value-bind (buffer buffer-p)
          (gethash node *buffer-table*)
        (if (not buffer-p)
            (call-next-method)
            (if (eq buffer '.range-immediate.)
                (cons 0 (make-iref-instruction transformation))
                (let ((load (make-load-instruction buffer transformation)))
                  (push load *loads*)
                  (cons 0 load)))))))

;; TODO This is just a quick hack.  A proper solution is needed eventually.
(defun simplify-operator (operator)
  (cond ((eq operator #'+) '+)
        ((eq operator #'-) '-)
        ((eq operator #'*) '*)
        ((eq operator #'/) '/)
        (t operator)))

(defmethod compute-value
    ((lazy-map lazy-map)
     (iteration-space shape)
     (transformation transformation))
  (cons (value-n lazy-map)
        (make-call-instruction
         (simplify-operator (operator lazy-map))
         (loop for input in (inputs lazy-map)
               collect
               (compute-value input iteration-space transformation)))))

(defmethod compute-value
    ((reduction reduction)
     (iteration-space shape)
     (transformation transformation))
  (let* ((inputs (inputs reduction))
         ;; We do not have to intersect the shape with the current
         ;; iteration space, because reductions only occur as the root
         ;; node of a kernel.
         (shape (shape (first inputs))))
    (cons (value-n reduction)
          (make-reduce-instruction
           (operator reduction)
           (loop for input in (inputs reduction)
                 collect
                 (compute-value input shape transformation))))))

(defmethod compute-value
    ((reference reference)
     (iteration-space shape)
     (transformation transformation))
  (compute-value
   (input reference)
   (transform
    (shape-intersection iteration-space (shape reference))
    (transformation reference))
   (compose-transformations
    (transformation reference)
    transformation)))

(defmethod compute-value
    ((fusion fusion)
     (iteration-space shape)
     (transformation transformation))
  (let ((input (find iteration-space (inputs fusion)
                     :key #'shape
                     :test #'shape-intersectionp)))
    (compute-value
     input
     (shape-intersection iteration-space (shape input))
     transformation)))

(defmethod compute-value
    ((lazy-array lazy-array)
     (iteration-space shape)
     (transformation transformation))
  (error "Can't IR convert ~S" lazy-array))
