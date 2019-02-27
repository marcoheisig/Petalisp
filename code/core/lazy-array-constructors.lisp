;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; References to Other Lazy Arrays

(defgeneric make-reference (input shape transformation)
  (:argument-precedence-order transformation shape input))

;;; Compose consecutive references.
(defmethod make-reference
    ((reference reference) (shape shape) (transformation transformation))
  (make-reference
   (input reference)
   shape
   (compose-transformations (transformation reference) transformation)))

;;; Drop references with no effect.
(defmethod make-reference
    ((lazy-array lazy-array) (shape shape) (identity-transformation identity-transformation))
  (if (set-equal (shape lazy-array) shape)
      lazy-array
      (call-next-method)))

;;; Handle empty shapes.
(defmethod make-reference
    ((lazy-array lazy-array) (empty-set empty-set) (transformation transformation))
  (empty-array))

;;; The default - construct a new reference.
(defmethod make-reference
    ((lazy-array lazy-array) (shape shape) (transformation transformation))
  (make-instance 'reference
    :element-type (element-type lazy-array)
    :inputs (list lazy-array)
    :shape shape
    :transformation transformation))

;;; Error handling.
(defmethod make-reference :before
    ((lazy-array lazy-array) (shape shape) (transformation transformation))
  (let ((relevant-shape (transform shape transformation))
        (input-shape (shape lazy-array)))
    (unless (and (= (rank relevant-shape) (rank input-shape))
                 (set-subsetp relevant-shape input-shape))
      (error "~@<The shape referenced by the current reference is ~S, ~
                 which is not a subspace of ~S, the shape of the input of ~
                 the current reference.~:@>"
             relevant-shape input-shape))))

(defun reshape (array &rest shapes-and-transformations)
  (labels ((reshape-with-shape (lazy-array shape)
             (make-reference
              lazy-array
              shape
              (broadcasting-transformation shape (shape lazy-array))))
           (reshape-with-transformation (lazy-array transformation)
             (make-reference
              lazy-array
              (transform (shape lazy-array) transformation)
              (invert-transformation transformation)))
           (reshape1 (lazy-array modifier)
             (if (shapep modifier)
                 (reshape-with-shape lazy-array modifier)
                 (reshape-with-transformation lazy-array modifier))))
    (reduce #'reshape1 shapes-and-transformations :initial-value (coerce-to-lazy-array array))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fusions

(defun sanitize-fusion-inputs (inputs)
  (let ((lazy-arrays
          (loop for input in inputs
                unless (empty-array-p input)
                  collect (coerce-to-lazy-array input))))
    (unless (petalisp.utilities:identical lazy-arrays :key #'rank)
      (error
       "~@<The shapes of the arguments to a fusion operation must ~
         have the same rank, but the supplied arguments have the ~
         ranks ~{~#[~;~S~;~S and ~S~:;~@{~S~#[~;, and ~:;, ~]~}~]~}.~:@>"
       (remove-duplicates (mapcar #'rank lazy-arrays))))
    lazy-arrays))

(defun fuse (&rest inputs)
  (let ((lazy-arrays (sanitize-fusion-inputs inputs)))
    ;; When given more than one input, check for disjointnes.
    (when (cddr lazy-arrays)
      (map-combinations
       (lambda (two-inputs)
         (destructuring-bind (input-1 input-2) two-inputs
           (let ((shape-1 (shape input-1))
                 (shape-2 (shape input-2)))
             (assert (not (set-intersectionp shape-1 shape-2)) ()
                     "~@<The index shapes of the arguments to a fusion operation ~
                         must be disjoint, but shape ~S and shape ~S have the ~
                         common subshape ~S.~:@>"
                     shape-1
                     shape-2
                     (set-intersection shape-1 shape-2)))))
       lazy-arrays :length 2 :copy nil))
    (make-fusion lazy-arrays)))

(defun fuse* (&rest inputs)
  (let* ((lazy-arrays (sanitize-fusion-inputs inputs))
         (shapes (subdivision (mapcar #'shape lazy-arrays)))
         (identity (identity-transformation (rank (first lazy-arrays)))))
    (flet ((reference-origin (shape)
             (make-reference
              (find shape lazy-arrays :from-end t :key #'shape :test #'set-subsetp)
              shape identity)))
      (make-fusion (mapcar #'reference-origin shapes)))))

;; Create a fusion, assuming INPUTS are non-empty, non-overlapping lazy-arrays.
(defun make-fusion (inputs)
  (let ((shape (shape-union (mapcar #'shape inputs))))
    (trivia:match inputs
      ((list) (empty-array))
      ((list x) x)
      (_ (make-instance 'fusion
           :element-type (simplified-types:simplify-type
                          `(or ,@(mapcar #'element-type inputs)))
           :inputs inputs
           :shape shape)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Immediates

(defun empty-array ()
  (load-time-value (make-instance 'empty-array)))

(defun make-array-immediate (array)
  (assert (arrayp array))
  (assert (typep (row-major-aref array 0)
                 '(not (cons lazy-array t))))
  (if (zerop (array-total-size array))
      (empty-array)
      (let ((element-type
              (simplified-types:simplify-type
               (array-element-type array))))
        (make-instance 'array-immediate
          :shape (shape array)
          :storage array
          :element-type element-type))))

(defun make-range-immediate (range)
  (make-instance 'range-immediate
    :element-type `(integer ,(range-start range) ,(range-end range))
    :shape (make-shape (list range))))

(defun indices (array-or-shape &optional (axis 0))
  (let ((shape (if (shapep array-or-shape)
                   array-or-shape
                   (shape array-or-shape))))
    (if (set-emptyp shape)
        (empty-array)
        (let ((rank (rank shape)))
          (assert (<= 0 axis (1- rank)))
          (make-reference
           (make-range-immediate (nth axis (ranges shape)))
           shape
           (make-transformation
            :input-rank rank
            :output-mask (vector axis)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Applications and Reductions

(defun broadcast-arrays (&rest arrays)
  (let* ((lazy-arrays (mapcar #'coerce-to-lazy-array arrays))
         (shape (apply #'broadcast-shapes lazy-arrays)))
    (values
     (map-into lazy-arrays
               (lambda (lazy-array) (reshape lazy-array shape))
               lazy-arrays)
     shape)))

(declaim (inline α) (notinline α-aux))
(defun α (arg-1 arg-2 &rest more-args)
  "Apply FUNCTION element-wise to OBJECT and MORE-OBJECTS, like a CL:MAPCAR
for Petalisp data structures.  When the rank of some of the inputs
mismatch, broadcast the smaller objects."
  (if (integerp arg-1)
      (apply #'α-aux arg-1 (coerce arg-2 'function) more-args)
      (apply #'α-aux 1 (coerce arg-1 'function) arg-2 more-args)))

(defun α-aux (n-outputs function &rest arguments)
  (declare (type (integer 0 (#.multiple-values-limit)) n-outputs)
           (type function function))
  (flet ((return-outputs (list-of-outputs)
           (return-from α-aux
             (values-list list-of-outputs))))
    (multiple-value-bind (inputs shape) (apply #'broadcast-arrays arguments)
      ;; Check whether the inputs are empty.
      (when (set-emptyp shape)
        (return-outputs
         (make-list n-outputs :initial-element (empty-array))))
      ;; Attempt constant folding.
      #+nil
      (block constant-folding
        (labels ((array-value (array)
                   (if (= 1 (array-total-size array))
                       (row-major-aref array 0)
                       (return-from constant-folding)))
                 (lazy-array-value (lazy-array)
                   (typecase lazy-array
                     (array-immediate
                      (array-value (storage lazy-array)))
                     (reference
                      (lazy-array-value (input lazy-array)))
                     (t (return-from constant-folding)))))
          (let* ((inputs (mapcar #'lazy-array-value inputs))
                 (values (multiple-value-list (apply function inputs))))
            (return-outputs
             (loop for value-n below n-outputs
                   collect (reshape (pop values) shape))))))
      ;; Otherwise, create a new application.
      (let ((restricted-function
              (apply #'restricted-functions:restrict nil function
                     (mapcar #'element-type inputs))))
        (return-outputs
         (loop for value-n below n-outputs
               collect
               (make-instance 'application
                 :operator restricted-function
                 :value-n value-n
                 :inputs inputs
                 :shape shape)))))))

(defun β (function array &rest more-arrays)
  (alexandria:coercef function 'function)
  (let ((k (1+ (length more-arrays))))
    (multiple-value-bind (inputs input-shape) (apply #'broadcast-arrays array more-arrays)
      (values-list
       (if (set-emptyp input-shape)
           (make-list k :initial-element (empty-array))
           (let* ((shape (make-shape (cdr (ranges input-shape))))
                  (input-element-types (mapcar #'element-type inputs))
                  (restricted-function
                    (apply #'restricted-functions:restrict nil function
                           (append input-element-types input-element-types))))
             (loop for value-n below k
                   collect
                   (make-instance 'reduction
                     :operator restricted-function
                     :value-n value-n
                     :inputs inputs
                     :shape shape))))))))
