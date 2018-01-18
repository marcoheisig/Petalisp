;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/transformations/affine-transformation
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/transformation
   :petalisp/core/transformations/identity-transformation)
  (:export
   #:affine-transformation
   #:affine-transformation?
   #:invertible?
   #:linear-operator
   #:input-constraints
   #:translation))

(in-package :petalisp/core/transformations/affine-transformation)

;;; Affine transformations in the sense of Petalisp consist of the
;;; following five elementary operations:
;;;
;;; (1) translating the indices by a constant
;;; (2) multiplying the indices by a constant
;;; (3) permuting the dimensions
;;; (4) introducing dimensions with an unary range
;;; (5) removing dimensions with an unary range
;;;
;;; The class `affine-transformation' contains all objects that are
;;; formed by functional composition of these five elementary operations.
;;;
;;; In linear algebra lingo, we have
;;;
;;; (1) adding a vector
;;; (2) multiplying with a diagonal matrix
;;; (3) multiplying with a permutation matrix
;;; (4) multiplying with an identity matrix, but with some zero rows inserted and adding a vector
;;; (5) multiplying with an identity matrix, but with some rows removed
;;;
;;; One observes, that every affine transformation on an index vector
;;; x can be represented as Ax + b, where b is the vector corresponding to
;;; (1) and (4) and A is the matrix corresponding to (2), (3), (4) and
;;; (5). The matrix A has a very particular structure - It has at most one
;;; nonzero entry per row and column. We call such a matrix
;;; `scaled-permutation-matrix'.

(define-class affine-transformation (transformation)
  ((input-constraints :type (simple-array (or null integer) (*)))
   (linear-operator :type scaled-permutation-matrix)
   (translation :type (simple-array integer (*))))
  (:metaclass funcallable-standard-class))

(defun affine-transformation (&key input-dimension output-dimension
                                input-constraints scaling translation permutation)
  (declare (type (or array-length null) input-dimension output-dimension))
  ;; derive/check input and output dimension
  (macrolet
      ((register (place vector-or-number)
         (once-only (vector-or-number)
           `(when ,vector-or-number
              (let ((value (if (vectorp ,vector-or-number)
                               (length ,vector-or-number)
                               ,vector-or-number)))
                (cond ((not ,place)
                       (setf ,place value))
                      ((/= ,place value)
                       (error "Contradictory values for ~A." ',place))))))))
    (register input-dimension input-constraints)
    (register output-dimension scaling)
    (register output-dimension translation)
    (register output-dimension permutation)
    (unless input-dimension (setf input-dimension output-dimension))
    (unless output-dimension (setf output-dimension input-dimension)))
  (unless input-dimension
    (error "Insufficient arguments to derive transformation dimensions."))
  ;; check for the identity transformation
  (if (and (= input-dimension output-dimension)
           (or (not input-constraints)
               (every #'null input-constraints))
           (or (not translation)
               (every #'zerop translation))
           (or (not scaling)
               (every (lambda (x) (= x 1)) scaling))
           (or (not permutation)
               (loop for i from 0 and j across permutation
                     always (= i j))))
      (identity-transformation input-dimension)
      ;; otherwise create a suitable affine transformation
      ;; TODO there is a vast potential for memoizing trivial arrays
      (let ((input-constraints
              (or input-constraints
                  (make-array input-dimension :initial-element nil)))
            (translation
              (or translation
                  (make-array output-dimension :initial-element 0)))
            (scaling
              (or scaling
                  (make-array output-dimension :initial-element 1)))
            (permutation
              (or permutation
                  (let ((permutation (make-array output-dimension)))
                    (loop for index below output-dimension do
                      (setf (aref permutation index) index))
                    permutation))))
        (with-memoization ((list input-constraints translation scaling permutation)
                           :test #'equalp)
          (make-instance 'affine-transformation
            :input-constraints input-constraints
            :linear-operator
            (scaled-permutation-matrix
             output-dimension input-dimension
             permutation scaling)
            :translation translation)))))

(defmethod enlarge-transformation
    ((transformation affine-transformation) scale offset)
  (let ((input-dimension (input-dimension transformation))
        (output-dimension (output-dimension transformation))
        (matrix (linear-operator transformation)))
    (let ((input-constraints (make-array (1+ input-dimension)))
          (permutation       (make-array (1+ output-dimension)))
          (scaling           (make-array (1+ output-dimension)))
          (translation       (make-array (1+ output-dimension))))
      (replace input-constraints (input-constraints transformation))
      (replace permutation       (spm-column-indices matrix))
      (replace scaling           (spm-values matrix))
      (replace translation       (translation transformation))
      (setf (aref input-constraints input-dimension) nil)
      (setf (aref permutation       output-dimension) input-dimension)
      (setf (aref scaling           output-dimension) scale)
      (setf (aref translation       output-dimension) offset)
      (affine-transformation
       :input-constraints input-constraints
       :permutation permutation
       :scaling scaling
       :translation translation))))

(defmethod generic-unary-funcall ((transformation affine-transformation)
                                  (s-expressions list))
  (map 'list (λ Ax b (cond ((eql Ax 0) b)
                           ((numberp Ax) (+ Ax b))
                           ((eql b 0) Ax)
                           (t `(+ ,Ax ,b))))
       (matrix-product (linear-operator transformation) s-expressions)
       (translation transformation)))

(defmethod input-dimension ((instance affine-transformation))
  (length (input-constraints instance)))

(defmethod output-dimension ((instance affine-transformation))
  (length (translation instance)))

(defmethod initialize-instance :before ((instance affine-transformation)
                                        &key
                                          input-constraints
                                          linear-operator
                                          translation)
  (assert (and (= (input-dimension linear-operator)
                  (length input-constraints))
               (= (output-dimension linear-operator)
                  (length translation)))
          (input-constraints linear-operator translation)
          "Incompatibe shapes:~%  ~S~%  ~S~%  ~S~%"
          input-constraints linear-operator translation))

(defmethod composition :around ((g affine-transformation) (f affine-transformation))
  (with-memoization ((cons f g) :test #'equal)
    (call-next-method)))

(defmethod composition ((g affine-transformation) (f affine-transformation))
  ;; A2 (A1 x + b1) + b2 = A2 A1 x + A2 b1 + b2
  (let ((A1 (linear-operator f))
        (A2 (linear-operator g))
        (b1 (translation f))
        (b2 (translation g)))
    (let ((input-constraints (input-constraints f))
          (linear-operator (matrix-product A2 A1))
          (translation (map 'vector #'+ (matrix-product A2 b1) b2)))
      (affine-transformation
       :input-constraints input-constraints
       :permutation (spm-column-indices linear-operator)
       :scaling (spm-values linear-operator)
       :translation translation))))

(defgeneric invertible? (transformation)
  (:method ((transformation identity-transformation)) t)
  (:method ((transformation affine-transformation))
    (let ((effective-input-dimension
            (- (input-dimension transformation)
               (count-if-not #'null (input-constraints transformation))))
          (effective-output-dimension
            (- (output-dimension transformation)
               (count-if #'zerop (spm-values (linear-operator transformation))))))
      (= effective-input-dimension effective-output-dimension))))

(defmethod inverse :around ((transformation affine-transformation))
  (with-memoization (transformation :test #'eq)
    (call-next-method)))

(defmethod inverse :before ((transformation affine-transformation))
  (assert (invertible? transformation)))

(defmethod inverse ((object affine-transformation))
  ;;    f(x) = (Ax + b)
  ;; f^-1(x) = A^-1(x - b) = A^-1 x - A^-1 b
  (let ((A (linear-operator object))
        (b (translation object))
        (input-constraints (make-array (output-dimension object)
                                       :initial-element nil
                                       :element-type '(or null integer))))
    ;; the new input constraints are the values of b whenever the
    ;; corresponding row of A is zero
    (loop for value across (spm-values A)
          for translation across b
          for row-index from 0 do
            (when (zerop value)
              (setf (aref input-constraints row-index) translation)))
    (let* ((linear-operator (matrix-inverse A))
           (translation (matrix-product linear-operator b)))
      (map-into translation #'- translation) ; negate b
      (loop for index below (length translation)
            for input-constraint across (input-constraints object) do
              (when input-constraint
                (assert (= (aref translation index) 0))
                (setf (aref translation index) input-constraint)))
      (affine-transformation
       :input-constraints input-constraints
       :permutation (spm-column-indices linear-operator)
       :scaling (spm-values linear-operator)
       :translation translation))))

(defmethod do-outputs ((transformation affine-transformation)
                       (function function)
                       &rest sequences)
  (let ((matrix (linear-operator transformation)))
    (loop for output-index from 0
          for input-index across (spm-column-indices matrix)
          for scaling across (spm-values matrix)
          for offset across (translation transformation) do
            (apply function output-index input-index scaling offset
                   (flet ((input-element (sequence)
                            (elt sequence input-index)))
                     (mapcar #'input-element sequences))))))

(defmethod print-object ((object affine-transformation) stream)
  (let ((inputs
          (loop for input-constraint across (input-constraints object)
                for i from 0
                for index-symbol = (format-symbol :keyword "I~I" i)
                collect (or input-constraint index-symbol))))
    (prin1 `(τ ,inputs ,(funcall object inputs))
           stream)))

