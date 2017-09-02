;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

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
   (translation-vector :type (simple-array integer (*))))
  (:metaclass funcallable-standard-class))

(defmethod input-dimension ((instance affine-transformation))
  (length (input-constraints instance)))

(defmethod output-dimension ((instance affine-transformation))
  (length (translation-vector instance)))

(defmethod initialize-instance :before ((instance affine-transformation)
                                        &key
                                          input-constraints
                                          linear-operator
                                          translation-vector)
  (assert (and (= (input-dimension linear-operator)
                  (length input-constraints))
               (= (output-dimension linear-operator)
                  (length translation-vector)))
          (input-constraints linear-operator translation-vector)
          "Incompatibe shapes:~%  ~S~%  ~S~%  ~S~%"
          input-constraints linear-operator translation-vector))

(defmethod equal? ((t1 affine-transformation)
                   (t2 affine-transformation))
  (and (equalp (input-constraints t1)
               (input-constraints t2))
       (equalp (translation-vector t1)
               (translation-vector t2))
       (equal? (linear-operator t1)
               (linear-operator t2))))

(defmethod composition ((g affine-transformation) (f affine-transformation))
  ;; A2(A1 x + b1) + b2 = A2 A1 x + A2 b1 + b2
  (let ((A1 (linear-operator f))
        (A2 (linear-operator g))
        (b1 (translation-vector f))
        (b2 (translation-vector g)))
    (let ((input-constraints (input-constraints f))
          (linear-operator (matrix-product A2 A1))
          (translation-vector (map 'vector #'+ (matrix-product A2 b1) b2)))
      (make-affine-transformation
       input-constraints
       linear-operator
       translation-vector))))

(defmethod inverse ((object affine-transformation))
  ;;    f(x) = (Ax + b)
  ;; f^-1(x) = A^-1(x - b) = A^-1 x - A^-1 b
  (let ((A (linear-operator object))
        (b (translation-vector object))
        (input-constraints (make-array (output-dimension object)
                                       :initial-element nil
                                       :element-type '(or null integer))))
    ;; the new input constraints are the values of b whenever the
    ;; corresponding row of A is zero
    (loop :for value :across (spm-values A)
          :for translation :across b
          :for row-index :from 0 :do
            (when (zerop value)
              (setf (aref input-constraints row-index) translation)))
    (let* ((linear-operator (matrix-inverse A))
           (translation-vector (matrix-product linear-operator b)))
      (map-into translation-vector #'- translation-vector) ; negate b
      (loop :for index :below (length translation-vector)
            :and input-constraint :across (input-constraints object) :do
              (when input-constraint
                (assert (= (aref translation-vector index) 0))
                (setf (aref translation-vector index) input-constraint)))
      (make-affine-transformation
       input-constraints
       linear-operator
       translation-vector))))

(defmethod print-object ((object affine-transformation) stream)
  (let ((inputs (loop :for input-constraint :across (input-constraints object)
                      :for sym :in (list-of-symbols (input-dimension object))
                      :collect (or input-constraint sym))))
    (prin1 `(τ ,inputs ,@(map 'list (λ Ax b (cond ((eql Ax 0) b)
                                                  ((numberp Ax) (+ Ax b))
                                                  ((eql b 0) Ax)
                                                  (t `(+ ,Ax ,b))))
                              (matrix-product (linear-operator object) inputs)
                              (translation-vector object)))
           stream)))

