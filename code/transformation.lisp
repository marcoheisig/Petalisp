;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; the valid index space transformations in Petalisp

(in-package :petalisp) (in-suite petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  index space transformations
;;;
;;; (1) translating the indices by a constant
;;; (2) multiplying the indices by a constant
;;; (3) permuting the dimensions
;;; (4) introducing dimensions with an unary range
;;; (5) removing dimensions with an unary range
;;;
;;; The class `index-space-transformation' contains all objects that are
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
;;; One observes, that every index space transformation on an index vector
;;; x can be represented as Ax + b, where b is the vector corresponding to
;;; (1) and (4) and A is the matrix corresponding to (2), (3), (4) and
;;; (5). The matrix A has a very particular structure - It has at most one
;;; nonzero entry per row and column. We call such a matrix
;;; `scaled-permutation-matrix'.


(define-class index-space-transformation (transformation)
  ((input-constraints :type (simple-array (or null integer) (*)))
   (linear-operator :type scaled-permutation-matrix)
   (translation-vector :type (simple-array integer (*)))))

(defmethod input-dimension ((instance index-space-transformation))
  (length (input-constraints instance)))

(defmethod output-dimension ((instance index-space-transformation))
  (length (translation-vector instance)))

(defmethod initialize-instance :before ((instance index-space-transformation)
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

(defmethod equal? ((t1 index-space-transformation)
                   (t2 index-space-transformation))
  (and (equalp (input-constraints t1)
               (input-constraints t2))
       (equalp (translation-vector t1)
               (translation-vector t2))
       (equal? (linear-operator t1)
               (linear-operator t2))))

(defmethod compose ((g index-space-transformation) (f index-space-transformation))
  ;; A2(A1 x + b1) + b2 = A2 A1 x + A2 b1 + b2
  (let ((A1 (linear-operator f))
        (A2 (linear-operator g))
        (b1 (translation-vector f))
        (b2 (translation-vector g)))
    (let ((input-constraints (input-constraints f))
          (linear-operator (compose A2 A1))
          (translation-vector (map 'vector #'+ (spm-vector-product A2 b1) b2)))
      (make-instance
       'index-space-transformation
       :input-constraints input-constraints
       :linear-operator linear-operator
       :translation-vector translation-vector))))

(defmethod invert ((object index-space-transformation))
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
    (let* ((linear-operator (invert A))
           (translation-vector (spm-vector-product linear-operator b)))
      (make-instance
       'index-space-transformation
       :input-constraints input-constraints
       :linear-operator linear-operator
       :translation-vector translation-vector))))

(defmethod print-object ((object index-space-transformation) stream)
  (let ((inputs (loop :for input-constraint :across (input-constraints object)
                      :for sym :in (list-of-symbols (input-dimension object))
                      :collect (or input-constraint sym))))
    (prin1 `(τ ,inputs ,@(map 'list (λ Ax b (cond ((eql Ax 0) b)
                                                  ((numberp Ax) (+ Ax b))
                                                  ((eql b 0) Ax)
                                                  (t `(+ ,Ax ,b))))
                              (spm-sexps-product (linear-operator object) inputs)
                              (translation-vector object)))
           stream)))

(defmethod classify-transformation ((f function)
                                    (input-constraints vector)
                                    (output-dimension integer))
  (let* ((input-dimension (length input-constraints))
         (args (map 'list
                    (λ constraint (or constraint 0))
                    input-constraints))
         ;; F is applied to many slightly different arguments, so we build a
         ;; vector pointing to the individual conses of ARGS for fast random
         ;; access.
         (arg-conses (make-array input-dimension)))
    (loop :for arg-cons :on args :and i :from 0 :do
      (setf (aref arg-conses i) arg-cons))
    ;; Initially x is the zero vector (except for input constraints, which
    ;; are ignored by A), so f(x) = Ax + b = b
    (let ((translation-vector (multiple-value-call #'vector (apply f args)))
          ;; now determine the super sparse matrix A
          (column-indices (make-array output-dimension
                                      :element-type 'array-index
                                      :initial-element 0))
          (values (make-array output-dimension
                              :element-type 'rational
                              :initial-element 0)))
      ;; set one input at a time from zero to one (ignoring those with
      ;; constraints) and check how it changes the result
      (loop :for input-constraint :across input-constraints
            :for arg-cons :across arg-conses
            :for column-index :from 0 :do
              (unless input-constraint
                (setf (car arg-cons) 1)
                ;; find the row of A corresponding to the mutated input
                (let ((results (multiple-value-call #'vector (apply f args))))
                  (loop :for result :across results
                        :for offset :across translation-vector
                        :for row-index :from 0 :do
                          (when (/= result offset)
                            (setf (aref column-indices row-index) column-index)
                            (setf (aref values row-index) (- result offset)))))
                (setf (car arg-cons) 0)))
      (let ((linear-operator
              (scaled-permutation-matrix
               output-dimension input-dimension column-indices values)))
        ;; optional but oh so helpful: check whether the derived mapping
        ;; satisfies other inputs, i.e. the mapping can indeed be represented
        ;; as Ax + b
        (loop :for input-constraint :across input-constraints
              :for arg-cons :across arg-conses :do
                (unless input-constraint
                  (setf (car arg-cons) 3)))
        (let* ((result-1 (multiple-value-call #'vector args))
               (Ax (spm-vector-product
                    linear-operator
                    (make-array input-dimension
                                :element-type 'rational
                                :initial-contents args)))
               (result-2 (map 'vector #'- Ax translation-vector)))
          (assert (every #'= result-1 result-2) ()
                  "Not a valid transformation:~%  ~S"
                  f))
        (if (and (= input-dimension output-dimension)
                 (every #'null input-constraints)
                 (every #'zerop translation-vector)
                 (spm-identity? linear-operator))
            (make-instance
             'identity-transformation
             :dimension input-dimension)
            (make-instance
             'index-space-transformation
             :input-constraints input-constraints
             :linear-operator linear-operator
             :translation-vector translation-vector))))))
