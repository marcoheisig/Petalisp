;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; the valid index space transformations in Petalisp

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; special linear algebra subroutines for index space transformations

(defstruct (super-sparse-matrix
            (:conc-name ssm-)
            (:constructor %super-sparse-matrix (m n column-indices values))
            (:copier nil)
            (:predicate super-sparse-matrix?))
  "A `super-sparse-matrix' is a matrix with at most one nonzero entry per
row and column. Such matrices arise as the combination of permutation
matrices, diagonal matrices and selection matrices."
  (m nil :type array-length :read-only t)
  (n nil :type array-length :read-only t)
  (column-indices nil :type (simple-array array-index (*)) :read-only t)
  (values nil :type (simple-array rational (*))))

(defun super-sparse-matrix (m n column-indices values)
  (assert (= m (length column-indices) (length values)))
  (assert (every (λ column-index (< -1 column-index n)) column-indices))
  (%super-sparse-matrix m n column-indices values))

(defun ssm-vector-dot-product (ssm vec)
  "For a given m times n super sparse matrix SSM and a n-vector VEC, this
function returns the m-vector that is the dot product of SSM and VEC."
  (declare (type super-sparse-matrix ssm)
           (type (simple-array rational (*)) vec))
  (with-unsafe-optimizations
    (flet ((column-index (row-index)
             (aref (ssm-column-indices ssm) row-index))
           (value (row-index)
             (aref (ssm-values ssm) row-index)))
      (let* ((rows (ssm-m ssm))
             (result (make-array rows :element-type 'rational)))
        (dotimes (row-index rows)
          (setf (aref result row-index)
                (* (value row-index)
                   (aref vec (column-index row-index)))))))))

(defun ssm-ssm-dot-product (ssm-1 ssm-2)
  (declare (type super-sparse-matrix ssm-1 ssm-2))
  (with-unsafe-optimizations
    (let* ((m (ssm-m ssm-1))
           (n (ssm-n ssm-2))
           (column-indices (make-array m :element-type 'array-index
                                         :initial-element 0))
           (values (make-array m :element-type 'rational
                                 :initial-element 0)))
      (dotimes (i m)
        (let* ((k (aref (ssm-column-indices ssm-1) i))
               (j (aref (ssm-column-indices ssm-2) k)))
          (setf (aref column-indices i) j)
          (setf (aref values i)
                (* (aref (ssm-values ssm-1) i)
                   (aref (ssm-values ssm-2) k)))))
      (super-sparse-matrix m n column-indices values))))

(defun ssm-sexps-dot-product (ssm sexps)
  (map 'list (λ col val (cond ((eql val 0) 0)
                              ((eql val 1) (elt sexps col))
                              (t `(* ,(elt sexps col) ,val))))
       (ssm-column-indices ssm)
       (ssm-values ssm)))

;;; Note that a super sparse matrix is not generally invertible. The
;;; inverse as returned from this function assumes A is only applied to
;;; vectors which are zero whenever the corresponding column is zero.
(defun ssm-inverse (ssm)
  (declare (type super-sparse-matrix ssm))
  (with-unsafe-optimizations
    (let* ((original-column-indices (ssm-column-indices ssm))
           (original-values (ssm-values ssm))
           (m (ssm-n ssm))
           (n (ssm-m ssm))
           (column-indices (make-array m :element-type 'array-index
                                         :initial-element 0))
           (values (make-array m :element-type 'rational
                                 :initial-element 0)))
      (dotimes (row-index m)
        (let ((column-index (position row-index original-column-indices)))
          (when column-index
            (setf (aref column-indices row-index) column-index)
            (let ((value (aref original-values column-index)))
              (setf (aref values row-index)
                    (if (zerop value) 0 (/ value)))))))
      (super-sparse-matrix m n column-indices values))))

(defun ssm-identity? (ssm)
  (let ((m (ssm-m ssm)) (n (ssm-n ssm)))
    (and (= m n)
         (loop :for row-index :below m
               :for value :across (ssm-values ssm)
               :for column-index :across (ssm-column-indices ssm)
               :always (and (= column-index row-index)
                            (= value 1))))))

(defmethod input-dimension ((instance super-sparse-matrix))
  (ssm-n instance))

(defmethod output-dimension ((instance super-sparse-matrix))
  (ssm-m instance))

(defmethod invert ((ssm super-sparse-matrix))
  (ssm-inverse ssm))

(defmethod compose ((g super-sparse-matrix) (f super-sparse-matrix))
  (ssm-ssm-dot-product g f))

(defmethod equal? ((a super-sparse-matrix) (b super-sparse-matrix)) (equalp a b))

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
;;; `super-sparse-matrix'.


(define-class index-space-transformation (transformation)
  ((input-constraints :type (simple-array (or null integer) (*)))
   (linear-operator :type super-sparse-matrix)
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
              (super-sparse-matrix
               output-dimension input-dimension column-indices values)))
        ;; optional but oh so helpful: check whether the derived mapping
        ;; satisfies other inputs, i.e. the mapping can indeed be represented
        ;; as Ax + b
        (loop :for input-constraint :across input-constraints
              :for arg-cons :across arg-conses :do
                (unless input-constraint
                  (setf (car arg-cons) 3)))
        (let* ((result-1 (multiple-value-call #'vector args))
               (Ax (ssm-vector-dot-product
                    linear-operator
                    (make-array input-dimension
                                :element-type 'rational
                                :initial-contents args)))
               (result-2 (map 'vector #'- Ax translation-vector)))
          (assert (every #'= result-1 result-2) ()
                  "Not a valid transformation:~%  ~S"
                  f))
        (make-instance
         'index-space-transformation
         :input-constraints input-constraints
         :linear-operator linear-operator
         :translation-vector translation-vector)))))

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
          (translation-vector (map 'vector #'+ (ssm-vector-dot-product A2 b1) b2)))
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
    (loop :for value :across (ssm-values A)
          :for translation :across b
          :for row-index :from 0 :do
            (when (zerop value)
              (setf (aref input-constraints row-index) translation)))
    (let* ((linear-operator (invert A))
           (translation-vector (ssm-vector-dot-product linear-operator b)))
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
                              (ssm-sexps-dot-product (linear-operator object) inputs)
                              (translation-vector object)))
           stream)))


(defmethod initialize-instance :after ((instance index-space-transformation)
                                       &key &allow-other-keys)
  (when (and (= (input-dimension instance) (output-dimension instance))
             (every #'null (input-constraints instance))
             (every #'zerop (translation-vector instance))
             (ssm-identity? (linear-operator instance)))
    (change-class
     instance 'identity-transformation
     :dimension (input-dimension instance))))
