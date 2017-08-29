;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; special linear algebra subroutines for index space transformations

(in-package :petalisp)

(defstruct (scaled-permutation-matrix
            (:conc-name spm-)
            (:constructor %scaled-permutation-matrix (m n column-indices values))
            (:copier nil) ; immutable -> no need to copy
            (:predicate scaled-permutation-matrix?))
  "A `scaled-permutation-matrix' is a matrix with at most one nonzero entry
per row and column."
  (column-indices nil :type (simple-array array-index (*)) :read-only t)
  (values nil :type (simple-array number (*)) :read-only t)
  (m nil :type array-length :read-only t)
  (n nil :type array-length :read-only t))

(defun scaled-permutation-matrix (m n column-indices values)
  (let ((column-indices
          (coerce column-indices '(simple-array array-index (*))))
        (values
          (coerce values '(simple-array number (*)))))
    (assert (= m (length column-indices) (length values))
            (m column-indices values)
            "Expected matrix creation arguments of lenght ~D, found:~%  ~S and~%  ~S~%"
            m column-indices values)
    (assert (every (λ column-index (< -1 column-index n)) column-indices)
            (n column-indices)
            "Some of the column indices in~%  ~S~%exceed the matrix width of ~D.~%"
            column-indices n)
    (assert (loop :for row-index :below m
                  :never
                  (and (/= 0 (elt values row-index))
                       (loop :for search-index :from (1+ row-index) :below m
                               :thereis
                               (and (/= 0 (elt values search-index))
                                    (= (elt column-indices row-index)
                                       (elt column-indices search-index))))))
            (column-indices values)
            "Duplicate column indices:~%  ~S" column-indices)
    (%scaled-permutation-matrix m n column-indices values)))

(test scaled-permutation-matrix
  (is (scaled-permutation-matrix?
       (scaled-permutation-matrix 3 3 #(0 1 2) #(5 4 3))))
  (signals error
    (scaled-permutation-matrix 1 1 #(2) #(1)))
  (signals error
    (scaled-permutation-matrix 2 2 #(0 0) #(1 1))))

(defmethod binary-product ((spm scaled-permutation-matrix) (vector simple-array))
  "For a given m times n  sparse matrix SPM and a n-vector VEC, this
function returns the m-vector that is the dot product of SPM and VEC."
  (declare (type scaled-permutation-matrix spm)
           (type (simple-array number (*)) vector))
  (with-unsafe-optimizations
    (flet ((column-index (row-index)
             (aref (spm-column-indices spm) row-index))
           (value (row-index)
             (aref (spm-values spm) row-index)))
      (let* ((rows (spm-m spm))
             (result (make-array rows :element-type 'number)))
        (dotimes (row-index rows)
          (setf (aref result row-index)
                (* (value row-index)
                   (aref vector (column-index row-index)))))
        result))))

(test spm-vector-product
  (is (equalp
       #(10 8)
       (product
        (scaled-permutation-matrix 2 2 #(1 0) #(2 2))
        #(4 5)))))

(defmethod binary-product ((spm-1 scaled-permutation-matrix) (spm-2 scaled-permutation-matrix))
  (declare (type scaled-permutation-matrix spm-1 spm-2))
  (with-unsafe-optimizations
    (let* ((m (spm-m spm-1))
           (n (spm-n spm-2))
           (column-indices (make-array m :element-type 'array-index
                                         :initial-element 0))
           (values (make-array m :element-type 'number
                                 :initial-element 0)))
      (dotimes (i m)
        (let* ((k (aref (spm-column-indices spm-1) i))
               (j (aref (spm-column-indices spm-2) k)))
          (setf (aref column-indices i) j)
          (setf (aref values i)
                (* (aref (spm-values spm-1) i)
                   (aref (spm-values spm-2) k)))))
      (scaled-permutation-matrix m n column-indices values))))

(test spm-spm-product
  (is (equalp
       (scaled-permutation-matrix 2 2 #(0 1) #(8 6))
       (product
        (scaled-permutation-matrix 2 2 #(1 0) #(2 2))
        (scaled-permutation-matrix 2 2 #(1 0) #(3 4)))))
  (is (equalp
       (scaled-permutation-matrix 3 5 #(0 0 0) #(0 0 0))
       (product
        (scaled-permutation-matrix 3 7 #(0 0 0) #(0 0 0))
        (scaled-permutation-matrix 7 5 #(0 1 2 3 4 0 0) #(6 6 6 6 6 0 0))))))

(defmethod binary-product ((spm scaled-permutation-matrix) (sexps list))
  (map 'list (λ col val
                (let ((sexp (elt sexps col)))
                  (cond ((eql val 0) 0)
                        ((eql val 1) sexp)
                        ((numberp sexp) (* val sexp))
                        (t `(* ,val ,sexp)))))
       (spm-column-indices spm)
       (spm-values spm)))

(test spm-sexps-product
  (is (equalp
       '(0 foo 84 (* 3 foo))
       (product
        (scaled-permutation-matrix 4 4 #(0 1 2 3) #(0 1 2 3))
        '(foo foo 42 foo)))))

;;; Note that a  sparse matrix is not generally invertible. The
;;; inverse as returned from this function assumes A is only applied to
;;; vectors which are zero whenever the corresponding column is zero.
(defmethod inverse ((spm scaled-permutation-matrix))
  (declare (type scaled-permutation-matrix spm))
  (with-unsafe-optimizations
    (let* ((original-column-indices (spm-column-indices spm))
           (original-values (spm-values spm))
           (m (spm-n spm))
           (n (spm-m spm))
           (column-indices (make-array m :element-type 'array-index
                                         :initial-element 0))
           (values (make-array m :element-type 'number
                                 :initial-element 0)))
      (dotimes (row-index m)
        (let ((column-index (position row-index original-column-indices)))
          (when column-index
            (setf (aref column-indices row-index) column-index)
            (let ((value (aref original-values column-index)))
              (setf (aref values row-index)
                    (if (zerop value) 0 (/ value)))))))
      (scaled-permutation-matrix m n column-indices values))))

(test spm-inverse
  (is (equalp
       (scaled-permutation-matrix 4 4 #(2 0 1 3) #(2 3 4 5))
       (inverse
        (scaled-permutation-matrix 4 4 #(1 2 0 3) #(1/3 1/4 1/2 1/5))))))

(defun spm-identity? (spm)
  (let ((m (spm-m spm)) (n (spm-n spm)))
    (and (= m n)
         (loop :for row-index :below m
               :for value :across (spm-values spm)
               :for column-index :across (spm-column-indices spm)
               :always (and (= column-index row-index)
                            (= value 1))))))

(test spm-identity?
  (is-true  (spm-identity? (scaled-permutation-matrix 3 3 #(0 1 2) #(1 1 1))))
  (is-false (spm-identity? (scaled-permutation-matrix 3 3 #(0 1 2) #(1 2 1))))
  (is-false (spm-identity? (scaled-permutation-matrix 3 3 #(1 0 2) #(1 1 1)))))

(defmethod input-dimension ((instance scaled-permutation-matrix))
  (spm-n instance))

(defmethod output-dimension ((instance scaled-permutation-matrix))
  (spm-m instance))

(defmethod compose ((g scaled-permutation-matrix) (f scaled-permutation-matrix))
  (product g f))

(defmethod equal? ((a scaled-permutation-matrix) (b scaled-permutation-matrix))
  (equalp a b)) ; we can use EQUALP, because both arguments are structs
