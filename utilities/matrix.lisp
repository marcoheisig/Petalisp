;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/utilities/matrix
  (:use :closer-common-lisp :alexandria)
  (:export
   #:matrix
   #:matrix?
   #:matrix-m
   #:matrix-n
   #:matrix-product
   #:matrix-sum
   #:matrix-inverse
   #:scaled-permutation-matrix
   #:scaled-permutation-matrix?
   #:spm-column-indices
   #:spm-values
   #:identity-matrix?))

(in-package :petalisp/utilities/matrix)

(defgeneric matrix-product (A B))

(defgeneric matrix-sum (A B))

(defgeneric matrix-inverse (M))

(defstruct (matrix
            (:constructor nil)
            (:copier nil)
            (:predicate matrix?))
  (m nil :type array-length :read-only t)
  (n nil :type array-length :read-only t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; scaled permutation matrices

(defstruct (scaled-permutation-matrix
            (:conc-name spm-)
            (:include matrix)
            (:constructor %scaled-permutation-matrix (m n column-indices values))
            (:copier nil) ; immutable -> no need to copy
            (:predicate scaled-permutation-matrix?))
  "A `scaled-permutation-matrix' is a matrix with at most one nonzero entry
per row and column."
  (column-indices nil :type (simple-array number (*)) :read-only t)
  (values         nil :type (simple-array number (*)) :read-only t))

(defun scaled-permutation-matrix (m n column-indices values)
  (let ((column-indices
          (coerce column-indices '(simple-array number (*))))
        (values
          (coerce values '(simple-array number (*)))))
    (assert (= m (length column-indices) (length values))
            (m column-indices values)
            "Expected matrix creation arguments of lenght ~D, found:~%  ~S and~%  ~S~%"
            m column-indices values)
    (assert (every (lambda (column-index value)
                      (or (zerop value)
                          (< -1 column-index n)))
                   column-indices values)
            (n column-indices)
            "Some of the column indices in~%  ~S~%exceed the matrix width of ~D.~%"
            column-indices n)
    (assert (loop for row-index below m
                  never
                  (and (/= 0 (elt values row-index))
                       (loop for search-index from (1+ row-index) below m
                               thereis
                               (and (/= 0 (elt values search-index))
                                    (= (elt column-indices row-index)
                                       (elt column-indices search-index))))))
            (column-indices values)
            "Duplicate column indices:~%  ~S" column-indices)
    (%scaled-permutation-matrix m n column-indices values)))

(defmethod matrix-product ((matrix scaled-permutation-matrix) (vector simple-array))
  "For a given m times n  sparse matrix SPM and a n-vector VEC, this
function returns the m-vector that is the dot product of SPM and VEC."
  (declare (type (simple-array number (*)) vector))
  (flet ((column-index (row-index)
           (aref (spm-column-indices matrix) row-index))
         (value (row-index)
           (aref (spm-values matrix) row-index)))
    (let* ((rows (matrix-m matrix))
           (result (make-array rows :element-type 'number)))
      (dotimes (row-index rows)
        (setf (aref result row-index)
              (let ((value (value row-index)))
                (if (= 0 value)
                    0
                    (* (value row-index)
                       (aref vector (column-index row-index)))))))
      result)))

(defmethod matrix-product ((spm-1 scaled-permutation-matrix) (spm-2 scaled-permutation-matrix))
  (declare (type scaled-permutation-matrix spm-1 spm-2))
  (let* ((m (matrix-m spm-1))
         (n (matrix-n spm-2))
         (column-indices (make-array m :element-type 'number
                                       :initial-element 0))
         (values (make-array m :element-type 'number
                               :initial-element 0)))
    (dotimes (i m)
      (if (zerop (aref (spm-values spm-1) i))
          (values)
          (let* ((k (aref (spm-column-indices spm-1) i))
                 (j (aref (spm-column-indices spm-2) k)))
            (setf (aref column-indices i) j)
            (setf (aref values i)
                  (* (aref (spm-values spm-1) i)
                     (aref (spm-values spm-2) k))))))
    (scaled-permutation-matrix m n column-indices values)))

(defmethod matrix-product ((spm scaled-permutation-matrix) (s-expressions list))
  (map 'list (lambda (col val)
               (if (eql val 0) 0
                   (let ((sexp (elt s-expressions col)))
                     (cond ((eql val 1) sexp)
                           ((numberp sexp) (* val sexp))
                           (t `(* ,val ,sexp))))))
       (spm-column-indices spm)
       (spm-values spm)))

;;; Note that a scaled permutation matrix is not generally invertible. The
;;; inverse as returned from this function assumes A is only applied to
;;; vectors which are zero whenever the corresponding column is zero.
(defmethod matrix-inverse ((spm scaled-permutation-matrix))
  (declare (type scaled-permutation-matrix spm))
  (let* ((original-column-indices (spm-column-indices spm))
         (original-values (spm-values spm))
         (m (matrix-n spm))
         (n (matrix-m spm))
         (column-indices (make-array m :element-type 'number
                                       :initial-element 0))
         (values (make-array m :element-type 'number
                               :initial-element 0)))
    (dotimes (row-index m)
      (let ((column-index
              (loop for column across original-column-indices
                    for value  across original-values
                    for position from 0
                      thereis (and (/= 0 value) (= column row-index) position))))
        (when column-index
          (setf (aref column-indices row-index) column-index)
          (let ((value (aref original-values column-index)))
            (setf (aref values row-index)
                  (if (zerop value) 0 (/ value)))))))
    (scaled-permutation-matrix m n column-indices values)))

(defun identity-matrix? (spm)
  (let ((m (matrix-m spm)) (n (matrix-n spm)))
    (and (= m n)
         (loop for row-index below m
               for value across (spm-values spm)
               for column-index across (spm-column-indices spm)
               always (and (= column-index row-index)
                            (= value 1))))))
