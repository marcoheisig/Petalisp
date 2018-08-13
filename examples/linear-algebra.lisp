(in-package :common-lisp-user)

(defpackage :petalisp/examples/linear-algebra
  (:shadowing-import-from :petalisp :set-difference)
  (:use :cl :petalisp)
  (:export
   #:transpose
   #:norm
   #:dot
   #:matmul))

(in-package :petalisp/examples/linear-algebra)

(defun to-matrix (x)
  (ecase (dimension x)
    (0 (transform x (τ () (0 0))))
    (1 (transform x (τ (i) (i 0))))
    (2 x)))

(defun transpose (x)
  (transform
   (to-matrix x)
   (τ (m n) (n m))))

(defun dot (x y)
  (transform
   (matmul
    (transpose x)
    (to-matrix y))
   (τ (0 0) ())))

(defun matmul (a b)
  (β #'+
     (α #'*
        (transform (to-matrix a) (τ (m n) (n m 0)))
        (transform (to-matrix b) (τ (n k) (n 0 k))))))

(defun norm (x)
  (α #'sqrt (dot x x)))
