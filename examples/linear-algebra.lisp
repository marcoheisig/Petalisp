(in-package :common-lisp-user)

(defpackage :petalisp/examples/linear-algebra
  (:shadowing-import-from :petalisp :set-difference)
  (:use :cl :petalisp)
  (:export
   #:transpose
   #:norm
   #:dot
   #:asum
   #:amax
   #:matmul))

(in-package :petalisp/examples/linear-algebra)

(defun as-matrix (x)
  (ecase (rank x)
    (0 (reshape x (τ () (0 0))))
    (1 (reshape x (τ (i) (i 0))))
    (2 x)))

(defun as-vector (x)
  (ecase (rank x)
    (0 (reshape x (τ () (0))))
    (1 x)))

(defun transpose (x)
  (reshape
   (as-matrix x)
   (τ (m n) (n m))))

(defun dot (x y)
  (reshape
   (matmul
    (transpose x)
    (as-matrix y))
   (τ (0 0) ())))

(defun norm (x)
  (α #'sqrt (dot x x)))

(defun asum (x)
  (β #'+ (α #'abs (as-vector x))))

(defun amax (x)
  (flet ((amax-fn (lmax lind rmax rind)
           (if (>= lmax rmax)
               (values lmax lind)
               (values rmax rind))))
    (let ((vector (as-vector x)))
      (nth-value 1 (β #'amax-fn vector (indices vector))))))

(defun matmul (a b)
  (β #'+
     (α #'*
        (reshape (as-matrix a) (τ (m n) (n m 0)))
        (reshape (as-matrix b) (τ (n k) (n 0 k))))))
