;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :common-lisp-user)

(defpackage :petalisp-test-suite/examples/linear-algebra
  (:use :cl :petalisp)
  (:export
   #:transpose
   #:norm
   #:dot
   #:matmul))

(in-package :petalisp-test-suite/examples/linear-algebra)

(defun to-matrix (x)
  (ecase (dimension x)
    (0 (-> x (τ ( ) (1 1))))
    (1 (-> x (τ (i) (i 1))))
    (2 x)))

(defun transpose (x)
  (-> (to-matrix x)
      (τ (m n) (n m))))

(defun dot (x y)
  (->
   (matmul
    (transpose x)
    (to-matrix y))
   (τ (1 1) ())))

(defun matmul (a b)
  (β #'+ #'identity
     (α #'*
        (-> a (τ (m n) (m 1 n)))
        (-> b (τ (n k) (1 k n))))))

