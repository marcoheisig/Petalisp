(in-package :common-lisp-user)

(defpackage :petalisp/examples/jacobi
  (:use :cl :petalisp)
  (:export #:jacobi))

(in-package :petalisp/examples/jacobi)

(defun interior (index-space)
  (with-index-space-accessors (rank start step-size end) index-space
    (loop for i below rank
          collect
          (let ((step (step-size i)))
            (list (+ (start i) step)
                  step
                  (- (end i) step))))))

(defun jacobi (u &key (iterations 1)
                   (h (/ (1- (expt (size u) (/ (dimension u))))))
                   (f 0))
  "Iteratively solve the Poisson equation -Δu = f for a given uniform grid
  with spacing h, using the Jacobi scheme."
  (let ((interior (interior u)))
    (ecase (dimension u)
      (1
       (loop repeat iterations do
         (setf u (fuse* u (α #'* (float 1/2)
                             (α #'+
                                (reshape (transform u (τ (i) ((1+ i)))) interior)
                                (reshape (transform u (τ (i) ((1- i)))) interior)
                                (reshape (α #'* (* h h) f) interior))))))
       u)
      (2
       (loop repeat iterations do
         (setf u (fuse* u (α #'* (float 1/4)
                             (α #'+
                                (reshape (transform u (τ (i j) ((1+ i) j))) interior)
                                (reshape (transform u (τ (i j) ((1- i) j))) interior)
                                (reshape (transform u (τ (i j) (i (1+ j)))) interior)
                                (reshape (transform u (τ (i j) (i (1- j)))) interior)
                                (reshape (α #'* (* h h) f) interior))))))
       u)
      (3
       (loop repeat iterations do
         (setf u (fuse* u (α #'* (float 1/6)
                             (α #'+
                                (reshape (transform u (τ (i j k) ((1+ i) j k))) interior)
                                (reshape (transform u (τ (i j k) ((1- i) j k))) interior)
                                (reshape (transform u (τ (i j k) (i (1+ j) k))) interior)
                                (reshape (transform u (τ (i j k) (i (1- j) k))) interior)
                                (reshape (transform u (τ (i j k) (i j (1+ k)))) interior)
                                (reshape (transform u (τ (i j k) (i j (1- k)))) interior)
                                (reshape (α #'* (* h h) f) interior))))))
       u))))
