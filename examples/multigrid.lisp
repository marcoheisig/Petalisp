(in-package :common-lisp-user)

(defpackage #:petalisp.examples.multigrid
  (:use #:common-lisp #:petalisp)
  (:export
   #:iterate
   #:lazy-jacobi-2d
   #:lazy-sor-2d
   #:lazy-rbgs-2d
   #:lazy-prolongate-2d
   #:lazy-restrict-2d
   #:lazy-v-cycle-2d))

(in-package #:petalisp.examples.multigrid)

(defun lazy-jacobi-2d (u f h)
  (lazy-sor-2d u f h 1))

(defun lazy-sor-2d (u f h omega)
  (with-lazy-arrays (u f h omega)
    (let* ((shape (shape-prefix (lazy-array-shape u) 2))
           (interior (funcall (peeling-reshaper :layers 1) shape))
           (h (lazy-reshape h 0 2))
           (omega (lazy-reshape omega 0 2)))
      (lazy-overwrite-and-harmonize
       u
       (lazy #'+
        (lazy #'* (lazy-reshape (lazy #'1- omega) interior)
                  (lazy-reshape u 2 interior))
        (lazy #'* omega 1/4
         (lazy #'+
          (lazy-reshape u (transform i j to (1+ i) j) interior)
          (lazy-reshape u (transform i j to (1- i) j) interior)
          (lazy-reshape u (transform i j to i (1+ j)) interior)
          (lazy-reshape u (transform i j to i (1- j)) interior)
          (lazy-reshape (lazy #'* (lazy #'* h h) f) 2 interior))))))))

(defun lazy-rbgs-2d (u f h)
  (with-lazy-arrays (u f h)
    (let* ((shape (shape-prefix (lazy-array-shape u) 2))
           (red-1 (funcall (peeling-reshaper :lower-layers '(1 1) :layers 1 :strides 2) shape))
           (red-2 (funcall (peeling-reshaper :lower-layers '(2 2) :layers 1 :strides 2) shape))
           (black-1 (funcall (peeling-reshaper :lower-layers '(1 2) :layers 1 :strides 2) shape))
           (black-2 (funcall (peeling-reshaper :lower-layers '(2 1) :layers 1 :strides 2) shape))
           (h (lazy-reshape h 0 2)))
      (flet ((stencil (space)
               (lazy #'* 1/4
                (lazy #'+
                 (lazy-reshape u (transform i j to (1+ i) j) space)
                 (lazy-reshape u (transform i j to (1- i) j) space)
                 (lazy-reshape u (transform i j to i (1+ j)) space)
                 (lazy-reshape u (transform i j to i (1- j)) space)
                 (lazy-reshape (lazy #'* h h f) 2 space)))))
        (lazy-overwrite-and-harmonize
         (lazy-overwrite-and-harmonize u (stencil red-1) (stencil red-2))
         (stencil black-1) (stencil black-2))))))

(defun lazy-prolongate-2d (u)
  (with-lazy-arrays (u)
    (let* ((a (lazy-reshape u
               (transform i j to (* 2 i) (* 2 j))))
           (b (lazy #'* 1/2
               (lazy #'+
                (lazy-reshape a 2
                 (peeling-reshaper :lower-layers '(0 1))
                 (transform i j to i (1- j)))
                (lazy-reshape a 2
                 (peeling-reshaper :upper-layers '(0 1))
                 (transform i j to i (1+ j))))))
           (c (lazy #'* 1/2
               (lazy #'+
                (lazy-reshape a 2
                 (peeling-reshaper :lower-layers '(1 0))
                 (transform i j to (1- i) j))
                (lazy-reshape a 2
                 (peeling-reshaper :upper-layers '(1 0))
                 (transform i j to (1+ i) j)))))
           (d (lazy #'* 1/4
               (lazy #'+
                (lazy-reshape a 2
                 (peeling-reshaper :lower-layers '(1 1) :upper-layers '(0 0))
                 (transform i j to (1- i) (1- j)))
                (lazy-reshape a 2
                 (peeling-reshaper :lower-layers '(0 1) :upper-layers '(1 0))
                 (transform i j to (1+ i) (1- j)))
                (lazy-reshape a 2
                 (peeling-reshaper :lower-layers '(1 0) :upper-layers '(0 1))
                 (transform i j to (1- i) (1+ j)))
                (lazy-reshape a 2
                 (peeling-reshaper :lower-layers '(0 0) :upper-layers '(1 1))
                 (transform i j to (1+ i) (1+ j)))))))
      (lazy-fuse a b c d))))

(defun lazy-restrict-2d (u)
  (with-lazy-arrays (u)
    (let* ((shape (shape-prefix (lazy-array-shape u) 2))
           (selection (funcall (peeling-reshaper :strides 2) shape))
           (interior (funcall (peeling-reshaper :layers 1) selection)))
      (lazy-reshape
       (lazy-overwrite
        (lazy-reshape u 2 selection)
        (lazy #'+
         (lazy #'* 1/4
          (lazy-reshape u 2 interior))
         (lazy #'* 1/8
          (lazy #'+
           (lazy-reshape u (transform i j to (1+ i) j) interior)
           (lazy-reshape u (transform i j to (1- i) j) interior)
           (lazy-reshape u (transform i j to i (1+ j)) interior)
           (lazy-reshape u (transform i j to i (1- j)) interior)))
         (lazy #'* 1/16
          (lazy #'+
           (lazy-reshape u (transform i j to (1+ i) (1+ j)) interior)
           (lazy-reshape u (transform i j to (1- i) (1+ j)) interior)
           (lazy-reshape u (transform i j to (1+ i) (1- j)) interior)
           (lazy-reshape u (transform i j to (1- i) (1- j)) interior)))))
       (transform i j to (/ i 2) (/ j 2))))))

(defun lazy-residual-2d (u b h)
  (with-lazy-arrays (u b h)
    (let* ((shape (shape-prefix (lazy-array-shape u) 2))
           (interior (funcall (peeling-reshaper :layers 1) shape)))
      (lazy-overwrite-and-harmonize
       (lazy-reshape 0 u)
       (lazy #'-
        (lazy-reshape b 2 interior)
        (lazy #'*
         (lazy-reshape (lazy #'/ (lazy #'* h h)) 0 2)
         (lazy #'-
          (lazy-reshape (lazy #'* 4 u) 2 interior)
          (lazy-reshape u (transform i j to (1+ i) j) interior)
          (lazy-reshape u (transform i j to (1- i) j) interior)
          (lazy-reshape u (transform i j to i (1+ j)) interior)
          (lazy-reshape u (transform i j to i (1- j)) interior))))))))

(defun lazy-smoothen-2d (u f h n)
  (with-lazy-arrays (u f h)
    (loop repeat n do (setf u (lazy-rbgs-2d u f h))
          finally (return u))))

(defun lazy-v-cycle-2d (u f h v1 v2)
  (with-lazy-arrays (u f h)
    (if (or (<= (range-size (lazy-array-range u 0)) 3)
            (<= (range-size (lazy-array-range u 1)) 3))
        (lazy-jacobi-2d u f h)
        (let* ((x (lazy-smoothen-2d u f h v1))
               (r (lazy-restrict-2d (lazy-residual-2d x f h)))
               (c (lazy-v-cycle-2d (lazy-reshape 0 r) r (lazy #'* 2 h) v1 v2)))
          (lazy-smoothen-2d (lazy #'+ x (lazy-prolongate-2d c)) f h v2)))))

(defun lazy-w-cycle-2d (u f h v1 v2 v3)
  (with-lazy-arrays (u f h)
    (if (or (<= (range-size (lazy-array-range u 0)) 3)
            (<= (range-size (lazy-array-range u 1)) 3))
        (lazy-jacobi-2d u f h)
        (let* ((x (lazy-smoothen-2d u f h v1))
               (r (lazy-restrict-2d (lazy-residual-2d x f h)))
               (c (lazy-w-cycle-2d (lazy-reshape 0 r) r (lazy #'* 2 h) v1 v2 v3))
               (x (lazy-smoothen-2d (lazy #'+ x (lazy-prolongate-2d c)) f h v2))
               (r (lazy-restrict-2d (lazy-residual-2d x f h)))
               (c (lazy-w-cycle-2d (lazy-reshape 0 r) r (lazy #'* 2 h) v1 v2 v3)))
          (lazy-smoothen-2d (lazy #'+ x (lazy-prolongate-2d c)) f h v3)))))

(defun lazy-f-cycle-2d (u f h v1 v2)
  (with-lazy-arrays (u f h)
    (if (or (<= (range-size (lazy-array-range u 0)) 3)
            (<= (range-size (lazy-array-range u 1)) 3))
        (lazy-jacobi-2d u f h)
        (let* ((r (lazy-restrict-2d (lazy-residual-2d u f h)))
               (c (lazy-f-cycle-2d (lazy-reshape 0 r) r (lazy #'* 2 h) v1 v2)))
          (lazy-v-cycle-2d (lazy #'+ u (lazy-prolongate-2d c)) f h v1 v2)))))

(defun lazy-l2-norm (x)
  (with-lazy-arrays (x)
    (lazy #'sqrt
     (lazy-reduce #'+
      (lazy-stack 0 (lazy-reshape 0 (~ 1)) (lazy #'* x x))))))

(defun lazy-max-norm (x)
  (with-lazy-arrays (x)
    (lazy-reduce #'max (lazy #'abs x))))

(defun multigrid-test (&key (ny 17) (nx 17) (h 1) (v1 2) (v2 2)
                         (iterations 20)
                         (element-type 'single-float))
  (let* ((s (~ ny ~ nx))
         (u (lazy-overwrite-and-harmonize
             (lazy-reshape (coerce 1 element-type) s)
             (lazy-reshape 0 s (peeling-reshaper :layers 1))))
         (f (lazy-reshape 0 s)))
    (format t "~&~10@A ~10@A ~10@A~%" "iterations" "l2-norm" "max-norm")
    (format t "---------------------------------~%")
    (loop for iteration from 1 to iterations do
      (let* ((u-new (lazy-f-cycle-2d u f h v1 v2))
             (u-vec (lazy-reshape (lazy #'1- u) 2 (flattening-reshaper))))
        (multiple-value-bind (u-new l2-norm max-norm)
            (compute u-new (lazy-l2-norm u-vec) (lazy-max-norm u-vec))
          (setf u u-new)
          (format t "~10D ~10,3E ~10,3E~%"
                  iteration
                  l2-norm
                  max-norm))))
    u))
