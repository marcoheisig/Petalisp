(in-package #:petalisp.benchmarks)

(defun lazy-jacobi-2d (u f h)
  (lazy-sor-2d u f h 1))

(defun lazy-sor-2d (u f h omega)
  (with-lazy-arrays (u f h omega)
    (let* ((shape (petalisp.core:shape-prefix (lazy-array-shape u) 2))
           (interior (funcall (peeler 1 1) shape)))
      (lazy-overwrite-and-harmonize
       u
       (lazy #'+
        (lazy #'* (lazy-reshape (lazy #'1- omega) interior)
                  (lazy-reshape u interior))
        (lazy #'* omega 1/4
         (lazy #'+
          (lazy-reshape u (transform i j to (1+ i) j) interior)
          (lazy-reshape u (transform i j to (1- i) j) interior)
          (lazy-reshape u (transform i j to i (1+ j)) interior)
          (lazy-reshape u (transform i j to i (1- j)) interior)
          (lazy-reshape (lazy #'* (lazy #'* h h) f) interior))))))))

(defun lazy-rbgs-2d (u f h)
  (with-lazy-arrays (u f h)
    (let* ((shape (petalisp.core:shape-prefix (lazy-array-shape u) 2))
           (red-1 (funcall (peeler '(1 1 2) '(1 1 2)) shape))
           (red-2 (funcall (peeler '(2 1 2) '(2 1 1)) shape))
           (black-1 (funcall (peeler '(1 1 2) '(2 1 2)) shape))
           (black-2 (funcall (peeler '(2 1 2) '(1 1 2)) shape)))
      (flet ((stencil (space)
               (lazy #'* 1/4
                (lazy #'+
                 (lazy-reshape u (transform i j to (1+ i) j) space)
                 (lazy-reshape u (transform i j to (1- i) j) space)
                 (lazy-reshape u (transform i j to i (1+ j)) space)
                 (lazy-reshape u (transform i j to i (1- j)) space)
                 (lazy-reshape (lazy #'* h h f) space)))))
        (lazy-overwrite-and-harmonize
         (lazy-overwrite-and-harmonize u (stencil red-1) (stencil red-2))
         (stencil black-1) (stencil black-2))))))

(defbenchmark jacobi (nbytes)
  (let* ((nwords (ceiling nbytes 8))
         (w (ceiling (sqrt (/ nwords 2))))
         (h (ceiling (/ (/ nwords 2) w)))
         (n 10)
         (src (make-array (list w h) :element-type 'double-float :initial-element 1d0))
         (dst (make-array (list w h) :element-type 'double-float :initial-element 1d0))
         (x (make-unknown :shape (~ w ~ h) :element-type 'double-float))
         (ev (evaluator
              (list x)
              (let ((v x))
                (loop repeat n do
                  (setf v (lazy-jacobi-2d v 0 1)))
                (list v)))))
    (assert (<= nbytes (* 8 2 w h)))
    (values
     (lambda ()
       (funcall ev dst src)
       (funcall ev src dst))
     (* n 4 2 (- 2 w) (- 2 h))
     1/4)))

(defbenchmark rbgs (nbytes)
  (let* ((nwords (ceiling nbytes 8))
         (w (ceiling (sqrt (/ nwords 2))))
         (h (ceiling (/ (/ nwords 2) w)))
         (n 10)
         (src (make-array (list w h) :element-type 'double-float :initial-element 1d0))
         (dst (make-array (list w h) :element-type 'double-float :initial-element 1d0))
         (x (make-unknown :shape (~ w ~ h) :element-type 'double-float))
         (ev (evaluator
              (list x)
              (let ((v x))
                (loop repeat n do
                  (setf v (lazy-rbgs-2d v 0 1)))
                (list v)))))
    (assert (<= nbytes (* 8 2 w h)))
    (values
     (lambda ()
       (funcall ev dst src)
       (funcall ev src dst))
     (* n 4 2 (- 2 w) (- 2 h))
     1/8)))

(defun lazy-prolongate-2d (u)
  (with-lazy-arrays (u)
    (let* ((a (lazy-reshape u
               (transform i j to (* 2 i) (* 2 j))))
           (b (lazy #'* 1/2
               (lazy #'+
                (lazy-reshape a
                 (peeler '(0 0) '(1 0))
                 (transform i j to i (1- j)))
                (lazy-reshape a
                 (peeler '(0 0) '(0 1))
                 (transform i j to i (1+ j))))))
           (c (lazy #'* 1/2
               (lazy #'+
                (lazy-reshape a
                 (peeler '(1 0) '(0 0))
                 (transform i j to (1- i) j))
                (lazy-reshape a
                 (peeler '(0 1) '(0 0))
                 (transform i j to (1+ i) j)))))
           (d (lazy #'* 1/4
               (lazy #'+
                (lazy-reshape a
                 (peeler '(1 0) '(1 0))
                 (transform i j to (1- i) (1- j)))
                (lazy-reshape a
                 (peeler '(0 1) '(1 0))
                 (transform i j to (1+ i) (1- j)))
                (lazy-reshape a
                 (peeler '(1 0) '(0 1))
                 (transform i j to (1- i) (1+ j)))
                (lazy-reshape a
                 (peeler '(0 1) '(0 1))
                 (transform i j to (1+ i) (1+ j)))))))
      (lazy-fuse a b c d))))

(defun lazy-restrict-2d (u)
  (with-lazy-arrays (u)
    (let* ((shape (petalisp.core:shape-prefix (lazy-array-shape u) 2))
           (selection (funcall (peeler '(0 0 2) '(0 0 2)) shape))
           (interior (funcall (peeler 1 1) selection)))
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
    (let* ((shape (petalisp.core:shape-prefix (lazy-array-shape u) 2))
           (interior (funcall (peeler 1 1) shape)))
      (lazy-overwrite-and-harmonize
       (lazy-reshape 0 u)
       (lazy #'-
        (lazy-reshape b interior)
        (lazy #'*
         (lazy-reshape (lazy #'/ (lazy #'* h h)))
         (lazy #'-
          (lazy-reshape (lazy #'* 4 u) interior)
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
      (lazy-stack
       (list 0 (lazy #'* x x)))))))

(defun lazy-max-norm (x)
  (with-lazy-arrays (x)
    (lazy-reduce #'max (lazy #'abs x))))

(defbenchmark multigrid-v-cycle (nbytes)
  (let* ((nwords (ceiling nbytes 8))
         (levels (ceiling (log (sqrt (/ nwords 2)) 2)))
         (w (1+ (expt 2 levels)))
         (h (1+ (expt 2 levels)))
         (src (make-array (list w h) :element-type 'double-float :initial-element 1d0))
         (dst (make-array (list w h) :element-type 'double-float :initial-element 1d0))
         (u (make-unknown :shape (~ w ~ h) :element-type 'double-float))
         (ev (evaluator
              (list u)
              (list
               (lazy-v-cycle-2d u 0 1 2 1)))))
    (assert (<= nbytes (* 8 2 w h)))
    (values
     (lambda ()
       (funcall ev dst src)
       (funcall ev src dst))
     (* (+ (* 7 3) ;; rbgs, 3 sweeps
           3 ;; prolongate
           1 ;; restrict
           7 ;; residual
           )
        2 ;; src->dst, dst->src
        1.5 ;; 1 + 0.25 + 0.125 + ...
        (- 2 w) (- 2 h))
     1/4)))
