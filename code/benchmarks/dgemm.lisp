(in-package #:petalisp.benchmarks)

(defun lazy-matmul (a b)
  (lazy-reduce #'+
   (lazy #'*
    (lazy-reshape a (transform m n to n m 0))
    (lazy-reshape b (transform n k to n 0 k)))))

(defbenchmark dgemm (nbytes)
  (let* ((m (ceiling (expt nbytes 1/3)))
         (n (ceiling (sqrt (/ nbytes m))))
         (k (ceiling (/ nbytes m n))))
    (matmul-bench m n k)))

(defbenchmark dgemm-n=8 (nbytes)
  (let* ((n (ceiling 8))
         (m (ceiling (sqrt (/ nbytes n))))
         (k (ceiling (/ nbytes m n))))
    (matmul-bench m n k)))

(defun matmul-bench (m n k)
  (let* ((flops (* 2 m n k))
         (bytes (* 8 (+ (* m n) (* n k) (* m k))))
         (a (make-array (list m n) :element-type 'double-float :initial-element 1d0))
         (b (make-array (list n k) :element-type 'double-float :initial-element 1d0))
         (c (make-array (list m k) :element-type 'double-float :initial-element 0d0))
         (ua (make-unknown :shape (~ m ~ n) :element-type 'double-float))
         (ub (make-unknown :shape (~ n ~ k) :element-type 'double-float))
         (ev (evaluator (list ua ub) (list (lazy-matmul a b)))))
    (values
     (lambda () (funcall ev c a b))
     flops
     (/ flops bytes))))
