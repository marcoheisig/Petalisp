(in-package #:petalisp.benchmarks)

(defun lazy-matmul (a b)
  (lazy-reduce #'+
   (lazy #'*
    (lazy-reshape a (transform m n to n m 0))
    (lazy-reshape b (transform n k to n 0 k)))))

(defbenchmark dgemm (nbytes)
  (let* ((nwords (ceiling nbytes 8))
         (m (ceiling (sqrt (/ nwords 3))))
         (n m)
         (k (ceiling (/ (- nwords (* m n))
                        (+ m n)))))
    (assert (<= nbytes (* 8 (+ (* m n) (* n k) (* m k)))))
    (matmul-bench m n k)))

(defbenchmark dgemm-n=8 (nbytes)
  (let* ((nwords (ceiling nbytes 8))
         (n 8)
         (m (max 1 (- (ceiling (sqrt nwords)) 8)))
         (k (ceiling (/ nwords (+ n m)))))
    (assert (<= nbytes (* 8 (+ (* m n) (* n k) (* m k)))))
    (matmul-bench m n k)))

(defbenchmark dgemv (nbytes)
  (let* ((nwords (ceiling nbytes 8))
         (m (max 1 (- (ceiling (sqrt nwords)) 1)))
         (n (ceiling (/ nwords m)))
         (k 1))
    (assert (<= nbytes (* 8 (+ (* m n) (* n k) (* m k)))))
    (matmul-bench m n k)))

(defun matmul-bench (m n k)
  (let* ((bytes (* 8 (+ (* m n) (* n k) (* m k))))
         (a (make-array (list m n) :element-type 'double-float :initial-element 1d0))
         (b (make-array (list n k) :element-type 'double-float :initial-element 1d0))
         (c (make-array (list m k) :element-type 'double-float :initial-element 0d0))
         (ua (make-unknown :shape (~ m ~ n) :element-type 'double-float))
         (ub (make-unknown :shape (~ n ~ k) :element-type 'double-float))
         (lc (lazy-matmul a b))
         (flops (flopcount lc))
         (ev (evaluator (list ua ub) (list lc))))
    (values
     (lambda () (funcall ev c a b))
     flops
     (/ flops bytes))))
