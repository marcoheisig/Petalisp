(in-package #:petalisp.benchmarks)

(defun lazy-scale (a x y)
  (lazy #'+ (lazy #'* a x) y))

(defun scale-bench (n rep)
  (let* ((x (make-array (list n) :element-type 'double-float :initial-element 1d0))
         (y (make-array (list n) :element-type 'double-float :initial-element 1d0))
         (u (make-unknown :shape (~ n) :element-type 'double-float))
         (v (make-unknown :shape (~ n) :element-type 'double-float))
         (w
           (let ((w v))
             (loop repeat rep do (setf w (lazy-scale 0.5d0 u w)))
             w))
         (flops (flopcount w))
         (ev
           (evaluator
            (list u v)
            (list w))))
    (values
     (lambda () (funcall ev y x y))
     flops
     (/ flops (* 2 8 n)))))

(defbenchmark daxpy (nbytes)
  (let* ((nwords (ceiling nbytes 8))
         (n (ceiling (/ nwords 2))))
    (assert (<= nbytes (* 8 2 n)))
    (scale-bench n 1)))

(defbenchmark daxpy-twice (nbytes)
  (let* ((nwords (ceiling nbytes 8))
         (n (ceiling (/ nwords 2))))
    (assert (<= nbytes (* 8 2 n)))
    (scale-bench n 2)))

(defbenchmark daxpy-thrice (nbytes)
  (let* ((nwords (ceiling nbytes 8))
         (n (ceiling (/ nwords 2))))
    (assert (<= nbytes (* 8 2 n)))
    (scale-bench n 3)))
