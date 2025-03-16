(in-package #:petalisp.benchmarks)

(defbenchmark sum (nbytes)
  (let* ((n (ceiling nbytes 8))
         (x (make-array (list n) :element-type 'double-float :initial-element 1d0))
         (r (make-array () :element-type 'double-float :initial-element 0d0))
         (ux (make-unknown :shape (~ n) :element-type 'double-float))
         (ur (lazy-reduce #'+ ux))
         (flops (flopcount ur))
         (ev (evaluator (list ux) (list ur))))
    (values
     (lambda () (funcall ev r x))
     flops
     (/ flops (* n 8)))))
