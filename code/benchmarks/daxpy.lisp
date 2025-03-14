(in-package #:petalisp.benchmarks)

(defun lazy-scale (a x y)
  (lazy #'+ (lazy #'* a x) y))

(defun scale-bench (n rep)
  (let* ((x (make-array (list n) :element-type 'double-float :initial-element 1d0))
         (y (make-array (list n) :element-type 'double-float :initial-element 1d0))
         (u (make-unknown :shape (~ n) :element-type 'double-float))
         (v (make-unknown :shape (~ n) :element-type 'double-float))
         (ev (evaluator
              (list u v)
              (let ((w v))
                (loop repeat rep do (setf w (lazy-scale 0.5d0 u w)))
                (list w)))))
    #+(or)
    (petalisp.graphviz:view
     (coerce
      (petalisp.ir:partition-program
       (petalisp.ir:program-from-lazy-arrays
        (list
         (lazy-scale 0.5d0 u v))))
      'list))
    (values
     (lambda () (funcall ev y x y))
     (* rep 2 n)
     1/8)))

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
