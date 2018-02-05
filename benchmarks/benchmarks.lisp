;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/benchmarks/benchmarks
  (:use :alexandria :common-lisp :the-cost-of-nothing)
  (:use
   :petalisp
   :petalisp/examples/jacobi
   :petalisp/examples/red-black-gauss-seidel
   :petalisp/examples/linear-algebra))

(in-package :petalisp/benchmarks/benchmarks)

(defun measure-bytes-consed (thunk)
  (let ((before (sb-ext:get-bytes-consed)))
    (funcall thunk)
    (coerce
     (- (sb-ext:get-bytes-consed) before)
     'double-float)))

(defmacro bytes-consed (&rest body)
  `(measure-bytes-consed
    (lambda ()
      ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Actual Benchmarks

(defun jacobi-2D-flops (&rest dim)
  (let* ((iterations 1000)
         (array (make-array dim :element-type 'double-float
                                :initial-element 1.0d0)))
    (/ (* iterations 4 (reduce #'* dim :key (lambda (dim) (- dim 2))))
       (measure-execution-time-of-thunk
        (lambda ()
          (loop repeat (1+ (ceiling iterations 10))
                for A = array then (schedule (jacobi A :iterations 10))
                finally (compute A)))))))

(defun benchmark-all ()
  (format t "Jacobi 2D flops: ~A" (jacobi-2D-flops 500 500)))
