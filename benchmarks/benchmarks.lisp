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

(defun iterative-benchmark (name small-problem large-problem)
  (let ((iterations 20))
    (let* ((flops
             (multiple-value-bind (problem flops)
                 (funcall large-problem iterations)
               (declare (ignore problem))
               (/ flops (benchmark (compute (funcall large-problem iterations))))))
           (definition-time
             (/ (benchmark (funcall small-problem iterations))
                iterations))
           (total-time
             (/ (benchmark (compute (funcall small-problem iterations)))
                iterations))
           (scheduler-time
             (- total-time definition-time))
           (definition-garbage
             (/ (bytes-consed (funcall small-problem iterations))
                iterations))
           (total-garbage
             (/ (bytes-consed (compute (funcall small-problem iterations)))
                iterations))
           (scheduler-garbage
             (- total-garbage definition-garbage)))
      (list
       name
       :flops flops
       :definition-garbage definition-garbage
       :scheduler-garbage scheduler-garbage
       :total-garbage total-garbage
       :definition-time definition-time
       :scheduler-time scheduler-time
       :total-time total-time))))

(defun jacobi-2d ()
  (let ((small-array (make-array '(5 5) :element-type 'double-float
                                        :initial-element 1.0d0))
        (large-array (make-array '(200 200) :element-type 'double-float
                                            :initial-element 1.0d0)))
    (iterative-benchmark
     :jacobi-2d
     (lambda (n) (values (jacobi small-array :iterations n)
                         (* n 3 3 4)))
     (lambda (n) (values (jacobi large-array :iterations n)
                         (* n 198 198 4))))))

(defun rbgs-2d ()
  (let ((small-array (make-array '(5 5) :element-type 'double-float
                                        :initial-element 1.0d0))
        (large-array (make-array '(200 200) :element-type 'double-float
                                            :initial-element 1.0d0)))
    (iterative-benchmark
     :rbgs-2d
     (lambda (n) (values (red-black-gauss-seidel small-array :iterations n)
                         (* n 3 3 4)))
     (lambda (n) (values (red-black-gauss-seidel large-array :iterations n)
                         (* n 198 198 4))))))

(defun jacobi-3d ()
  (let ((small-array (make-array '(5 5 5) :element-type 'double-float
                                        :initial-element 1.0d0))
        (large-array (make-array '(50 50 50) :element-type 'double-float
                                             :initial-element 1.0d0)))
    (iterative-benchmark
     :jacobi-3d
     (lambda (n) (values (jacobi small-array :iterations n)
                         (* n 3 3 3 6)))
     (lambda (n) (values (jacobi large-array :iterations n)
                         (* n 48 48 48 6))))))

(defun rbgs-3d ()
  (let ((small-array (make-array '(5 5 5) :element-type 'double-float
                                        :initial-element 1.0d0))
        (large-array (make-array '(50 50 50) :element-type 'double-float
                                             :initial-element 1.0d0)))
    (iterative-benchmark
     :rbgs-3d
     (lambda (n) (values (red-black-gauss-seidel small-array :iterations n)
                         (* n 3 3 3 6)))
     (lambda (n) (values (red-black-gauss-seidel large-array :iterations n)
                         (* n 48 48 48 6))))))

(defun benchmark-all ()
  (print (jacobi-2d))
  (print (jacobi-3d))
  (print (rbgs-2d))
  (print (rbgs-3d)))
