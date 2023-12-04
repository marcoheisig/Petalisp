(in-package :common-lisp-user)

(defpackage #:petalisp.benchmarks.jacobi
  (:use #:common-lisp #:petalisp)
  (:export
   #:lazy-jacobi
   #:run-benchmark))

(in-package #:petalisp.benchmarks.jacobi)

(defun lazy-jacobi (u n)
  (with-lazy-arrays (u)
    (let ((interior (lazy-reshape u (peeler 1 1))))
      (loop repeat n do
        (setf u (lazy-overwrite
                 u
                 (lazy #'* (float 1/4)
                  (lazy #'+
                   (lazy-reshape u (transform i j to (1+ i) j) interior)
                   (lazy-reshape u (transform i j to (1- i) j) interior)
                   (lazy-reshape u (transform i j to i (1+ j)) interior)
                   (lazy-reshape u (transform i j to i (1- j)) interior))))))
      (values u))))

(defun run-benchmark (h w n)
  (let* ((dimensions (list w h))
         (src (make-array dimensions :initial-element 0d0 :element-type 'double-float))
         (dst (make-array dimensions :initial-element 0d0 :element-type 'double-float))
         (evaluator
           (let ((x (make-unknown :shape (~ w ~ h) :element-type 'double-float)))
             (evaluator
              (list x)
              (list (lazy-jacobi x n)))))
         (t0 (get-internal-real-time))
         (dtmax (* internal-time-units-per-second 3))
         (nrep 0))
    (loop until (> (- (get-internal-real-time) t0) dtmax) do
      (funcall evaluator dst src)
      (incf nrep))
    (float
     (/ (* n (- w 2) (- h 2) 4 nrep internal-time-units-per-second)
        (- (get-internal-real-time) t0)))))

(defun run-benchmarks (&key (step 20000) (start step) (size 30))
  (format t "~&| ~4A | ~4A | ~7A |~%" "h" "w" "flops")
  (format t "~&|------+------+---------|~%")
  (loop repeat size for n from start by step do
    (let* ((h (isqrt n))
           (w (floor n h)))
      (format t "~&| ~4D | ~4D | ~4,2E |~%"
              h w (run-benchmark h w 100)))))
