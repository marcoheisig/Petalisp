(in-package #:petalisp.benchmarks)

;; Each benchmark-fn takes one argument that is the number of bytes of memory
;; to use, and returns three values:
;;
;; - the benchmark thunk
;; - the number of flops per thunk invocation
;; - the number of flops per byte, aka the arithmetic intensity
(defstruct benchmark
  (name nil :type symbol :read-only t)
  (fn nil
   :type function
   :read-only t))

(defparameter *seconds-per-benchmark* 4)

(defparameter *cache-size* 9e6
  "The size of the shared outer-level cache in bytes.")

;; likwid-bench -t load_avx -w S0:6MB
;; likwid-bench -t store_avx -w S0:6MB
(defparameter *cache-throughput* (+ 208e9 137e9)
  "The throughput of the shared outer-level cache in bytes per second.")

;; likwid-bench -t load_avx -w S0:1GB
;; likwid-bench -t store_avx -w S0:1GB
(defparameter *memory-throughput* (+ 32e9 12e9)
  "The throughput of the main memory in bytes per second.")

;; likwid-bench -t peakflops_avx -w S0:6MB
(defparameter *max-flops* 124e9
  "The maximum number of floating-point operations the CPU can execute per second.")

(defun print-benchmark-table (benchmark &optional (rows 10))
  (format t "#+TITLE: ~A"
          (benchmark-name benchmark))
  (format t "~&|     bytes | flops/byte | roofline |     flops |~%")
  (format t "~&+-----------+------------+----------+-----------+~%")
  (let* ((limit (* 2 *cache-size*))
         (step (ceiling limit rows)))
    (loop for row from 1 to rows do
      (print-benchmark-row benchmark (* row step)))))

(defun print-benchmark-row (benchmark nbytes)
  (multiple-value-bind (thunk flops-per-thunk flops-per-byte)
      (funcall (benchmark-fn benchmark) nbytes)
    (let* ((tmax (* internal-time-units-per-second *seconds-per-benchmark*))
           (tstart (get-internal-real-time))
           (tend tstart)
           (count 0))
      (loop
        (funcall thunk)
        (incf count)
        (setf tend (get-internal-real-time))
        (when (> (- tend tstart) tmax)
          (return)))
      (let ((time (float (/ (- tend tstart) internal-time-units-per-second))))
        (format t "~&| ~9,2E | ~9,2F | ~9,2E | ~9,2E |~%"
                nbytes
                flops-per-byte
                (min *max-flops* (* *cache-throughput* flops-per-byte))
                (/ (* flops-per-thunk count) time))))))

(defvar *benchmarks* '()
  "The names of all available benchmarks.")

(defmacro defbenchmark (name lambda-list &body body)
  (declare (symbol name))
  `(progn
     (defparameter ,name
       (make-benchmark
        :name ',name
        :fn (lambda ,lambda-list ,@body)))
     (pushnew ',name *benchmarks*)))

(defun flopcount (&rest lazy-arrays)
  (let ((flops 0))
    (maphash
     (lambda (fn count)
       (ecase fn
         ((typo:two-arg-double-float+
           typo:two-arg-double-float-
           typo:two-arg-double-float*)
          (incf flops count))))
     (apply #'petalisp.core:callcount lazy-arrays))
    flops))
