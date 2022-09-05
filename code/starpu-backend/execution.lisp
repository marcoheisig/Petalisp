;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.starpu-backend)

(defmethod backend-evaluator
    ((starpu-backend starpu-backend)
     (unknowns list)
     (lazy-arrays list))
  (funcall (evaluator-generator (length unknowns) (length lazy-arrays))
           (make-cstate starpu-backend unknowns lazy-arrays)))

(let ((cache (make-hash-table)))
  (defun evaluator-generator (number-of-arguments number-of-results)
    (alexandria:ensure-gethash
     number-of-arguments
     (alexandria:ensure-gethash number-of-results cache (make-hash-table))
     (let ((results (result-variables number-of-results))
           (arguments (argument-variables number-of-arguments)))
       (compile
        nil
        `(lambda (cstate)
           (lambda (,@results ,@arguments)
             (petalisp.utilities:with-pinned-objects (,@results ,@arguments)
               (starpu:with-starpu-activity
                 (let ((dstate (make-dstate cstate)))
                   (execute cstate dstate ,@arguments)
                   (finalize cstate dstate ,@results)))))))))))

(defun generate-variable (prefix integer)
  (intern
   (with-output-to-string (stream)
     (loop for char across (string prefix) do
       (write-char char stream))
     (format stream "~D" integer))
   #.*package*))

(defun result-variables (n)
  (loop for i below n collect (generate-variable "DST" i)))

(defun argument-variables (n)
  (loop for i below n collect (generate-variable "SRC" i)))

(defun execute (cstate dstate &rest arguments)
  (with-accessors ((kernel-codelet-vector cstate-kernel-codelet-vector)
                   (program cstate-program)
                   (argument-buffers cstate-argument-buffers)) cstate
    (with-accessors ((buffer-data-vector dstate-buffer-data-vector)) dstate
      ;; Initialize the arguments.
      (loop for argument in arguments
            for buffer in argument-buffers
            do (setf (svref buffer-data-vector (buffer-number buffer))
                     (let ((data (make-starpu-handle buffer)))
                       (sb-sys:with-pinned-objects (argument)
                         (cffi:foreign-funcall
                          "memcpy"
                          :pointer (starpu:data-local-pointer data)
                          :pointer (starpu::pinned-array-data-pointer argument)
                          :size (starpu:data-bytes data)))
                       data)))
      ;; Initialize all other leaf arrays.
      (loop for (buffer . leaf) in (program-leaf-alist program) do
        (unless (delayed-unknown-p (lazy-array-delayed-action leaf))
          (setf (svref buffer-data-vector (buffer-number buffer))
                (let ((data (make-starpu-handle buffer)))
                  (cffi:foreign-funcall
                   "memcpy"
                   :pointer (starpu:data-local-pointer data)
                   :pointer (starpu::pinned-array-data-pointer
                             (delayed-array-storage
                              (lazy-array-delayed-action leaf)))
                   :size (starpu:data-bytes data))
                  data))))
      (petalisp.ir:do-program-kernels (kernel program)
        (starpu:task-insert
         (svref kernel-codelet-vector (petalisp.ir:kernel-number kernel))
         :data
         (let ((result '()))
           (do-kernel-outputs (buffer kernel)
             (push :w result)
             (push (svref buffer-data-vector (buffer-number buffer)) result))
           (do-kernel-inputs (buffer kernel)
             (push :r result)
             (push (svref buffer-data-vector (buffer-number buffer)) result))
           (nreverse result))
         :args
         (let ((result '()))
           (dolist (range (shape-ranges (kernel-iteration-space kernel)))
             (push :int64 result)
             (push (range-start range) result)
             (push :int64 result)
             (push (range-end range) result)
             (push :int64 result)
             (push (range-step range) result))
           (do-kernel-outputs (buffer kernel)
             (let* ((shape (buffer-shape buffer)))
               (loop repeat (shape-rank shape) do
                 (push :uint64 result)
                 (push 0 result))
               (loop for axis from 3 below (1- (shape-rank shape)) do
                 (push :uint64 result)
                 (push (range-size (shape-range shape axis)) result))))
           (do-kernel-inputs (buffer kernel)
             (let* ((shape (buffer-shape buffer)))
               (loop repeat (shape-rank shape) do
                 (push :uint64 result)
                 (push 0 result))
               (loop for axis from 3 below (1- (shape-rank shape)) do
                 (push :uint64 result)
                 (push (range-size (shape-range shape axis)) result))))
           (do-kernel-instructions (instruction kernel)
             (when (iterating-instruction-p instruction)
               (map-transformation-outputs
                (lambda (output-index input-index scaling offset)
                  (declare (ignore output-index input-index))
                  (unless (< (abs scaling) *kernel-scaling-threshold*)
                    (push :int64 result)
                    (push scaling result))
                  (unless (< (abs offset) *kernel-offset-threshold*)
                    (push :int64 result)
                    (push offset result)))
                (instruction-transformation instruction))))
           (nreverse result))))
      (starpu:task-wait-for-all))))

(defun finalize (cstate dstate &rest results)
  (with-accessors ((program cstate-program)) cstate
    (with-accessors ((buffer-data-vector dstate-buffer-data-vector)) dstate
      (values-list
       (prog1 (loop for root-buffer in (petalisp.ir:program-root-buffers program)
                    for data = (svref buffer-data-vector (buffer-number root-buffer))
                    do (starpu:data-fetch data)
                    collect (starpu:data-contents data))
         (petalisp.ir:do-program-buffers (buffer program)
           (when (starpu:datap (svref buffer-data-vector (buffer-number buffer)))
             (starpu:data-unregister-no-coherency
              (svref buffer-data-vector (buffer-number buffer))))))))))
