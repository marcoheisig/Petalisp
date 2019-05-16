;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defun make-dummy-kernel ()
  (make-kernel :iteration-space (~) :reduction-range (range 0)))

(defmethod compute-immediates ((lazy-arrays list) (native-backend native-backend))
  (let ((root-buffers (ir-from-lazy-arrays lazy-arrays)))
    (normalize-ir root-buffers)
    (loop for root-buffer in root-buffers
          for lazy-array in lazy-arrays
          ;; We add a fictitious kernel to the outputs of each root buffer,
          ;; to avoid that their memory is reclaimed.
          do (push (make-dummy-kernel) (buffer-outputs root-buffer))
          collect
          (if (immediatep lazy-array)
              lazy-array
              (coerce-to-lazy-array
               (buffer-storage
                (compute-buffer root-buffer native-backend)))))))

(defmethod compute-buffer ((buffer buffer) (native-backend native-backend))
  (when (null (buffer-storage buffer))
    (setf (buffer-storage buffer)
          (memory-pool-allocate
           (memory-pool native-backend)
           (petalisp.type-codes:type-specifier-from-type-code
            (buffer-type-code buffer))
           (mapcar #'range-size (ranges (buffer-shape buffer)))))
    (map-buffer-inputs
     (lambda (kernel)
       (execute-kernel kernel native-backend))
     buffer))
  buffer)

(defmethod execute-kernel :before ((kernel kernel) (native-backend native-backend))
  (map-kernel-inputs
   (lambda (buffer)
     (compute-buffer buffer native-backend))
   kernel))

(defmethod execute-kernel
    ((kernel kernel) (native-backend native-backend))
  (unless (kernel-executedp kernel)
    (compile-and-execute-kernel kernel native-backend)
    (setf (kernel-executedp kernel) t)
    ;; Free the memory of buffers that are no longer in use.
    (map-kernel-inputs
     (lambda (buffer)
       (when (every #'kernel-executedp (buffer-outputs buffer))
         (free-storage buffer native-backend)))
     kernel)))

(defun compile-and-execute-kernel (kernel backend)
  (let ((ranges (load-time-value (make-array 0 :adjustable t :fill-pointer 0) nil))
        (reduction-range (load-time-value (make-array 3) nil))
        (arrays (load-time-value (make-array 0 :adjustable t :fill-pointer 0) nil))
        (functions (load-time-value (make-array 0 :adjustable t :fill-pointer 0) nil))
        (compiled-kernel
          (let ((blueprint (kernel-blueprint kernel)))
            (petalisp.utilities:with-hash-table-memoization (blueprint)
                (compile-cache backend)
              (compile nil (lambda-expression-from-blueprint blueprint))))))
    (setf (fill-pointer ranges) 0)
    (setf (fill-pointer arrays) 0)
    (setf (fill-pointer functions) 0)
    ;; Initialize the range arguments.
    (loop for range in (ranges (kernel-iteration-space kernel))
          for offset from 0 by 3 do
            (multiple-value-bind (start step end)
                (range-start-step-end range)
              (vector-push-extend start ranges)
              (vector-push-extend step ranges)
              (vector-push-extend end ranges)))
    ;; Initialize the reduction range.
    (let ((range (kernel-reduction-range kernel)))
      (unless (null range)
        (multiple-value-bind (start step end)
            (range-start-step-end range)
          (setf (aref reduction-range 0) start)
          (setf (aref reduction-range 1) step)
          (setf (aref reduction-range 2) end))))
    ;; Initialize the array arguments.
    (loop for buffer in (kernel-buffers kernel) do
      (vector-push-extend (the array (buffer-storage buffer)) arrays))
    ;; Initialize the function arguments.
    (map-instructions
     (lambda (instruction)
       (cond ((call-instruction-p instruction)
              (let ((operator (call-instruction-operator instruction)))
                (when (functionp operator)
                  (vector-push-extend operator functions))))
             ((reduce-instruction-p instruction)
              (let ((operator (reduce-instruction-operator instruction)))
                (when (functionp operator)
                  (vector-push-extend operator functions))))))
     kernel)
    ;; Now call the compiled kernel.
    (funcall compiled-kernel ranges reduction-range arrays functions)))

(defgeneric free-storage (buffer backend))

(defmethod free-storage ((buffer buffer) (backend native-backend))
  (values))

(defmethod free-storage ((buffer buffer) (native-backend native-backend))
  (let ((memory-pool (memory-pool native-backend))
        (storage (buffer-storage buffer)))
    (unless (null storage)
      (setf (buffer-storage buffer) nil)
      (when (buffer-reusablep buffer)
        (memory-pool-free memory-pool storage)))))
