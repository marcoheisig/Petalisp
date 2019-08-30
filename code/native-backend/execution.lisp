;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defun make-dummy-kernel ()
  (make-kernel :iteration-space (~)))

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

(defmethod compute-immediates :after ((lazy-arrays list) (native-backend native-backend))
  (memory-pool-reset
   (memory-pool native-backend)))

(defmethod compute-buffer ((buffer buffer) (native-backend native-backend))
  (when (null (buffer-storage buffer))
    (setf (buffer-storage buffer)
          (memory-pool-allocate
           (memory-pool native-backend)
           (petalisp.type-inference:type-specifier
            (buffer-ntype buffer))
           (mapcar #'range-size (shape-ranges (buffer-shape buffer)))))
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
    (setf (kernel-executedp kernel) t)
    (compile-and-execute-kernel kernel native-backend)
    ;; Free the memory of buffers that are no longer in use.
    (map-kernel-inputs
     (lambda (buffer)
       (when (every #'kernel-executedp (buffer-outputs buffer))
         (free-storage buffer native-backend)))
     kernel)))

(defun kernel-ranges (kernel)
  (let* ((iteration-space (kernel-iteration-space kernel))
         (rank (shape-rank iteration-space))
         (vector (make-array (* 3 rank))))
    (loop for index below rank
          for range in (shape-ranges iteration-space) do
      (multiple-value-bind (start step end)
          (range-start-step-end range)
        (setf (svref vector (+ (* 3 index) 0)) start)
        (setf (svref vector (+ (* 3 index) 1)) step)
        (setf (svref vector (+ (* 3 index) 2)) end)))
    vector))

(defun kernel-arrays (kernel size)
  (let ((buffers (make-array size))
        (vector (make-array size))
        (current 0))
    (flet ((register-buffer (buffer)
             (unless (find buffer buffers :test #'eq :end current)
               (cond ((= current size)
                      (kernel-arrays kernel (* 5 size)))
                     (t
                      (setf (svref buffers current) buffer)
                      (setf (svref vector current) (buffer-storage buffer))
                      (incf current))))))
      (map-kernel-load-instructions
       (lambda (load-instruction)
         (register-buffer
          (load-instruction-buffer load-instruction)))
       kernel)
      (map-kernel-store-instructions
       (lambda (store-instruction)
         (register-buffer
          (store-instruction-buffer store-instruction)))
       kernel)
      vector)))

(defun kernel-functions (kernel size)
  (let ((vector (make-array size))
        (current 0))
    (flet ((register-function (function)
             ;; If FUNCTION is a symbol, it is part of the kernel blueprint
             ;; and we don't need to pass it explicitly.
             (unless (symbolp function)
               (unless (find function vector :test #'eq :end current)
                 (cond ((= current size)
                        (kernel-functions kernel (* 5 size)))
                       (t
                        (setf (svref vector current) function)
                        (incf current)))))))
      (map-instructions
       (lambda (instruction)
         (cond ((call-instruction-p instruction)
                (register-function (call-instruction-operator instruction)))
               ((reduce-instruction-p instruction)
                (register-function (reduce-instruction-operator instruction)))))
       kernel)
      vector)))

(defun compile-and-execute-kernel (kernel backend)
  (let ((ranges (kernel-ranges kernel))
        (arrays (kernel-arrays kernel 8))
        (functions (kernel-functions kernel 8))
        (compiled-kernel
          (let ((blueprint (kernel-blueprint kernel)))
            (petalisp.utilities:with-hash-table-memoization (blueprint)
                (compile-cache backend)
              (compile nil (lambda-expression-from-blueprint blueprint))))))
    ;; Now call the compiled kernel.
    (funcall compiled-kernel ranges arrays functions)))

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
