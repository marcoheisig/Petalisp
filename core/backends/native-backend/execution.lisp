;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

(defmethod compute-immediates ((strided-arrays list)
                               (native-backend native-backend))
  (let ((root-buffers (petalisp-ir:ir-from-strided-arrays strided-arrays native-backend)))
    (petalisp-ir:normalize-ir root-buffers)
    (loop for root-buffer in root-buffers
          for strided-array in strided-arrays
          ;; We add a fictitious kernel to the outputs of each root buffer,
          ;; to avoid that their memory is reclaimed.
          do (push (make-instance 'kernel) (petalisp-ir:outputs root-buffer))
          collect
          (if (immediatep strided-array)
              strided-array
              (immediate-from-buffer
               (compute-buffer root-buffer native-backend)
               native-backend)))))

(defmethod compute-buffer ((buffer immediate-buffer) (native-backend native-backend))
  buffer)

(defmethod compute-buffer ((buffer non-immediate-buffer) (native-backend native-backend))
  (unless (slot-boundp buffer '%storage)
    (setf (storage buffer)
          (memory-pool-allocate
           (memory-pool native-backend)
           (element-type buffer)
           (mapcar #'set-size (ranges (shape buffer)))))
    (loop for kernel in (inputs buffer) do
      (execute-kernel kernel native-backend))
    buffer))

(defmethod execute-kernel :before
    ((kernel kernel) (native-backend native-backend))
  (loop for load in (petalisp-ir:loads kernel) do
    (compute-buffer (petalisp-ir:buffer load) native-backend)))

(defmethod execute-kernel
    ((kernel kernel) (native-backend native-backend))
  (unless (executedp kernel)
    (compile-and-execute-kernel kernel native-backend)
    (setf (executedp kernel) t)
    ;; Free the memory of buffers that are no longer in use.
    (flet ((maybe-free (buffer)
             (when (every #'executedp (petalisp-ir:outputs buffer))
               (free-storage buffer native-backend))))
      (mapc (compose #'maybe-free #'petalisp-ir:buffer)
            (petalisp-ir:stores kernel))
      (mapc (compose #'maybe-free #'petalisp-ir:buffer)
            (petalisp-ir:reduction-stores kernel)))))

(defun compile-and-execute-kernel (kernel backend)
  (let ((ranges (load-time-value (make-array 0 :adjustable t :fill-pointer 0) nil))
        (arrays (load-time-value (make-array 0 :adjustable t :fill-pointer 0) nil))
        (functions (load-time-value (make-array 0 :adjustable t :fill-pointer 0) nil))
        (lambda-expression
          (lambda-expression-from-blueprint (petalisp-ir:blueprint kernel))))
    (setf (fill-pointer ranges) 0)
    (setf (fill-pointer arrays) 0)
    (setf (fill-pointer functions) 0)
    ;; Initialize the range arguments.
    (loop for range in (ranges (petalisp-ir:iteration-space kernel))
          for offset from 0 by 3 do
            (multiple-value-bind (start step end)
                (range-start-step-end range)
              (vector-push-extend start ranges)
              (vector-push-extend step ranges)
              (vector-push-extend end ranges)))
    ;; Initialize the array arguments.
    (loop for buffer in (petalisp-ir:kernel-buffers kernel) do
      (vector-push-extend (the array (storage buffer)) arrays))
    ;; Initialize the function arguments.
    (petalisp-ir:map-instructions
     (lambda (instruction)
       (when (typep instruction
                    '(or petalisp-ir:call-instruction petalisp-ir:reduce-instruction))
         (let ((operator (operator instruction)))
           (when (functionp operator)
             (vector-push-extend (operator instruction) functions)))))
     kernel)
    ;; Now call the compiled kernel.
    (funcall (compile nil lambda-expression) ranges arrays functions)))

(defgeneric free-storage (buffer backend))

(defmethod free-storage ((buffer buffer) (backend native-backend))
  (values))

(defmethod free-storage ((buffer non-immediate-buffer) (backend native-backend))
  (let ((memory-pool (memory-pool backend))
        (storage (storage buffer)))
    (setf (storage buffer) nil)
    (memory-pool-free memory-pool storage)
    (values)))
