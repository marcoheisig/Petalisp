;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

(defmethod compute-buffer
    ((buffer petalisp-ir:buffer) (native-backend native-backend))
  (values))

(defmethod compute-buffer :around
    ((buffer petalisp-ir:buffer) (backend backend))
  (call-next-method)
  buffer)

(defmethod compute-buffer
    ((buffer native-backend-buffer) (native-backend native-backend))
  (unless (slot-boundp buffer '%storage)
    (setf (storage buffer)
          (memory-pool-allocate
           (memory-pool native-backend)
           (element-type buffer)
           (mapcar #'set-size (ranges (shape buffer)))))
    (loop for kernel in (inputs buffer) do
      (execute-kernel kernel native-backend))))

(defmethod execute-kernel :before
    ((kernel native-backend-kernel) (native-backend native-backend))
  (loop for buffer in (inputs kernel) do
    (compute-buffer buffer native-backend)))

(defmethod execute-kernel
    ((kernel native-backend-kernel) (native-backend native-backend))
  (unless (executedp kernel)
    ;; Run the kernel.
    (funcall (compile-blueprint (compute-blueprint kernel) native-backend) kernel)
    (setf (executedp kernel) t)
    ;; Free the memory of buffers that are no longer in use.
    (loop for buffer in (inputs kernel) do
      (when (every #'executedp (petalisp-ir:outputs buffer))
        (free-storage buffer native-backend)))))

(defun free-storage (buffer backend)
  (let ((memory-pool (memory-pool backend))
        (storage (storage buffer)))
    (setf (storage buffer) nil)
    (memory-pool-free memory-pool storage)
    (values)))
