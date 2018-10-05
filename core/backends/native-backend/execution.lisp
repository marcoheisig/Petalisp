;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

(defgeneric compute-buffer (buffer backend))

(defgeneric execute-kernel (kernel backend))

(defmethod compute-immediates ((strided-arrays list)
                               (native-backend native-backend))
  (let ((root-buffers (petalisp-ir:ir-from-strided-arrays strided-arrays native-backend)))
    (petalisp-ir:normalize-ir root-buffers)
    (loop for root-buffer in root-buffers
          for strided-array in strided-arrays
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
  (loop for (buffer . nil) in (petalisp-ir:loads kernel) do
    (compute-buffer buffer native-backend)))

(defmethod execute-kernel
    ((kernel kernel) (native-backend native-backend))
  (unless (executedp kernel)
    ;; Run the kernel.
    (funcall (compile-blueprint (blueprint kernel))
             kernel)
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
