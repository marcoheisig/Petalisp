;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

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
    (compile-and-execute-kernel kernel native-backend)
    (setf (executedp kernel) t)
    ;; Free the memory of buffers that are no longer in use.
    (loop for buffer in (inputs kernel) do
      (when (every #'executedp (petalisp-ir:outputs buffer))
        (free-storage buffer native-backend)))))

(defun compile-and-execute-kernel (kernel backend)
  (let ((range-arguments
          (load-time-value
           (make-array 0 :adjustable t)
           nil))
        (storage-arguments
          (load-time-value
           (make-array 0 :adjustable t))))
    ;; Initialize the range arguments.
    (adjust-array range-arguments (* 3 (dimension (petalisp-ir:iteration-space kernel))))
    (loop for range in (ranges (petalisp-ir:iteration-space kernel))
          for offset from 0 by 3 do
            (multiple-value-bind (start step end)
                (range-start-step-end range)
              (setf (aref range-arguments (+ offset 0)) start)
              (setf (aref range-arguments (+ offset 1)) step)
              (setf (aref range-arguments (+ offset 2)) end)))
    ;; Initialize the storage arguments.
    (adjust-array storage-arguments (length (buffers kernel)))
    (loop for buffer in (buffers kernel)
          for index from 0 do
            (setf (aref storage-arguments index)
                  (storage buffer)))
    ;; Now call the compiled kernel.
    (funcall (compile-blueprint (petalisp-ir:blueprint kernel))
             range-arguments
             storage-arguments)))

(defun free-storage (buffer backend)
  (let ((memory-pool (memory-pool backend))
        (storage (storage buffer)))
    (setf (storage buffer) nil)
    (memory-pool-free memory-pool storage)
    (values)))
