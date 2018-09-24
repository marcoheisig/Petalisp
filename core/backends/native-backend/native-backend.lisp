;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

;;; This is the default Petalisp backend.  It generates portable, highly
;;; optimized Lisp code and compiles it using CL:COMPILE.

(defgeneric compute-buffer (buffer backend))

(defgeneric execute-kernel (kernel backend))

(defclass native-backend
    (asynchronous-backend)
  ((%memory-pool :initarg :memory-pool :reader memory-pool)
   (%worker-pool :initarg :worker-pool :reader worker-pool)))

(defun make-native-backend (&key (threads 1))
  (check-type threads positive-integer)
  (make-instance 'native-backend
    :memory-pool (make-memory-pool)
    :worker-pool (lparallel:make-kernel threads)))

(defmethod compute-immediates ((strided-arrays list)
                               (native-backend native-backend))
  (let ((root-buffers (petalisp-ir:ir-from-strided-arrays strided-arrays native-backend)))
    (loop for root-buffer in root-buffers
          for strided-array in strided-arrays
          collect
          (if (immediatep strided-array)
              strided-array
              (immediate-from-buffer
               (compute-buffer root-buffer native-backend))))))
