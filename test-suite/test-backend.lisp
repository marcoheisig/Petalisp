;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-test-suite)

;;; When a backend inherits from TEST-BACKEND-MIXIN, an error is
;;; automatically signaled whenever the backend yields results that differ
;;; from those of the reference backend.

(defgeneric immediate-equalp (immediate-1 immediate-2))

(defclass test-backend (backend)
  ((%reference-backend :initform (make-instance 'reference-backend) :reader reference-backend)
   (%native-backend :initform (make-instance 'native-backend) :reader native-backend)))

(defmethod compute-immediates ((data-structures list) (test-backend test-backend))
  (let ((reference-backend-results
          (compute-immediates data-structures (reference-backend test-backend)))
        (native-backend-results
          (compute-immediates data-structures (native-backend test-backend))))
    (loop for data-structure in data-structures
          for reference-backend-result in reference-backend-results
          for native-backend-result in native-backend-results
          unless (immediate-equalp reference-backend-result native-backend-result) do
            (error "~@<The native backend evaluated the data structure ~A to ~
                       ~A, which differs from the reference solution ~A.~:@>"
                   data-structure native-backend-result reference-backend-result))))

(defmethod immediate-equalp ((immediate-1 immediate) (immediate-2 immediate))
  nil)

(defmethod immediate-equalp ((immediate-1 array-immediate) (immediate-2 array-immediate))
  (equalp (storage immediate-1)
          (storage immediate-2)))

(defmethod immediate-equalp ((immediate-1 scalar-immediate) (immediate-2 scalar-immediate))
  (equalp (storage immediate-1)
          (storage immediate-2)))
