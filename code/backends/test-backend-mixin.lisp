;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;; When a backend inherits from TEST-BACKEND-MIXIN, an error is
;;; automatically signaled whenever the backend yields results that differ
;;; from those of the reference backend.

(defclass test-backend-mixin (backend)
  ())

(defmethod compute-immediates :around (data-structures (backend test-backend-mixin))
  (let ((next-method-results
          (call-next-method))
        (reference-results
          (compute-immediates
           data-structures
           (make-instance 'reference-backend))))
    (loop for data-structure in data-structures
          for next-method-result in next-method-results
          for reference-result in reference-results
          unless (data-structure-equality next-method-result reference-result) do
            (error "~@<The backend ~A evaluated the data structure ~A to ~
                     ~A, which differs from the reference solution ~A.~:@>"
                   backend data-structure next-method-result reference-result))))
