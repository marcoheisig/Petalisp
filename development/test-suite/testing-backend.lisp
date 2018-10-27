;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

(defclass testing-backend (backend)
  ((%reference-backend
    :reader reference-backend
    :initform (petalisp-reference-backend:make-reference-backend))
   (%ir-backend
    :reader ir-backend
    :initform (petalisp-ir-backend:make-ir-backend))
   (%native-backend
    :reader native-backend
    :initform
    ;; TODO
    (petalisp-native-backend:make-native-backend :threads 2)
    #+nil
    (petalisp-reference-backend:make-reference-backend))))

(defun make-testing-backend ()
  (make-instance 'testing-backend))

(defun immediate-equalp (immediate-1 immediate-2)
  (equalp (storage immediate-1)
          (storage immediate-2)))

(defmethod compute-immediates ((data-structures list) (testing-backend testing-backend))
  (with-accessors ((reference-backend reference-backend)
                   (ir-backend ir-backend)
                   (native-backend native-backend)) testing-backend
    (let ((reference-solutions
            (compute-immediates data-structures reference-backend)))
      (loop for backend in (list ir-backend native-backend) do
        (loop for immediate in (compute-immediates data-structures backend)
              for expected-immediate in reference-solutions
              for index from 0 do
                (1am:is (immediate-equalp immediate expected-immediate))))
      reference-solutions)))

(defmethod delete-backend ((testing-backend testing-backend))
  (delete-backend (reference-backend testing-backend))
  (delete-backend (ir-backend testing-backend))
  (delete-backend (native-backend testing-backend))
  (call-next-method))
