;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.test-suite)

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
    #+nil
    (petalisp-ir-backend:make-ir-backend)
    (petalisp-native-backend:make-native-backend))))

(defun make-testing-backend ()
  (make-instance 'testing-backend))

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
                (is (approximately-equal immediate expected-immediate))))
      reference-solutions)))

(defmethod delete-backend ((testing-backend testing-backend))
  (delete-backend (reference-backend testing-backend))
  (delete-backend (ir-backend testing-backend))
  (delete-backend (native-backend testing-backend))
  (call-next-method))

(defun call-with-testing-backend (thunk)
  (let ((*backend* (make-testing-backend)))
    (unwind-protect (funcall thunk)
      (delete-backend *backend*))))

(defmacro with-testing-backend (&body body)
  `(call-with-testing-backend (lambda () ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Equality

(defgeneric approximately-equal (a b))

(defmethod approximately-equal ((a t) (b t))
  (eql a b))

(defmethod approximately-equal ((a immediate) (b immediate))
  (approximately-equal
   (lisp-datum-from-immediate a)
   (lisp-datum-from-immediate b)))

(defmethod approximately-equal ((array-1 array) (array-2 array))
  (and (equal (array-dimensions array-1)
              (array-dimensions array-2))
       (loop for index below (array-total-size array-1)
             always (approximately-equal
                     (row-major-aref array-1 index)
                     (row-major-aref array-2 index)))))

(defmethod approximately-equal ((a single-float) (b single-float))
  (< (abs (- a b)) (* 64 single-float-epsilon)))

(defmethod approximately-equal ((a double-float) (b double-float))
  (< (abs (- a b)) (* 64 double-float-epsilon)))
