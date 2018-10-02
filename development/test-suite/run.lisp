;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test Machinery

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
    #+nil
    (petalisp-native-backend:make-native-backend :threads 2)
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

(defmacro check (expr &rest results)
  `(progn
     ,@(loop for result in results
             for n from 0
             collect
             `(1am:is
               (equalp ,result (compute (nth-value ,n ,expr)))))))

(defun ndarray (n &optional (length 10))
  "Create a LENGTH^N array of double floats."
  (generate-array
   :element-type 'double-float
   :dimensions (make-list n :initial-element length)
   :element-generator (make-double-float-generator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Entry Point

(defun run-test-suite ()
  (format t "~&== Testing Petalisp ==~%")
  (print-platform-information)
  (print-system-statistics :petalisp)
  (print-system-statistics :petalisp-core)
  (print-system-statistics :petalisp-reference-backend)
  (print-system-statistics :petalisp-ir-backend)
  (print-system-statistics :petalisp-native-backend)
  (print-package-statistics :petalisp)
  (format t "~&Git revision: ~a" (system-git-revision :petalisp))
  (let ((*backend* (make-testing-backend)))
    (unwind-protect (1am:run)
      (delete-backend *backend*))))

