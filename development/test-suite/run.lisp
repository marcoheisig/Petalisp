;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test Machinery

(defclass testing-backend (backend)
  ((%reference-backend :initform (make-instance 'reference-backend) :reader reference-backend)
   (%ir-backend :initform (make-instance 'ir-backend) :reader ir-backend)
   ;; TODO actually use the native backend here.
   (%native-backend :initform (make-instance 'reference-backend) :reader native-backend)))

(defun immediate-equalp (immediate-1 immediate-2)
  (equalp (storage immediate-1)
          (storage immediate-2)))

(defmethod compute-immediates ((data-structures list) (testing-backend testing-backend))
  (with-accessors ((reference-backend reference-backend)
                   (ir-backend ir-backend)
                   (native-backend native-backend)) testing-backend
      (let ((reference-backend-results
              (compute-immediates data-structures reference-backend))
            (ir-backend-results
              (compute-immediates data-structures ir-backend))
            (native-backend-results
              (compute-immediates data-structures native-backend)))
        (unless (every #'immediate-equalp reference-backend-results ir-backend-results)
          (error "The IR backend computes wrong results"))
        (unless (every #'immediate-equalp reference-backend-results native-backend-results)
          (error "The native backend computes wrong results"))
        native-backend-results)))

(defmacro check (expr &rest results)
  `(progn
     ,@(loop for result in results
             for n from 0
             collect
             `(fiveam:is
               (equalp ,result (compute (nth-value ,n ,expr)))))))

(defun ndarray (n &optional (length 10))
  "Create a LENGTH^N array of double floats."
  (generate-instance 'array
                     :element-type 'double-float
                     :dimensions (make-list n :initial-element length)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Entry Point

(fiveam:def-suite petalisp)

(defun run-test-suite (&optional debug)
  (format t "~&== Testing Petalisp ==~%")
  (print-platform-information)
  (print-system-statistics :petalisp)
  (print-package-statistics :petalisp)
  (format t "~&Git revision: ~a" (system-git-revision :petalisp))
  (let ((fiveam:*on-error*   (if debug :debug *on-error*))
        (fiveam:*on-failure* (if debug :debug *on-failure*))
        (petalisp:*backend*
          (load-time-value
           (make-instance 'testing-backend))))
    (fiveam:run! 'petalisp)))

