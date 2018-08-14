;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-test-suite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test Machinery

(defclass testing-backend (backend)
  ((%reference-backend :initform (make-instance 'reference-backend) :reader reference-backend)
   ;; TODO actually use the native backend here.
   (%native-backend :initform (make-instance 'reference-backend) :reader native-backend)))

(defmethod compute-immediates ((data-structures list) (testing-backend testing-backend))
  (let ((reference-backend-results
          (compute-immediates data-structures (reference-backend testing-backend)))
        (native-backend-results
          (compute-immediates data-structures (native-backend testing-backend))))
    (loop for data-structure in data-structures
          for reference-backend-result in reference-backend-results
          for native-backend-result in native-backend-results do
            (fiveam:is (equalp (storage reference-backend-result)
                               (storage native-backend-result))))
    native-backend-results))

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

