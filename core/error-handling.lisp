;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/error-handling
  (:use :closer-common-lisp :alexandria)
  (:export
   #:report-condition
   #:petalisp-user-error))

(in-package :petalisp/core/error-handling)

(defgeneric report-condition (condition stream))

;;; We differentiate between two kinds of errors -- those triggered by
;;; improper usage of Petalisp and those occurring even though Petalisp has
;;; been used correctly. The former are expected to be far more frequent
;;; and part of the everyday user experience, while the latter should
;;; ideally never arise. Consequentially, errors triggered by the user
;;; should emit a detailed and helpful report, while the latter do not even
;;; deserve their own condition type.

(define-condition petalisp-user-error (error) ()
  (:report report-condition))

;;; For now, we define a default reporter that uses the name of the
;;; condition as message.

(defmethod report-condition ((condition petalisp-user-error) stream)
  (write-symbol-as-sentence (class-name (class-of condition)) stream))

(defun write-symbol-as-sentence (symbol stream)
  (let ((name (symbol-name symbol)))
    ;; upcase the first letter
    (write-char (char-upcase (aref name 0)) stream)
    ;; replace all hyphens in the body
    (loop for index from 1 below (length name) do
      (let ((char (aref name index)))
        (if (char= char #\-)
            (write-char #\space stream)
            (write-char (char-downcase char) stream))))
    ;; write the trailing dot
    (write-char #\. stream)))
