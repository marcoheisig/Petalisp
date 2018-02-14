;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/error-handling
  (:use :closer-common-lisp :alexandria)
  (:export
   #:petalisp-user-error
   #:demand))

(in-package :petalisp/core/error-handling)

;;; We differentiate between two kinds of errors -- those triggered by
;;; improper usage of Petalisp and those occurring even though Petalisp has
;;; been used correctly. The former are expected to be far more frequent
;;; and part of the everyday user experience, while the latter should
;;; ideally never arise. Consequentially, errors triggered by the user
;;; should emit a detailed and helpful report, while the latter do not even
;;; deserve their own condition type.

(define-condition petalisp-user-error (simple-error) ())

;;; A wrapper around ASSERT, that signals a condition of type
;;; PETALISP-USER-ERROR.
(defmacro demand (test-form &body control-string-and-arguments)
  (destructuring-bind (control-string &rest arguments)
      control-string-and-arguments
    (check-type control-string string)
    `(assert ,test-form () 'petalisp-user-error
             :format-control ,control-string
             :format-arguments (list ,@arguments))))
