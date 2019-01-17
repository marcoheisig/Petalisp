;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package :petalisp-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Error Handling
;;;
;;; We differentiate between two kinds of errors -- those triggered by
;;; improper usage of Petalisp (user errors) and those occurring even
;;; though Petalisp has been used correctly (internal errors).  The former
;;; are expected to be far more frequent and part of the everyday user
;;; experience, while the latter should ideally never arise.
;;;
;;; We frequently use CL:ASSERT to ensure the internal consistency of
;;; Petalisp.  However, for user input, we want CL:ASSERT to emit
;;; conditions of type PETALISP-USER-ERROR instead of CL:ERROR.  This can
;;; be done, but the syntax is quite verbose.  To simplify the notation in
;;; this case, we provide the shorthand macro DEMAND.

(define-condition petalisp-user-error (simple-error) ())

(defmacro demand (test-form &body control-string-and-arguments)
  (destructuring-bind (control-string &rest arguments)
      control-string-and-arguments
    (check-type control-string string)
    `(assert ,test-form () 'petalisp-user-error
             :format-control ,control-string
             :format-arguments (list ,@arguments))))
