;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

;;; A thunk that computes a list whose first argument is a function name
;;; and whose remaining elements are type specifiers.
(defvar *specialization-error-thunk*)

(define-condition specialization-error (error)
  ((%thunk :initarg :thunk :reader specialization-error-thunk)))

(define-condition wrong-number-of-arguments (specialization-error)
  ()
  (:report
   (lambda (condition stream)
     (let ((form (funcall (specialization-error-thunk condition))))
       (format stream
               "~@<Cannot call ~S with ~R arguments.~:@>"
               (first form)
               (length (rest form)))))))

(define-condition invalid-arguments (specialization-error)
  ()
  (:report
   (lambda (condition stream)
     (let ((form (funcall (specialization-error-thunk condition)))
           (*print-circle* nil))
       (format stream
               "~@<Invalid call to ~S with ~
              ~{~#[no arguments~;~
                  one argument of type ~S~;~
                  arguments of types ~S and ~S~:;~
                  arguments of types ~@{~S~#[~;, and ~:;, ~]~}~]~:}.~:@>"
               (first form)
               (rest form))))))

(defun abort-specialization ()
  (error 'invalid-arguments :thunk *specialization-error-thunk*))
