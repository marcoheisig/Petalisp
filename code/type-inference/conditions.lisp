;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-condition give-up-specialization (condition)
  ())

(defun give-up-specialization ()
  (signal 'give-up-specialization))

(define-condition specialization-error (error)
  ((%function :initarg :function :reader specialization-error-function)))

(define-condition wrong-number-of-arguments (specialization-error)
  ((%arguments :initarg :arguments :reader specialization-error-arguments))
  (:report
   (lambda (condition stream)
     (format stream
             "~@<Cannot call ~S with ~R argument~:P.~:@>"
             (specialization-error-function condition)
             (length
              (specialization-error-arguments condition))))))

(define-condition invalid-arguments (specialization-error)
  ((%argument-types :initarg :argument-types :reader specialization-error-argument-types))
  (:report
   (lambda (condition stream)
     (format stream
             "~@<Invalid call to ~S with ~
              ~{~#[no arguments~;~
                   one argument of type ~S~;~
                   arguments of types ~S and ~S~:;~
                   arguments of types ~@{~S~#[~;, and ~:;, ~]~}~]~:}.~:@>"
             (specialization-error-function condition)
             (specialization-error-argument-types condition)))))

(define-condition invalid-differentiation-index (specialization-error)
  ((%index :initarg :index :reader specialization-error-index)
   (%arguments :initarg :arguments :reader specialization-error-arguments))
  (:report
   (lambda (condition stream)
     (format stream
             "~@<Invalid differentiation index ~D for a call to ~S with ~R argument~:P.~:@>"
             (specialization-error-index condition)
             (specialization-error-function condition)
             (length
              (specialization-error-arguments condition))))))

(define-condition non-numeric-differentiation-argument (specialization-error)
  ((%index :initarg :index :reader specialization-error-index)
   (%arguments :initarg :arguments :reader specialization-error-arguments))
  (:report
   (lambda (condition stream)
     (format stream
             "~@<The ~:R argument of the function ~S, ~S, ~
                 does not have a number type.~:@>"
             (specialization-error-index condition)
             (specialization-error-function condition)
             (nth
              (specialization-error-index condition)
              (specialization-error-arguments condition))))))
