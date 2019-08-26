;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-condition give-up-specialization (condition)
  ())

(defun give-up-specialization ()
  (signal 'give-up-specialization))

(define-condition specialization-error (error)
  ((%function :initarg :function :reader specialization-error-function)))

(define-condition no-specialization-rule (specialization-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream
             "~@<No specialization rule for ~S.~:@>"
             (specialization-error-function condition)))))

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
