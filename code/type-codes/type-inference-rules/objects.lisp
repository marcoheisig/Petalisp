;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

(define-type-inference-rule ensure-generic-function (function-name &rest args)
  (declare (ignore args))
  (check-type-code function-name function-name)
  (type-code-from-type-specifier 'function))

;; TODO SLOT-BOUNDP SLOT-EXISTS-P SLOT-MAKUNBOUND SLOT-VALUE

;; TODO MAKE-LOAD-FORM-SAVING-SLOTS, UNBOUND-SLOT-INSTANCE

(define-type-inference-rule class-of (object)
  (check-type-code object t)
  (type-code-from-type-specifier 'class))
