;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defclass network ()
  ((%parameters
    :initarg :parameters
    :initform (alexandria:required-argument :parameters)
    :reader network-parameters)
   (%outputs
    :initarg :outputs
    :initform (alexandria:required-argument :outputs)
    :reader network-outputs)
   (%evaluator
    :initarg :evaluator
    :initform (alexandria:required-argument :evaluator)
    :reader network-evaluator)))

(defun make-network (&rest outputs)
  (let ((unknowns (lazy-unknowns outputs)))
    (make-instance 'network
      :parameters unknowns
      :outputs outputs
      :evaluator (evaluator outputs unknowns))))

(defun call-network (network &rest plist)
  (multiple-value-list
   (apply
    (network-evaluator network)
    (append
     (loop for output in (network-outputs network)
           collect nil)
     (loop for parameter in (network-parameters network)
           for value = (getf plist parameter '.missing.)
           when (eq value '.missing.) do
             (error "Missing parameter: ~S" parameter)
           collect
           (compute
            (lazy-reshape
             (lazy #'coerce value (lazy-array-element-type parameter))
             (lazy-array-shape parameter))))))))
