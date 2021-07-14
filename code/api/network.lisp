;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defgeneric compile-network-on-backend (network backend))

(defclass network ()
  ((%parameters :initarg :parameters :reader network-parameters)
   (%outputs :initarg :outputs :reader network-outputs)))

(defun make-network (&rest outputs)
  (multiple-value-bind (lazy-thunks lazy-unknowns)
      (lazy-thunks-and-unknowns outputs)
    (mapc #'force-lazy-thunk lazy-thunks)
    (make-instance 'network
      :parameters lazy-unknowns
      :outputs outputs)))

(defun call-network (network &rest plist)
  (apply (compile-network-on-backend network *backend*)
         (loop for parameter in (network-parameters network)
               for value = (getf plist parameter '.missing.)
               when (eq value '.missing.) do
                 (error "Missing parameter: ~S" parameter)
               collect
               (lazy-reshape
                (lazy #'coerce value (lazy-array-element-type parameter))
                (lazy-array-shape parameter)))))

;;; This is a simple, albeit slow way of compiling a network.  We simply
;;; return a closure that substitutes all networks parameters with the provided
;;; values and then computes that new graph.
(defmethod compile-network-on-backend
    ((network network) (backend backend))
  (lambda (&rest args)
    (backend-compute
     backend
     (substitute-lazy-arrays
      (network-outputs network)
      args
      (network-parameters network)))))
