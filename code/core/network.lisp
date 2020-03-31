;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defgeneric compile-network-on-backend (network backend))

(defclass network ()
  ((%parameters :initarg :parameters :reader network-parameters)
   (%outputs :initarg :outputs :reader network-outputs)))

(defun make-network (&rest outputs)
  (make-instance 'network
    :parameters (derive-parameters outputs)
    :outputs outputs))

(defun derive-parameters (outputs)
  (let ((table (make-hash-table :test #'eq))
        (parameters '()))
    (labels ((scan (lazy-array)
               (cond ((= 1 (refcount lazy-array))
                      (process lazy-array))
                     ((not (gethash lazy-array table))
                      (setf (gethash lazy-array table) t)
                      (process lazy-array))))
             (process (lazy-array)
               (cond ((typep lazy-array 'parameter)
                      (push lazy-array parameters))
                     (t
                      (mapc #'scan (inputs lazy-array))))))
      (mapc #'scan outputs))
    parameters))

(defun call-network (network &rest plist &key &allow-other-keys)
  (let ((args (loop for parameter in (network-parameters network)
                    for value = (getf plist parameter '.missing.)
                    collect
                    (if (eq value '.missing.)
                        (if (typep parameter 'optional-parameter)
                            (optional-parameter-value parameter)
                            (error "Missing parameter: ~S." parameter))
                        value))))
    (apply (compile-network-on-backend network *backend*) args)))

;;; This is a simple, albeit slow way of compiling a network.  We simply
;;; return a closure that substitutes all networks parameters with the provided
;;; values and then computes that new graph.
(defmethod compile-network-on-backend
    ((network network) (backend backend))
  (lambda (&rest args)
    (compute-on-backend
     (substitute-arrays
      (network-outputs network)
      args
      (network-parameters network))
     backend)))
