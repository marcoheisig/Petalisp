;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(uiop:define-package :petalisp/core/transformations/invertible-transformation
  (:use :closer-common-lisp :alexandria)
  (:import-from :bordeaux-threads)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/transformation)
  (:export
   #:invertible-transformation
   #:cached-inverse-transformation-mixin))

(in-package :petalisp/core/transformations/invertible-transformation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass invertible-transformation (transformation)
  ()
  (:metaclass funcallable-standard-class))

(defclass cached-inverse-transformation-mixin ()
  ((%cached-inverse :accessor cached-inverse
                    :initform nil))
  (:metaclass funcallable-standard-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod invertible-transformation-p
    ((transformation invertible-transformation))
  t)

(defparameter *lock* (bt:make-lock "cached inverse lock"))

(defmethod invert-transformation :around
    ((transformation cached-inverse-transformation-mixin))
  (flet ((check-cache ()
           (when-let ((cached-inverse (cached-inverse transformation)))
             (return-from invert-transformation cached-inverse))))
    (check-cache)
    (bt:with-lock-held (*lock*)
      (check-cache)
      (let ((computed-inverse (call-next-method)))
            (setf (cached-inverse computed-inverse) transformation)
            (setf (cached-inverse transformation) computed-inverse)
            computed-inverse))))

(defmethod invert-transformation
    ((transformation transformation))
  (error 'petalisp-user-error
         "~:<The transformation ~W is not invertible.~:@>"
         transformation))
