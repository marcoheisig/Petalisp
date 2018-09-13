;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass invertible-transformation (transformation)
  ())

(defclass cached-inverse-transformation-mixin ()
  ((%cached-inverse :accessor cached-inverse
                    :initform nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(define-class-predicate invertible-transformation :hyphenate t)

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
