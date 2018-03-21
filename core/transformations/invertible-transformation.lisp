;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/transformations/invertible-transformation
  (:use :closer-common-lisp :alexandria)
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
  ())

(defclass cached-inverse-transformation-mixin ()
  ((%cached-inverse :accessor cached-inverse
                    :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod invertible-transformation-p
    ((transformation invertible-transformation))
  t)

(defmethod invert-transformation :around
    ((transformation cached-inverse-transformation-mixin))
  (or (cached-inverse transformation)
      (let ((computed-inverse (call-next-method)))
        (prog1 computed-inverse
          (when (typep computed-inverse 'cached-inverse-transformation-mixin)
            (setf (cached-inverse computed-inverse) transformation))
          (setf (cached-inverse transformation) computed-inverse)))))
