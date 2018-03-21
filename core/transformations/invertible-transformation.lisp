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

;; Somehow caching the inverse makes things break...
#+nil
(defmethod invert-transformation :around
    ((transformation cached-inverse-transformation-mixin))
  (let ((cached-inverse (cached-inverse transformation)))
    (let ((result (or cached-inverse
                      (let ((computed-inverse (call-next-method)))
                        (setf (cached-inverse computed-inverse) transformation)
                        (setf (cached-inverse transformation) computed-inverse)
                        computed-inverse))))
      (assert (and (= (input-dimension result)
                      (output-dimension transformation))
                   (= (output-dimension result)
                      (input-dimension transformation))))
      result)))

(defmethod invert-transformation
    ((transformation transformation))
  (error 'petalisp-user-error
         "~:<The transformation ~W is not invertible.~:@>"))
