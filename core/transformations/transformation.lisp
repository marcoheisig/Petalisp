;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/transformations/transformation
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all)
  (:export
   #:transformation
   #:input-dimension
   #:output-dimension
   #:composition
   #:inverse
   #:do-outputs
   #:enlarge-transformation))

(in-package :petalisp/core/transformations/transformation)

(defclass transformation (unary-funcallable-object) ()
  (:metaclass funcallable-standard-class))

(defgeneric input-dimension (transformation)
  (:method ((A matrix)) (matrix-n A)))

(defgeneric output-dimension (transformation)
  (:method ((A matrix)) (matrix-m A)))

(defgeneric composition (g f)
  (:method ((g function) (f function))
    (alexandria:compose g f))
  (:method :before ((g transformation) (f transformation))
    (assert (= (input-dimension g) (output-dimension f)))))

(defgeneric inverse (transformation))


;;; For each output of TRANSFORMATION, invoke FUNCTION with the output
;;; index, input index, the scaling, the offset and the corresponding
;;; values of each sequence in INPUT-SEQUENCES.
;;;
;;; Important: When the scaling is zero, the values of the corresponding
;;; input values are undefined.
(defgeneric do-outputs (transformation function &rest input-sequences))


;;; Given a transformation mapping from (i1 ... iN) to (j1 ... jM),
;;; return a transformation mapping from (i1 ... iN iN+1) to
;;; (j1 ... jM iN+1).
(defgeneric enlarge-transformation (transformation scale offset))
