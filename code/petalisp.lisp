;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Petalisp Vocabulary

(in-package :petalisp)

(define-class transformation () ())

(define-class data-structure ()
  ((element-type :initform t)
   (predecessors :initform nil
                 :type list)))

(define-class index-space (data-structure)
  ())

(define-class application (data-structure)
  ((operator :type function)))

(define-class reduction (data-structure)
  ((operator :type function)))

(define-class repetition (data-structure)
  ())

(define-class fusion (data-structure)
  ())

(define-class reference (data-structure)
  ((transformation :type transformation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; data-flow node constructors

(defgeneric application (operator object &rest more-objects)
  (:method :before ((operator function) (object data-structure) &rest more-objects)
    (let/de ((objects (list* object more-objects)))
      (check-arity operator (length objects))
      (assert (identical objects :test #'equal? :key #'index-space)))))

(defgeneric reduction (operator object)
  (:method :before ((operator function) (object data-structure))
    (check-arity operator (dimension object))))

(defgeneric repetition (object space)
  (:method :before ((object data-structure) space)
    (assert (<= (dimension object) (dimension space)))))

(defgeneric fusion (object &rest more-objects)
  (:method :before ((object data-structure) &rest more-objects)
    (let/de ((objects (list* object more-objects)))
      (assert (identical objects :test #'= :key #'dimension)))))

(defgeneric reference (object space transformation)
  (:method :before ((object data-structure) space (transformation transformation))
    (assert (and (subspace? space object)
                 (= (dimension space) (input-dimension transformation))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  classes and methods concerning index spaces

(defgeneric index-space (object)
  (:method ((object index-space))
    object))

(defgeneric broadcast (space-1 space-2))

(defgeneric intersection (space-1 space-2)
  (:method :before ((space-1 index-space) (space-2 index-space))
    (assert (= (dimension space-1) (dimension space-2)))))

(defgeneric difference (space-1 space-2)
  (:method :before ((space-1 index-space) (space-2 index-space))
    (assert (= (dimension space-1) (dimension space-2)))))

(defgeneric subspace? (space-1 space-2)
  (:method ((space-1 t) (space-2 t))
    (equal? space-1 (intersection space-1 space-2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  miscellaneous Petalisp functions

(defgeneric name (object))

(defgeneric dimension (object))

(defgeneric size (object))

(defgeneric equal? (object-1 object-2)
  (:documentation "Two objects are EQUAL? if their use in Petalisp will
  always result in identical behavior."))

(defgeneric result-type (function &rest arguments))

(defgeneric compute (&rest objects))

(defgeneric evaluate (node))

(defgeneric binary-sum (object-1 object-2)
  (:method ((a number) (b number))
    (+ a b)))

(defgeneric binary-product (object-1 object-2)
  (:method ((a number) (b number))
    (* a b)))

(declaim (inline sum product))

(defun sum (object &rest more-objects)
  "Returns the sum of the given objects, as computed by BINARY-SUM."
  (if (null more-objects)
      object
      (reduce #'binary-sum more-objects :initial-value object)))

(defun product (object &rest more-objects)
  "Returns the product of the given objects, as computed by BINARY-PRODUCT."
  (if (null more-objects)
      object
      (reduce #'binary-product more-objects :initial-value object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  classes and methods concerning transformations

(defgeneric classify-transformation (function input-constraints nargout))

(defgeneric compose (g f)
  (:method ((g function) (f function))
    (alexandria:compose g f))
  (:method :before ((g transformation) (f transformation))
    (assert (= (input-dimension g) (output-dimension f)))))

(defgeneric invert (transformation))

(defgeneric input-dimension (transformation))

(defgeneric output-dimension (transformation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  input and output

(defgeneric petalispify (object)
  (:method ((object data-structure))
    object))


(defgeneric depetalispify (object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  default behavior

(defun predecessor (node)
  (ematch (predecessors node)
    ((list first) first)))

(defun subdivision (object &rest more-objects)
  "Return a list of disjoint objects. Each resulting object is a proper
subspace of one or more of the arguments and their fusion covers all
arguments."
  (flet ((shatter (dust object)
           (let ((object-w/o-dust (list object)))
             (nconc
              (loop for particle in dust do
                (setf object-w/o-dust
                      (loop for x in object-w/o-dust
                            append (difference x particle)))
                    append (difference particle object)
                    when (intersection particle object) collect it)
              object-w/o-dust))))
    (reduce #'shatter more-objects :initial-value (list object))))
