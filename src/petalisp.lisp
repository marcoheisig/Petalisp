;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Petalisp operations

(in-package :petalisp)

(defgeneric constant (shape value))

(defgeneric unary-operation (operator object))

(defgeneric binary-operation (operator object-1 object-2))

(defgeneric reduction (function dimension object))

(defgeneric selection (shape object))

(defgeneric fusion (&rest objects))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Petalisp objects

(defclass index-space ()
  ((%shape :initarg :shape :reader shape)))

(defclass petalisp-object (graph-node)
  ((%shape :initarg :shape :reader shape)
   (%type :initarg :type :reader type)))

(defclass constant (petalisp-object)
  ((%value :initarg :value :reader value)))

(defclass reduction (petalisp-object)
  ((%value :initarg :value :reader value)))

(defclass selection (petalisp-object)
  ((%value :initarg :value :reader value)))

(defclass fusion (petalisp-object) ())

(defclass unary-operation ()
  ((%arg :initarg :arg :reader arg)))

(defclass binary-operation ()
  ((%arg1 :initarg :arg1 :reader arg1)
   (%arg2 :initarg :arg2 :reader arg2)))

(defclass + (binary-operation) ())
(defclass - (binary-operation) ())
(defclass * (binary-operation) ())
(defclass / (binary-operation) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The petalisp implementation

(defmethod constant (shape value)
  (make-instance 'constant :shape shape
                           :value value
                           :type (type-of value)))

(defmethod unary-operation ((operator symbol) object)
  (apply #'make-instance
         (find-symbol (symbol-name operator) :petalisp)
         :arg object))

(defmethod binary-operation ((operator symbol) object-1 object-2)
  (apply #'make-instance
         (find-symbol (symbol-name operator) :petalisp)
         :arg1 object-1 :arg2 object-2))

(defmethod reduction ((function binary-operation) dimension object)
  nil)

(defmethod selection (shape object)
  nil)

(defmethod fusion (&rest objects)
  nil)
