;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Petalisp Core

(in-package :petalisp)

(defclass petalisp-object ()
  ((%index-space :initarg :index-space :reader index-space)
   (%element-type :initarg :element-type :reader element-type))
  :documentation
  "The superclass of all objects handled by Petalisp.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; input - a protocol to generate Petalisp objects

(defclass input (petalisp-object) ()
  :documentation
  "The superclass of all Petalisp leaf nodes, that is Petalisp objects that
  are not the result of applying an elementary Petalisp function to other
  Petalisp objects.")

(defclass constant-input (input)
  ((%value :initarg :value :reader value))
  :documentation
  "A Petalisp object that returns the same value for all indices.")

(defclass index-input (input) ()
  :documentation
  "A Petalisp object that where each value equals its index.")

(defclass lisp-input (input)
  ((%lisp-object :initarg :lisp-object :reader lisp-object))
  :documentation
  "A Petalisp object that is constructed from a given lisp constant. A lisp
  array will be used to build a Petalisp object with the same dimensions
  and elements. All other objects are transformed to a Petalisp object
  that contains exactly this one element.")

(defun petalispify-type (type)
  (upgraded-array-element-type type))

(defun from-lisp (lisp-object)
  (multiple-value-bind (index-space element-type)
      (cond
        ((arrayp lisp-object)
         (values
          (mapcar (lambda (dim) (range 1 1 dim))
                  (array-dimensions lisp-object))
          (petalispify-type
           (array-element-type lisp-object))))
        (t
         (values
          (x 1)
          (petalispify-type (type-of lisp-object)))))
    (make-instance
     'lisp-input
     :lisp-object lisp-object
     :index-space index-space
     :element-type element-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; α - distributed function application

(defun α (function &rest arguments)
  ;; treat arguments that are not of type PETALISP-OBJECT as LISP-INPUT
  (let ((arguments
          (mapcar
           (lambda (arg)
             (cond ((typep arg 'petalisp-object) arg)
                   (t (from-lisp arg))))))))
  ;; check whether dimensions are compatible
  ;; TODO
  (list* function arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; β - reduction of the trailing dimension

(defun β (function object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; select - take a subset of an array

(defun %select () nil)

(defmacro select)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; permute - permute an array's indices

(defun permute (&rest dimensions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; reshape - modify the numbering of array elements

