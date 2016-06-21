;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Petalisp Core

(in-package :petalisp)

(defclass petalisp-object ()
  ((%index-space :initarg :index-space :reader index-space)
   (%element-type :initarg :element-type :reader element-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; α - distributed function application

(defmacro α (function &rest arguments)
  ;; TODO
  (list* function arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; β - reduction of the trailing dimension

(defmacro β)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; input - a protocol to generate Petalisp objects

(defclass input (petalisp-object) ()
  :documentation
  "The superclass of all Petalisp leaf nodes, that is Petalisp objects that
  are not the result of applying an elementary Petalisp function to other
  Petalisp object.")

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

