;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric coerce-to-lazy-array (array))

(defgeneric total-size (lazy-array))

(defgeneric element-type (lazy-array))

(defgeneric shape (lazy-array))

(defgeneric inputs (lazy-array))

(defgeneric refcount (lazy-array))

(defgeneric (setf refcount) (value lazy-array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass lazy-array ()
  ((%element-type :initarg :element-type :reader element-type)
   (%shape :initarg :shape :reader shape :reader shape)
   (%refcount :initform 0 :accessor refcount))
  (:default-initargs :element-type t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(petalisp.utilities:define-class-predicate lazy-array :hyphenate t)

(defmethod coerce-to-lazy-array ((lazy-array lazy-array))
  lazy-array)

(defmethod total-size ((lazy-array lazy-array))
  (set-size (shape lazy-array)))

(defmethod total-size ((finite-set finite-set))
  (set-size finite-set))

(defmethod initialize-instance :after ((lazy-array lazy-array)
                                       &key &allow-other-keys)
  (mapc (lambda (input) (incf (refcount input)))
        (inputs lazy-array)))

(defmethod shape ((shape shape))
  shape)

(defmethod rank ((lazy-array lazy-array))
  (rank (shape lazy-array)))

(defun input (object)
  (destructuring-bind (input) (inputs object) input))

(defmethod print-object ((lazy-array lazy-array) stream)
  (print-unreadable-object (lazy-array stream :type t)
    (format stream "~S ~S" (element-type lazy-array) (shape lazy-array))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lisp Objects as Strided Arrays

(defmethod total-size ((object t))
  1)

(defmethod rank ((object t))
  0)

(defmethod element-type ((object t))
  (type-of object))

(defmethod shape ((object t))
  (load-time-value (make-shape)))

(defmethod inputs ((object t))
  '())

(defmethod total-size ((array array))
  (array-total-size array))

(defmethod rank ((array array))
  (array-rank array))

(defmethod element-type ((array array))
  (array-element-type array))

(defmethod shape ((array array))
  (apply #'make-shape
         (loop for axis below (array-rank array)
               collect
               (range 0 1 (1- (array-dimension array axis))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pattern Matching

(trivia:defpattern lazy-array (shape)
  (alexandria:with-gensyms (it)
    `(trivia:guard1 ,it (lazy-array-p ,it)
                    (shape ,it) ,shape)))
