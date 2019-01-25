;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defclass empty-set (finite-set)
  ())

(defun empty-set ()
  (load-time-value
   (make-instance 'empty-set)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on Empty Sets

(defmethod set-for-each ((function function) (set empty-set))
  (values))

(defmethod set-elements ((set empty-set))
  '())

(defmethod set-size ((set empty-set))
  0)

(defmethod set-emptyp ((set empty-set))
  t)

(defmethod set-contains ((set empty-set) (object t))
  nil)

(defmethod set-difference ((any-set any-set) (empty-set empty-set))
  (declare (ignore empty-set))
  any-set)

(defmethod set-equal ((set-1 empty-set) (set-2 empty-set))
  (declare (ignore set-1 set-2))
  t)

(define-method-pair set-equal ((empty-set empty-set) (any-set any-set))
  (declare (ignore empty-set any-set))
  nil)

(define-method-pair set-intersection ((empty-set empty-set) (any-set any-set))
  (declare (ignore any-set))
  empty-set)

(define-method-pair set-intersectionp ((empty-set empty-set) (any-set any-set))
  (declare (ignore any-set empty-set))
  nil)

(define-method-pair set-union ((empty-set empty-set) (any-set any-set))
  (declare (ignore empty-set))
  any-set)
