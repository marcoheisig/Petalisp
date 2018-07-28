;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

(defclass empty-set (finite-set)
  ())

(defun empty-set ()
  (load-time-value
   (make-instance 'empty-set)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on Empty Sets

(defmethod set-difference ((set-1 any-set) (set-2 empty-set))
  set-1)

(defmethod set-elements ((set empty-set))
  '())

(defmethod set-emptyp ((set empty-set))
  t)

(defmethod set-equal ((set-1 empty-set) (set-2 empty-set))
  t)

(defmethod set-intersection ((set-1 any-set) (set-2 empty-set))
  nil)

(defmethod set-intersection ((set-1 empty-set) (set-2 any-set))
  nil)

(defmethod set-intersectionp ((set-1 empty-set) (set-2 any-set))
  nil)

(defmethod set-intersectionp ((set-1 any-set) (set-2 empty-set))
  nil)

(defmethod set-size ((set empty-set))
  0)

(defmethod set-union ((set-1 any-set) (set-2 empty-set))
  set-1)

(defmethod set-union ((set-1 empty-set) (set-2 any-set))
  set-2)
