;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric set-difference (set-1 set-2))

(defgeneric set-elements (set))

(defgeneric set-emptyp (set))

(defgeneric set-equal (set-1 set-2))

(defgeneric set-intersection (set-1 set-2))

(defgeneric set-intersectionp (set-1 set-2))

(defgeneric set-size (set))

(defgeneric set-union (set-1 set-2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass any-set ()
  ())

(defclass infinite-set (any-set)
  ())

(defclass finite-set (any-set)
  ())

(defclass empty-set (finite-set)
  ())

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Miscellaneous Other Methods

(defmethod set-elements ((set infinite-set))
  (error "Cannot enumerate the elements of an infinite set."))

(defmethod set-emptyp ((set any-set))
  nil)

(defmethod set-equal ((set-1 finite-set) (set-2 infinite-set))
  nil)

(defmethod set-equal ((set-1 infinite-set) (set-2 finite-set))
  nil)

(defmethod set-intersectionp ((set-1 any-set) (set-2 any-set))
  (and (set-intersection set-1 set-2) t))

(defmethod set-size ((set finite-set))
  (length (set-elements set)))

(defmethod set-size ((set infinite-set))
  (error "Cannot determine the size of an infinite set."))

