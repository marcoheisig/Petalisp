;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;; This protocol for working with sets is fairly generic, with one
;;; exception.  The set elements are always compared with EQUAL.  The
;;; rationale for this is that we want to avoid the complexity of dealing
;;; with equivalence classes, ordered sets and so on.  The predicate EQUAL
;;; strikes a balance between flexibility and the principle of least
;;; surprise.  It is general enough for a comparison of strings, numbers,
;;; characters, and conses thereof, but, unlike EQUALP, does distinguish
;;; the case of characters and strings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric set-contains (set object))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Miscellaneous Other Methods

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
