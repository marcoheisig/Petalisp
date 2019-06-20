;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(deftype function-name ()
  '(or symbol
    (cons (eql setf) (cons symbol nil))))

(deftype function-designator ()
  '(or symbol function))

(deftype character-designator ()
  '(or (vector character 1) character))

(deftype string-designator ()
  '(or character symbol string))

(deftype package-designator ()
  '(or package string-designator))

(deftype radix ()
  '(integer 2 36))

(deftype character-code ()
  '(integer 0 (#.char-code-limit)))

(deftype arity ()
  '(integer 0 (#.call-arguments-limit)))

;; The representation of byte specifiers is implementation-dependent.
;; However, under the not-so-bold assumption that each implementation
;; consistently uses a uniform representation, we can get surprisingly far.
(deftype byte-specifier ()
  `(or ,(type-of (byte 0 0))
       ,(type-of (byte 16 253))))

(deftype complex-short-float ()
  '(complex short-float))

(deftype complex-single-float ()
  '(complex single-float))

(deftype complex-double-float ()
  '(complex double-float))

(deftype complex-long-float ()
  '(complex long-float))

(deftype generalized-boolean ()
  't)
