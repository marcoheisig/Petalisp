;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.utilities
  (:use #:closer-common-lisp #:alexandria)
  (:export
   #:with-memoization
   #:with-multiple-value-memoization
   #:with-hash-table-memoization
   #:with-multiple-value-hash-table-memoization
   #:with-vector-memoization
   #:with-multiple-value-vector-memoization
   #:identical
   #:symmetric-function
   #:define-method-pair
   #:define-class-predicate
   #:extended-euclid
   #:optimizing-constructor))
