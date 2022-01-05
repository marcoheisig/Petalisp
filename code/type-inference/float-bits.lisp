;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

;;; A Common Lisp implementation may provide up to four distinct floating
;;; point types - short-float, single-float, double-float and long-float.
;;; This code figures out how many bits an implementation uses to represent
;;; each of these types.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun float-exponent-bits (float)
    (1+ (integer-length (1- (nth-value 1 (decode-float float))))))

  (defun float-bits (float)
    (+ (float-digits float)
       (float-exponent-bits float))))

(defconstant +short-float-bits+ (float-bits most-positive-short-float))

(defconstant +single-float-bits+ (float-bits most-positive-single-float))

(defconstant +double-float-bits+ (float-bits most-positive-double-float))

(defconstant +long-float-bits+ (float-bits most-positive-long-float))
