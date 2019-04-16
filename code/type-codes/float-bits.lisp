;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

;;; A Common Lisp implementation may provide up to four distinct floating
;;; point types - short-float, single-float, double-float and long-float.
;;; For our type code technique, we need to figure out how many bits an
;;; implementation uses to represent each of these types.

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

(defparameter *float-types*
  (list (list 'short-float +short-float-bits+)
        (list 'single-float +single-float-bits+)
        (list 'double-float +double-float-bits+)
        (list 'long-float +long-float-bits+)))

(defun float-type-specifier-from-bits (bits)
  (or (first (find bits *float-types* :key #'second)) t))
