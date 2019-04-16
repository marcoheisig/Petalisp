;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

;;; Type codes are an efficient encoding for a certain set of Common Lisp
;;; types, namely, all number types for which modern Lisp implementations
;;; have a specialized representation.

(deftype type-code () '(unsigned-byte 7))

;;; Bit 1 stores whether we are dealing with a complex type.

(declaim (inline type-code-complexp))
(defun type-code-complexp (type-code)
  (declare (type-code type-code))
  (not (zerop (logand type-code #b0000001))))

;;; Bit 2 stores whether we are dealing with a floating point type.

(declaim (inline type-code-floatp))
(defun type-code-floatp (type-code)
  (declare (type-code type-code))
  (not (zerop (logand type-code #b0000010))))

;;; Bit 3 stores whether we are dealing with a signed integer type.

(declaim (inline type-code-unsignedp))
(defun type-code-unsignedp (type-code)
  (declare (type-code type-code))
  (zerop (logand type-code #b0000100)))

(declaim (inline type-code-signedp))
(defun type-code-signedp (type-code)
  (declare (type-code type-code))
  (not (type-code-unsignedp type-code)))

;;; Bits 4-7 encode the binary logarithm of the size of the type - with two
;;; exceptions: A value of zero represents the empty type and a value of 15
;;; represents the universal type.

(deftype type-code-bits ()
  '(unsigned-byte 16))

(declaim (inline type-code-bits))
(defun type-code-bits (type-code)
  (declare (type-code type-code))
  (ash 1 (1- (ash type-code -3))))

(defconstant +empty-type-code+ #b0000000)

(declaim (inline empty-type-code-p))
(defun empty-type-code-p (type-code)
  (declare (type-code type-code))
  (= (type-code-bits type-code) 0))

(defconstant +universal-type-code+ #b1111000)

(declaim (inline universal-type-code-p))
(defun universal-type-code-p (type-code)
  (declare (type-code type-code))
  (= (type-code-bits type-code) (expt 2 15)))

;;; The low-level type code constructor.

(declaim (inline make-type-code))
(defun make-type-code (&key (bits 0) (complexp nil) (floatp nil) (signedp nil))
  (declare (type-code-bits bits))
  (cond ((= bits 0) +empty-type-code+)
        ((= bits (expt 2 15)) +universal-type-code+)
        ((plusp bits)
         (let ((n (ash (1+ (integer-length (1- bits))) 3))
               (complex-mask (if complexp #b0000001 #b0000000))
               (float-mask (if floatp #b0000010 #b0000000))
               (signed-mask (if signedp #b0000100 #b0000000)))
           (logior n complex-mask float-mask signed-mask)))))

;;; The conversion from type codes to type specifiers is straightforward.

(defun uncached-type-specifier-from-type-code (type-code)
  ;; Our heuristic is that we assume an implementation has special support
  ;; for a particular type if and only if this type is an upgraded array
  ;; element type.
  (upgraded-array-element-type
   (cond ((empty-type-code-p type-code) nil)
         ((universal-type-code-p type-code) t)
         ;; Complex types.
         ((type-code-complexp type-code)
          (let ((component-type
                  (uncached-type-specifier-from-type-code
                   ;; We clear the complex bit and cut the number
                   ;; of bits by half.
                   (- (logand type-code #b1111110) #b0001000))))
            (if (eq component-type t)
                t
                `(complex ,component-type))))
         ;; Float types.
         ((type-code-floatp type-code)
          (float-type-specifier-from-bits (type-code-bits type-code)))
         ;; Signed Integer types.
         ((type-code-signedp type-code)
          `(signed-byte ,(type-code-bits type-code)))
         ((type-code-unsignedp type-code)
          `(unsigned-byte ,(type-code-bits type-code)))
         (t (error "Not a valid type code: ~D" type-code)))))

;;; Every integer from 0 to 127 is a valid type code.

(defun all-type-codes ()
  (load-time-value
   (loop for i below 128 collect i)))

(defun all-type-code-type-specifiers ()
  (load-time-value
   (remove-duplicates
    (map 'list #'uncached-type-specifier-from-type-code (all-type-codes))
    :test #'equal)))
