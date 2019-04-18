;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

(alexandria:define-constant +types+
    (sort
     (remove-duplicates
      (map 'vector #'upgraded-array-element-type
           `(nil
             t
             character
             short-float
             single-float
             double-float
             long-float
             (complex short-float)
             (complex single-float)
             (complex double-float)
             (complex long-float)
             ,@(loop for bits in '(1 2 4 8 16 32 64)
                     collect `(signed-byte ,bits)
                     collect `(unsigned-byte ,bits))))
      :test #'equal)
     #'subtypep)
  :test #'equalp)

(defconstant type-code-limit (length +types+))

(deftype type-code ()
  `(integer 0 (,(length +types+))))

(deftype type-cache (n)
  `(simple-array t ,(loop repeat n collect type-code-limit)))

(macrolet ((type-code (type)
             `(or (position (upgraded-array-element-type ',type)
                            +types+
                            :test #'alexandria:type=)
                  (error "Not a relevant array element type: ~S" ',type))))
  (defconstant +empty-type-code+ (type-code nil))
  (defconstant +universal-type-code+ (type-code t))
  (defconstant +character-type-code+ (type-code character))
  (defconstant +short-float-type-code+ (type-code short-float))
  (defconstant +single-float-type-code+ (type-code single-float))
  (defconstant +double-float-type-code+ (type-code double-float))
  (defconstant +long-float-type-code+ (type-code long-float))
  (defconstant +complex-short-float-type-code+ (type-code (complex short-float)))
  (defconstant +complex-single-float-type-code+ (type-code (complex single-float)))
  (defconstant +complex-double-float-type-code+ (type-code (complex double-float)))
  (defconstant +complex-long-float-type-code+ (type-code (complex long-float)))
  (defconstant +signed-byte-1-type-code+ (type-code (signed-byte 1)))
  (defconstant +signed-byte-2-type-code+ (type-code (signed-byte 2)))
  (defconstant +signed-byte-4-type-code+ (type-code (signed-byte 4)))
  (defconstant +signed-byte-8-type-code+ (type-code (signed-byte 8)))
  (defconstant +signed-byte-16-type-code+ (type-code (signed-byte 16)))
  (defconstant +signed-byte-32-type-code+ (type-code (signed-byte 32)))
  (defconstant +signed-byte-64-type-code+ (type-code (signed-byte 64)))
  (defconstant +unsigned-byte-1-type-code+ (type-code (unsigned-byte 1)))
  (defconstant +unsigned-byte-2-type-code+ (type-code (unsigned-byte 2)))
  (defconstant +unsigned-byte-4-type-code+ (type-code (unsigned-byte 4)))
  (defconstant +unsigned-byte-8-type-code+ (type-code (unsigned-byte 8)))
  (defconstant +unsigned-byte-16-type-code+ (type-code (unsigned-byte 16)))
  (defconstant +unsigned-byte-32-type-code+ (type-code (unsigned-byte 32)))
  (defconstant +unsigned-byte-64-type-code+ (type-code (unsigned-byte 64))))

(declaim (inline type-specifier-from-type-code))
(defun type-specifier-from-type-code (type-code)
  (aref +types+ type-code))

(defun signed-integer-type (bits)
  (declare (type (integer 1 *) bits))
  (if (<= bits 64)
      (case (integer-length (1- bits))
        (0 +signed-byte-1-type-code+)
        (1 +signed-byte-2-type-code+)
        (2 +signed-byte-4-type-code+)
        (3 +signed-byte-8-type-code+)
        (4 +signed-byte-16-type-code+)
        (5 +signed-byte-32-type-code+)
        (6 +signed-byte-64-type-code+))
      +universal-type-code+))

(defun unsigned-integer-type (bits)
  (declare (type (integer 1 *) bits))
  (if (<= bits 64)
      (case (integer-length (1- bits))
        (0 +unsigned-byte-1-type-code+)
        (1 +unsigned-byte-2-type-code+)
        (2 +unsigned-byte-4-type-code+)
        (3 +unsigned-byte-8-type-code+)
        (4 +unsigned-byte-16-type-code+)
        (5 +unsigned-byte-32-type-code+)
        (6 +unsigned-byte-64-type-code+))
      +universal-type-code+))

(defun type-code-of (object)
  (cond ((floatp object)
         (typecase object
           (short-float +short-float-type-code+)
           (single-float +single-float-type-code+)
           (double-float +double-float-type-code+)
           (long-float +long-float-type-code+)
           (t +universal-type-code+)))
        ((complexp object)
         (typecase object
           ((complex short-float) +complex-short-float-type-code+)
           ((complex single-float) +complex-single-float-type-code+)
           ((complex double-float) +complex-double-float-type-code+)
           ((complex long-float) +complex-long-float-type-code+)
           (t +universal-type-code+)))
        ((integerp object)
         (cond ((minusp object)
                (signed-integer-type (1+ (integer-length (abs object)))))
               ((plusp object)
                (unsigned-integer-type (integer-length object)))
               ((zerop object)
                +unsigned-byte-1-type-code+)))
        ((characterp object) +character-type-code+)
        (t +universal-type-code+)))

(defun make-type-code-cache (dimension fn)
  (let ((cache (make-array (loop repeat dimension collect type-code-limit))))
    (labels ((rec (n type-codes)
               (if (= n 0)
                   (setf (apply #'aref cache type-codes)
                         (apply fn type-codes))
                   (loop for type-code below type-code-limit do
                           (rec (1- n) (cons type-code type-codes))))))
      (rec dimension '()))
    cache))

(defmacro with-type-code-caching (type-codes &body body)
  (assert (every #'symbolp type-codes))
  (assert (null (intersection type-codes lambda-list-keywords)))
  (let ((n (length type-codes))
        (cache (gensym "CACHE")))
    `(let ((,cache (load-time-value
                    (make-type-code-cache ,n (lambda ,type-codes ,@body)))))
       (declare (type-code ,@type-codes)
                (type (type-cache ,n) ,cache)
                (optimize (speed 3) (safety 0)))
       (aref ,cache ,@type-codes))))

(defun type-code-from-type-specifier (type-specifier)
  (position type-specifier +types+ :test #'subtypep))

(defun type-code-union (type-code-1 type-code-2)
  (with-type-code-caching (type-code-1 type-code-2)
    (type-code-from-type-specifier
     `(or ,(type-specifier-from-type-code type-code-1)
          ,(type-specifier-from-type-code type-code-2)))))

(defun type-code-intersection (type-code-1 type-code-2)
  (with-type-code-caching (type-code-1 type-code-2)
    (type-code-from-type-specifier
     `(and ,(type-specifier-from-type-code type-code-1)
           ,(type-specifier-from-type-code type-code-2)))))
