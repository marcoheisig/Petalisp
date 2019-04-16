;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conversion of Type Codes to Type Specifiers

(define-type-code-cache *type-specifier-from-type-code-cache* (type-code)
  (uncached-type-specifier-from-type-code type-code))

(defun type-specifier-from-type-code (type-code)
  (access-type-code-cache *type-specifier-from-type-code-cache* type-code))

(define-compiler-macro type-specifier-from-type-code (&whole form type-code)
  (cond ((integerp type-code)
         (locally (declare (notinline type-specifier-from-type-code))
           (type-specifier-from-type-code type-code)))
        ((constantp type-code)
         `(load-time-value
           (locally (declare (notinline type-specifier-from-type-code))
             (type-specifier-from-type-code ,type-code))))
        (t form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Object Type Codes

(defun type-code-of (object)
  (cond ((integerp object)
         (cond ((plusp object)
                (make-type-code :bits (integer-length object) :signedp nil))
               ((zerop object)
                (make-type-code :bits 1 :signedp nil))
               ((minusp object)
                (make-type-code :bits (1+ (integer-length object)) :signedp t))))
        ((floatp object)
         (etypecase object
           (short-float
            (make-type-code :bits +short-float-bits+ :floatp t))
           (single-float
            (make-type-code :bits +single-float-bits+ :floatp t))
           (double-float
            (make-type-code :bits +double-float-bits+ :floatp t))
           (long-float
            (make-type-code :bits +long-float-bits+ :floatp t))))
        ((complexp object)
         (etypecase object
           ((complex short-float)
            (make-type-code :bits (* 2 +short-float-bits+) :floatp t :complexp t))
           ((complex single-float)
            (make-type-code :bits (* 2 +single-float-bits+) :floatp t :complexp t))
           ((complex double-float)
            (make-type-code :bits (* 2 +double-float-bits+) :floatp t :complexp t))
           ((complex long-float)
            (make-type-code :bits (* 2 +long-float-bits+) :floatp t :complexp t))))
        (t (make-type-code))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conversion of Type Specifiers to Type Codes

(defun type-code-from-type-specifier (type-specifier)
  (if (consp type-specifier)
      (case (car type-specifier)
        (and
         (cond ((null (cdr type-specifier))
                (make-type-code))
               ((null (cdr (cdr type-specifier)))
                (type-code-from-type-specifier (cadr type-specifier)))
               (t
                (reduce #'type-code-intersection (cdr type-specifier)
                        :key #'type-code-from-type-specifier))))
        (or
         (if (null (cdr type-specifier))
             (make-type-code)
             (reduce #'type-code-union (cdr type-specifier)
                     :key #'type-code-from-type-specifier)))
        (eql
         (type-code-of (cadr type-specifier)))
        (member
         (if (null (cdr type-specifier))
             (make-type-code)
             (reduce #'type-code-union (cdr type-specifier)
                     :key #'type-code-of)))
        (mod
         (let ((n (cadr type-specifier)))
           (type-code-of (1- n))))
        (signed-byte
         (let ((bits (cadr type-specifier)))
           (if (integerp bits)
               (make-type-code :bits bits :signedp t)
               (make-type-code))))
        (unsigned-byte
         (let ((bits (cadr type-specifier)))
           (if (integerp bits)
               (make-type-code :bits bits :signedp nil)
               (make-type-code))))
        (integer
         (flet ((canonicalize-limit (limit fn)
                  (cond ((and (consp limit)
                              (integerp (car limit)))
                         (funcall fn (car limit)))
                        ((integerp limit)
                         limit)
                        (t nil))))
           (let ((lower-limit (canonicalize-limit (second type-specifier) #'1+))
                 (upper-limit (canonicalize-limit (third type-specifier) #'1-)))
             (if (and (integerp lower-limit)
                      (integerp upper-limit))
                 (type-code-union
                  (type-code-of lower-limit)
                  (type-code-of upper-limit))
                 (make-type-code)))))
        (complex
         (let ((part-code (type-code-from-type-specifier (cadr type-specifier))))
           (+ (logior part-code #b0000001) #b0001000)))
        (short-float (make-type-code :bits +short-float-bits+ :floatp t))
        (single-float (make-type-code :bits +single-float-bits+ :floatp t))
        (double-float (make-type-code :bits +double-float-bits+ :floatp t))
        (long-float (make-type-code :bits +long-float-bits+ :floatp t))
        (otherwise (make-type-code)))
      (case type-specifier
        (bit (make-type-code :bits 1))
        (fixnum (load-time-value (type-code-of most-negative-fixnum)))
        (short-float (make-type-code :bits +short-float-bits+ :floatp t))
        (single-float (make-type-code :bits +single-float-bits+ :floatp t))
        (double-float (make-type-code :bits +double-float-bits+ :floatp t))
        (long-float (make-type-code :bits +long-float-bits+ :floatp t))
        (otherwise (make-type-code)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type Codes as Sets

(defun type-code-complement (type-code)
  (declare (ignore type-code))
  (make-type-code))

(defun canonicalize-type-code (type-code)
  (type-code-from-type-code-id (type-code-id type-code)))

(defun uncached-type-code-intersection (type-code-1 type-code-2)
  (let ((type-code-1 (canonicalize-type-code type-code-1))
        (type-code-2 (canonicalize-type-code type-code-2)))
    (cond ((zerop type-code-1)
           type-code-2)
          ((zerop type-code-2)
           type-code-1)
          ((= type-code-1 type-code-2)
           type-code-1)
          ((or (type-code-complexp type-code-1)
               (type-code-complexp type-code-2)
               (type-code-floatp type-code-1)
               (type-code-floatp type-code-2))
           (make-type-code))
          ((and (type-code-signedp type-code-1)
                (type-code-signedp type-code-2))
           (let ((bits (min (type-code-bits type-code-1)
                            (type-code-bits type-code-2))))
             (make-type-code :signedp t :bits bits)))
          ((and (type-code-unsignedp type-code-1)
                (type-code-unsignedp type-code-2))
           (let ((bits (min (type-code-bits type-code-1)
                            (type-code-bits type-code-2))))
             (make-type-code :signedp nil :bits bits)))
          ((and (type-code-signedp type-code-1)
                (type-code-unsignedp type-code-2))
           (let ((bits (min (1- (type-code-bits type-code-1))
                            (type-code-bits type-code-2))))
             (make-type-code :signedp nil :bits bits)))
          ((and (type-code-unsignedp type-code-1)
                (type-code-signedp type-code-2))
           (let ((bits (min (type-code-bits type-code-1)
                            (1- (type-code-bits type-code-2)))))
             (make-type-code :signedp nil :bits bits))))))

(define-type-code-cache *type-code-intersection-cache* (type-code-1 type-code-2)
  (uncached-type-code-intersection type-code-1 type-code-2))

(defun type-code-intersection (type-code-1 type-code-2)
  (access-type-code-cache *type-code-intersection-cache* type-code-1 type-code-2))

(defun uncached-type-code-union (type-code-1 type-code-2)
  (let ((type-code-1 (canonicalize-type-code type-code-1))
        (type-code-2 (canonicalize-type-code type-code-2)))
    (cond ((or (zerop type-code-1)
               (zerop type-code-2))
           (make-type-code))
          ((= type-code-1 type-code-2)
           type-code-1)
          ((or (type-code-complexp type-code-1)
               (type-code-complexp type-code-2)
               (type-code-floatp type-code-1)
               (type-code-floatp type-code-2))
           (make-type-code))
          ((and (type-code-signedp type-code-1)
                (type-code-signedp type-code-2))
           (let ((bits (max (type-code-bits type-code-1)
                            (type-code-bits type-code-2))))
             (make-type-code :signedp t :bits bits)))
          ((and (not (type-code-signedp type-code-1))
                (not (type-code-signedp type-code-2)))
           (let ((bits (max (type-code-bits type-code-1)
                            (type-code-bits type-code-2))))
             (make-type-code :signedp nil :bits bits)))
          ((and (type-code-signedp type-code-1)
                (not (type-code-signedp type-code-2)))
           (let ((bits (max (type-code-bits type-code-1)
                            (1+ (type-code-bits type-code-2)))))
             (make-type-code :signedp t :bits bits)))
          ((and (not (type-code-signedp type-code-1))
                (type-code-signedp type-code-2))
           (let ((bits (max (1+ (type-code-bits type-code-1))
                            (type-code-bits type-code-2))))
             (make-type-code :signedp t :bits bits))))))

(define-type-code-cache *type-code-union-cache* (type-code-1 type-code-2)
  (uncached-type-code-union type-code-1 type-code-2))

(defun type-code-union (type-code-1 type-code-2)
  (access-type-code-cache *type-code-union-cache* type-code-1 type-code-2))
