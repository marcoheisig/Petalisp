;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

;;; The constant vector +TYPES+ is the fundamental building block of this
;;; library.  It contains one entry for each performance-relevant Common
;;; Lisp type specifier.  The specifiers are sorted in most-specific-first
;;; order, so that a linear search with subtypep automatically yields the
;;; most specific entry.
(alexandria:define-constant +types+
    ;; We remove duplicate types here, mostly to get rid of superfluous
    ;; floating-point and complex types.
    (remove-duplicates
     #(nil
       character
       function
       short-float
       single-float
       long-float
       double-float
       float
       bit
       (unsigned-byte 2)
       (unsigned-byte 4)
       (unsigned-byte 8)
       (unsigned-byte 16)
       (unsigned-byte 32)
       (unsigned-byte 64)
       (signed-byte 8)
       (signed-byte 16)
       (signed-byte 32)
       (signed-byte 64)
       integer
       rational
       real
       (complex short-float)
       (complex single-float)
       (complex long-float)
       (complex double-float)
       complex
       number
       t)
     :test #'alexandria:type=)
  :test #'equalp)

(defconstant type-code-limit (length +types+))

(deftype type-code ()
  `(integer 0 (#. type-code-limit)))

(defun type-code-from-type-specifier (type-specifier)
  (the type-code (position type-specifier +types+ :test #'subtypep)))

(define-compiler-macro type-code-from-type-specifier (&whole form type-specifier)
  (if (constantp type-specifier)
      (locally (declare (notinline type-code-from-type-specifier))
        (type-code-from-type-specifier (eval type-specifier)))
      form))

(declaim (inline type-specifier-from-type-code))
(defun type-specifier-from-type-code (type-code)
  (declare (type-code type-code))
  (svref +types+ type-code))

(defun type-code-of (object)
  (macrolet ((type-code-of-dispatcher ()
               `(typecase object
                  ,@(loop for type across +types+
                          for type-code from 0
                          collect `(,type ,type-code)))))
    (type-code-of-dispatcher)))

(define-compiler-macro type-code-of (&whole form object)
  (if (constantp object)
      (locally (declare (notinline type-code-of))
        (type-code-of (eval object)))
      form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type Code Caching

(deftype type-cache (rank)
  `(simple-array type-code ,(loop repeat rank collect type-code-limit)))

(defun make-type-code-cache (rank fn)
  (let ((cache (make-array (loop repeat rank collect type-code-limit)
                           :element-type 'type-code)))
    (labels ((rec (n type-codes)
               (if (= n 0)
                   (setf (apply #'aref cache type-codes)
                         (apply fn type-codes))
                   (loop for type-code below type-code-limit do
                           (rec (1- n) (cons type-code type-codes))))))
      (rec rank '()))
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

(defun type-code-union (type-code-1 type-code-2)
  (with-type-code-caching (type-code-1 type-code-2)
    (type-code-from-type-specifier
     `(or ,(type-specifier-from-type-code type-code-1)
          ,(type-specifier-from-type-code type-code-2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type Code Reasoning

(defun subtypep-mask (type-specifier &optional env)
  (loop for type across +types+
        for bit = (ash 1 type-code-limit) then (ash bit -1)
        sum (if (subtypep type type-specifier env) bit 0)))

(defmacro type-code-subtypecase (type-code &body clauses &environment env)
  (alexandria:with-gensyms (type-mask)
    `(let ((,type-mask (ash 1 (- type-code-limit ,type-code))))
       (cond
         ,@(loop for (type-specifier . body) in clauses
                 collect `((plusp (logand ,type-mask ,(subtypep-mask type-specifier env)))
                           ,@body))))))
