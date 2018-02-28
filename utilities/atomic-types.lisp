;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/utilities/atomic-types
  (:use :closer-common-lisp :alexandria :trivia)
  (:export
   #:atomic-type
   #:complex-short-float
   #:complex-single-float
   #:complex-double-float
   #:complex-long-float
   . #.
   (loop for type in (append (loop for i from 1 to 64 collect `(signed-byte ,i))
                             (loop for i from 1 to 64 collect `(unsigned-byte ,i)))
         collect (make-symbol (format nil "~{~W~^-~}" type)))))

(in-package :petalisp/utilities/atomic-types)

;;; For some applications, it may be desirable to use atomic type
;;; specifiers only. This library introduces atomic aliases for all
;;; upgraded array element types, and a function ATOMIC-TYPE to convert
;;; type specifiers to their atomic equivalent.

(defmacro define-everything ()
  (let* ((compound-types
           (append (loop for i from 1 to 64 collect `(signed-byte ,i))
                   (loop for i from 1 to 64 collect `(unsigned-byte ,i))
                   (loop for ftype in '(short-float single-float double-float long-float)
                         collect `(complex ,ftype))))
         (alist
           (loop for compound-type in compound-types
                 for atomic-type = (format-symbol *package* "~{~W~^-~}" compound-type)
                 collect (cons compound-type atomic-type)))
         (signed-atomics
           (loop for (compound-type . atomic-type) in alist
                 when (eq (car compound-type) 'signed-byte)
                   collect atomic-type))
         (unsigned-atomics
           (loop for (compound-type . atomic-type) in alist
                 when (eq (car compound-type) 'unsigned-byte)
                   collect atomic-type)))
    `(progn
       (defun atomic-type (type)
         "Return an atomic type specifier that is a supertype of TYPE."
         (match type
           ((type symbol) type)
           ((list 'signed-byte n) (aref #(,@signed-atomics) (1- n)))
           ((list 'unsigned-byte n) (aref #(,@unsigned-atomics) (1- n)))
           ((list 'complex 'short-float) 'complex-short-float)
           ((list 'complex 'single-float) 'complex-single-float)
           ((list 'complex 'double-float) 'complex-double-float)
           ((list 'complex 'long-float) 'complex-long-float)
           (otherwise 't)))
       ;; define all the atomic types
       ,@(loop for (compound-type . atomic-type) in alist
               collect `(deftype ,atomic-type () ',compound-type)))))

(define-everything)
