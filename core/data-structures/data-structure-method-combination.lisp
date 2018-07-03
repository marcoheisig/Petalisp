;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(uiop:define-package :petalisp/core/data-structures/data-structure-method-combination
  (:use :closer-common-lisp :alexandria :trivia)
  (:export
   #:data-structure-constructor))

(in-package :petalisp/core/data-structures/data-structure-method-combination)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Data Structure Construction Method Combination
;;;
;;; The construction of data structures in Petalisp happens according to
;;; the following protocol:
;;;
;;; 1. Rigorous argument checks are performed (least specific first)
;;;
;;; 2. All applicable optimizations are attempted (most specific
;;;    first). The value of the first non-NIL optimization is returned.
;;;
;;; 3. If and only if no optimization succeeds, the primary method is
;;;    executed to produce a suitable data structure.
;;;
;;; Finally, the variables *CHECK* and *OPTIMIZE* can be used to
;;; dynamically disable these stages if performance demands it.

(defvar *check* t)

(defvar *optimize* t)

(define-method-combination data-structure-constructor ()
  ((check (:check))
   (optimize (:optimize))
   (primary () :required t))
  (flet ((call-methods (methods)
           (loop for method in methods
                 collect `(call-method ,method))))
    `(progn
       (when *check* ,@(call-methods (reverse check)))
       (or (when *optimize* ,@(call-methods optimize))
           (call-method ,(first primary) ,(rest primary))))))
