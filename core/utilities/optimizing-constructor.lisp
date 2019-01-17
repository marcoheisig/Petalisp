;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package :petalisp-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Optimizing Constructor Method Combination
;;;
;;; A method combination for creating an object from a set of arguments,
;;; according to the following protocol:
;;;
;;; 1. Rigorous argument checks are performed (least specific first)
;;;
;;; 2. All applicable optimizations are attempted (most specific
;;;    first). The value of the first non-NIL optimization is returned.
;;;
;;; 3. If and only if no optimization succeeds, the primary method is
;;;    executed to produce a suitable instance.

(define-method-combination optimizing-constructor ()
  ((check (:check))
   (optimize (:optimize))
   (primary () :required t))
  (flet ((call (method)
           `(call-method ,method)))
    `(progn
       ,@(mapcar #'call (reverse check))
       (or ,@(mapcar #'call optimize)
           (call-method ,(first primary) ,(rest primary))))))
