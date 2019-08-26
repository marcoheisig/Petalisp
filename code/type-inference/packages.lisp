;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.type-inference
  (:use #:common-lisp)
  (:export
   ;; Utilities.
   #:function-arity

   ;; Auxiliary types.
   #:function-name
   #:function-designator
   #:character-designator
   #:string-designator
   #:package-designator
   #:radix
   #:character-code
   #:arity
   #:byte-specifier
   #:complex-short-float
   #:complex-single-float
   #:complex-double-float
   #:complex-long-float
   #:generalized-boolean
   #:multiple-value-count

   ;; Ntype manipulation.
   #:ntype
   #:ntypep
   #:type-specifier
   #:ntype-of
   #:array-element-ntype
   #:empty-ntype-p
   #:universal-ntype-p
   #:with-ntype-caching
   #:ntype-union
   #:ntype-subtypecase

   ;; Type inference.
   #:define-external-rewrite-rule
   #:define-rewrite-rules
   #:abort-specialization
   #:give-up-specialization
   #:defop
   #:specialize
   #:infer-ntypes

   ;; Specialized functions.
   #:and-fn
   #:or-fn
   #:prog2-fn
   ;; The remaining specialized functions have been gathered automatically
   ;; by the function at the end of this file.
   ))

#+(or)
(defun print-specialized-functions ()
  (let* ((package (find-package '#:petalisp.type-inference))
         (names
           (remove-duplicates
            (loop for sym being the symbols of package
                  for name = (string sym)
                  when (eq (symbol-package sym) package)
                    when (find #\. name)
                      unless (search "EXTERNAL-REWRITE-RULE" name)
                        collect name)
            :test #'string=)))
    (dolist (name (sort names #'string<=))
      (format t "~&#:~(~A~)~%" name))))
