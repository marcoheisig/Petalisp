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
   #:type-specifier

   ;; Ntype manipulation.
   #:ntype
   #:type-specifier
   #:ntype-of
   #:generalize-ntype
   #:with-ntype-caching
   #:ntype-subtypecase
   #:array-element-ntype
   #:empty-ntype-p
   #:universal-ntype-p
   #:eql-ntype-p
   #:ntype-union
   #:ntype-subtypep
   #:ntype-subtypepc1
   #:coerce-to-ntype
   #:list-ntypes

   ;; Type inference.
   #:abort-specialization
   #:give-up-specialization
   #:check-ntype
   #:define-rule
   #:rewrite-as
   #:rewrite-default
   #:wrapper-ntype
   #:wrap-constant
   #:wrap-function
   #:define-instruction
   #:define-simple-instruction
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
