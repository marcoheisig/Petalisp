;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

(define-predicate-type-inference-rule symbolp symbol)

(define-predicate-type-inference-rule keywordp keyword)

(define-type-inference-rule make-symbol (name)
  (check-type-code name string)
  (type-code-from-type-specifier 'symbol))

(define-type-inference-rule copy-symbol
    (symbol &optional (copy-properties (type-code-from-type-specifier 'null)))
  (check-type-code symbol symbol)
  (type-code-from-type-specifier 'symbol))

(define-type-inference-rule gensym
    (&optional (x (type-code-from-type-specifier 'string)))
  (check-type-code x (or integer string))
  (type-code-from-type-specifier 'symbol))

(define-type-inference-rule gentemp
    (&optional (prefix (type-code-from-type-specifier 'string))
               (package (type-code-from-type-specifier 'package)))
  (check-type-code prefix string)
  (check-type-code package package-designator)
  (type-code-from-type-specifier 'symbol))

(define-type-inference-rule symbol-name (symbol)
  (check-type-code symbol symbol)
  (type-code-from-type-specifier 'string))

(define-type-inference-rule symbol-package (symbol)
  (check-type-code symbol symbol)
  (type-code-from-type-specifier '(or package null)))

(define-type-inference-rule symbol-plist (symbol)
  (check-type-code symbol symbol)
  (type-code-from-type-specifier 'list))

(define-type-inference-rule symbol-value (symbol)
  (check-type-code symbol symbol)
  (type-code-from-type-specifier 't))
