;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(defmacro define-predicate-rule (predicate type-specifier)
  (alexandria:with-gensyms (object ntype)
    `(define-rule ,predicate (,object)
       (let ((,ntype (wrapper-ntype ,object)))
         (if (eql-ntype-p ,ntype)
             (,predicate ,ntype)
             (ntype-subtypecase ,ntype
               ((not ,type-specifier) (rewrite-as nil))
               (,type-specifier (rewrite-default (ntype '(not null))))
               (t (rewrite-default (ntype 'generalized-boolean)))))))))

(define-predicate-rule arrayp array)
(define-predicate-rule bit-vector-p bit-vector)
(define-predicate-rule characterp character)
(define-predicate-rule compiled-function-p compiled-function)
(define-predicate-rule complexp complex)
(define-predicate-rule consp cons)
(define-predicate-rule floatp float)
(define-predicate-rule functionp function)
(define-predicate-rule hash-table-p hash-table)
(define-predicate-rule integerp integer)
(define-predicate-rule keywordp keyword)
(define-predicate-rule listp list)
(define-predicate-rule numberp number)
(define-predicate-rule packagep package)
(define-predicate-rule random-state-p random-state)
(define-predicate-rule rationalp rational)
(define-predicate-rule realp real)
(define-predicate-rule streamp stream)

;;; The remaining rules cannot be handled by DEFINE-PREDICATE-RULE, because
;;; the domain of these functions is limited to numbers.

(define-rule minusp (real)
  (let ((ntype (wrapper-ntype real)))
    (with-constant-folding (minusp (ntype real))
      (ntype-subtypecase ntype
        ((real * (0)) (rewrite-default (ntype '(not null))))
        ((real 0 *) (rewrite-as nil))
        (t (rewrite-default (ntype 'generalized-boolean)))))))

(define-rule plusp (real)
  (let ((ntype (wrapper-ntype real)))
    (with-constant-folding (plusp (ntype real))
      (ntype-subtypecase ntype
        ((real (0) *) (rewrite-default (ntype '(not null))))
        ((real * 0) (rewrite-as nil))
        (t (rewrite-default (ntype 'generalized-boolean)))))))

(define-rule zerop (number)
  (let ((ntype (wrapper-ntype number)))
    (with-constant-folding (zerop (ntype number))
      (ntype-subtypecase ntype
        (zero (rewrite-default (ntype '(not null))))
        ((not zero) (rewrite-as nil))
        (t (rewrite-default (ntype 'generalized-boolean)))))))

(define-rule evenp (integer)
  (with-constant-folding (evenp ((wrapper-ntype integer) integer))
    (rewrite-default (ntype 'generalized-boolean))))

(define-rule oddp (integer)
  (with-constant-folding (oddp ((wrapper-ntype integer) integer))
    (rewrite-default (ntype 'generalized-boolean))))
