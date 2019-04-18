;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

(define-type-inference-rule apply (function &rest args)
  (if (and (= function +universal-type-code+)
           (not (null args)))
      +universal-type-code+
      +empty-type-code+))

(define-type-inference-rule fdefinition (name)
  (if (= name +universal-type-code+)
      +universal-type-code+
      +empty-type-code+))

(define-type-inference-rule fboundp (name)
  (if (= name +universal-type-code+)
      +universal-type-code+
      +empty-type-code+))

(define-type-inference-rule fmakunbound (name)
  (if (= name +universal-type-code+)
      +universal-type-code+
      +empty-type-code+))

(define-type-inference-rule funcall (function &rest args)
  (if (= function +universal-type-code+)
      +universal-type-code+
      +empty-type-code+))

(define-type-inference-rule function-lambda-expression (function)
  (if (= name +universal-type-code+)
      (values +universal-type-code+ +universal-type-code+ +universal-type-code+)
      +empty-type-code+))

(define-type-inference-rule functionp (object)
  (declare (ignore object))
  +universal-type-code+)

(define-type-inference-rule compiled-function-p (object)
  (declare (ignore object))
  +universal-type-code+)

(define-type-inference-rule not (object)
  (declare (ignore object))
  +universal-type-code+)

(define-type-inference-rule eq (x y)
  (declare (ignore x y))
  +universal-type-code+)

(define-type-inference-rule eql (x y)
  (declare (ignore x y))
  +universal-type-code+)

(define-type-inference-rule equal (x y)
  (declare (ignore x y))
  +universal-type-code+)

(define-type-inference-rule equalp (x y)
  (declare (ignore x y))
  +universal-type-code+)

(define-type-inference-rule identity (object)
  object)

(define-type-inference-rule complement (function)
  (if (= function +universal-type-code+)
      +universal-type-code+
      +empty-type-code+))

(define-type-inference-rule constantly (value)
  (declare (ignore value))
  +universal-type-code+)

(define-type-inference-rule every (predicate &rest sequences)
  (if (and (= predicate +universal-type-code+)
           (not (null sequences)))
      +universal-type-code+
      +empty-type-code+))

(define-type-inference-rule some (predicate &rest sequences)
  (if (and (= predicate +universal-type-code+)
           (not (null sequences)))
      +universal-type-code+
      +empty-type-code+))

(define-type-inference-rule notevery (predicate &rest sequences)
  (if (and (= predicate +universal-type-code+)
           (not (null sequences)))
      +universal-type-code+
      +empty-type-code+))

(define-type-inference-rule notany (predicate &rest sequences)
  (if (and (= predicate +universal-type-code+)
           (not (null sequences)))
      +universal-type-code+
      +empty-type-code+))

(define-type-inference-rule values (&rest objects)
  (values-list objects))

(define-type-inference-rule values-list (list)
  (if (= list +universal-type-code+)
      +universal-type-code+
      +empty-type-code+))

(define-type-inference-rule get-setf-expansion (place &optional (environment +universal-type-code+))
  (values +universal-type-code+
          +universal-type-code+
          +universal-type-code+
          +universal-type-code+
          +universal-type-code+))

