;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package :petalisp-core)

;;; There are two good reasons to do type inference.  The first one is to
;;; detect incorrect programs early, the second one is for performance.  We
;;; want both.
;;;
;;; Petalisp differs significantly from the general problem of type
;;; inference in Common Lisp:
;;;
;;; - there is no control flow (loops, recursion, branches etc.)
;;;
;;; - there are only pure functions
;;;
;;; This permits us to compute all types eagerly during the creation of
;;; data flow nodes.
;;;
;;; Type inference works by associating known functions with inference
;;; functions.  Each inference function is a closure that receives a list of
;;; type specifiers and returns the following values:
;;;
;;; 1. A list of types, one for each of the returned multiple values.
;;;
;;; 2. T when there can be more values than indicated by 1., otherwise NIL.
;;;
;;; 3. A list of conditions that may be signaled.
;;;
;;; 4. The name of the given function, or NIL.
;;;
;;; Furthermore, the type inference function will directly signal an error
;;; if it can be proven that a call with arguments of the given types is
;;; unconditionally erroneous.
;;;
;;; Examples:
;;;
;;; (infer-type #'+ '(float float float))
;;; => (float), NIL, NIL, +
;;;
;;; (infer-type #'+ '())
;;; => (bit), NIL, NIL, +
;;;
;;; (infer-type #'/ '(integer integer))
;;; => (rational), NIL, (DIVISION-BY-ZERO), /
;;;
;;; (infer-type #'floor '(float))
;;; => (integer, real), NIL, (DIVISION-BY-ZERO), floor
;;;
;;; (infer-type #'char-code '(hash-table))
;;; => An error is signaled

(defvar *inference-table* (make-hash-table :test #'eq))

(defun default-type-inference-function (arguments)
  (declare (ignore arguments))
  (values '() t '(condition) nil))

(defmacro type-inferrer (function)
  `(gethash ,function *inference-table* #'default-type-inference-function))

(defmacro define-type-inferrer (function-name lambda-list &body body)
  (with-gensyms (argument-types)
    `(setf (type-inferrer #',function-name)
           (lambda (,argument-types)
             (destructuring-bind ,lambda-list ,argument-types
               ,@body)))))

(defun infer-type (function argument-types)
  (check-arity function (length argument-types))
  (funcall (type-inferrer function) argument-types))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility Functions for Building Type Inferrers

(defun check-arity (function number-of-supplied-arguments)
  (multiple-value-bind (mandatory-arguments max-arguments)
      (function-arity function)
    (unless (>= number-of-supplied-arguments mandatory-arguments)
      (error "~@<Only ~R argument~:P given for ~S, with ~
                 ~R mandatory argument~:P.~:@>"
             number-of-supplied-arguments
             function
             mandatory-arguments))
    (unless (<= number-of-supplied-arguments max-arguments)
      (error "~@<Received ~R argument~:P for ~S, which ~
                 expects ~:[at most~;exactly~] ~R argument~:P.~:@>"
             number-of-supplied-arguments
             function
             (= max-arguments mandatory-arguments)
             max-arguments))))
