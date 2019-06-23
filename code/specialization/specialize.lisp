;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parameterization

(declaim (function *process-argument* *process-constant* *process-call*))
(defvar *process-argument*)
(defvar *process-constant*)
(defvar *process-call*)

(declaim (inline %process-argument))
(defun %process-argument (argument)
  (funcall *process-argument* argument))

(declaim (inline process-argument))
(defun process-argument (argument)
  (multiple-value-bind (type-code value)
      (%process-argument argument)
    (values (type-codes type-code) value)))

(declaim (inline process-constant))
(defun process-constant (constant)
  (multiple-value-bind (type-code value)
      (funcall *process-constant* constant)
    (values (type-codes type-code) value)))

(declaim (inline process-call))
(defun process-call (type-codes function-name &rest arguments)
  (values
   type-codes
   (apply *process-call* function-name arguments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Rewrite Rules

(defvar *external-rewrite-rules* (make-hash-table :test #'eq))

(defvar *internal-rewrite-rules* (make-hash-table :test #'eq))

(defstruct (external-rewrite-rule
            (:predicate external-rewrite-rule-p))
  (name nil :type symbol)
  (min-arguments nil :type arity)
  (max-arguments nil :type arity)
  (fn nil :type function))

(defstruct (internal-rewrite-rule
            (:predicate internal-rewrite-rule))
  (name nil :type symbol)
  (type-codes nil :type list)
  (arity nil :type arity)
  (fn nil :type (or function null)))

(defun argument-type-code (argument)
  (values
   (%process-argument argument)))

(defun argument-type (argument)
  (type-specifier-from-type-code
   (argument-type-code argument)))

(defun invoke-external-rewrite-rule (function arguments)
  (multiple-value-bind (external-rewrite-rule present-p)
      (gethash function *external-rewrite-rules*)
    (cond
      ;; Case 1 - No external rewrite rule exists.
      ((not present-p)
       (multiple-value-bind (min-arity max-arity)
           (function-arity function)
         (unless (<= min-arity (length arguments) max-arity)
           (error 'wrong-number-of-arguments
                  :thunk (lambda ()
                           (list* function (mapcar #'argument-type arguments)))))
         (values '() nil)))
      ;; Case 2 - An external rewrite rule exists - use it.
      (present-p
       (with-accessors ((name external-rewrite-rule-name)
                        (min-arguments external-rewrite-rule-min-arguments)
                        (max-arguments external-rewrite-rule-max-arguments)
                        (fn external-rewrite-rule-fn))
           external-rewrite-rule
         (flet ((error-thunk ()
                  (list* name (mapcar #'argument-type arguments))))
           (declare (dynamic-extent #'error-thunk))
           (let ((*specialization-error-thunk* #'error-thunk))
             (unless (<= min-arguments (length arguments) max-arguments)
               (error 'wrong-number-of-arguments :thunk *specialization-error-thunk*))
             (apply fn arguments))))))))

(defun find-internal-rewrite-rule (name arity)
  (multiple-value-bind (internal-rewrite-rule present-p)
      (gethash name *internal-rewrite-rules*)
    (unless present-p
      (error "There is no internal rewrite rule with name ~S." name))
    (unless (= arity (internal-rewrite-rule-arity internal-rewrite-rule))
      (error "Reference to rewrite rule ~S with wrong arity ~D." name arity))
    internal-rewrite-rule))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Entry Function

(defun specialize (function arguments
                   &key
                     (process-argument #'type-code-of)
                     (process-constant #'type-code-of)
                     (process-call #'values))
  "Returns a list of type codes describing the set of values returned by
FUNCTION when applied to ARGUMENTS.  Returns a second value that is
obtained by processing the functions, constants and arguments of the most
specialized call in a user-defined manner.

Arguments:

FUNCTION - A function.

ARGUMENTS - A list of objects.  Each such object must be a suitable argument
for the function PROCESS-ARGUMENT.

PROCESS-ARGUMENT - A function that is invoked once for each argument in ARGUMENTS.  It
must return a type code, and, optionally, an arbitrary object for further
processing.

PROCESS-CONSTANT - A function that is invoked on each constant that appears
in a rewrite rule.  It must return a type code, and, optionally, an
arbitrary object for further processing.

PROCESS-CALL - A function that is invoked with a first argument that is a
function name, and zero or more objects that have previously been returned
as second argument of calls to PROCESS-ARGUMENT, PROCESS-CONSTANT or
PROCESS-CALL.
"
  (let ((*process-argument* process-argument)
        (*process-constant* process-constant)
        (*process-call* process-call))
    (invoke-external-rewrite-rule function arguments)))

(defun specialize-verbosely (function arguments
                             &key
                               (process-argument
                                (lambda (x)
                                  (values (type-code-of x) x)))
                               (process-constant
                                (lambda (x)
                                  (values (type-code-of x) x)))
                               (process-call #'list))
  (specialize
   function arguments
   :process-argument
   (lambda (argument)
     (multiple-value-bind (type-code value)
         (funcall process-argument argument)
       (format *trace-output* "~&Process argument: ~S => ~S, ~S~%"
               argument type-code value)
       (values type-code value)))
   :process-constant
   (lambda (constant)
     (multiple-value-bind (type-code value)
         (funcall process-constant constant)
       (format *trace-output* "~&Process constant: ~S => ~S, ~S~%"
               constant type-code value)
       (values type-code value)))
   :process-call
   (lambda (function &rest values)
     (let ((value (apply process-call function values)))
       (format *trace-output* "~&Process call: ~S~{~% - ~S~}~% => ~S~%"
               function values value)
       value))))
