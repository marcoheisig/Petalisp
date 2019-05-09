;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

(defvar *type-inference-functions* (make-hash-table :test #'eq))

(defmacro with-type-inference-barrier (&body body)
  `(catch '.type-inference. ,@body))

(defun give-up-type-inference ()
  (throw '.type-inference.
    (values)))

(defun abort-type-inference ()
  (throw '.type-inference.
    (type-code-from-type-specifier 'nil)))

(defmacro check-type-code (type-code type)
  `(type-code-subtypecase ,type-code
     ((not ,type) (abort-type-inference))))

(defun values-type-codes (function &rest argument-type-codes)
  "Returns one or more type codes that describe what values will be
returned by FUNCTION when called with arguments that match the supplied
ARGUMENT-TYPE-CODES.

Returns a single value that is the empty type code, if and only if it can
be determined that FUNCTION will never return for the specified argument
types.
"
  (let* ((fn (coerce function 'function))
         (inference-function (gethash fn *type-inference-functions*)))
    (with-type-inference-barrier
      (if inference-function
          (apply inference-function argument-type-codes)
          (multiple-value-bind (mandatory max)
              (function-arity fn)
            (if (<= mandatory (length argument-type-codes) max)
                (give-up-type-inference)
                (type-code-from-type-specifier 'nil)))))))

(defun register-type-inference-function (fn inference-fn)
  (multiple-value-bind (mandatory max) (function-arity fn)
    (declare (type (integer 0 (#.call-arguments-limit)) mandatory max))
    (setf (gethash fn *type-inference-functions*)
          (lambda (&rest type-codes)
            (if (<= mandatory (length type-codes) max)
                (apply inference-fn type-codes)
                (type-code-from-type-specifier 'nil))))))

(defmacro define-type-inference-rule (name lambda-list &body body)
  "Define a type inference rule for the function with the supplied NAME.
The supplied LAMBDA-LIST must accept as many arguments as the corresponding
function, while consisting only of mandatory, optional and rest arguments.

During type inference, each statement in BODY will be evaluated in an
environment where each lambda list argument is bound to a type code.  It
should return zero or more type codes, describing what values the function
will return if called with arguments of the prescribed types.

If a type inference function can determine that the call can never be
successful, it should call ABORT-TYPE-INFERENCE.  If a type inference
function can determine that it won't be able to derive any useful type
information, it should call GIVE-UP-TYPE-INFERENCE.  Each type inference
rule will automatically abort when the number of supplied argument type
codes does not match LAMBDA-LIST.

Examples:

 (define-type-inference-rule cl:identity (type-code)
   type-code)

 (define-type-inference-rule cl:values (&rest type-codes)
   (values-list type-codes))

 (define-type-inference-rule cl:values-list (type-code)
   (give-up-type-inference))

 (define-type-inference-rule cl:constantly (type-code)
   (type-code-from-type-specifier 'function))

 (define-type-inference-rule float-sign (type-code-1 &optional (type-code-2 type-code-1))
   (check-type-code type-code-1 float)
   (check-type-code type-code-2 float)
   type-code-2)
"
  (dolist (keyword (intersection lambda-list lambda-list-keywords))
    (unless (member keyword '(&optional &rest))
      (error "Invalid type inference rule keyword: ~S" keyword)))
  `(register-type-inference-function
    (function ,name)
    (lambda ,lambda-list ,@body)))

(defmacro define-predicate-type-inference-rule (name type)
  "Define an inference rule for the function named NAME that is a predicate
that returns - as a generalized boolean - whether its argument is of TYPE.

Examples:

 (define-predicate-type-inference-rule numberp number)

 (define-predicate-type-inference-rule random-state-p random-state)
"
  `(define-type-inference-rule ,name (object)
     (type-code-subtypecase object
       ((not ,type) (type-code-from-type-specifier 'null))
       (t (type-code-from-type-specifier 't)))))
