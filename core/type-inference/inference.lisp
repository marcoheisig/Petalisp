;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/type-inference/inference
  (:use :closer-common-lisp :alexandria)
  (:use
   #:petalisp/utilities/all
   #:petalisp/core/error-handling)
  (:export
   #:register-type-inferrer
   #:infer-type))

(in-package :petalisp/core/type-inference/inference)

;;; There are two good reasons to do type inference. The first one is to
;;; detect incorrect programs early, the second one is for performance. We
;;; attempt to do both.
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
;;; functions. Each inference function is a closure that receives a list of
;;; type specifiers and returns two values: The inferred type and the
;;; function symbol of the function whose type is currently inferred. A
;;; primary value of NIL can be returned to signal an invalid function
;;; call.

;;; *INFERENCE-TABLE* is a hash table, mapping function
;;; designators to type inferrers. Each function has two entries in this
;;; table --- one for the function symbol and one for the corresponding
;;; function object. Both have the same inference function as their
;;; value. This allows the interchangeable use of symbols and functions as
;;; function designators.

(defvar *inference-table* (make-hash-table :test #'eq))

(defun register-type-inferrer (symbol inferrer)
  (check-type symbol symbol)
  (check-type inferrer function)
  (let ((function (if (fboundp symbol)
                      (symbol-function symbol)
                      (error "Not a function designator: ~A" symbol)))
        (closure
          (lambda (argument-types)
            (values (funcall inferrer argument-types)
                    symbol))))
    (flet ((register (key value)
             (setf (gethash key *inference-table*) value)))
      (register symbol closure)
      (register function closure))))

(defun infer-type (function-designator argument-types)
  ;; Return a supertype of all possible results of applying OPERATOR to
  ;; arguments of the given ARGUMENT-TYPES.
  ;;
  ;; As a second argument, return a function designator, that is either EQ to
  ;; the supplied FUNCTION-DESIGNATOR, or whose symbol function is EQ to the
  ;; supplied FUNCTION-DESIGNATOR. The rationale for this second argument is
  ;; that, due to compiler macros and inlining, symbols are 'more useful' than
  ;; the functions they represent.
  ;;
  ;; Examples:
  ;;  (infer-type #'+ '(u8 u8 u8))
  ;; => u16, +
  ;;  (infer-type '* '())
  ;; => bit, *
  (let ((function (coerce function-designator 'function))
        (number-of-arguments (length argument-types)))
    (multiple-value-bind (mandatory-arguments max-arguments)
        (function-arity function)
      (demand (>= number-of-arguments mandatory-arguments)
        "~@<Only ~R argument~:P given for ~S, with ~
         ~R mandatory argument~:P.~:@>"
        number-of-arguments function-designator mandatory-arguments)
      (demand (<= number-of-arguments max-arguments)
        "~@<Received ~R argument~:P for ~S, which ~
          accepts at most ~R argument~:P.~:@>"
        number-of-arguments function-designator max-arguments)))
  (if-let ((inferrer (gethash function-designator *inference-table*)))
    (multiple-value-bind (type symbol)
        (funcall inferrer argument-types)
      (demand (not (null type))
        "~@<There is no valid way to call the function ~S ~
             with arguments of type ~{~#[~;and ~S~;~S ~:;~S, ~]~}.~:@>"
        symbol argument-types)
      (values type symbol))
    (values 't function-designator)))
