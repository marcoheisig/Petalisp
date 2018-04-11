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
;;; primary value of NIL can be returned to signal an erroneous function
;;; call.

(defvar *inference-table* (make-hash-table :test #'eq))

(defun register-type-inferrer (symbol inferrer)
  (check-type symbol symbol)
  (check-type inferrer function)
  (let ((function (coerce symbol 'function))
        (entry (cons inferrer symbol)))
    (flet ((register (key value)
             (setf (gethash key *inference-table*) value)))
      (register symbol entry)
      (register function entry))))

(defun inferrer-and-function-designator (function-designator)
  (if-let ((entry (gethash function-designator *inference-table*)))
    (values (car entry) (cdr entry))
    (values (constantly 't) function-designator)))

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
  (multiple-value-bind (inferrer function-designator)
      (inferrer-and-function-designator function-designator)
    ;; Check the argument count.
    (let ((number-of-arguments (length argument-types)))
      (multiple-value-bind (mandatory-arguments max-arguments)
          (function-arity function-designator)
        (demand (>= number-of-arguments mandatory-arguments)
          "~@<Only ~R argument~:P given for ~S, with ~
              ~R mandatory argument~:P.~:@>"
          number-of-arguments function-designator mandatory-arguments)
        (demand (<= number-of-arguments max-arguments)
          "~@<Received ~R argument~:P for ~S, which ~
              expects ~:[at most~;exactly~] ~R argument~:P.~:@>"
          number-of-arguments
          function-designator
          (= max-arguments mandatory-arguments)
          max-arguments)))
    ;; Infer the result type.
    (let ((type (funcall inferrer argument-types)))
      (demand (not (null type))
        "~@<There is no valid way to call the function ~S ~
            with arguments of type ~{~#[~;and ~S~;~S ~:;~S, ~]~}.~:@>"
        function-designator argument-types)
      (values type function-designator))))
