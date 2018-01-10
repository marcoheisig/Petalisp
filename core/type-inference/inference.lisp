;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/type-inference/inference
  (:use :closer-common-lisp :alexandria)
  (:export
   #:register-type-inference-function
   #:infer-function-designator-and-type))

(in-package :petalisp/core/type-inference/inference)

(defvar *function-designator-inferrers* (make-hash-table :test #'eq)
  "A hash table, mapping function designators to type inferrers.")

(defun register-type-inference-function (function-designator inference-function)
  (check-type inference-function function)
  (flet ((make-inferrer (second-argument)
           (lambda (argument-types)
             (values (funcall inference-function argument-types)
                     second-argument)))
         (register (key value)
           (setf (gethash key *function-designator-inferrers*) value)))
    (etypecase function-designator
      (function
       (register function-designator (make-inferrer function-designator)))
      (symbol
       (let* ((symbol function-designator)
              (function (if (fboundp symbol)
                            (symbol-function symbol)
                            (error "Not a function designator: ~A" symbol)))
              (inferrer (make-inferrer symbol)))
         (register symbol inferrer)
         (register function inferrer))))))

(defun infer-function-designator-and-type (function-designator argument-types)
   "Return a supertype of all possible results of applying OPERATOR to
arguments of the given ARGUMENT-TYPES.

As a second argument, return a function designator, that is either EQ to
the supplied FUNCTION-DESIGNATOR, or whose symbol function is EQ to the
supplied FUNCTION-DESIGNATOR. The rationale for this second argument is
that, due to compiler macros and inlining, symbols are 'more useful' than
the functions they represent.

Examples:
 (infer-type #'+ '(u8 u8 u8))
=> u16, +
 (infer-type '* '())
=> bit, *"
  (if-let ((inferrer (gethash function-designator *function-designator-inferrers*)))
    (funcall inferrer argument-types)
    (values t function-designator)))
