;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(defun specialize (function
                   wrappers
                   wrapper-ntype
                   wrap-constant
                   wrap-function
                   default)
  "Traverses a decomposition of FUNCTION into successive calls to more
specialized functions, using the supplied wrapping functions.  Returns the
values returned by PROCESS-MULTIPLE-VALUE-FUNCTION.

Arguments:

FUNCTION - A function designator, or the name of a special operator.

ARGUMENTS - A list of wrapped objects.

WRAPPER-NTYPE - A function that is applied to obtain the ntype of a wrapped
object.

WRAP-CONSTANT - A function that is applied to turn a constant into a
wrapped object.

WRAP-FUNCTION - A function that is invoked with a first argument that is a
list of ntypes of length K, a second argument that is a function
designator, and a list of wrapped objects.  It must return K wrapped
objects - one for each ntype.

DEFAULT - A thunk that is executed to produce a default value when the
supplied FUNCTION and WRAPPERS are too complicated for specialization.

May signal an error of type WRONG-NUMBER-OF-ARGUMENTS or INVALID-ARGUMENTS
when the number or type of the supplied WRAPPERS is not suitable for the
supplied FUNCTION.
"
  (let ((*wrapper-ntype* wrapper-ntype)
        (*wrap-constant* wrap-constant)
        (*wrap-function* wrap-function))
    (handler-case (apply (specializer function) wrappers)
      ;; A program error is an indication that we had an argument
      ;; mismatch.
      (give-up-specialization ()
        (funcall default))
      (program-error ()
        ;; FIXME: Actually make sure we have an invalid number of
        ;; arguments.
        (error 'wrong-number-of-arguments
               :function function
               :arguments wrappers)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Derived Functionality

(defun infer-ntypes (function ntypes default)
  (specialize
   function
   ntypes
   #'identity
   #'ntype-of
   (lambda (ntypes function arguments)
     (declare (ignore function arguments))
     (values-list ntypes))
   default))

(defun expression-builder (function arguments)
  (flet ((wrap-constant (object)
           (cons (ntype-of object) object))
         (wrap-function (ntypes function arguments)
           (let ((expression (cons function (mapcar #'cdr arguments))))
             (values-list
              (loop for ntype in ntypes
                    collect
                    (cons ntype expression))))))
    (let ((result
            (specialize
             function
             arguments
             #'first
             #'wrap-constant
             #'wrap-function
             (lambda ()
               (cons (ntype 't) (list* function (mapcar #'cdr arguments)))))))
      (values (cdr result) (car result)))))

