;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(defun specialize (function
                   wrappers
                   wrapper-ntype
                   wrap-constant
                   wrap-function)
  " Traverses a decomposition of FUNCTION into successive calls to more
specialized functions, using the supplied PROCESS-* functions.  Returns the
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

Signals the condition GIVE-UP-SPECIALIZATION when the supplied function and
arguments are too complicated.
"
  (let ((*wrapper-ntype* wrapper-ntype)
        (*wrap-constant* wrap-constant)
        (*wrap-function* wrap-function))
    (handler-case (apply (find-rule function) wrappers)
      ;; A program error is an indication that we had an argument
      ;; mismatch.
      (program-error ()
        ;; FIXME: Actually make sure we have an invalid number of
        ;; arguments.
        (error 'wrong-number-of-arguments
               :function function
               :arguments wrappers)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Debug Utilities

(defun specialize-verbosely (function
                             wrappers
                             wrapper-ntype
                             wrap-constant
                             wrap-function)
  (specialize
   function
   wrappers
   wrapper-ntype
   (lambda (constant)
     (let ((wrapper (funcall wrap-constant constant)))
       (format *trace-output* "~&Wrap constant: ~S => ~S~%"
               constant wrapper)
       wrapper))
   (lambda (ntypes function arguments)
     (let ((wrappers
             (multiple-value-list
              (funcall wrap-function ntypes function arguments))))
       (format *trace-output* "~&Wrap function: ~S ~S~{~% - ~S~}~% => ~{ ~S~}~%"
               ntypes function arguments wrappers)
       (values-list wrappers)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Derived Functionality

(defun infer-ntypes (function &rest ntypes)
  (handler-case
      (specialize
       function
       ntypes
       #'identity
       #'ntype-of
       (lambda (ntypes function arguments)
         (declare (ignore function arguments))
         (values-list ntypes)))
    (give-up-specialization ()
      (values))))

(defun expression-builder (function &rest ntypes)
  (flet ((wrap-object (object)
           (cons (ntype-of object) object))
         (wrap-function (ntypes function arguments)
           (let ((expression (cons function arguments)))
             (values-list
              (loop for ntype in ntypes
                    collect
                    (cons ntype expression))))))
    (specialize
     function
     (mapcar #'list ntypes)
     #'first
     #'wrap-object
     #'wrap-function)))

