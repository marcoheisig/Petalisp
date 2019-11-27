;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(defmacro define-differentiator (function lambda-list index &body body)
  (check-type function symbol)
  (let ((name
          (intern
           (let ((*package* (find-package '#:keyword)))
             (format nil "~:@(differentiate/~S~)" function))
           #.*package*)))
    (multiple-value-bind (min-arguments max-arguments)
        (lambda-list-arity lambda-list)
      `(progn
         ;; Make sure the lambda list arity matches that of the function.
         (when (functionp (fdefinition ',function))
           (multiple-value-bind (min max) (function-arity ',function)
             (unless (and (<= ,min-arguments min)
                          (>= ,max-arguments max))
               (error "The lambda list of the differentiator ~S ~@
                       is not compatible with the function ~S."
                      ',name ',function))))
         ;; Define the differentiator...
         (%define-differentiator (,function ,name) ,lambda-list ,index ,@body)
         ;; ... and place it in the function database.
         (setf (differentiator ',function) #',name)))))

(defmacro %define-differentiator ((function name) lambda-list index &body body)
  (multiple-value-bind (required optional rest keyword)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (unless (null keyword)
      (error "Rule lambda lists must not contain keywords.~@
              The differentiator ~S violates this constraint."
             name))
    (multiple-value-bind (remaining-forms declarations)
        (alexandria:parse-body body)
      (let ((suppliedps
              (loop for (name init suppliedp) in optional
                    collect
                    (if (null suppliedp)
                        (gensym "SUPPLIEDP")
                        suppliedp))))
        (alexandria:with-gensyms (n nargs)
          `(defun ,name ,(cons n lambda-list)
             ,@declarations
             ;; Insert a prologue that checks whether the differentiation
             ;; index is valid.
             (let ((,nargs (+ ,(length required)
                              ,@(when rest `((length ,rest)))
                              ,@(loop for suppliedp in suppliedps
                                      collect
                                      `(when suppliedp 1 0)))))
               (unless (<= 0 ,n (1- ,nargs))
                 (invalid-differentiation-index ,n ,nargs ',function)))
             (let ((,index ,n))
               ;; For convenience, add an ignorable declaration to the
               ;; index if it can only be zero.
               ,@(when (and (= 1 (length required))
                            (null optional)
                            (null rest))
                   `((declare (ignorable ,index))))
               ,@remaining-forms)))))))
