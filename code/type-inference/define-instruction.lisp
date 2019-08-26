;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(defmacro define-instruction ((base-name instruction-name)
                              result-types arguments
                              &body body)
  `(progn
     (declaim (inline ,instruction-name))
     (defun ,instruction-name ,arguments
       (the (values ,@result-types)
            (,base-name ,@arguments)))
     (define-rule ,instruction-name ,arguments ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Simple Instructions
;;;
;;; A simple instruction is an alias for an existing function, but with a
;;; fixed number of arguments and values and without side-effects.

(defmacro define-simple-instruction ((base-name instruction-name) result-types argument-types)
  (let ((arguments (mapcar #'gensymify argument-types))
        (ntypes (loop repeat (length argument-types) collect (gensym "NTYPE")))
        (foldable (gensym "FOLDABLE")))
    `(define-instruction (,base-name ,instruction-name) ,result-types ,arguments
       (let ((,foldable t)
             ,@(loop for argument in arguments
                     for ntype in ntypes
                     collect `(,ntype (wrapper-ntype ,argument))))
         ,@(loop for argument-type in argument-types
                 for ntype in ntypes
                 collect
                 `(cond ((%ntypep ,ntype)
                         (setf ,foldable nil)
                         (ntype-subtypecase ,ntype
                           ((not ,argument-type) (abort-specialization))
                           (t (values))))
                        (t
                         (unless (typep ,ntype ',argument-type)
                           (abort-specialization)))))
         (if ,foldable
             (handler-case (wrap-constant (funcall #',base-name ,@ntypes))
               (error () (rewrite-default ,@result-types)))
             (rewrite-default ,@result-types))))))

(defun gensymify (x)
  (if (symbolp x)
      (gensym (symbol-name x))
      (gensym)))
