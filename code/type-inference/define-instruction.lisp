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
        (ntypes (loop repeat (length argument-types) collect (gensym "NTYPE"))))
    `(define-instruction (,base-name ,instruction-name) ,result-types ,arguments
       (let (,@(loop for argument in arguments
                     for ntype in ntypes
                     collect `(,ntype (wrapper-ntype ,argument))))
         (with-constant-folding (,base-name ,@(mapcar #'list ntypes argument-types))
           (rewrite-default
            (loop for type in result-types
                  collect
                  `',(ntype result-types))))))))

(defun gensymify (x)
  (if (symbolp x)
      (gensym (symbol-name x))
      (gensym)))

(defmacro with-constant-folding ((function &rest ntype-specs) &body body)
  (alexandria:with-gensyms (foldable default)
    `(let ((,foldable t))
       ,@(loop for (ntype type) in ntype-specs
               collect
               `(cond ((%ntypep ,ntype)
                       (setf ,foldable nil)
                       (ntype-subtypecase ,ntype
                         ((not ,type) (abort-specialization))
                         (t (values))))
                      (t
                       (unless (typep ,ntype ',type)
                         (abort-specialization)))))
       (flet ((,default () ,@body))
         (declare (dynamic-extent #',default))
         (if ,foldable
             (handler-case (wrap-constant (funcall #',function ,@(mapcar #'first ntype-specs)))
               (error () (,default)))
             (,default))))))

