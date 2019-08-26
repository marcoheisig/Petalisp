;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(defvar *rules* (make-hash-table :test #'eq))

(defun default-rule (&rest arguments)
  (declare (ignore arguments))
  (give-up-specialization))

(defun find-rule (function)
  (or (gethash function *rules*)
      #'default-rule))

(define-compiler-macro find-rule (&whole form function)
  (if (constantp function)
      `(load-time-value
        (locally (declare (notinline find-rule))
          (find-rule ,function)))
      form))

(defun (setf find-rule) (value function)
  (check-type function symbol)
  (setf (gethash function *rules*) value)
  (when (and (fboundp function)
             (functionp (fdefinition function)))
    (setf (gethash (fdefinition function) *rules*) value)))

(defmacro define-rule (function lambda-list &body body)
  (check-type function symbol)
  (let ((rule-name
          (intern
           (let ((*package* (find-package '#:keyword)))
             (format nil "~:@(specialize/~S~)" function))
           #.*package*)))
    (multiple-value-bind (min-arguments max-arguments)
        (lambda-list-arity lambda-list)
      `(progn
         ;; Make sure the lambda list arity matches that of the function.
         (when (functionp (fdefinition ',function))
           (multiple-value-bind (min max) (function-arity (fdefinition ',function))
             (unless (and (<= ,min-arguments min)
                          (>= ,max-arguments max))
               (error "The lambda list of the external rule ~S ~@
                       is not compatible with the function ~S."
                      ',rule-name ',function))))
         ;; Define the rule as a regular function.
         (%define-rule (,function ,rule-name) ,lambda-list ,@body)
         ;; Place this function in the external rule database.
         (setf (find-rule ',function) #',rule-name)))))

;;; This macro provides two local macros REWRITE-AS and REWRITE-DEFAULT to
;;; combine the original wrapped objects using further rules, or by passing
;;; the name and types of the current rule, respectively.
(defmacro %define-rule ((function-name rule-name) lambda-list &body body)
  (multiple-value-bind (required optional rest keyword)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (assert (null keyword) ()
            "Rule lambda lists must not contain keywords.~@
             The rule ~S violates this constraint."
            rule-name)
    (multiple-value-bind (remaining-forms declarations)
        (alexandria:parse-body body)
      `(defun ,rule-name ,lambda-list
         ,@declarations
         (flet ((abort-specialization ()
                  (%abort-specialization
                   ',function-name
                   (list* ,@required ,@(mapcar #'first optional) ,rest))))
           (declare (ignorable #'abort-specialization))
           (macrolet ((rewrite-default (&rest type-specifiers)
                        `(wrap-function
                          ',(mapcar #'ntype type-specifiers)
                          ',',function-name
                          ,'(list* ,@required ,@(mapcar #'first optional) ,rest)))
                      (check-ntype (object ntype)
                        `(ntype-subtypecase (wrapper-ntype ,object)
                           ((not ,ntype) (abort-specialization))
                           (t (values)))))
             ,@remaining-forms))))))

(declaim (notinline %abort-specialization))
(defun %abort-specialization (function arguments)
  (error 'invalid-arguments
         :function function
         :argument-types
         (mapcar (alexandria:compose #'type-specifier #'wrapper-ntype) arguments)))

(defmacro rewrite-as (form)
  (expand-rewrite-as-form form))

(defun expand-rewrite-as-form (form)
  (cond ((consp form)
         `(funcall
           (find-rule ',(first form))
           ,@(mapcar #'expand-rewrite-as-form (rest form))))
        ((member form '(nil t))
         `(wrap-constant ,form))
        ((symbolp form)
         form)
        (t `(wrap-constant ,form))))
