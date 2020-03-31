;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(defmacro define-specializer (function lambda-list &body body)
  (check-type function symbol)
  (let ((name
          (intern
           (let ((*package* (find-package '#:keyword)))
             (format nil "~:@(specialize/~S~)" function))
           #.*package*)))
    (multiple-value-bind (min-arguments max-arguments)
        (lambda-list-arity lambda-list)
      `(progn
         ;; Make sure the lambda list arity matches that of the function.
         (when (functionp (fdefinition ',function))
           (multiple-value-bind (min max) (function-arity ',function)
             (unless (and (<= ,min-arguments min)
                          (>= ,max-arguments max))
               (error "The lambda list of the specializer ~S ~@
                       is not compatible with the function ~S."
                      ',name ',function))))
         ;; Define the specializer...
         (%define-specializer (,function ,name) ,lambda-list ,@body)
         ;; ... and place it in the function database.
         (setf (specializer ',function) #',name)))))

(defmacro %define-specializer ((function name) lambda-list &body body)
  (multiple-value-bind (required optional rest keyword)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (assert (null keyword) ()
            "Rule lambda lists must not contain keywords.~@
             The specializer ~S violates this constraint."
            name)
    (multiple-value-bind (remaining-forms declarations)
        (alexandria:parse-body body)
      `(defun ,name ,lambda-list
         ,@declarations
         (flet ((abort-specialization ()
                  (%abort-specialization
                   ',function
                   (list* ,@required ,@(mapcar #'first optional) ,rest)))
                (wrap-default (&rest ntypes)
                  (wrap-function
                   (apply #'list-ntypes ntypes)
                   ',function
                   (list* ,@required ,@(mapcar #'first optional) ,rest))))
           (declare (ignorable #'abort-specialization #'wrap-default))
           ,@remaining-forms)))))

(defmacro check-ntype (object ntype)
  `(ntype-subtypecase (wrapper-ntype ,object)
     ((not ,ntype) (abort-specialization))
     (t (values))))

(defmacro abort-specialization (&rest args)
  (declare (ignore args))
  (error "ABORT-SPECIALIZATION can only be called from within specializers."))

(defmacro wrap-default (&rest args)
  (declare (ignore args))
  (error "WRAP-DEFAULT can only be called from within specializers."))

(declaim (notinline %abort-specialization))
(defun %abort-specialization (function arguments)
  (error 'invalid-arguments
         :function function
         :argument-types
         (mapcar (alexandria:compose #'type-specifier #'wrapper-ntype) arguments)))
