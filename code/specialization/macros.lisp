;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(defmacro rewrite-lambda (lambda-list &body body)
  (let* ((env (loop for var in lambda-list
                    collect (list var (gensym "TYPE-CODE") (gensym "VALUE"))))
         (block-name (gensym "REWRITE-LAMBDA"))
         (arguments (loop for (var type-code value) in env
                          collect type-code collect value))
         (type-codes (loop for (var type-code value) in env
                           collect type-code))
         (values (loop for (var type-code value) in env
                       collect value)))
    `(lambda ,arguments
       (block ,block-name
         (macrolet ((rewrite-as (form)
                      `(return-from ,',block-name
                         ,(expand-rewrite-as-form ',env form)))
                    (rewrite-default (name type-codes)
                      `(return-from ,',block-name
                         (process-call ',type-codes ',name ,@',values))))
           (let ,(loop for var in lambda-list
                       for type-code in type-codes
                       collect `(,var ,type-code))
             (declare (ignorable ,@lambda-list))
             ,@body))))))

(defun expand-rewrite-as-form (env form)
  (let ((bindings '()))
    (labels ((add-binding (type-codes value form)
               (push (list type-codes value form) bindings)
               (values type-codes value))
             (expand (form)
               (if (and (symbolp form)
                        (not (constantp form)))
                   (destructuring-bind (name type-code value)
                       (or (find form env :key #'first :test #'eq :from-end t)
                           (error "Undefined variable ~S." form))
                     (declare (ignore name))
                     (values `(type-codes ,type-code) value))
                   (let ((type-codes (gensym "TYPE-CODES"))
                         (value (gensym "VALUE")))
                     (if (consp form)
                         (let ((arguments
                                 (loop for subform in (rest form)
                                       append
                                       (multiple-value-bind (type-codes value)
                                           (expand subform)
                                         `((first ,type-codes) ,value)))))
                           (add-binding
                            type-codes value
                            `(funcall
                              (load-time-value
                               (the function
                                    (internal-rewrite-rule-fn
                                     (find-internal-rewrite-rule ',(first form) ',(length (rest form))))))
                              ,@arguments)))
                         (add-binding type-codes value `(process-constant ',form))))))
             (wrap (bindings body)
               (if (null bindings)
                   body
                   (destructuring-bind (type-codes value values-form)
                       (first bindings)
                     (wrap
                      (rest bindings)
                      `(multiple-value-bind (,type-codes ,value) ,values-form
                         (declare (list ,type-codes))
                         ,body))))))
      (multiple-value-bind (type-codes value) (expand form)
        (wrap bindings `(values ,type-codes ,value))))))

(defmacro rewrite-funcall (rewrite-fn &rest argument-forms)
  (let ((bindings (loop for form in argument-forms
                        collect (list (gensym "TYPE-CODES")
                                      (gensym "VALUE")
                                      form))))
    (labels ((wrap-in-bindings (bindings body)
               (if (null bindings)
                   body
                   (destructuring-bind (type-codes value values-form)
                       (first bindings)
                     (wrap-in-bindings
                      (rest bindings)
                      `(multiple-value-bind (,type-codes ,value) ,values-form
                         (declare (list ,type-codes))
                         ,body))))))
      (wrap-in-bindings
       (reverse bindings)
       `(funcall
         ,rewrite-fn
         ,@(loop for (type-codes value nil) in bindings
                 collect `(first ,type-codes)
                 collect value))))))

(defmacro rewrite-let (bindings &body body)
  `(rewrite-funcall
    (rewrite-lambda ,(mapcar #'first bindings) ,@body)
    ,@(mapcar #'second bindings)))

(defmacro define-external-rewrite-rule (name lambda-list &body body)
  (let ((rewrite-rule-name
          (intern (format nil "~:@(external-rewrite-rule/~S~)" name) #.*package*)))
    (multiple-value-bind (min-arguments max-arguments)
        (lambda-list-arity lambda-list)
      (let ((rule (gensym "RULE")))
        `(progn
           (defun ,rewrite-rule-name ,lambda-list ,@body)
           (let ((,rule (make-external-rewrite-rule
                         :name ',name
                         :min-arguments ',min-arguments
                         :max-arguments ',max-arguments
                         :fn #',rewrite-rule-name)))
             (setf (gethash ',name *external-rewrite-rules*) ,rule)
             (setf (gethash #',name *external-rewrite-rules*) ,rule)))))))

(defmacro define-internal-rewrite-rule (name types lambda-list &body body)
  (multiple-value-bind (min-arguments max-arguments)
      (lambda-list-arity lambda-list)
    (let ((type-codes (mapcar #'type-code-from-type-specifier types)))
      (assert (= min-arguments max-arguments))
      ;; We need to move registration of the internal rewrite rule to
      ;; the load time, because the subsequent definition of the
      ;; external rewrite rule of the same name will look up its
      ;; internal rewrite rule at load time, too.
      `(load-time-value
        (setf (gethash ',name *internal-rewrite-rules*)
              (make-internal-rewrite-rule
               :name ',name
               :type-codes ',type-codes
               :arity ',min-arguments
               :fn
               (rewrite-lambda ,lambda-list
                 ,@body
                 (rewrite-default ,name ,type-codes))))))))

(defmacro define-rewrite-rules (name types lambda-list &body body)
  (multiple-value-bind (min-arguments max-arguments)
      (lambda-list-arity lambda-list)
    (assert (= min-arguments max-arguments))
    `(progn
       (define-internal-rewrite-rule ,name ,types ,lambda-list ,@body)
       (define-external-rewrite-rule ,name ,lambda-list
         (rewrite-funcall
          (rewrite-lambda ,lambda-list (rewrite-as (,name ,@lambda-list)))
          ,@(loop for argument in lambda-list
                  collect `(process-argument ,argument)))))))

(defmacro check-type-code (form type)
  `(type-code-subtypecase ,form
     ((not ,type) (abort-specialization))))

(defmacro check-argument (argument type)
  `(check-type-code (%process-argument ,argument) ,type))

(defmacro defop ((base-name name) output-types input-types
                 &optional (arguments () arguments-supplied-p) &body body)
  (let ((args (or arguments
                  (loop for input-type in input-types
                        collect
                        (if (symbolp input-type)
                            (gensym (symbol-name input-type))
                            (gensym "ARG"))))))
    `(progn
       (declaim (inline ,name))
       (defun ,name ,args
         (declare
          ,@(loop for arg in args
                  for type in input-types
                  collect `(type ,type ,arg)))
         (the (values ,@output-types)
              (,base-name ,@args)))
       (define-rewrite-rules ,name ,output-types ,args
         ;; If arguments are supplied, we assume the programmer will
         ;; perform type checking herself.
         ,@(if arguments-supplied-p
               (list)
               (loop for arg in args
                     for type in input-types
                     collect `(check-type-code ,arg ,type)))
         ,@body))))
