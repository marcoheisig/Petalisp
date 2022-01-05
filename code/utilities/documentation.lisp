;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

(defun ensure-documentation (name new-value kind)
  (check-type new-value string)
  (symbol-macrolet ((documentation (documentation name kind)))
    (cond ((null documentation)
           (setf documentation new-value))
          ((string= documentation new-value)
           (values))
          (t
           (warn "Changing the ~(~A~) documentation of ~A." kind name)
           (setf documentation new-value)))))

(defun build-documentation (docstring example-forms example-thunks)
  (assert (= (length example-forms)
             (length example-thunks)))
  (with-output-to-string (stream)
    (write-string docstring stream)
    (unless (null example-forms)
      (let ((*print-case* :downcase))
        (format stream "~&~%Example~P:" (length example-forms))
        (loop for example-form in example-forms
              for example-thunk in example-thunks do
                (format stream "~&~% ~A~%" example-form)
                (handler-case
                    (format stream "~{  => ~A~%~}"
                            (multiple-value-list
                             (funcall example-thunk)))
                  (error (e)
                    (format stream "  >> ~A" (class-name (class-of e))))))))))

(defmacro expand-documentation (form &rest examples)
  `(build-documentation
    ,form
    ',examples
    (list
     ,@(mapcar (lambda (form) `(lambda () ,form)) examples))))

(defmacro document-compiler-macro (name &body body)
  (assert (compiler-macro-function name))
  `(ensure-documentation ',name (expand-documentation ,@body) 'compiler-macro))

(defmacro document-function (name &body body)
  (assert (fboundp name))
  `(ensure-documentation ',name (expand-documentation ,@body) 'function))

(defmacro document-method-combination (name &body body)
  (assert (symbolp name))
  `(ensure-documentation ',name (expand-documentation ,@body) 'method-combination))

(defmacro document-setf-expander (name &body body)
  `(ensure-documentation ',name (expand-documentation ,@body) 'setf))

(defmacro document-structure (name &body body)
  (assert (typep (find-class name) 'structure-class))
  `(ensure-documentation ',name (expand-documentation ,@body) 'structure))

(defmacro document-type (name &body body)
  (typep nil name)
  `(ensure-documentation ',name (expand-documentation ,@body) 'type))

(defmacro document-variable (name &body body)
  `(ensure-documentation ',name (expand-documentation ,@body) 'variable))

(macrolet ((define-multi-documenter (name documenter)
             `(defmacro ,name (names &body body)
                (alexandria:with-gensyms (documentation)
                  `(let ((,documentation (expand-documentation ,@body)))
                     ,@(loop for name in names
                             collect
                             `(,',documenter ,name ,documentation)))))))
  (define-multi-documenter document-compiler-macros document-compiler-macro)
  (define-multi-documenter document-functions document-function)
  (define-multi-documenter document-method-combinations document-method-combination)
  (define-multi-documenter document-setf-expanders document-setf-expander)
  (define-multi-documenter document-structures document-structure)
  (define-multi-documenter document-types document-type)
  (define-multi-documenter document-variables document-variable))
