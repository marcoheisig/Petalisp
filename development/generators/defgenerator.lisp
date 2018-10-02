;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

(defun parse-defwrapper-lambda-list (lambda-list)
  (multiple-value-bind (required optional rest keyword)
      (parse-ordinary-lambda-list lambda-list)
    (when (not rest)
      (setf rest (gensym "REST")))
    (values
     ;; The lambda list, potentially augmented with a &rest parameter.
     `(,@required
       ,@(unless (null optional)
           `(&optional ,@optional))
       &rest ,rest
       ,@(unless (null keyword)
           `(&key
             ,@(loop for ((keyword-name name) init suppliedp) in keyword
                     collect `((,keyword-name ,name) ,init
                               ,@(when suppliedp `(,suppliedp)))))))
     ;; Forwarded arguments, i.e. all required arguments, optional
     ;; arguments, and the &REST argument.
     `(,@required
       ,@(loop for (name nil nil) in optional
               collect name)
       ,rest)
     ;; Ignorable symbols, i.e., all keyword arguments, since they are
     ;; transferred as part of the &REST argument.
     (loop for ((nil name) nil nil) in keyword
           collect name))))

;;; DEFWRAPPER is similar to DEFUN, except that the function body is
;;; wrapped in a local function CALL-WITH-SAME-ARGUMENTS, that permits
;;; calling another function with the same arguments as this function.
;;;
;;; You may now wonder why we don't simply use a function with a single
;;; &REST argument, so that we can forward the arguments using APPLY.  The
;;; reason is that we want to keep the lambda list of the wrapper as
;;; descriptive as possible.
(defmacro defwrapper (name lambda-list &body body)
  (multiple-value-bind (wrapper-lambda-list arguments ignorable)
      (parse-defwrapper-lambda-list lambda-list)
    (multiple-value-bind (body-forms declarations documentation)
        (parse-body body)
      `(defun ,name ,wrapper-lambda-list
         ,@(when documentation (list documentation))
         ,@declarations
         (declare (ignorable ,@ignorable))
         (flet ((call-with-same-arguments (function)
                  (apply function ,@arguments)))
           ,@body-forms)))))

(defmacro defgenerator (name lambda-list &body body)
  (check-type name symbol)
  (let ((make-generator (symbolicate '#:make- name '#:-generator))
        (generate (symbolicate '#:generate- name)))
    `(progn
       (defun ,make-generator ,lambda-list
         ,@body)
       (defwrapper ,generate ,lambda-list
         (funcall
          (call-with-same-arguments #',make-generator))))))
