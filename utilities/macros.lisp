;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/utilities/macros
  (:use :closer-common-lisp :alexandria :iterate)
  (:export
   #:define-class))

(in-package :petalisp/utilities/macros)

(defmacro define-class (class-name superclass-names slot-specifiers &rest class-options)
  "Defines a class using DEFCLASS, but defaulting to a :READER of SLOT-NAME
  and an :INITARG of :SLOT-NAME. Additionally, defines a CLASS-NAME?
  predicate."
  (flet ((extend-slot-specifier (slot-specifier)
           (destructuring-bind (slot-name &rest plist)
               (ensure-list slot-specifier)
             (let ((initarg (unless (getf plist :initarg)
                              (list :initarg (make-keyword slot-name))))
                   (reader (unless (or (getf plist :reader)
                                       (getf plist :accessor)
                                       (getf plist :writer))
                             (list :reader slot-name))))
               `(,slot-name ,@initarg ,@reader ,@plist)))))
    `(progn
       (defclass ,class-name ,superclass-names
         ,(mapcar #'extend-slot-specifier slot-specifiers)
         ,@class-options)
       (declaim (inline ,(symbolicate class-name "?")))
       (defun ,(symbolicate class-name "?") (x)
         (typep x ',class-name)))))

(defmacro do-sequence ((var sequence &optional result) &body body)
  "Iterate over the elements of SEQUENCE."
  (check-type var symbol)
  (once-only (sequence)
    `(block nil
       (map nil #'(lambda (,var) (tagbody ,@body)) ,sequence)
       (let ((,var nil)) (declare (ignorable var)) ,result))))

(defmacro funcall-form (function-designator &rest arguments)
  (if (and (consp function-designator)
           (eq (car function-designator) 'quote))
      `(,(cadr function-designator) ,@arguments)
      `(funcall ,function-designator ,@arguments)))
