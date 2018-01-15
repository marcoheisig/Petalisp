;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/utilities/macros
  (:use :closer-common-lisp :alexandria :iterate)
  (:export
   #:λ
   #:define-class))

(in-package :petalisp/utilities/macros)

(defmacro λ (&rest symbols-and-expr)
  "A shorthand notation for lambda expressions, provided your Lisp
implementation is able to treat λ as a character.

Examples:
 (λ x x) -> (lambda (x) x)
 (λ x y (+ x y)) -> (lambda (x y) (+ x y))"
  `(lambda ,(butlast symbols-and-expr)
     ,@(last symbols-and-expr)))

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

(defmacro maybe-ignore-errors (&body forms)
  `(restart-case (progn ,@forms)
    (ignore ()
      :report "Ignore the problem and continue.")))

(defmacro define-task-queue (thread-name)
  (check-type thread-name symbol)
  (let ((queue-name (symbolicate thread-name "-TASK-QUEUE")))
    `(progn
       (defvar ,queue-name (make-queue))
       (defvar ,thread-name
         (make-thread
          (λ (loop
               (ignore-errors
                (maybe-ignore-errors
                  (funcall (dequeue ,queue-name))))))
          :name ,(string thread-name)))
       (defun ,(symbolicate "RUN-IN-" thread-name) (thunk)
         (enqueue thunk ,queue-name)
         (values)))))

(defmacro funcall-form (function-designator &rest arguments)
  (if (and (consp function-designator)
           (eq (car function-designator) 'quote))
      `(,(cadr function-designator) ,@arguments)
      `(funcall ,function-designator ,@arguments)))
