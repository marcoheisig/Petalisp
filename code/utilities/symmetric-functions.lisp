;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;; This file introduces the generic function class
;;; SYMMETRIC-GENERIC-FUNCTION for functions of exactly two arguments,
;;; where (f A B) is equivalent to (f B A).  In particular, we solve the
;;; problem that when a method is defined on ((A A-class) (B B-class)),
;;; there should also be a corresponding method ((B B-class) (A A-class)).
;;; When such an asymmetric method is defined on a symmetric generic
;;; function, we automatically derive and add the other method via
;;; introspection.
;;;
;;; A minor problem that arises with this approach is that the generated
;;; function is slower than its counterpart.  To avoid this minor
;;; performance penalty, users can use DEFINE-OPTIMIZED-SYMMETRIC-METHOD
;;; instead of DEFMETHOD for the asymmetric case.
;;;
;;; Example:
;;;
;;; (defgeneric generic-* (a b)
;;;  (:generic-function-class symmetric-generic-function))
;;;
;;; ;; Add a single symmetric method.
;;; (defmethod generic-* ((a number) (b number))
;;;   (* a b))
;;;
;;; ;; Add methods for (integer string) and (string integer).  When used this way,
;;; ;; the (string integer) method is slightly faster than its counterpart.
;;; (defmethod generic-* ((string string) (integer integer))
;;;   (apply 'concatenate 'string (make-list integer :initial-element string)))
;;;
;;; ;; Add methods for (float integer) and (integer float) and ensure that
;;; ;; both are equally fast.
;;; (define-optimized-symmetric-method generic-* ((float float) (integer integer))
;;;   (* float integer))

(defclass symmetric-generic-function (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class))

(defmethod shared-initialize :after
    ((gf symmetric-generic-function) slot-names &key &allow-other-keys)
  (let ((lambda-list (generic-function-lambda-list gf)))
    (unless (and (= 2 (length lambda-list))
                 (null (intersection lambda-list lambda-list-keywords)))
      (error "Not a symmetric function lambda list: ~S" lambda-list))))

(defvar *symmetric-methods* '())

(defun call-avoiding-symmetric-method-recursion (gf-name qualifiers specializers thunk)
  (destructuring-bind (s1 s2) specializers
    (unless (eql s1 s2)
      (let ((key-1 (list gf-name qualifiers (identity specializers)))
            (key-2 (list gf-name qualifiers (reverse specializers))))
        (unless (or (find key-1 *symmetric-methods* :test #'equal)
                    (find key-2 *symmetric-methods* :test #'equal))
          (let ((*symmetric-methods* (list* key-1 *symmetric-methods*)))
            (funcall thunk)))))))

(defmacro avoiding-symmetric-method-recursion ((gf qualifiers specializers) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (call-avoiding-symmetric-method-recursion
      ,gf ,qualifiers ,specializers
      (lambda () ,@body))))

(defmethod add-method :after ((generic-function symmetric-generic-function)
                              (method standard-method))
  (let ((qualifiers (method-qualifiers method))
        (specializers (method-specializers method))
        (function (method-function method)))
    (avoiding-symmetric-method-recursion
        ((generic-function-name generic-function) qualifiers specializers)
      (add-method generic-function
                  (make-instance 'standard-method
                    :qualifiers (method-qualifiers method)
                    :specializers (reverse (method-specializers method))
                    :lambda-list (reverse (method-lambda-list method))
                    :function (lambda (args methods)
                                (funcall function (reverse args) methods)))))))

(defun parse-defmethod (args)
  (labels ((parse (qualifiers rest)
             (if (consp (car rest))
                 (values (reverse qualifiers) (car rest) (cdr rest))
                 (parse (cons (car rest) qualifiers) (cdr rest)))))
    (parse '() args)))

(defun intern-specializer (specializer)
  (trivia:ematch specializer
    ((type symbol) (find-class specializer))
    ((list 'eql object) (intern-eql-specializer object))
    ((type class) specializer)))

(defmacro define-optimized-symmetric-method (name &rest args)
  (multiple-value-bind (qualifiers lambda-list body)
      (parse-defmethod args)
    (let ((specializers (mapcar #'intern-specializer (extract-specializer-names lambda-list))))
      (if (eql (first specializers) (second specializers))
          `(defmethod ,name ,@qualifiers ,lambda-list ,@body)
          `(avoiding-symmetric-method-recursion (',name ',qualifiers ',specializers)
             (defmethod ,name ,@qualifiers ,(identity lambda-list) ,@body)
             (defmethod ,name ,@qualifiers ,(reverse lambda-list) ,@body))))))

