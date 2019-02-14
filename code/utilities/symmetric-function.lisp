;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

(defclass symmetric-function (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Argument checking

(defun validate-symmetric-method (method all-methods)
  (let ((specializers (method-specializers method)))
    (unless (= 2 (length (method-specializers method)))
      (error "Not a binary method: ~S" method))
    (unless (or (eql (first specializers) (second specializers))
                (find (reverse specializers) all-methods
                      :test #'equal
                      :key #'method-specializers))
      (error "The method ~S has no symmetric counterpart" method))))

(defmethod compute-applicable-methods-using-classes
    ((generic-function symmetric-function) classes)
  (declare (ignore classes))
  (let ((methods (generic-function-methods generic-function)))
    (loop for method in methods do
      (validate-symmetric-method method methods)))
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenience Macro DEFINE-METHOD-PAIR

(defun parse-defmethod (args)
  (labels ((parse (qualifiers rest)
             (if (consp (car rest))
                 (values (reverse qualifiers) (car rest) (cdr rest))
                 (parse (cons (car rest) qualifiers) (cdr rest)))))
    (parse '() args)))

(defmacro define-method-pair (name &rest args)
  (multiple-value-bind (qualifiers lambda-list body)
      (parse-defmethod args)
    `(progn
       (defmethod ,name ,@qualifiers ,(identity lambda-list) ,@body)
       (defmethod ,name ,@qualifiers ,(reverse lambda-list) ,@body))))
