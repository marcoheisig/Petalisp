;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

(defun function-lambda-list (function &optional (errorp t))
  "Return the lambda list of FUNCTION, or an approximation thereof."
  (let ((arglist (trivial-arguments:arglist function)))
    (if (eq arglist :unknown)
        (if errorp
            (error "Cannot determine the lambda list of ~S." function)
            '(&rest anything))
        arglist)))

(defun lambda-list-arity (lambda-list)
  "Return two values:
   1. the number of mandatory arguments
   2. the maximal number of permissible arguments"
  (let ((mandatory-arguments 0)
        (max-arguments 0)
        (upper-bound-p t)
        (mandatory-increment 1)
        (max-increment 1))
    (declare (type (integer 0 (#.call-arguments-limit))
                   mandatory-arguments max-arguments
                   mandatory-increment max-increment)
             (type boolean upper-bound-p))
    (dolist (item lambda-list)
      (case item
        ((&key)
         (setf max-increment 2)
         (setf mandatory-increment 0))
        ((&optional)
         (setf max-increment 1)
         (setf mandatory-increment 0))
        ((&aux)
         (setf max-increment 0)
         (setf mandatory-increment 0))
        ((&rest &allow-other-keys #+ccl ccl::&lexpr #+sbcl sb-int:&more)
         (setf max-increment 0)
         (setf mandatory-increment 0)
         (setf upper-bound-p nil))
        (t
         (incf mandatory-arguments mandatory-increment)
         (incf max-arguments max-increment))))
    (if upper-bound-p
        (values mandatory-arguments max-arguments)
        (values mandatory-arguments (1- call-arguments-limit)))))

(defun function-arity (function)
  (lambda-list-arity
   (function-lambda-list function nil)))
