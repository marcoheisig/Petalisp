;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/utilities/function-lambda-lists
  (:use :closer-common-lisp :alexandria)
  (:export
   #:function-lambda-list
   #:lambda-list-arity
   #:function-arity))

(in-package :petalisp/utilities/function-lambda-lists)

;;; this function is derived from CLOCC, credit goes to Sam Steingold
(defun function-lambda-list (function)
  "Return the lambda list of FUNCTION. Signal an error if the
implementation has no means to determine the function's lambda list."
  #+allegro (excl:arglist function)
  #+clisp (sys::arglist function)
  #+(or cmu scl)
  (let ((f (coerce function 'function)))
    (etypecase f
      (standard-generic-function (pcl:generic-function-lambda-list f))
      (eval:interpreted-function (eval:interpreted-function-arglist f))
      (function (read-from-string (kernel:%function-arglist f)))))
  #+cormanlisp (ccl:function-lambda-list
                (typecase function
                  (symbol (fdefinition function))
                  (t function)))
  #+ccl (ccl:arglist function)
  #+gcl (let ((f (etypecase function
                   (symbol function)
                   (function (si:compiled-function-name function)))))
          (get f 'si:debug))
  #+lispworks (lw:function-lambda-list function)
  #+lucid (lcl:arglist function)
  #+sbcl (sb-introspect:function-lambda-list function)
  #-(or allegro clisp cmu scl cormanlisp gcl lispworks lucid sbcl ccl)
  (error "Not implemented."))

(defun lambda-list-arity (lambda-list)
  "Return two values:
   1. the number of mandatory arguments
   2. the maximal number of permissible arguments"
  (let ((mandatory-arguments 0)
        (max-arguments 0)
        (upper-bound? t)
        (mandatory-increment 1)
        (max-increment 1))
    (declare
     (type (integer 0 #.call-arguments-limit)
           mandatory-arguments max-arguments
           mandatory-increment max-increment)
     (type boolean upper-bound?))
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
        ((&rest &allow-other-keys)
         (setf max-increment 0)
         (setf mandatory-increment 0)
         (setf upper-bound? nil))
        (t
         (incf mandatory-arguments mandatory-increment)
         (incf max-arguments max-increment))))
    (if upper-bound?
        (values mandatory-arguments max-arguments)
        (values mandatory-arguments call-arguments-limit))))

(defun function-arity (function)
  "Return two values:
   1. the number of mandatory arguments
   2. the maximal number of permissible arguments"
  (lambda-list-arity (function-lambda-list function)))
