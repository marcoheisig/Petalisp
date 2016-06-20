;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Petalisp / Common Lisp interaction

(in-package :petalisp)

(defparameter *petalisp-macrolet-definitions*
  (loop for op in *petalisp-operators* collect
        `(,op (&whole form &rest rest)
              (declare (ignore rest))
              (cl-to-petalisp form))))

(defun cl-to-petalisp (form)
  (destructuring-bind (op &rest args) form
    (ecase (length args)
      (1 (make-instance op :arg (first args)))
      (2 (make-instance op :arg1 (first args) :arg2 (second args))))))

(defmacro petalisp (&body body)
  `(macrolet ,*petalisp-macrolet-definitions* ,@body))
