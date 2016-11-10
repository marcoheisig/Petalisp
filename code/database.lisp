;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;; knowledge on Lisp functions for type inference and optimization

(in-package :petalisp)

;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;;  type inference
;;; _________________________________________________________________

(defmethod result-type ((f t) &rest types)
  (declare (ignore arguments))
  t)

(defmethod element-type ((array array))
  (array-element-type array))

(defmethod initialize-instance :after ((instance application) &key &allow-other-keys)
  (setf (slot-value instance 'element-type)
        (apply #'result-type (operator instance)
               (mapcar #'element-type (predecessors instance)))))

(defmethod initialize-instance :after ((instance reduction) &key &allow-other-keys)
  (setf (slot-value instance 'element-type)
        (element-type (car (predecessors instance)))))

(defun numeric-supertype (&rest types)
  (let ((complex? nil)
        (base-type nil))
    (flet ((upgrade (&rest args)
             (declare (dynamic-extent args))
             (match args
               ((list 'nil x) x)
               ((list x 'nil) x)
               ((list 'single-float 'single-float) 'single-float)
               ((list 'single-float 'double-float) 'double-float)
               ((list 'double-float 'single-float) 'double-float)
               ((list 'double-float 'double-float) 'double-float)
               (otherwise t))))
      (loop for type in types do
        (cond
          ((subtypep type 'complex)
           (setf complex? t)
           (setf base-type (upgrade base-type (cadr type))))
          (t
           (setf base-type (upgrade base-type type))))))
    (if complex? `(complex ,base-type) base-type)))

(defmethod result-type ((f (eql #'+)) &rest types)
  (apply #'numeric-supertype types))
