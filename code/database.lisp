;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;; knowledge on Lisp functions for type inference and optimization

(in-package :petalisp)

;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;;  type inference
;;; _________________________________________________________________

(defmethod result-type ((f t) &rest types)
  (declare (ignore types))
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

(defun numeric-supertype (&rest args)
  (declare (dynamic-extent args))
  (match args
    ((list (or (and t1 (list 'complex x)) x)
           (or (and t2 (list 'complex y)) y))
     (unless (or t1 t2) (fail))
     (let ((base-type (numeric-supertype x y)))
       (case base-type
         ((single-float) '(complex-single-float))
         ((double-float) '(complex double-float))
         ((t)            '(complex real))
         (otherwise      `(complex ,base-type)))))
    ((list 'nil x) x)
    ((list x 'nil) x)
    ((list 'single-float 'single-float) 'single-float)
    ((list 'single-float 'double-float) 'double-float)
    ((list 'double-float 'single-float) 'double-float)
    ((list 'double-float 'double-float) 'double-float)
    (otherwise t)))

(defmethod result-type ((f (eql #'+)) &rest types)
  (declare (dynamic-extent types))
  (reduce #'numeric-supertype types))

(defmethod result-type ((f (eql #'-)) &rest types)
  (declare (dynamic-extent types))
  (reduce #'numeric-supertype types))

(defmethod result-type ((f (eql #'*)) &rest types)
  (declare (dynamic-extent types))
  (reduce #'numeric-supertype types))

(defmethod result-type ((f (eql #'/)) &rest types)
  (declare (dynamic-extent types))
  (reduce #'numeric-supertype types))
