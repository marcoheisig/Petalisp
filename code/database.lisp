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
  (declare (dynamic-extent types))
  (labels ((upgrade (&rest args)
             (declare (dynamic-extent args))
             (match args
               ((list (or (and t1 (list 'complex x)) x)
                      (or (and t2 (list 'complex y)) y))
                (unless (or t1 t2) (fail))
                (let ((base-type (upgrade x y)))
                  (cond
                    ;; the first two cases avoid consing
                    ((and t1 (eq base-type x)) t1)
                    ((and t2 (eq base-type y)) t2)
                    (t `(complex ,base-type)))))
               ((list 'nil x) x)
               ((list x 'nil) x)
               ((list 'single-float 'single-float) 'single-float)
               ((list 'single-float 'double-float) 'double-float)
               ((list 'double-float 'single-float) 'double-float)
               ((list 'double-float 'double-float) 'double-float)
               (otherwise t))))
    (reduce #'upgrade types)))

(defmethod result-type ((f (eql #'+)) &rest types)
  (declare (dynamic-extent types))
  (apply #'numeric-supertype types))
