;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

(defclass form-builder ()
  ((%form :initarg :form :reader form)
   (%tail :initarg :tail :accessor tail)))

(defun make-form-builder (form &optional (tail (last form)))
  (make-instance 'form-builder
    :form form
    :tail tail))

(defun push-form (form-builder form &optional tail)
  (let ((cons (cons form nil)))
    (setf (cdr (tail form-builder)) cons)
    (setf (tail form-builder)
          (if (consp tail) tail cons))
    form))

(defun push-binding (form-builder variables values-form)
  (let ((tail `((declare (ignorable ,@variables)))))
    (push-form
     form-builder
     (if (= 1 (length variables))
         `(let ((,(first variables) ,values-form)) . ,tail)
         `(multiple-value-bind ,variables ,values-form . ,tail))
     tail)))

(defun make-loop-form-builder (var type start step end)
  (make-form-builder
   `(loop for ,var ,type from ,start by ,step below ,end do)))

(defun make-reduction-form-builder (var type min max reduction-spec)
  (let* ((tail (cons nil nil)))
    (make-form-builder
     `(labels ((divide-and-conquer (min max)
                 (declare (type ,type min max))
                 (if (= min max)
                     (let ((,var min)) . ,tail)
                     (let* ((size (- max min))
                            (mid (+ min (floor size 2))))
                       (multiple-value-call ,(make-reduction-lambda reduction-spec)
                         (divide-and-conquer min mid)
                         (divide-and-conquer (1+ mid) max))))))
        (divide-and-conquer ,min ,max))
     tail)))

(defun iota-map (function n)
  (loop for i below n collect (funcall function i)))

;;; REDUCTION-SPEC is an (operator . arity) alist.
(defun make-reduction-lambda (reduction-spec)
  (let* ((k (loop for elt in reduction-spec sum (cdr elt)))
         (left-variables (iota-map #'left-variable k))
         (right-variables (iota-map #'right-variable k))
         (result-variables (iota-map #'result-variable k))
         (form-builder
           (make-form-builder `(lambda ,(append left-variables right-variables)))))
    (loop for (operator . arity) in reduction-spec
          for start = 0 then end
          for end = arity then (+ end arity) do
            (push-binding
             form-builder
             (subseq result-variables start end)
             `(,operator ,@(subseq left-variables start end)
                         ,@(subseq right-variables start end))))
    (push-form form-builder `(values ,@result-variables))
    (form form-builder)))
