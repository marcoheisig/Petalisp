;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

(defclass reduction-block (basic-block)
  ((%reduction-min :initarg :reduction-min :reader reduction-min)
   (%reduction-max :initarg :reduction-max :reader reduction-max)
   (%reduction-var :initarg :reduction-var :reader reduction-var)
   (%reduction-var-type :initarg :reduction-var-type :reader reduction-var-type)
   (%reduction-spec :initarg :reduction-spec :reader reduction-spec)
   (%result-symbols :accessor result-symbols)))

(defun make-reduction-block (&rest args)
  (apply #'make-instance 'reduction-block args))

(defmethod form :around ((reduction-block reduction-block))
  `(labels ((divide-and-conquer (min max)
              (declare (type ,(reduction-var-type reduction-block) min max))
              (if (= min max)
                  (let ((,(reduction-var reduction-block) min))
                    ,(call-next-method))
                  (let* ((size (- max min))
                         (mid (+ min (floor size 2))))
                    (multiple-value-call
                        ,(make-reduction-lambda (reduction-spec reduction-block))
                      (divide-and-conquer min mid)
                      (divide-and-conquer (1+ mid) max))))))
     (divide-and-conquer
      ,(reduction-min reduction-block)
      ,(reduction-max reduction-block))))

(defmethod tail-form ((reduction-block reduction-block))
  `(values ,@(result-symbols reduction-block)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Assembling the Reduction Lambda

(defclass reduction-lambda-block (lambda-block)
  ((%result-symbols :accessor result-symbols)))

(defun make-reduction-lambda-block (&rest args)
  (apply #'make-instance 'reduction-lambda-block args))

(defmethod tail-form ((reduction-lambda-block reduction-lambda-block))
  `(values ,@(result-symbols reduction-lambda-block)))

(defun make-reduction-lambda (reduction-spec)
  (let* ((k (loop for elt in reduction-spec sum (cdr elt)))
         (left-symbols (loop repeat k collect (gensym)))
         (right-symbols (loop repeat k collect (gensym)))
         (result-symbols '())
         (basic-block
           (make-reduction-lambda-block :lambda-list (append left-symbols right-symbols))))
    (loop for (operator . arity) in reduction-spec
          for start = 0 then end
          for end = arity then (+ end arity)
          for left = (subseq left-symbols start end)
          for right = (subseq right-symbols start end) do
            (let ((form (if (symbolp operator)
                            `(,operator ,@left ,@right)
                            `(funcall (aref functions ,operator) ,@left ,@right))))
              (loop for i below arity
                    do (push (value-symbol i form basic-block) result-symbols))))
    (setf (result-symbols basic-block) (nreverse result-symbols))
    (form basic-block)))
