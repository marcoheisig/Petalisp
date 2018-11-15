;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

;;; Reduction blocks are the most complicated part of this backend.  They
;;; are not linear, but consist of two parts:
;;;
;;; 1. The actual body.  This is where new instructions are appended.  It
;;;    is invoked once for each index in the interval between REDUCTION-MIN
;;;    and REDUCTION-MAX.  In contrast to regular basic blocks, this one
;;;    ends in a values form, that returns all inputs of all reduce
;;;    instructions in the correct order.
;;;
;;; 2. The reduction lambda.  This is the function that is used to combine
;;;    the multiple values of two invocations to a single one.  It contains
;;;    one instruction for each reduce instruction in the blueprint and
;;;    ends in a values form similar to the one of the body.

(defclass reduction-block (basic-block)
  ((%start :initarg :start :reader reduction-start)
   (%step :initarg :step :reader reduction-step)
   (%end :initarg :end :reader reduction-end)
   (%var :initarg :var :reader reduction-var)
   (%var-type :initarg :var-type :reader reduction-var-type)
   (%size-bits :initarg :size-bits :reader reduction-size-bits)
   (%step-bits :initarg :step-bits :reader reduction-step-bits)
   (%reductions :initarg :reductions :reader reductions)
   (%reduction-symbols :accessor reduction-symbols)))

(defun make-reduction-block (&rest args)
  (apply #'make-instance 'reduction-block args))

(defmethod form :around ((reduction-block reduction-block))
  (with-accessors ((start reduction-start)
                   (step reduction-step)
                   (end reduction-end)
                   (type reduction-var-type)
                   (size-bits reduction-size-bits)
                   (step-bits reduction-step-bits)) reduction-block
    (let ((step (if (= 1 step-bits) 1 step)))
      `(lambda ()
         (labels ((divide-and-conquer (min max)
                    (declare (type ,(reduction-var-type reduction-block) min max))
                    (if (= min max)
                        (let ((,(reduction-var reduction-block) (+ min (* ,start ,step))))
                          (declare (ignorable ,(reduction-var reduction-block)))
                          ,(call-next-method))
                        (let ((mid (+ min (floor (- max min) 2))))
                          (multiple-value-call ,(make-reduction-lambda (reductions reduction-block))
                            (divide-and-conquer min mid)
                            (divide-and-conquer (1+ mid) max))))))
           (divide-and-conquer 0 (/ (- ,end ,start) ,step)))))))

(defmethod tail-form ((reduction-block reduction-block))
  `(values ,@(reduction-symbols reduction-block)))

(defun reduction-arity (reductions)
  (loop for (nil nil . arguments) in reductions
        sum (length arguments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Assembling the Reduction Lambda

(defclass reduction-lambda-block (lambda-block)
  ((%result-symbols :accessor result-symbols)))

(defun make-reduction-lambda-block (&rest args)
  (apply #'make-instance 'reduction-lambda-block args))

(defmethod tail-form ((reduction-lambda-block reduction-lambda-block))
  `(values ,@(result-symbols reduction-lambda-block)))

(defun make-reduction-lambda (reductions)
  (let* ((k (reduction-arity reductions))
         (left-symbols (loop repeat k collect (gensym)))
         (right-symbols (loop repeat k collect (gensym)))
         (result-symbols '())
         (basic-block
           (make-reduction-lambda-block :lambda-list (append left-symbols right-symbols))))
    (loop for (nil operator . arguments) in reductions
          for arity = (length arguments)
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
