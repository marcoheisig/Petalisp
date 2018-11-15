;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

;;; Reduction blocks are the most complicated part of this backend.  They
;;; are not linear, but consist of several parts:
;;;
;;; 1. The actual body.  This is where new instructions are appended.  It
;;;    is invoked once for each index in the interval between REDUCTION-MIN
;;;    and REDUCTION-MAX.  In contrast to regular basic blocks, this one
;;;    has to end in a values form, that returns all inputs of all reduce
;;;    instructions in the correct order.
;;;
;;; 2. The reduction lambda.  This is the function that is used to combine
;;;    the multiple values of two invocations to a single one.  It contains
;;;    one instruction for each reduce instruction in the blueprint and
;;;    ends in a values form similar to the one of the body.
;;;
;;; 3. The epilogue.  This code binds the multiple values returned by the
;;;    reduction and contains one or more store instructions that write the
;;;    values to memory locations.  By convention, the successor of the
;;;    reduction block is treated as the epilogue.

(defclass reduction-block (basic-block)
  ((%reduction-start :initarg :reduction-start :reader reduction-start)
   (%reduction-step :initarg :reduction-step :reader reduction-step)
   (%reduction-end :initarg :reduction-end :reader reduction-end)
   (%reduction-var :initarg :reduction-var :reader reduction-var)
   (%reduction-var-type :initarg :reduction-var-type :reader reduction-var-type)
   (%reductions :initarg :reductions :reader reductions)
   (%reduction-symbols :accessor reduction-symbols)))

(defun make-reduction-block (&rest args)
  (apply #'make-instance 'reduction-block args))

(defmethod form :around ((reduction-block reduction-block))
  `(lambda ()
     (let ((start ,(reduction-start reduction-block))
           (step ,(reduction-step reduction-block))
           (end ,(reduction-end reduction-block)))
       (labels ((divide-and-conquer (min max)
                  (declare (type ,(reduction-var-type reduction-block) min max))
                  (if (= min max)
                      (let ((,(reduction-var reduction-block) (+ min (* start step))))
                        (declare (ignorable ,(reduction-var reduction-block)))
                        ,(call-next-method))
                      (let ((mid (+ min (floor (- max min) 2))))
                        (multiple-value-call ,(make-reduction-lambda (reductions reduction-block))
                          (divide-and-conquer min mid)
                          (divide-and-conquer (1+ mid) max))))))
         (divide-and-conquer 0 (/ (- end start) step))))))

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
