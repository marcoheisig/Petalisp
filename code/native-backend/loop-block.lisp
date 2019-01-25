;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defclass loop-block (basic-block)
  ((%start :initarg :start :reader loop-start)
   (%step :initarg :step :reader loop-step)
   (%end :initarg :end :reader loop-end)
   (%var :initarg :var :reader loop-var)
   (%var-type :initarg :var-type :reader loop-var-type)
   (%size-bits :initarg :size-bits :reader loop-size-bits)
   (%step-bits :initarg :step-bits :reader loop-step-bits)))

(defun make-loop-block (&rest args)
  (apply #'make-instance 'loop-block args))

(defmethod form :around ((loop-block loop-block))
  (with-accessors ((start loop-start)
                   (step loop-step)
                   (end loop-end)
                   (type loop-var-type)
                   (var loop-var)
                   (size-bits loop-size-bits)
                   (step-bits loop-step-bits)) loop-block
    (let ((step (if (= 1 step-bits) 1 step)))
      (if (= 1 size-bits)
          `(let ((,var (the ,type ,start)))
             ,(call-next-method))
          `(loop for ,var ,type from ,start by ,step to ,end
                 do ,(call-next-method))))))
