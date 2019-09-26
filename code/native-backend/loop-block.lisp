;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defclass loop-block (basic-block)
  ((%start :initarg :start :reader loop-start)
   (%step :initarg :step :reader loop-step)
   (%end :initarg :end :reader loop-end)
   (%var :initarg :var :reader loop-var)
   (%info :initarg :info :reader loop-info)))

(defun make-loop-block (&rest args)
  (apply #'make-instance 'loop-block args))

(defmethod form :around ((loop-block loop-block))
  (with-accessors ((start loop-start)
                   (step loop-step)
                   (end loop-end)
                   (var loop-var)
                   (info loop-info)) loop-block
    (ecase info
      (:single
       `(let ((,var (the fixnum ,start)))
          (declare (ignorable ,var))
          ,(call-next-method)))
      (:contiguous
       `(loop for ,var fixnum from ,start to ,end
              do ,(call-next-method)))
      (:strided
       `(loop for ,var fixnum from ,start by ,step to ,end
              do ,(call-next-method))))))
