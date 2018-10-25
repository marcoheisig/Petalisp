;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

(defclass loop-block (basic-block)
  ((%loop-start :initarg :loop-start :reader loop-start)
   (%loop-step :initarg :loop-step :reader loop-step)
   (%loop-end :initarg :loop-end :reader loop-end)
   (%loop-var :initarg :loop-var :reader loop-var)
   (%loop-var-type :initarg :loop-var-type :reader loop-var-type)))

(defun make-loop-block (&rest args)
  (apply #'make-instance 'loop-block args))

(defmethod form :around ((loop-block loop-block))
  `(loop for ,(loop-var loop-block) ,(loop-var-type loop-block)
         from ,(loop-start loop-block)
         by ,(loop-step loop-block)
           to ,(loop-end loop-block)
         do ,(call-next-method)))
