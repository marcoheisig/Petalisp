;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass contiguous-range (range)
  ((%start :initarg :start :reader range-start)
   (%end :initarg :end :reader range-end)))

(defclass strided-range (range)
  ((%start :initarg :start :reader range-start)
   (%step :initarg :step :reader range-step)
   (%end :initarg :end :reader range-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod range-step ((range contiguous-range))
  1)

(defmethod make-range ((start integer) (step integer) (end integer))
  (make-instance 'strided-range :start start :step step :end end))

(defmethod make-range ((start integer) (step (eql 1)) (end integer))
  (make-instance 'contiguous-range :start start :end end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Intersection of Ranges

(defmethod set-intersection ((range-1 contiguous-range) (range-2 contiguous-range))
  (let ((start (max (range-start range-1)
                    (range-start range-2)))
        (end (min (range-end range-1)
                  (range-end range-2))))
    (when (<= start end)
      (make-range start 1 end))))
