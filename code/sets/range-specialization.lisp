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
  (if (zerop step)
      (if (= start end)
          (make-instance 'contiguous-range :start start :end start)
          (error "Bad step size 0 for range with start ~d and end ~d" start end))
      (let ((steps (truncate (- end start) step)))
        (if (= steps 0)
            (make-instance 'contiguous-range :start start :end start)
            (let ((congruent-end (+ start (* step steps))))
              (let ((step (abs step))
                    (start (min start congruent-end))
                    (end (max start congruent-end)))
                (if (= 1 step)
                    (make-instance 'contiguous-range :start start :end end)
                    (make-instance 'strided-range :start start :step step :end end))))))))

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
