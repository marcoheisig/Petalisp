;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass scheduler-queue-mixin ()
  ((%scheduler-queue :initform (lparallel.queue:make-queue) :reader scheduler-queue)
   (%scheduler-thread :accessor scheduler-thread)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod initialize-instance :after
    ((scheduler-queue-mixin scheduler-queue-mixin) &key &allow-other-keys)
  (setf (scheduler-thread scheduler-queue-mixin)
        (bt:make-thread
         (lambda ()
           (loop
             (funcall
              (lparallel.queue:pop-queue (scheduler-queue scheduler-queue-mixin)))))
         :name (format nil "~A scheduler thread" (class-name (class-of scheduler-queue-mixin))))))

(defmethod schedule-on-backend
    ((data-structures list)
     (scheduler-queue-mixin scheduler-queue-mixin))
  (let ((promise (lparallel.promise:promise)))
    (lparallel.queue:push-queue
     (lambda ()
       (lparallel.promise:fulfill promise
         (compute-on-backend data-structures scheduler-queue-mixin)))
     (scheduler-queue scheduler-queue-mixin))
    promise))
