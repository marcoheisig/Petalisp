;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric compute-on-backend (data-structures backend))

(defgeneric schedule-on-backend (data-structures backend))

(defgeneric compute-immediates (data-structures backend))

(defgeneric overwrite-instance (instance replacement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass backend ()
  ((%scheduler-queue :initform (lparallel.queue:make-queue) :reader scheduler-queue)
   (%scheduler-thread :accessor scheduler-thread)))

(defmethod initialize-instance :after ((backend backend) &key &allow-other-keys)
  (setf (scheduler-thread backend)
        (bt:make-thread
         (lambda ()
           (loop
             (funcall
              (lparallel.queue:pop-queue (scheduler-queue backend)))))
         :name (format nil "~A scheduler thread" (class-name (class-of backend))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod compute-on-backend ((data-structures list) (backend backend))
  (let* ((collapsing-transformations
           (mapcar (compose #'collapsing-transformation #'shape)
                   data-structures))
         (immediates
           (compute-immediates
            (mapcar #'transform data-structures collapsing-transformations)
            backend)))
    (mapcar (lambda (data-structure collapsing-transformation immediate)
              (overwrite-instance
               data-structure
               (make-reference
                immediate
                (shape data-structure)
                collapsing-transformation)))
            data-structures collapsing-transformations immediates)
    (values-list (mapcar #'storage immediates))))

(defmethod schedule-on-backend ((data-structures list) (backend backend))
  (let ((promise (lparallel.promise:promise)))
    (lparallel.queue:push-queue
     (lambda ()
       (lparallel.promise:fulfill promise
         (compute-on-backend data-structures backend)))
     (scheduler-queue backend))
    promise))

(defmethod overwrite-instance ((instance immediate) (replacement immediate))
  (change-class instance (class-of replacement)
    :storage (storage replacement)))

(defmethod overwrite-instance ((instance reference) (replacement reference))
  (reinitialize-instance instance
    :transformation (transformation replacement)
    :inputs (inputs replacement)))

(defmethod overwrite-instance (instance (replacement reference))
  (change-class instance (class-of replacement)
    :transformation (transformation replacement)
    :inputs (inputs replacement)))

(defmethod overwrite-instance (instance (replacement immediate))
  (change-class instance (class-of replacement)
    :storage (storage replacement)))
