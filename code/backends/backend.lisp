;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric compute-asynchronously (data-structures backend)
  (:documentation
   "Instruct BACKEND to compute all given data structures asynchronously, i.e.,
to convert each object to an immediate value or a non-permuting reference
to an immediate value.  Return an object that can be passed to
LPARALLEL.PROMISE:FORCE to block until the task is complete."))

(defgeneric compute-synchronously (data-structures backend)
  (:documentation
   "Instruct backend to replace all given data structures with immediate
values or non-permuting references to immediate values."))

(defgeneric compute-immediates (data-structures backend))

(defgeneric overwrite-instance (instance substitute))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass backend ()
  ((%scheduler-queue :reader scheduler-queue
                     :initform (lparallel.queue:make-queue))
   (%scheduler-thread :accessor scheduler-thread))
  (:documentation
   "A backend is an abstraction over a set of hardware
resources. All handling of kernels --- such as performance analysis,
compilation and execution --- is done in the context of a particular
backend."))

(defmethod initialize-instance :after ((backend backend) &key &allow-other-keys)
  (setf (scheduler-thread backend)
        (bt:make-thread
         (lambda ()
           (loop
             (funcall
              (lparallel.queue:pop-queue (scheduler-queue backend)))))
         :name (format nil "~A scheduler thread"
                       (class-name (class-of backend))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod compute-asynchronously ((data-structures list) (backend backend))
  (let ((promise (lparallel.promise:promise)))
    (lparallel.queue:push-queue
     (lambda ()
       (lparallel.promise:fulfill promise
         (compute-synchronously data-structures backend)))
     (scheduler-queue backend))
    promise))

(defmethod compute-synchronously ((data-structures list) (backend backend))
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
    immediates))

(defmethod overwrite-instance ((instance reference) (substitute reference))
  (reinitialize-instance instance
    :transformation (transformation substitute)
    :inputs (inputs substitute)))

(defmethod overwrite-instance (instance (substitute reference))
  (change-class instance (class-of substitute)
    :transformation (transformation substitute)
    :inputs (inputs substitute)))

(defmethod overwrite-instance (instance (substitute immediate))
  (change-class instance (class-of substitute)
    :storage (storage substitute)))
