;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;; The generic function MAKE-REQUEST is invoked by the SCHEDULE function
;;; to prepare a list of lazy arrays for asynchronous execution.  This
;;; function must return an opaque object that can be supplied to the
;;; various REQUEST- functions.
;;;
;;; Backend developers can rely on the fact that the second argument is
;;; always a list of collapsed lazy arrays, i.e., lazy arrays whose shape
;;; consists only of ranges with a start of zero and a stride of one.
(defgeneric make-request (backend lazy-arrays))

;;; Returns whether the supplied object is a request.
(defgeneric requestp (request))

;;; Returns the backend on which the request shall be executed.
(defgeneric request-backend (request))

;;; Returns the lazy arrays whose computation has been requested.
(defgeneric request-lazy-arrays (request))

;;; Block until REQUEST is finished.
(defgeneric request-wait (request))

;;; Signal that the supplied REQUEST is finished.
(defgeneric request-finish (request))

;;; Check whether the supplied REQUEST has already been computed.
(defgeneric request-finishedp (request))

(defclass request ()
  ((%backend
    :initarg :backend
    :initform (alexandria:required-argument :backend)
    :reader request-backend)
   (%lazy-arrays
    :initarg :lazy-arrays
    :initform (alexandria:required-argument :lazy-arrays)
    :type list
    :reader request-lazy-arrays)
   (%lock
    :initform (bordeaux-threads:make-lock "Petalisp Request Lock")
    :reader request-lock)
   (%cvar
    :initform (bordeaux-threads:make-condition-variable)
    :reader request-cvar)
   (%finishedp
    :initform nil
    :reader request-finishedp
    :writer (setf %request-finishedp))))

(defmethod print-object ((request request) stream)
  (print-unreadable-object (request stream :type t :identity t)
    (format stream "~S ~A" :finishedp (request-finishedp request))))

(defmethod requestp ((object t))
  nil)

(defmethod requestp ((request request))
  t)

(defmethod make-request (backend lazy-arrays)
  (make-instance 'request
    :backend backend
    :lazy-arrays lazy-arrays))

(defmethod request-wait ((request request))
  (unless (request-finishedp request)
    (let ((lock (request-lock request))
          (cvar (request-cvar request)))
      (bordeaux-threads:with-lock-held (lock)
        (unless (request-finishedp request)
          (bordeaux-threads:condition-wait cvar lock))))))

(defmethod request-finish ((request request))
  (unless (request-finishedp request)
    (let ((lock (request-lock request))
          (cvar (request-cvar request)))
      (bordeaux-threads:with-lock-held (lock)
        (setf (%request-finishedp request) t)
        (bordeaux-threads:condition-notify cvar)))))
