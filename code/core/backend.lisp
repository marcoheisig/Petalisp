;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;; This special variable will be bound later, once at least one backend
;;; has been loaded.
(defvar *backend*)

;;; Each call to SCHEDULE creates a request object that is enqueued here
;;; until it is picked up by the scheduler thread.
(defvar *scheduler-queue*
  (lparallel.queue:make-queue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

;;; The generic function DELETE-BACKEND can be invoked by a user to
;;; explicitly disable the supplied backend and free any resources that
;;; might be held by it.
(defgeneric delete-backend (backend))

;;; The generic function BACKEND-COMPUTE is invoked by COMPUTE to force the
;;; evaluation of some lazy arrays in a blocking fashion.  Backend
;;; developers can rely on the fact that the second argument is always a
;;; list of collapsed lazy arrays, i.e., lazy arrays whose shape consists
;;; only of ranges with a start of zero and a stride of one.  This function
;;; must return one Common Lisp array for each of the supplied lazy arrays.
(defgeneric backend-compute (backend lazy-arrays))

;;; Ensures that the supplied REQUEST is computed eventually on BACKEND.
;;; This function is invoked exactly once for each request, by the Petalisp
;;; scheduler thread.
;;;
;;; Backend developers have two options here: The first option is to have
;;; this function block until the scheduled operation is complete, in which
;;; case all scheduled operations will be run sequentially in the order of
;;; the calls to SCHEDULE.  The second option is to perform some additional
;;; task scheduling withing the backend.  In that case the function may
;;; return as soon as the request has been registered in the backend's
;;; scheduling infrastructure.
(defgeneric backend-schedule (backend request))

;;; Returns whether the lazy arrays's collapsed value can be obtained at
;;; O(1) cost, so that we don't have to do any work during COMPUTE or
;;; SCHEDULE.
(defgeneric trivial-lazy-array-p (lazy-array delayed-action))

;;; Obtain the computed value of an lazy array that is trivial in the sense
;;; of TRIVIAL-LAZY-ARRAY-P.
(defgeneric trivial-lazy-array-value (lazy-array delayed-action))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass backend ()
  ())

(defclass deleted-backend ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod delete-backend ((backend backend))
  (change-class backend 'deleted-backend))

;;; In case a more specific method of BACKEND-SCHEDULE signals an error,
;;; handle it by replacing the delayed actions of all lazy arrays of the
;;; request with delayed failures.
(defmethod backend-schedule :around (backend request)
  (handler-case (call-next-method)
    (serious-condition (condition)
      (let ((delayed-failure (make-delayed-failure :condition condition))
            (lazy-arrays (request-lazy-arrays request)))
        (bordeaux-threads:with-recursive-lock-held (*lazy-array-lock*)
          (loop for lazy-array in lazy-arrays do
            (let ((delayed-wait (lazy-array-delayed-action lazy-array)))
              (setf (lazy-array-delayed-action lazy-array)
                    delayed-failure)
              (setf (delayed-wait-delayed-action delayed-wait)
                    delayed-failure))))))))

;;; This default method that implements BACKEND-SCHEDULE by suitably
;;; calling BACKEND-COMPUTE.
(defmethod backend-schedule (backend request)
  (let* ((lazy-arrays (request-lazy-arrays request))
         (temporaries
           (loop for lazy-array in lazy-arrays
                 collect
                 (let* ((delayed-wait (lazy-array-delayed-action lazy-array))
                        (delayed-action (delayed-wait-delayed-action delayed-wait)))
                   (make-lazy-array
                    :shape (lazy-array-shape lazy-array)
                    :ntype (lazy-array-ntype lazy-array)
                    :depth (lazy-array-depth lazy-array)
                    :refcount 999
                    :delayed-action delayed-action))))
         (results (backend-compute backend temporaries)))
    (bordeaux-threads:with-recursive-lock-held (*lazy-array-lock*)
      (loop for lazy-array in lazy-arrays
            for result in results
            do (let* ((delayed-wait (lazy-array-delayed-action lazy-array))
                      (delayed-action (make-delayed-array :storage result)))
                 (setf (lazy-array-delayed-action lazy-array)
                       delayed-action)
                 (setf (delayed-wait-delayed-action delayed-wait)
                       delayed-action))))
    (request-finish request)))

(defmethod trivial-lazy-array-p
    ((lazy-array lazy-array)
     (delayed-action delayed-action))
  nil)

(defmethod trivial-lazy-array-p
    ((lazy-array lazy-array)
     (delayed-array delayed-array))
  (array-value
   (delayed-array-storage delayed-array)))

(defmethod trivial-lazy-array-p
    ((lazy-array lazy-array)
     (delayed-reshape delayed-reshape))
  (let ((transformation (delayed-reshape-transformation delayed-reshape)))
    (and (transformation-invertiblep transformation)
         ;; Check that the transformation is non-permuting.
         (loop for output across (transformation-output-mask transformation)
               for axis from 0
               always (eql output axis))
         (trivial-array-p (delayed-reshape-input delayed-reshape)))))

(defmethod trivial-lazy-array-value
    ((lazy-array lazy-array)
     (delayed-array delayed-array))
  (array-value (delayed-array-storage delayed-array)))

(defmethod trivial-lazy-array-value
    ((lazy-array lazy-array)
     (delayed-reshape delayed-reshape))
  (trivial-array-value (delayed-reshape-input delayed-reshape)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; API

(defmacro with-backend (backend-creation-form &body body)
  `(let ((*backend* ,backend-creation-form))
     (unwind-protect (progn ,@body)
       (delete-backend *backend*))))

(defun compute (&rest arrays)
  (values-list
   (compute-list-of-arrays arrays)))

(defun compute-list-of-arrays (arrays)
  (let* ((backend *backend*)
         (lazy-arrays (remove-if #'trivial-array-p arrays))
         (transformations
           (loop for lazy-array in lazy-arrays
                 collect
                 (collapsing-transformation
                  (lazy-array-shape lazy-array))))
         (collapsed-lazy-arrays
           (mapcar #'transform-lazy-array lazy-arrays transformations))
         ;; Tell the backend to compute the supplied lazy arrays after
         ;; collapsing them.
         (result-arrays (backend-compute backend collapsed-lazy-arrays)))
    ;; Project the results back to the shape of the original lazy arrays,
    ;; and overwrite the original delayed action with that of the projected
    ;; one.  This way we avoid recomputing the same lazy array over and
    ;; over again.
    (bordeaux-threads:with-recursive-lock-held (*lazy-array-lock*)
      (loop for lazy-array in lazy-arrays
            for transformation in transformations
            for collapsed-lazy-array in collapsed-lazy-arrays
            for result-array in result-arrays
            do (setf (lazy-array-delayed-action collapsed-lazy-array)
                     (make-delayed-array :storage result-array))
               (unless (eq lazy-array collapsed-lazy-array)
                 (setf (lazy-array-delayed-action lazy-array)
                       (make-delayed-reshape
                        :transformation transformation
                        :input collapsed-lazy-array)))))
    ;; At this point, all arrays should be trivial.
    (mapcar #'trivial-array-value arrays)))

(defun schedule (&rest arrays)
  (let* ((backend *backend*)
         (lazy-arrays (remove-if #'trivial-array-p arrays))
         (transformations
           (loop for lazy-array in lazy-arrays
                 collect
                 (collapsing-transformation
                  (lazy-array-shape lazy-array))))
         (collapsed-lazy-arrays
           (mapcar #'transform-lazy-array lazy-arrays transformations))
         (request (make-request backend collapsed-lazy-arrays)))
    ;; Ensure that REQUEST is well formed.
    (assert (requestp request))
    (assert (eq (request-backend request) backend))
    ;; Mutate the affected lazy arrays so that they won't be scheduled or
    ;; computed repeatedly.
    (bordeaux-threads:with-recursive-lock-held (*lazy-array-lock*)
      (loop for lazy-array in lazy-arrays
            for transformation in transformations
            for collapsed-lazy-array in collapsed-lazy-arrays
            do (setf (lazy-array-delayed-action collapsed-lazy-array)
                     (make-delayed-wait
                      :request request
                      :delayed-action (lazy-array-delayed-action collapsed-lazy-array)))
               (unless (eq lazy-array collapsed-lazy-array)
                 (setf (lazy-array-delayed-action lazy-array)
                       (make-delayed-reshape
                        :transformation transformation
                        :input collapsed-lazy-array)))))
    (lparallel.queue:push-queue request *scheduler-queue*)
    request))

(defun wait (&rest requests)
  (mapc #'request-wait requests)
  nil)

(defun trivial-array-p (array)
  (typecase array
    (lazy-array (trivial-lazy-array-p array (lazy-array-delayed-action array)))
    (otherwise t)))

(defun trivial-array-value (array)
  (typecase array
    (array (array-value array))
    (lazy-array (trivial-lazy-array-value array (lazy-array-delayed-action array)))
    (otherwise array)))

(defun array-value (array)
  (declare (array array))
  (if (zerop (array-rank array))
      (aref array)
      array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Scheduler

(defun scheduler-thread ()
  (loop
    (handler-case
        (loop for request = (lparallel.queue:pop-queue *scheduler-queue*)
              unless (not request)
                do (backend-schedule (request-backend request) request))
      (condition (c)
        (format *error-output* "Petalisp Scheduler Error: ~A" c)))))

(defvar *scheduler-thread*
  (bordeaux-threads:make-thread
   'scheduler-thread
   :name "Petalisp Scheduler Thread"))
