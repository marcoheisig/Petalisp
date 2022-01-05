;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;; This special variable will be bound later, once at least one backend
;;; has been loaded.
(defvar *backend*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

;;; The generic function BACKEND-COMPUTE is invoked to force the evaluation
;;; of some lazy arrays in a blocking fashion.  Backend developers can rely
;;; on the fact that the second argument is always a list of collapsed lazy
;;; arrays, i.e., lazy arrays whose shape consists only of ranges with a
;;; start of zero and a stride of one.  This function must return one
;;; delayed array for each supplied lazy array.
(defgeneric backend-compute (backend lazy-arrays))

;;; The generic function BACKEND-SHEDULE is invoked to force the evaluation
;;; of some lazy arrays in a non-blocking fashion.  This function must
;;; return an opaque object that can later be passed to BACKEND-WAIT to
;;; obtain the actual results.
;;;
;;; The third argument is a function that expects a list of delayed actions
;;; -- one for each of the supplied lazy arrays -- and will use these as a
;;; replacement for the original delayed actions.
(defgeneric backend-schedule (backend lazy-arrays fixup))

;;; The generic function BACKEND-WAIT receives as its second argument a
;;; list of objects that have been returned by earlier calls to
;;; BACKEND-SCHEDULE.  It blocks until all the corresponding operations
;;; have been performed.
(defgeneric backend-wait (backend requests))

;;; The generic function DELETE-BACKEND can be invoked by a user to
;;; explicitly disable that backend and free any resources that might be
;;; held by it.
(defgeneric delete-backend (backend))

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

;;; The default methods for BACKEND-SHEDULE and BACKEND-WAIT are very
;;; primitive and don't perform any asynchronous work at all.  Backend
;;; developers are encouraged to override both methods with something
;;; sensible.
(defmethod backend-schedule
    ((backend backend)
     (lazy-arrays list)
     (finalizer function))
  (lambda ()
    (funcall
     finalizer
     (mapcar (lambda (array) (make-delayed-array :storage array))
             (backend-compute backend lazy-arrays)))))

(defmethod backend-wait
    ((backend backend)
     (requests list))
  (mapc #'funcall requests))

(defmethod delete-backend ((backend backend))
  (change-class backend 'deleted-backend))

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
  ;; Collapse all arguments so that they can be represented as Lisp arrays.
  (let* ((lazy-arrays (mapcar #'lazy-array arrays))
         (transformations
           (loop for lazy-array in lazy-arrays
                 collect
                 (collapsing-transformation
                  (lazy-array-shape lazy-array))))
         (collapsed-lazy-arrays
           (mapcar #'transform-lazy-array lazy-arrays transformations))
         ;; Tell the backend to compute the supplied lazy arrays after
         ;; collapsing them.
         (result-arrays (backend-compute *backend* collapsed-lazy-arrays)))
    ;; Project the results back to the shape of the original arguments, and
    ;; replace these arguments with the newly computed stuff.  This is the
    ;; only side-effect that we ever perform on data flow graphs, but it is
    ;; an important one.  Otherwise, we'd have to recompute each lazy array
    ;; for each data flow graph it appears in.
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
                      :input collapsed-lazy-array))))
    ;; Return the lisp datum corresponding to each immediate.
    (mapcar #'array-value result-arrays)))

(defun array-value (array)
  (declare (array array))
  (if (zerop (array-rank array))
      (aref array)
      array))

(declaim (bordeaux-threads:lock *schedule-lock*))
(defvar *schedule-lock* (bordeaux-threads:make-lock "Petalisp Schedule Lock"))

(defun schedule (&rest arrays)
  (schedule-list-of-arrays arrays))

(defun schedule-list-of-arrays (arrays)
  (let* ((lazy-arrays (mapcar #'lazy-array arrays))
         (transformations
           (loop for lazy-array in lazy-arrays
                 collect
                 (collapsing-transformation
                  (lazy-array-shape lazy-array))))
         (collapsed-lazy-arrays
           (mapcar #'transform-lazy-array lazy-arrays transformations)))
    (backend-schedule
     *backend*
     collapsed-lazy-arrays
     (lambda (results)
       (loop for lazy-array in lazy-arrays
             for collapsed-lazy-array in collapsed-lazy-arrays
             for transformation in transformations
             for result in results
             do (setf (lazy-array-delayed-action collapsed-lazy-array)
                      (etypecase result
                        (delayed-action result)
                        (array (make-delayed-array :storage result))))
                (unless (eq lazy-array collapsed-lazy-array)
                  (setf (lazy-array-delayed-action lazy-array)
                        (make-delayed-reshape
                         :transformation transformation
                         :input collapsed-lazy-array))))))))

(defun wait (&rest requests)
  (backend-wait *backend* requests)
  nil)
