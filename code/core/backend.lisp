;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;; This special variable will be bound later, once at least one backend
;;; has been loaded.
(defvar *backend*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

;;; The generic function BACKEND-COMPUTE is invoked whenever some lazy
;;; arrays have to be turned into immediates in a blocking fashion.
;;; Backend developers can rely on the fact that the second argument is
;;; always a list of either CL:ARRAYS or collapsed lazy arrays, i.e., lazy
;;; arrays whose shape consists only of ranges with a start of zero and a
;;; stride of one.
(defgeneric backend-compute (backend lazy-arrays))

;;; The generic function BACKEND-SHEDULE recognizes the task of computing
;;; some lazy arrays in some backend-specific way and returns an opaque
;;; object that can later be passed to BACKEND-WAIT to obtain the actual
;;; results.
;;;
;;; The third argument is a function that replaces lazy arrays with their
;;; computed immediates.  Backend developers must invoke this function with
;;; the list of computed immediates once these immediates are available.
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

(defmethod backend-compute :before
    ((backend backend)
     (lazy-arrays list))
  (loop for lazy-array in lazy-arrays do
    (unless (computablep lazy-array)
      (error "Attempt to compute the non-computible array ~S."
             lazy-array))))

(defmethod backend-schedule :before
    ((backend backend)
     (lazy-arrays list)
     (finalizer function))
  (loop for lazy-array in lazy-arrays do
    (unless (computablep lazy-array)
      (error "Attempt to schedule the non-computible array ~S."
             lazy-array))))

;;; The default methods for BACKEND-SHEDULE and BACKEND-WAIT are very
;;; primitive and don't perform any asynchronous work at all.  Backend
;;; developers are encouraged to override both methods with something
;;; sensible.
(defmethod backend-schedule
    ((backend backend)
     (lazy-arrays list)
     (finalizer function))
  (lambda ()
    (funcall finalizer (backend-compute backend lazy-arrays))))

(defmethod backend-wait
    ((backend backend)
     (requests list))
  (mapc #'funcall requests))

(defmethod delete-backend ((backend backend))
  (change-class backend 'deleted-backend))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; API

(defun lisp-datum-from-immediate (immediate)
  (let ((array (array-from-immediate immediate)))
    (if (zerop (array-rank array))
        (aref array)
        array)))

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
         ;; Have the backend perform the actual work.
         (immediates
           (backend-compute
            *backend*
            (mapcar #'transform lazy-arrays transformations))))
    ;; Project the results back to the shape of the original arguments, and
    ;; replace these arguments with the newly computed stuff.  This is the
    ;; only side-effect that we ever perform on data flow graphs, but it is
    ;; an important one.  Otherwise, we'd have to recompute each lazy array
    ;; for each data flow graph it appears in.
    (loop for lazy-array in lazy-arrays
          for transformation in transformations
          for immediate in immediates
          do (replace-lazy-array
              lazy-array
              (lazy-reshape (lazy-array immediate)
                            (lazy-array-shape lazy-array)
                            transformation)))
    ;; Return the lisp datum corresponding to each immediate.
    (mapcar #'lisp-datum-from-immediate immediates)))

(defun schedule (&rest arrays)
  (schedule-list-of-arrays arrays))

(defun schedule-list-of-arrays (arrays)
  (let* ((lazy-arrays (mapcar #'lazy-array arrays))
         (transformations
           (loop for lazy-array in lazy-arrays
                 collect
                 (collapsing-transformation
                  (lazy-array-shape lazy-array)))))
    (backend-schedule
     *backend*
     (mapcar #'transform lazy-arrays transformations)
     (lambda (immediates)
       (loop for lazy-array in lazy-arrays
             for transformation in transformations
             for immediate in immediates
             do (replace-lazy-array
                 lazy-array
                 (lazy-reshape (lazy-array immediate)
                               (lazy-array-shape lazy-array)
                               transformation)))))))

(defun wait (&rest requests)
  (backend-wait *backend* requests)
  nil)

