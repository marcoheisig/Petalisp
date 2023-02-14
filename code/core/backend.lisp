;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;; This special variable will be bound later, once at least one backend
;;; has been loaded.
(defvar *backend*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric backendp (object)
  (:documentation
   "Returns whether the supplied OBJECT is a Petalisp backend."))

(defgeneric delete-backend (backend)
  (:documentation
   "Permanently disable the supplied BACKEND and free any resources that might
be held by it.  No other actions may be performed on a backend once it has
been deleted."))

(defgeneric backend-compute (backend lazy-arrays)
  ;; We change the argument precedence order so that we can add default
  ;; methods for the case where the second argument is null.
  (:argument-precedence-order lazy-arrays backend)
  (:documentation
   "Returns a list of delayed local arrays, one for each of the supplied list
of LAZY-ARRAYS.

This function is invoked by COMPUTE, which guarantees that the supplied
LAZY-ARRAYS are already collapsed.  The resulting delayed actions replace
the delayed actions of the corresponding LAZY-ARRAYS."))

(defgeneric backend-evaluator (backend unknowns lazy-arrays)
  ;; We change the argument precedence order so that we can add default
  ;; methods for the case where the second or third argument are null.
  (:argument-precedence-order unknowns lazy-arrays backend)
  (:documentation
   "For a supplied BACKEND, list of UNKNOWNS of length N, and list of
LAZY-ARRAYS of length K, returns a function with K+N arguments that
returns, as multiple values, the K array values obtained by computing the
supplied arrays after substituting the Ith unknown with the supplied
argument in position K+I.

The first N arguments specify which storage to use for the results.  A
value of NIL indicates that the corresponding result shall be a fresh
array.  A value that is an array ensures that the result is written to that
array.

An error is signaled if any of the supplied arrays has a different shape or
element type as the corresponding result or unknown."))

(defgeneric backend-compute-asynchronously (backend lazy-arrays)
  (:argument-precedence-order lazy-arrays backend)
  (:documentation
   "Returns a REQUEST object that can be used to wait until all of the
supplied LAZY-ARRAYS have been computed."))

;;; REQUEST

(defgeneric requestp (object)
  (:documentation
   "Returns whether the supplied OBJECT is a request."))

(defgeneric request-wait (request)
  (:documentation
   "Block until all lazy arrays that are part of the supplied REQUEST have
been computed."))

(defgeneric request-completedp (request)
  (:documentation
   "Returns whether all lazy arrays that are part of the supplied REQUEST have
already been computed."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass backend ()
  ())

(defclass deleted-backend ()
  ())

(defclass request ()
  ())

(defclass completed-request (request)
  ())

(defclass backend-stats ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod backendp ((object t))
  nil)

(defmethod backendp ((backend backend))
  t)

(defmethod delete-backend ((backend backend))
  (change-class backend 'deleted-backend))

(defmethod backend-compute ((backend backend) (lazy-arrays null))
  (list))

;;; The default method for BACKEND-COMPUTE is implemented in terms of
;;; BACKEND-EVALUATOR, and the default method for BACKEND-EVALUATOR is
;;; implemented in terms of BACKEND-COMPUTE.  A consequence is that
;;; erroneous or incomplete backends would lead to an infinite recursion.
;;; We avoid this with the following special variable.
(defvar *backend-compute-recursion* nil)

;;; Unless there is a more specific version of BACKEND-COMPUTE available
;;; for a particular backend, attempt to emulate it by creating an
;;; evaluator and calling that.
(defmethod backend-compute ((backend backend) (lazy-arrays list))
  (if (not *backend-compute-recursion*)
      (let ((*backend-compute-recursion* t))
        (mapcar #'make-delayed-array
                (multiple-value-list
                 (apply (backend-evaluator backend '() lazy-arrays)
                        (make-list (length lazy-arrays) :initial-element nil)))))
      (call-next-method)))

;;; Check whether the supplied arguments are well formed.
(defmethod backend-evaluator :before
    ((backend backend)
     (unknowns list)
     (lazy-arrays list))
  (dolist (lazy-array lazy-arrays)
    (unless (lazy-array-p lazy-array)
      (error "Not a lazy array: ~S" lazy-array)))
  (dolist (unknown unknowns)
    (unless (and (lazy-array-p unknown)
                 (delayed-unknown-p (lazy-array-delayed-action unknown)))
      (error "Not an unknown: ~S" unknown))))

;;; A trivial implementation of the evaluator with zero results.  We define
;;; this default method so that backend developers never have to worry
;;; about this case themselves.
(defmethod backend-evaluator
    ((backend backend)
     (unknowns list)
     (lazy-arrays null))
  (let ((nargs (length unknowns)))
    (lambda (&rest arguments)
      (unless (= (length arguments) nargs)
        (error "~:(~R~) arguments supplied to an evaluator that expected ~R arguments."
               (length arguments) nargs))
      (loop for unknown in unknowns
            for argument in arguments
            unless (compatible-with-lazy-array-p argument unknown)
              do (error "The argument ~S is incompatible with the unknown ~S."
                        argument
                        unknown))
      (values))))

;;; This default method for BACKEND-EVALUATOR constructs the evaluator in
;;; terms of calling BACKEND-COMPUTE on a graph where all unknowns have
;;; been substituted with the supplied arguments.  Doing so has of course
;;; no performance benefits over simply calling BACKEND-COMPUTE, thus
;;; negating the main advantage of evaluators over direct evaluation.
;;; Nevertheless, it is better to have a mediocre implementation than none
;;; at all.
(defmethod backend-evaluator
    ((backend backend)
     (unknowns list)
     (lazy-arrays list))
  (let ((n-results (length lazy-arrays))
        (n-unknowns (length unknowns)))
    (lambda (&rest arrays)
      (unless (= (length arrays) (+ n-results n-unknowns))
        (error "~D arguments supplied to an evaluator expecting ~D arguments."
               (length arrays) (+ n-results n-unknowns)))
      (values-list
       (mapcar (alexandria:compose #'array-value #'delayed-array-storage)
               (backend-compute
                backend
                (substitute-lazy-arrays
                 lazy-arrays
                 (mapcar #'lazy-array (nthcdr n-results arrays))
                 unknowns)))))))

;;; Computing zero lazy arrays is of course instantaneous.
(defmethod backend-compute-asynchronously
    ((backend backend)
     (lazy-arrays null))
  (make-instance 'completed-request))

;;; A simple default method that is not asynchronous, but fulfills the
;;; specification.
(defmethod backend-compute-asynchronously
    ((backend backend)
     (lazy-arrays list))
  (compute-list-of-arrays lazy-arrays)
  (make-instance 'completed-request))

(defmethod requestp ((object t))
  nil)

(defmethod requestp ((request request))
  nil)

(defmethod request-wait ((completed-request completed-request))
  (values))

(defmethod request-completedp ((request request))
  nil)

(defmethod request-completedp ((completed-request completed-request))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; API

(defmacro with-temporary-backend (backend-creation-form &body body)
  `(let ((*backend* ,backend-creation-form))
     (unwind-protect (progn ,@body)
       (delete-backend *backend*))))

(defun compute (&rest arrays)
  (values-list
   (compute-list-of-arrays arrays)))

(defun compute-list-of-arrays (arrays)
  (let* ((lazy-arrays (remove-if #'trivial-object-p arrays))
         (transformations
           (loop for lazy-array in lazy-arrays
                 collect
                 (collapsing-transformation
                  (lazy-array-shape lazy-array))))
         (collapsed-lazy-arrays
           (mapcar #'lazy-reshape-using-transformation lazy-arrays transformations))
         (delayed-arrays
           (backend-compute *backend* collapsed-lazy-arrays)))
    ;; Project the results back to the shape of the original lazy arrays,
    ;; and overwrite the original delayed action with that of the projected
    ;; one.  This way we avoid recomputing the same lazy array over and
    ;; over again.
    (bordeaux-threads:with-recursive-lock-held (*lazy-array-lock*)
      (loop for lazy-array in lazy-arrays
            for transformation in transformations
            for collapsed-lazy-array in collapsed-lazy-arrays
            for delayed-array in delayed-arrays
            do (setf (lazy-array-delayed-action collapsed-lazy-array)
                     delayed-array)
            do (unless (eq lazy-array collapsed-lazy-array)
                 (setf (lazy-array-delayed-action lazy-array)
                       (make-delayed-reshape
                        :transformation transformation
                        :input collapsed-lazy-array)))))
    ;; All arrays are now trivial objects.
    (mapcar #'trivial-object-value arrays)))

(defun compute-asynchronously (&rest arrays)
  (backend-compute-asynchronously
   *backend*
   (remove-if #'trivial-object-p arrays)))

(defun wait (&rest requests)
  (mapc #'request-wait requests)
  (values))

(defun evaluator (unknowns arrays)
  (backend-evaluator *backend* unknowns (mapcar #'lazy-array arrays)))
