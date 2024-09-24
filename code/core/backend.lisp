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
   "Returns whether the supplied object is a backend."))

(defgeneric delete-backend (backend)
  (:documentation
   "Permanently disable the supplied backend and free any resources that are
held by it.  Once a backend has been deleted, any further call to an
evaluation function on that backend will signal an error."))

(defgeneric backend-compute (backend lazy-arrays)
  ;; We change the argument precedence order so that we can add default
  ;; methods for the case where the second argument is null.
  (:argument-precedence-order lazy-arrays backend)
  (:documentation
   "Returns a list of delayed array actions, one for each element of the supplied list
of lazy arrays.  This function should only invoked by COMPUTE, which guarantees
that the supplied lazy arrays are already deflated."))

(defgeneric backend-evaluator (backend unknowns lazy-arrays)
  ;; We change the argument precedence order so that we can add default
  ;; methods for the case where the second or third argument are null.
  (:argument-precedence-order unknowns lazy-arrays backend)
  (:documentation
   "For a supplied backend, list of unknowns of length N, and list of
lazy arrays of length K, returns a function with K plus N arguments that
returns, as multiple values, the K array values obtained by computing the
supplied arrays after substituting the Ith unknown with the supplied argument
in position K plus I.

The first K arguments of the resulting evaluator function specify which storage
to use for the results, where a value of NIL indicates that the corresponding
result is a freshly allocated array, whereas a value that is an array ensures
that the result is written to that array.  The remaining N arguments specify
the data that is used as substitute for the corresponding unknown.  Signals an
error if any of the arguments of an evaluator has a different shape or element
type as the corresponding result or unknown."))

(defgeneric backend-compute-asynchronously (backend lazy-arrays)
  (:argument-precedence-order lazy-arrays backend)
  (:documentation
   "Returns a request object that can be used to wait until all of the
supplied lazy arrays have been computed."))

(defgeneric backend-debug-flag (backend)
  (:documentation
   "Returns whether the supplied backend runs in debug mode, where it trades
performance for ease of debugging."))

(defgeneric (setf backend-debug-flag) (value backend)
  (:documentation
   "Set the backend's debug flag to true or false."))

;;; REQUEST

(defgeneric requestp (object)
  (:documentation
   "Returns whether the supplied object is a request."))

(defgeneric request-wait (request)
  (:documentation
   "Block until all lazy arrays that are part of the supplied request have
been computed."))

(defgeneric request-completedp (request)
  (:documentation
   "Returns whether all lazy arrays that are part of the supplied request have
already been computed."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass backend ()
  ((%debug-flag
    :initarg :debug
    :accessor backend-debug-flag
    :type boolean
    :initform nil)))

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
  t)

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
                 (deflating-transformation
                  (lazy-array-shape lazy-array))))
         (deflated-lazy-arrays
           (mapcar #'lazy-reshape lazy-arrays transformations))
         (delayed-arrays
           (backend-compute *backend* deflated-lazy-arrays)))
    ;; Project the results back to the shape of the original lazy arrays,
    ;; and overwrite the original delayed action with that of the projected
    ;; one.  This way we avoid recomputing the same lazy array over and
    ;; over again.
    (bordeaux-threads-2:with-recursive-lock-held (*lazy-array-lock*)
      (loop for lazy-array in lazy-arrays
            for transformation in transformations
            for deflated-lazy-array in deflated-lazy-arrays
            for delayed-array in delayed-arrays
            do (setf (lazy-array-delayed-action deflated-lazy-array)
                     delayed-array)
            do (unless (eq lazy-array deflated-lazy-array)
                 (setf (lazy-array-delayed-action lazy-array)
                       (make-delayed-reshape
                        :transformation transformation
                        :input deflated-lazy-array)))))
    ;; All arrays are now trivial objects.
    (mapcar #'trivial-object-value arrays)))

(defun compute-asynchronously (&rest arrays)
  (backend-compute-asynchronously
   *backend*
   (remove-if #'trivial-object-p arrays)))

(defun wait (&rest requests)
  (mapc #'request-wait requests)
  (values))

(defun completedp (&rest requests)
  (every #'request-completedp requests))

(defun evaluator (unknowns arrays)
  (declare (list unknowns arrays))
  (the (values function &optional)
       (backend-evaluator *backend* unknowns (mapcar #'lazy-array arrays))))
