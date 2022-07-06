;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;; This lock must be held when mutating lazy array delayed actions.
(defvar *lazy-array-lock*
  (bordeaux-threads:make-recursive-lock "Petalisp Lazy Array Lock"))

(defstruct (delayed-action
            (:predicate delayed-action-p)
            (:copier nil)
            (:constructor nil)))

(defstruct (lazy-array
            (:predicate lazy-array-p)
            (:copier nil))
  ;; The shape of a lazy array describes the set of indices for which the
  ;; lazy array has associated values.
  (shape (alexandria:required-argument :shape)
   :type shape
   :read-only t)
  ;; The ntype of a lazy array is a conservative estimate on the the type
  ;; of each individual element.
  (ntype (alexandria:required-argument :ntype)
   :type t
   :read-only t)
  ;; The depth of a lazy array that is the result of wrapping an actual
  ;; Common Lisp array or scalar is zero.  The depth of any other lazy
  ;; array is one more than the maximum depth of the arrays it depends on.
  (depth 0
   :type (and unsigned-byte fixnum)
   :read-only t)
  ;; The refcount of a lazy array is an integer that is incremented
  ;; whenever the lazy array is referenced by another lazy array.  If a
  ;; lazy array has a refcount of one, we know that each of its elements is
  ;; only accessed at most once, and we can usually optimize it away during
  ;; IR conversion.
  (refcount 0
   :type (and unsigned-byte fixnum)
   :read-only nil)
  ;; The delayed action of a lazy array describes how its elements can be
  ;; computed.  This is not a constant slot - invoking 'schedule' or
  ;; 'compute' on a lazy array will change the delayed action, and backends
  ;; are also permitted to add and use their own delayed actions.  But any
  ;; new delayed must produce the very same elements as the one it
  ;; replaces.
  (delayed-action (alexandria:required-argument :delayed-action)
   :type delayed-action
   :read-only nil))

(defun lazy-array-element-type (lazy-array)
  (declare (lazy-array lazy-array))
  (petalisp.type-inference:type-specifier
   (lazy-array-ntype lazy-array)))

(declaim (inline lazy-array-rank))
(defun lazy-array-rank (lazy-array)
  (declare (lazy-array lazy-array))
  (shape-rank
   (lazy-array-shape lazy-array)))

(declaim (inline lazy-array-size))
(defun lazy-array-size (lazy-array)
  (declare (lazy-array lazy-array))
  (shape-size
   (lazy-array-shape lazy-array)))

(declaim (inline lazy-array-ranges))
(defun lazy-array-ranges (lazy-array)
  (declare (lazy-array lazy-array))
  (shape-ranges
   (lazy-array-shape lazy-array)))

(defun lazy-array-inputs (lazy-array)
  (declare (lazy-array lazy-array))
  (delayed-action-inputs
   (lazy-array-delayed-action lazy-array)))

(defmethod print-object ((lazy-array lazy-array) stream)
  (print-unreadable-object (lazy-array stream :type t :identity nil)
    (format stream "~S ~S"
            (lazy-array-element-type lazy-array)
            (lazy-array-shape lazy-array))))

(defmethod shape-designator-shape ((lazy-array lazy-array))
  (lazy-array-shape lazy-array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delayed Actions
;;;
;;; A delayed action describes how the elements of a lazy array can be
;;; computed if necessary.  This can range from extremely simple actions
;;; like accessing the corresponding elements of a Common Lisp array to
;;; more complicated actions like accessing the nth value of the multiple
;;; values returned by a function when applied to some arguments.
;;;
;;; We also amend the constructor of each delayed action to automatically
;;; increment the refcount of each referenced lazy array.

;;; A delayed map action is executed by applying the specified operator
;;; element-wise to the specified inputs.
(defstruct (delayed-map
            (:include delayed-action)
            (:constructor %make-delayed-map))
  (operator (alexandria:required-argument :operator)
   :type (or function symbol))
  (inputs (alexandria:required-argument :inputs)
   :type list))

(declaim (inline make-delayed-map))
(defun make-delayed-map (&key operator inputs)
  (dolist (input inputs)
    (incf (lazy-array-refcount input)))
  (%make-delayed-map :operator operator :inputs inputs))

;;; A delayed multiple value map is the same as a delayed map, but for a
;;; function that produces multiple values.  Because of the nature of its
;;; return values, a lazy array whose delayed action is a delayed multiple
;;; value map must only appear as the input of a delayed nth value action
;;; and never be visible to the user.
(defstruct (delayed-multiple-value-map
            (:include delayed-map)
            (:constructor %make-delayed-multiple-value-map))
  ;; A list of ntypes, one for each return value.
  (ntypes (alexandria:required-argument :ntypes)
   :type list))

(declaim (inline make-delayed-multiple-value-map))
(defun make-delayed-multiple-value-map (&key operator inputs ntypes)
  (dolist (input inputs)
    (incf (lazy-array-refcount input)))
  (%make-delayed-multiple-value-map :operator operator :inputs inputs :ntypes ntypes))

;;; A delayed nth-value action is executed by referencing the nth value of
;;; the targeted delayed multiple value map.
(defstruct (delayed-nth-value
            (:include delayed-action)
            (:constructor %make-delayed-nth-value))
  (number (alexandria:required-argument :number)
   :type petalisp.type-inference:argument-index)
  (input (alexandria:required-argument :input)
   :type lazy-array))

(declaim (inline make-delayed-nth-value))
(defun make-delayed-nth-value (&key number input)
  (incf (lazy-array-refcount input))
  (%make-delayed-nth-value :number number :input input))

;;; A delayed reshape action is executed by assigning each index the value
;;; of the specified input lazy array at the position that is obtained by
;;; applying the specified transformation to the index.
(defstruct (delayed-reshape
            (:include delayed-action)
            (:constructor %make-delayed-reshape))
  (transformation (alexandria:required-argument :transformation)
   :type transformation)
  (input (alexandria:required-argument :input)
   :type lazy-array))

(declaim (inline make-delayed-reshape))
(defun make-delayed-reshape (&key transformation input)
  ;; The elements of an input of a broadcasting reference (which we
  ;; implement as a delayed reshape operation with a non-invertible
  ;; transformation) are referenced more than once, so we increment the
  ;; refcount by a number larger than one to warn the IR conversion.
  (incf (lazy-array-refcount input)
        (if (transformation-invertiblep transformation) 1 2))
  (%make-delayed-reshape :transformation transformation :input input))

;;; A delayed fuse action is executed by assigning each index the unique
;;; value of the same index in on of the specified inputs.
(defstruct (delayed-fuse
            (:include delayed-action)
            (:constructor %make-delayed-fuse))
  (inputs (alexandria:required-argument :inputs)
   :type list))

(declaim (inline delayed-fuse))
(defun make-delayed-fuse (&key inputs)
  (dolist (input inputs)
    (incf (lazy-array-refcount input)))
  (%make-delayed-fuse :inputs inputs))

;;; A delayed range action is executed by assigning each index a value that
;;; is that index.
(defstruct (delayed-range
            (:include delayed-action)))

;;; A delayed array action is executed by accessing the values of the
;;; underlying storage array.
(defstruct (delayed-array
            (:include delayed-action))
  (storage (alexandria:required-argument :storage)
   :type simple-array))

;;; A delayed nop action is inserted as the delayed action of an empty
;;; lazy array.
(defstruct (delayed-nop
            (:include delayed-action)))

;;; A delayed unknown cannot be executed.  It is the delayed action of lazy
;;; arrays that serve as formal parameters only.
(defstruct (delayed-unknown
            (:include delayed-action)))

;;; A delayed wait is executed by calling WAIT on the request, and then
;;; executing the delayed action of it.
(defstruct (delayed-wait
            (:include delayed-action))
  (request (alexandria:required-argument :request)
   :type t
   :read-only t)
  ;; Initially, this slot holds the delayed action of that lazy array
  ;; before it was scheduled.  Later, once the request has completed, the
  ;; slot is updated to the delayed action of the computed result.
  (delayed-action (alexandria:required-argument :delayed-action)
   :type delayed-action))

;;; A delayed failure is executed by raising its condition via ERROR.  This
;;; kind of delayed action is generated when a serious condition is raised
;;; in the asynchronous execution after a SCHEDULE operation.
(defstruct (delayed-failure
            (:include delayed-action))
  (condition (alexandria:required-argument :condition)
   :type condition
   :read-only t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lazy Array Constructors

(defun lazy-array (object)
  (typecase object
    (lazy-array object)
    (array (lazy-array-from-array object))
    (t (lazy-array-from-scalar object))))

(defun lazy-array-from-scalar (object)
  (make-lazy-array
   :shape (load-time-value (make-shape '()))
   :ntype (petalisp.type-inference:ntype-of object)
   :delayed-action
   (make-delayed-array
    :storage (petalisp.type-inference:make-rank-zero-array object))))

(defun lazy-array-from-array (array)
  (declare (array array))
  (if (zerop (array-rank array))
      (lazy-array-from-scalar (aref array))
      (make-lazy-array
       :shape (array-shape array)
       :ntype (petalisp.type-inference:array-element-ntype array)
       :delayed-action
       (make-delayed-array :storage (simplify-array array)))))

;;; Turn arrays into simple arrays.
(defun simplify-array (array)
  (if (typep array 'simple-array)
      array
      (let ((copy (make-array (array-dimensions array)
                              :element-type (array-element-type array))))
        (loop for index below (array-total-size array) do
          (setf (row-major-aref copy index)
                (row-major-aref array index)))
        copy)))

(defun lazy-array-from-range (range)
  (if (size-one-range-p range)
      (lazy-ref
       (lazy-array-from-scalar (range-start range))
       (make-shape (list range))
       (make-transformation
        :input-rank 1
        :output-rank 0))
      (make-lazy-array
       :shape (make-shape (list range))
       :ntype (petalisp.type-inference:ntype-union
               (petalisp.type-inference:ntype-of (range-start range))
               (petalisp.type-inference:ntype-of (range-last range)))
       :delayed-action (make-delayed-range))))

(defun empty-lazy-arrays (n shape)
  (if (zerop n)
      (values)
      (let ((empty-array (empty-lazy-array shape)))
        (case n
          (1 (values empty-array))
          (2 (values empty-array empty-array))
          (3 (values empty-array empty-array empty-array))
          (otherwise
           (values-list
            (make-list n :initial-element empty-array)))))))

(defun empty-lazy-array (shape)
  (unless (empty-shape-p shape)
    (error "Cannot create an empty array from the non-empty shape ~S."
           shape))
  (make-lazy-array
   :shape shape
   :ntype nil
   :delayed-action (load-time-value (make-delayed-nop))))

(declaim (inline make-unknown))
(defun make-unknown (&key (shape (make-shape '())) element-type)
  (declare (shape shape))
  (make-lazy-array
   :shape shape
   :ntype (petalisp.type-inference:ntype element-type)
   :delayed-action (make-delayed-unknown)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Miscellaneous

(defun subdivide-arrays (arrays)
  (subdivide-shapes
   (loop for array in arrays
         collect
         (etypecase array
           (array (array-shape array))
           (lazy-array (lazy-array-shape array))))))

(defun delayed-map-number-of-values (delayed-map)
  (declare (delayed-map delayed-map))
  (if (delayed-multiple-value-map-p delayed-map)
      (length (delayed-multiple-value-map-ntypes delayed-map))
      1))

(defun maxdepth (lazy-arrays)
  (let ((maxdepth 0))
    (declare (fixnum maxdepth))
    (dolist (lazy-array lazy-arrays)
      (let ((depth (lazy-array-depth lazy-array)))
        (when (> depth maxdepth)
          (setf maxdepth depth))))
    maxdepth))

;;; Computing the inputs of a delayed action.
(defgeneric delayed-action-inputs (delayed-action)
  (:method ((delayed-action delayed-action))
    '())
  (:method ((delayed-map delayed-map))
    (delayed-map-inputs delayed-map))
  (:method ((delayed-nth-value delayed-nth-value))
    (list (delayed-nth-value-input delayed-nth-value)))
  (:method ((delayed-reshape delayed-reshape))
    (list (delayed-reshape-input delayed-reshape)))
  (:method ((delayed-fuse delayed-fuse))
    (delayed-fuse-inputs delayed-fuse)))

(defun lazy-unknowns (graph-roots)
  (let ((table (make-hash-table :test #'eq))
        (delayed-unknowns '()))
    (labels ((scan (lazy-array)
               (cond ((= 1 (lazy-array-refcount lazy-array))
                      (process lazy-array))
                     ((not (gethash lazy-array table))
                      (setf (gethash lazy-array table) t)
                      (process lazy-array))))
             (process (lazy-array)
               (typecase (lazy-array-delayed-action lazy-array)
                 (delayed-unknown (push lazy-array delayed-unknowns))
                 (otherwise (mapc #'scan (lazy-array-inputs lazy-array))))))
      (mapc #'scan graph-roots))
    (values delayed-unknowns)))
