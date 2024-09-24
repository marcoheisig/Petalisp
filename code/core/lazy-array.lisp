;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

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
   :type typo:ntype
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
  (refcount 0 ;; TODO Make thread-safe, or abandon the refcount altogether.
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
  (typo:ntype-type-specifier
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

(declaim (inline lazy-array-dimension))
(defun lazy-array-dimension (lazy-array axis)
  (declare (lazy-array lazy-array) (axis axis))
  (shape-dimension
   (lazy-array-shape lazy-array)
   axis))

(declaim (inline lazy-array-dimensions))
(defun lazy-array-dimensions (lazy-array)
  (declare (lazy-array lazy-array))
  (shape-dimensions
   (lazy-array-shape lazy-array)))

(declaim (inline lazy-array-range))
(defun lazy-array-range (lazy-array axis)
  (declare (lazy-array lazy-array) (axis axis))
  (shape-range (lazy-array-shape lazy-array) axis))

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

(defstruct (delayed-map
            (:include delayed-action)
            (:constructor %make-delayed-map))
  "A delayed map describes the element-wise application of a function to some
number of lazy arrays.  A delayed map has two slots: The first slot is the
fnrecord of the function being mapped, i.e., the entry of that function in the
database of our type inference library Typo.  The second slot is a list of lazy
arrays that the function is being mapped over.  All lazy arrays that appear as
an input of a delayed map must have the same shape."
  (fnrecord (alexandria:required-argument :fnrecord)
   :type typo:fnrecord)
  (inputs (alexandria:required-argument :inputs)
   :type list))

(declaim (inline make-delayed-map))
(defun make-delayed-map (&key fnrecord inputs)
  (dolist (input inputs)
    (incf (lazy-array-refcount input)))
  (%make-delayed-map :fnrecord fnrecord :inputs inputs))

(defstruct (delayed-multiple-value-map
            (:include delayed-map)
            (:constructor %make-delayed-multiple-value-map))
  "A delayed multiple value map describes the element-wise application of a
multiple-valued function to some number of lazy arrays of the same shape.  It
has the same first two slots as a delayed map, a third slot that is Typo's
description of the type of the multiple value pack it creates, and a fourth
slot that is a mutable bit vector that tracks which of the multiple values have
been referenced so far.  The bit vector is later used to eliminate unused
values altogether.

Because of the nature of its return values, a lazy array whose delayed action
is a delayed multiple value map must only appear as the input of a delayed nth
value action and never be visible to the user."
  (values-ntype (alexandria:required-argument :values-ntype)
   :type typo:values-ntype
   :read-only t)
  ;; An unsigned integer whose Kth bit indicates that the Kth value of the
  ;; multiple value map is being referenced.
  (refbits 0 :type unsigned-byte)
  ;; A lock that has to be held when manipulating the refbits.
  (refbits-lock (bordeaux-threads-2:make-lock :name "Refbits Lock")
   :type bordeaux-threads-2:lock))

(declaim (inline make-delayed-multiple-value-map))
(defun make-delayed-multiple-value-map (&key fnrecord inputs values-ntype (refbits 0))
  (dolist (input inputs)
    (incf (lazy-array-refcount input)))
  (%make-delayed-multiple-value-map
   :fnrecord fnrecord
   :inputs inputs
   :values-ntype values-ntype
   :refbits refbits))

(defstruct (delayed-nth-value
            (:include delayed-action)
            (:constructor %make-delayed-nth-value))
  "A delayed nth-value describes the process of referencing the nth value of
some lazy array whose delayed action is a delayed multiple value map.  Its
first slot is the position of the value being referenced, and its second slot
is a lazy array defined by a delayed multiple value map."
  (number (alexandria:required-argument :number)
   :type typo:argument-index)
  (input (alexandria:required-argument :input)
   :type lazy-array))

(declaim (inline make-delayed-nth-value))
(defun make-delayed-nth-value (&key number input)
  (let ((delayed-action (lazy-array-delayed-action input)))
    (unless (typep delayed-action 'delayed-multiple-value-map)
      (error "Can only take the Nth value of multiple value maps."))
    (incf (lazy-array-refcount input))
    (with-slots (refbits refbits-lock) delayed-action
      (bordeaux-threads-2:with-lock-held (refbits-lock)
        (setf refbits (logior refbits (ash 1 number)))))
    (%make-delayed-nth-value
     :number number
     :input input)))

(defstruct (delayed-reshape
            (:include delayed-action)
            (:constructor %make-delayed-reshape))
  "A delayed reshape describes the process of assigning each index the value of
the specified input lazy array at the position that is obtained by applying the
specified transformation to that index.  It has one slot that stores the
transformation, and one slot that stores that lazy array being referenced."
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
  (%make-delayed-reshape
   :transformation transformation
   :input input))

(defstruct (delayed-fuse
            (:include delayed-action)
            (:constructor %make-delayed-fuse))
  "A delayed fuse describes the process of assigning each index the corresponding value from
the sole input lazy array that contains it.  It has one slot that is the list
of lazy arrays being fused."
  (inputs (alexandria:required-argument :inputs)
   :type list))

(declaim (inline delayed-fuse))
(defun make-delayed-fuse (&key inputs)
  (dolist (input inputs)
    (incf (lazy-array-refcount input)))
  (%make-delayed-fuse
   :inputs inputs))

(defstruct (delayed-range
            (:include delayed-action))
  "A delayed range describes the process of assigning each index the sole integer
contained in that index.  This delayed action must only appear in the
definition of lazy arrays of rank one.")

(defstruct (delayed-array
            (:include delayed-action)
            (:constructor %make-delayed-array (storage)))
  "A delayed array describes the process of assigning each index the value of some
existing Common Lisp array at that index.  It has one slot that is the existing
array being referenced."
  (storage (alexandria:required-argument :storage)
   :type array))

(defun make-delayed-array (object)
  (%make-delayed-array (value-array object)))

(defstruct (delayed-nop
            (:include delayed-action))
  "A delayed nop is the delayed action of any lazy array with zero
elements.  It cannot be computed.")

(defstruct (delayed-unknown
            (:include delayed-action))
  "A delayed unknown is used as the delayed action of any lazy array created by
the function MAKE-UNKNOWN.  It cannot be computed.")

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
  "A delayed failure describes a lazy array that was involved in an asynchronous evaluation
that signaled an error.  It has one slot that is the condition that should be
re-signaled whenever this delayed action is part of a synchronous evaluation."
  (condition (alexandria:required-argument :condition)
   :type condition
   :read-only t))

(defun lazy-unknown-p (lazy-array)
  (declare (lazy-array lazy-array))
  (typep (lazy-array-delayed-action lazy-array) 'delayed-unknown))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lazy Array Constructors

(defun lazy-array (object)
  (the (values lazy-array &optional)
       (typecase object
         (lazy-array object)
         (array (lazy-array-from-array object))
         (t (lazy-array-from-scalar object)))))

(defun lazy-array-from-scalar (object)
  (make-lazy-array
   :shape (load-time-value (make-shape '()))
   :ntype (typo:ntype-of object)
   :delayed-action (make-delayed-array (make-rank-zero-array object))))

(defun lazy-array-from-array (array)
  (declare (array array))
  (case (array-total-size array)
    (0 (empty-lazy-array (array-shape array)))
    (1 (let* ((value (row-major-aref array 0))
              (scalar (lazy-array-from-scalar value))
              (rank (array-rank array)))
         (if (zerop rank)
             scalar
             (make-lazy-array
              :shape (array-shape array)
              :ntype (lazy-array-ntype scalar)
              :delayed-action
              (make-delayed-reshape
               :input scalar
               :transformation
               (make-transformation
                :input-mask (make-array rank :initial-element 0)
                :output-rank 0))))))
    (otherwise
     (make-lazy-array
      :shape (array-shape array)
      :ntype (typo:array-element-ntype array)
      :delayed-action
      (make-delayed-array (simplify-array array))))))

(defun simplify-array (array)
  "Returns an array with the same shape and elements as ARRAY, but that is
guaranteed to be simple."
  (if (typep array 'simple-array)
      array
      (let ((copy (make-array (array-dimensions array)
                              :element-type (array-element-type array))))
        (loop for index below (array-total-size array) do
          (setf (row-major-aref copy index)
                (row-major-aref array index)))
        copy)))

(defun lazy-array-from-range (range)
  (if (range-with-size-one-p range)
      (lazy-ref
       (lazy-array-from-scalar (range-start range))
       (make-shape (list range))
       (make-transformation
        :input-rank 1
        :output-rank 0))
      (make-lazy-array
       :shape (make-shape (list range))
       :ntype (typo:ntype-union
               (typo:ntype-of (range-start range))
               (typo:ntype-of (range-last range)))
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
  (unless (shape-emptyp shape)
    (error "Cannot create an empty array from the non-empty shape ~S."
           shape))
  (make-lazy-array
   :shape shape
   :ntype (typo:empty-ntype)
   :delayed-action (load-time-value (make-delayed-nop))))

(declaim (inline make-unknown))
(defun make-unknown (&key (shape (make-shape '())) (element-type 't))
  (declare (shape shape))
  (make-lazy-array
   :shape shape
   :ntype (typo:type-specifier-ntype element-type)
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
      (integer-length (delayed-multiple-value-map-refbits delayed-map))
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

(defun compatible-with-lazy-array-p (object lazy-array)
  (declare (lazy-array lazy-array))
  (typecase object
    (array
     (and (array-has-shape-p object (lazy-array-shape lazy-array))
          (typo:ntype=
           (typo:array-element-ntype object)
           (lazy-array-ntype lazy-array))))
    (t
     (and (shape-emptyp (lazy-array-shape lazy-array))
          (typo:ntype-subtypep
           (typo:ntype-of object)
           (lazy-array-ntype lazy-array))))))

(defun make-rank-zero-array (value)
  (macrolet ((body ()
               `(ecase (typo:ntype-index (typo:ntype-of value))
                  ,@(loop for index below typo:+primitive-ntype-limit+
                          collect
                          (let* ((element-ntype (typo:primitive-ntype-from-index index))
                                 (element-type (typo:ntype-type-specifier element-ntype)))
                            `(,index (make-array
                                      '()
                                      :element-type ',element-type
                                      :initial-element value)))))))
    (body)))

(defun array-value (array)
  (declare (array array))
  (if (zerop (array-rank array))
      (aref array)
      array))

(defun value-array (object)
  (typecase object
    (array object)
    (otherwise (make-rank-zero-array object))))

(defun trivial-object-p (object)
  "Returns whether the supplied OBJECT is an array, a lazy array that can
be cheaply converted to an array, or a scalar."
  (typecase object
    (lazy-array (trivial-lazy-array-p object (lazy-array-delayed-action object)))
    (otherwise t)))

(defgeneric trivial-lazy-array-p (lazy-array delayed-action)
  (:documentation
   "Returns whether the supplied LAZY-ARRAYS and its DELAYED-ACTION have a
corresponding deflated array that can be obtained cheaply.")
  (:method ((lazy-array lazy-array)
            (delayed-action delayed-action))
    nil)
  (:method ((lazy-array lazy-array)
            (delayed-array delayed-array))
    t)
  (:method ((lazy-array lazy-array)
            (delayed-reshape delayed-reshape))
    (and (transformation-identityp (delayed-reshape-transformation delayed-reshape))
         (shape= (lazy-array-shape lazy-array)
                 (lazy-array-shape (delayed-reshape-input delayed-reshape)))
         (trivial-object-p (delayed-reshape-input delayed-reshape)))))

(defun trivial-object-value (object)
  "Returns the value of an OBJECT that is trivial in the sense of
TRIVIAL-OBJECT-P."
  (typecase object
    (array (array-value object))
    (lazy-array (trivial-lazy-array-value object (lazy-array-delayed-action object)))
    (otherwise object)))

(defgeneric trivial-lazy-array-value (lazy-array delayed-action)
  (:documentation
   "Obtain the array corresponding to the supplied LAZY-ARRAY, which must be
trivial in the sense of TRIVIAL-LAZY-ARRAY-P.")
  (:method ((lazy-array lazy-array)
            (delayed-array delayed-array))
    (array-value (delayed-array-storage delayed-array)))
  (:method ((lazy-array lazy-array)
            (delayed-reshape delayed-reshape))
    (trivial-object-value (delayed-reshape-input delayed-reshape))))

;;; This lock must be held when mutating lazy array delayed actions.
(defvar *lazy-array-lock*
  (bordeaux-threads-2:make-recursive-lock :name "Petalisp Lazy Array Lock"))
