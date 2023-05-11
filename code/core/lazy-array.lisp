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

(declaim (inline lazy-array-range))
(defun lazy-array-range (lazy-array axis)
  (declare (lazy-array lazy-array) (rank axis))
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

;;; A delayed map action is executed by applying the function specified by
;;; its fnrecord element-wise to the specified inputs.
(defstruct (delayed-map
            (:include delayed-action)
            (:constructor %make-delayed-map))
  (fnrecord (alexandria:required-argument :fnrecord)
   :type typo:fnrecord)
  (inputs (alexandria:required-argument :inputs)
   :type list))

(declaim (inline make-delayed-map))
(defun make-delayed-map (&key fnrecord inputs)
  (dolist (input inputs)
    (incf (lazy-array-refcount input)))
  (%make-delayed-map :fnrecord fnrecord :inputs inputs))

;;; A delayed multiple value map is the same as a delayed map, but for a
;;; function that produces multiple values.  Because of the nature of its
;;; return values, a lazy array whose delayed action is a delayed multiple
;;; value map must only appear as the input of a delayed nth value action
;;; and never be visible to the user.
(defstruct (delayed-multiple-value-map
            (:include delayed-map)
            (:constructor %make-delayed-multiple-value-map))
  ;; An unsigned integer whose Kth bit indicates that the Kth value of the
  ;; multiple value map is being referenced.
  (values-ntype (alexandria:required-argument :values-ntype)
   :type typo:values-ntype
   :read-only t)
  (refbits 0 :type unsigned-byte))

(declaim (inline make-delayed-multiple-value-map))
(defun make-delayed-multiple-value-map (&key fnrecord inputs values-ntype (refbits 0))
  (dolist (input inputs)
    (incf (lazy-array-refcount input)))
  (%make-delayed-multiple-value-map
   :fnrecord fnrecord
   :inputs inputs
   :values-ntype values-ntype
   :refbits refbits))

;;; A delayed nth-value action is executed by referencing the nth value of
;;; the targeted delayed multiple value map.
(defstruct (delayed-nth-value
            (:include delayed-action)
            (:constructor %make-delayed-nth-value))
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
    (atomics:atomic-update
     (delayed-multiple-value-map-refbits delayed-action)
     (lambda (refbits)
       (logior refbits (ash 1 number))))
    (%make-delayed-nth-value
     :number number
     :input input)))

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
  (%make-delayed-reshape
   :transformation transformation
   :input input))

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
  (%make-delayed-fuse
   :inputs inputs))

;;; A delayed range action is executed by assigning each index a value that
;;; is that index.
(defstruct (delayed-range
            (:include delayed-action)))

;;; A delayed array action is executed by accessing the values of the
;;; underlying storage array.
(defstruct (delayed-array
            (:include delayed-action)
            (:constructor %make-delayed-array (storage)))
  (storage (alexandria:required-argument :storage)
   :type array))

(defun make-delayed-array (object)
  (%make-delayed-array (value-array object)))

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

(defun lazy-unknown-p (lazy-array)
  (declare (lazy-array lazy-array))
  (typep (lazy-array-delayed-action lazy-array) 'delayed-unknown))

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
   :ntype (typo:ntype-of object)
   :delayed-action (make-delayed-array (make-rank-zero-array object))))

(defun lazy-array-from-array (array)
  (declare (array array))
  (if (zerop (array-rank array))
      (lazy-array-from-scalar (aref array))
      (make-lazy-array
       :shape (array-shape array)
       :ntype (typo:array-element-ntype array)
       :delayed-action
       (make-delayed-array (simplify-array array)))))

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
  (unless (empty-shape-p shape)
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
     (and (empty-shape-p (lazy-array-shape lazy-array))
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
corresponding collapsed array that can be obtained cheaply.")
  (:method ((lazy-array lazy-array)
            (delayed-action delayed-action))
    nil)
  (:method ((lazy-array lazy-array)
            (delayed-array delayed-array))
    t)
  (:method ((lazy-array lazy-array)
            (delayed-reshape delayed-reshape))
    (and (identity-transformation-p (delayed-reshape-transformation delayed-reshape))
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
  (bordeaux-threads:make-recursive-lock "Petalisp Lazy Array Lock"))
