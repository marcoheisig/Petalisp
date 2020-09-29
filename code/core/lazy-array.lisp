;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric lazy-array-p (object))

(defgeneric empty-array-p (object))

(defgeneric immediatep (object))

(defgeneric total-size (array))

(defgeneric element-ntype (array))

(defgeneric element-type (array))

(defgeneric array-shape (array))

(defgeneric lazy-array (array))

(defgeneric lazy-array-inputs (array))

(defgeneric lazy-array-shape (lazy-array))

(defgeneric lazy-array-refcount (lazy-array))

(defgeneric lazy-array-depth (lazy-array))

(defgeneric replace-lazy-array (lazy-array replacement))

(defgeneric lazy-multiple-value-ref-value-n (array))

(defgeneric lazy-map-operator (array))

(defgeneric lazy-map-number-of-values (array))

(defgeneric array-immediate-storage (array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass lazy-array ()
  ((%computable
    :initarg :computable
    :reader computablep
    :accessor %computablep)))

(defclass immediate (lazy-array)
  ()
  (:default-initargs :computable t))

(defclass non-immediate (lazy-array)
  ((%inputs
    :initarg :inputs
    :reader lazy-array-inputs
    :type list)))

(defclass empty-array (immediate)
  ())

(defclass non-empty-array (lazy-array)
  ((%shape
    :initarg :shape
    :initform (alexandria:required-argument :shape)
    :reader array-shape
    :reader lazy-array-shape)
   (%ntype
    :initarg :ntype
    :initform (alexandria:required-argument :ntype)
    :reader element-ntype)
   (%refcount
    :reader lazy-array-refcount
    :accessor %lazy-array-refcount
    :type unsigned-byte
    :initform 0)
   (%depth
    :reader lazy-array-depth
    :accessor %lazy-array-depth
    :type unsigned-byte
    :initform 0)))

(defclass non-empty-immediate (non-empty-array immediate)
  ())

(defclass non-empty-non-immediate (non-empty-array non-immediate)
  ())

(defclass array-immediate (non-empty-immediate)
  ((%reusablep
    :initarg :reusablep
    :initform nil
    :reader reusablep)
   (%storage
    :initarg :storage
    :reader array-immediate-storage
    :type simple-array)))

(defclass range-immediate (non-empty-immediate)
  ())

(defclass lazy-map (non-empty-non-immediate)
  ((%operator
    :initarg :operator
    :reader lazy-map-operator
    :type (or function symbol))))

(defclass lazy-multiple-value-map (lazy-map)
  ((%number-of-values
    :initarg :number-of-values
    :reader lazy-map-number-of-values
    :type (integer 0 (#.multiple-values-limit)))))

(defclass lazy-multiple-value-ref (non-empty-non-immediate)
  ((%value-n
    :initarg :value-n
    :reader lazy-multiple-value-ref-value-n
    :type (integer 0 (#.(1- multiple-values-limit))))))

(defclass lazy-fuse (non-empty-non-immediate)
  ())

(defclass lazy-reshape (non-empty-non-immediate)
  ((%transformation
    :initarg :transformation
    :reader transformation)))

(defclass parameter (non-empty-immediate)
  ()
  (:default-initargs :computable nil))

(defclass optional-parameter (parameter)
  ((%value
    :initarg :value
    :initform nil
    :accessor optional-parameter-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

;;; Turn any supplied :element-type argument into a suitable :ntype.
(defmethod shared-initialize
    ((instance non-empty-array) slot-names
     &rest args
     &key
       (element-type nil element-type-supplied-p))
  (if (and element-type-supplied-p
           (or (eql slot-names 't)
               (member '%ntype slot-names)))
      (apply #'call-next-method instance slot-names
             :ntype (petalisp.type-inference:ntype element-type)
             args)
      (call-next-method)))

(defmethod initialize-instance :after
    ((non-immediate non-immediate) &key &allow-other-keys)
  (loop
    with computablep = t
    for input in (lazy-array-inputs non-immediate)
    maximize (lazy-array-depth input) into lazy-array-depth
    do (incf (%lazy-array-refcount input))
    unless (computablep input) do (setf computablep nil)
      finally
         (setf (%computablep non-immediate)
               computablep)
         (setf (%lazy-array-depth non-immediate)
               (1+ lazy-array-depth))))

(defmethod lazy-array ((lazy-array lazy-array))
  lazy-array)

(defmethod lazy-array ((array array))
  (make-array-immediate array))

(defmethod lazy-array ((object t))
  (make-scalar-immediate object))

(defmethod replace-lazy-array
    ((instance empty-array)
     (replacement empty-array))
  ;; TODO
  instance)

(defmethod replace-lazy-array ((instance lazy-reshape) (replacement lazy-reshape))
  (reinitialize-instance instance
    :transformation (transformation replacement)
    :inputs (lazy-array-inputs replacement)))

(defmethod replace-lazy-array ((instance lazy-array) (replacement lazy-reshape))
  (change-class instance (class-of replacement)
    :transformation (transformation replacement)
    :inputs (lazy-array-inputs replacement)))

(defmethod replace-lazy-array ((instance lazy-array) (replacement array-immediate))
  (change-class instance (class-of replacement)
    :storage (array-immediate-storage replacement)))

(defmethod replace-lazy-array ((instance lazy-array) (replacement range-immediate))
  (change-class instance (class-of replacement)))

(defmethod lazy-array-p ((object t))
  (declare (ignore object))
  nil)

(defmethod lazy-array-p ((lazy-array lazy-array))
  (declare (ignore lazy-array))
  t)

(defmethod empty-array-p ((object t))
  (declare (ignore object))
  nil)

(defmethod empty-array-p ((array array))
  (zerop (array-total-size array)))

(defmethod empty-array-p ((empty-array empty-array))
  (declare (ignore empty-array))
  t)

(defmethod immediatep ((object t))
  (declare (ignore object))
  nil)

(defmethod immediatep ((immediate immediate))
  (declare (ignore immediate))
  t)

(defmethod total-size ((object t))
  1)

(defmethod total-size ((array array))
  (array-total-size array))

(defmethod total-size ((empty-array empty-array))
  0)

(defmethod total-size ((non-empty-array non-empty-array))
  (shape-size (array-shape non-empty-array)))

(defmethod element-type ((object t))
  (petalisp.type-inference:type-specifier
   (element-ntype object)))

(defmethod element-ntype ((object t))
  (petalisp.type-inference:ntype-of object))

(defmethod element-ntype ((array array))
  (petalisp.type-inference:array-element-ntype array))

(defmethod element-ntype ((empty-array empty-array))
  (petalisp.type-inference:ntype 'nil))

(defmethod array-shape ((object t))
  (load-time-value
   (make-shape '())))

(defmethod array-shape ((array array))
  (make-shape
   (loop for axis below (array-rank array)
         collect
         (range (array-dimension array axis)))))

(defmethod array-shape ((empty-array empty-array))
  (make-empty-shape 0))

(defmethod rank ((object t))
  0)

(defmethod rank ((array array))
  (array-rank array))

(defmethod rank ((lazy-array lazy-array))
  (shape-rank (array-shape lazy-array)))

(defmethod rank ((shape shape))
  (shape-rank shape))

(defmethod lazy-array-inputs ((object t))
  '())

(defun lazy-array-input (object)
  (destructuring-bind (input) (lazy-array-inputs object) input))

(defmethod lazy-map-number-of-values ((lazy-map lazy-map))
  1)

(defmethod print-object ((empty-array empty-array) stream)
  (print-unreadable-object (empty-array stream :type t :identity t)))

(defmethod print-object ((lazy-array lazy-array) stream)
  (print-unreadable-object (lazy-array stream :type t :identity t)
    (format stream "~S ~S"
            (element-type lazy-array)
            (array-shape lazy-array))))

;; TODO remove this function?
(defmethod transform ((lazy-array lazy-array) (transformation transformation))
  (lazy-reshape
   lazy-array
   (transform (array-shape lazy-array) transformation)
   (invert-transformation transformation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Immediate Constructors

(defun make-scalar-immediate (object)
  (make-instance 'array-immediate
    :shape (load-time-value (make-shape '()))
    :ntype (petalisp.type-inference:ntype-of object)
    :storage (petalisp.type-inference:make-rank-zero-array object)))

(defun make-array-immediate (array &optional reusablep)
  (check-type array array)
  (cond ((zerop (array-total-size array))
         (empty-array))
        ((zerop (array-rank array))
         (make-scalar-immediate (aref array)))
        (t
         (make-instance 'array-immediate
           :shape (array-shape array)
           :storage (simplify-array array)
           :reusablep reusablep
           :ntype (petalisp.type-inference:array-element-ntype array)))))

(defun simplify-array (array)
  (if (typep array 'simple-array)
      array
      (let ((copy (make-array (array-dimensions array)
                              :element-type (array-element-type array))))
        (loop for index below (array-total-size array) do
          (setf (row-major-aref copy index)
                (row-major-aref array index)))
        copy)))

(defun make-range-immediate (range)
  (if (size-one-range-p range)
      (lazy-reshape
       (make-scalar-immediate (range-start range))
       (make-shape (list range))
       (make-transformation
        :input-rank 1
        :output-rank 0))
      (make-instance 'range-immediate
        :shape (make-shape (list range))
        :ntype (petalisp.type-inference:ntype-union
                (petalisp.type-inference:ntype-of (range-start range))
                (petalisp.type-inference:ntype-of (range-last range))))))

(defun empty-array ()
  (load-time-value
   (make-instance 'empty-array)))

(defun empty-arrays (n)
  (case n
    (0 (values))
    (1 (values (empty-array)))
    (2 (values (empty-array) (empty-array)))
    (otherwise (values-list (make-list n :initial-element (empty-array))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pattern Matching

(trivia:defpattern lazy-array (shape)
  (alexandria:with-gensyms (it)
    `(trivia:guard1 ,it (lazy-array-p ,it)
                    (shape ,it) ,shape)))
