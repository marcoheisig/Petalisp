;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric lazy-array-p (object))

(defgeneric empty-array-p (object))

(defgeneric immediatep (object))

(defgeneric reusablep (object))

(defgeneric total-size (array))

(defgeneric element-ntype (array))

(defgeneric element-type (array))

(defgeneric shape (array))

(defgeneric inputs (array))

(defgeneric value-n (array))

(defgeneric number-of-values (array))

(defgeneric storage (array))

(defgeneric operator (array))

(defgeneric lazy-array (array))

(defgeneric replace-lazy-array (lazy-array replacement))

(defgeneric refcount (array))

(defgeneric depth (array))

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
    :reader inputs
    :type list)))

(defclass empty-array (immediate)
  ())

(defclass non-empty-array (lazy-array)
  ((%shape
    :initarg :shape
    :reader shape)
   (%ntype
    :initarg :ntype
    :reader element-ntype)
   (%refcount
    :reader refcount
    :accessor %refcount
    :type unsigned-byte
    :initform 0)
   (%depth
    :reader depth
    :accessor %depth
    :type unsigned-byte
    :initform 0))
  (:default-initargs
   :shape (~)
   :ntype (petalisp.type-inference:ntype 't)))

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
    :type simple-array
    :reader storage)))

(defclass range-immediate (non-empty-immediate)
  ())

(defclass lazy-map (non-empty-non-immediate)
  ((%operator
    :initarg :operator
    :reader operator
    :type (or function symbol))))

(defclass single-value-lazy-map (lazy-map)
  ())

(defclass multiple-value-lazy-map (lazy-map)
  ((%number-of-values
    :initarg :number-of-values
    :reader number-of-values
    :type (integer 0 (#.multiple-values-limit)))
   (%value-n
    :initarg :value-n
    :reader value-n
    :type (integer 0 (#.(1- multiple-values-limit))))
   ;; The identity is a cons cell that is shared among all instances that
   ;; are the result of mapping a multiple valued operator over some
   ;; arguments.  It is later used to merge instances into a single kernel
   ;; whenever possible.
   (%identity
    :initarg :identity
    :reader identity-of
    :type cons)))

(defclass lazy-fuse (non-empty-non-immediate)
  ())

(defclass lazy-reshape (non-empty-non-immediate)
  ((%transformation
    :initarg :transformation
    :reader transformation)))

(defclass parameter (non-empty-immediate)
  ()
  (:default-initargs :computable nil :shape (~)))

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
       (element-type nil element-type-supplied-p)
       (inputs '()))
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
    for input in (inputs non-immediate)
    sum 1 into refcount fixnum
    maximize (depth input) into depth
    unless (computablep input) do (setf computablep nil)
      finally
         (setf (%computablep non-immediate)
               computablep)
         (setf (%refcount non-immediate)
               refcount)
         (setf (%depth non-immediate)
               (1+ depth))))

(defmethod lazy-array ((lazy-array lazy-array))
  lazy-array)

(defmethod lazy-array ((array array))
  (make-array-immediate array))

(defmethod lazy-array ((object t))
  (make-scalar-immediate object))

(defmethod replace-lazy-array ((instance lazy-reshape) (replacement lazy-reshape))
  (reinitialize-instance instance
    :transformation (transformation replacement)
    :inputs (inputs replacement)))

(defmethod replace-lazy-array ((instance lazy-array) (replacement lazy-reshape))
  (change-class instance (class-of replacement)
    :transformation (transformation replacement)
    :inputs (inputs replacement)))

(defmethod replace-lazy-array ((instance lazy-array) (replacement array-immediate))
  (change-class instance (class-of replacement)
    :storage (storage replacement)))

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
  (shape-size (shape non-empty-array)))

(defmethod element-type ((object t))
  (petalisp.type-inference:type-specifier
   (element-ntype object)))

(defmethod element-ntype ((object t))
  (petalisp.type-inference:ntype-of object))

(defmethod element-ntype ((array array))
  (petalisp.type-inference:array-element-ntype array))

(defmethod element-ntype ((empty-array empty-array))
  (petalisp.type-inference:ntype 'nil))

(defmethod shape ((object t))
  (~))

(defmethod shape ((array array))
  (if (zerop (array-total-size array))
      nil
      (%make-shape
       (loop for axis below (array-rank array)
             collect
             (range 0 1 (1- (array-dimension array axis))))
       (array-rank array))))

(defmethod shape ((empty-array empty-array))
  (error "The empty array has no shape."))

(defmethod shape ((shape shape))
  shape)

(defmethod rank ((object t))
  0)

(defmethod rank ((array array))
  (array-rank array))

(defmethod rank ((lazy-array lazy-array))
  (shape-rank (shape lazy-array)))

(defmethod rank ((shape shape))
  (shape-rank shape))

(defmethod inputs ((object t))
  '())

(defun input (object)
  (destructuring-bind (input) (inputs object) input))

(defmethod print-object ((empty-array empty-array) stream)
  (print-unreadable-object (empty-array stream :type t :identity t)))

(defmethod print-object ((lazy-array lazy-array) stream)
  (print-unreadable-object (lazy-array stream :type t :identity t)
    (format stream "~S ~S"
            (element-type lazy-array)
            (shape lazy-array))))

;; TODO remove this function?
(defmethod transform ((lazy-array lazy-array) (transformation transformation))
  (lazy-reshape
   lazy-array
   (transform (shape lazy-array) transformation)
   (invert-transformation transformation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Immediate Constructors

(defvar *scalar-immediate-cache*
  (trivial-garbage:make-weak-hash-table :weakness :value))

(defmacro scalar-immediate-cache (key)
  `(values (gethash ,key *scalar-immediate-cache*)))

(defun make-scalar-immediate (object)
  (or (scalar-immediate-cache object)
      (setf (scalar-immediate-cache object)
            (make-instance 'array-immediate
              :shape (~)
              :ntype (petalisp.type-inference:ntype-of object)
              :storage (petalisp.type-inference:make-rank-zero-array object)))))

(defun make-array-immediate (array &optional reusablep)
  (check-type array array)
  (cond ((zerop (array-total-size array))
         (empty-array))
        ((zerop (array-rank array))
         (make-scalar-immediate (aref array)))
        (t
         (make-instance 'array-immediate
           :shape (shape array)
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
       (~r range)
       (make-transformation
        :input-rank 1
        :output-rank 0))
      (make-instance 'range-immediate
        :shape (~r range)
        :ntype
        (petalisp.type-inference:ntype-union
         (petalisp.type-inference:ntype-of (range-start range))
         (petalisp.type-inference:ntype-of (range-end range))))))

(defun indices (array-or-shape &optional (axis 0))
  (if (null array-or-shape)
      (empty-array)
      (let* ((shape (shape array-or-shape))
             (rank (shape-rank shape)))
        (unless (<= 0 axis (1- rank))
          (error "~@<Invalid axis ~A for a shape with rank ~D.~:@>" axis rank))
        (lazy-reshape
         (make-range-immediate (nth axis (shape-ranges shape)))
         shape
         (make-transformation
          :input-rank rank
          :output-mask (vector axis))))))

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
