;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric lazy-array-p (object))

(defgeneric empty-array-p (object))

(defgeneric immediatep (object))

(defgeneric reusablep (object))

(defgeneric coerce-to-lazy-array (array))

(defgeneric total-size (array))

(defgeneric element-ntype (array))

(defgeneric element-type (array))

(defgeneric shape (array))

(defgeneric inputs (array))

(defgeneric value-n (array))

(defgeneric storage (array))

(defgeneric operator (array))

(defgeneric refcount (array))

(defgeneric increment-refcount (array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass lazy-array ()
  ())

(defclass immediate (lazy-array)
  ())

(defclass non-immediate (lazy-array)
  ((%inputs :initarg :inputs :reader inputs)))

(defclass empty-array (immediate)
  ())

(defclass non-empty-array (lazy-array)
  ((%shape :initarg :shape :reader shape :reader shape)
   (%ntype :initarg :ntype :reader element-ntype)))

(defclass non-empty-immediate (non-empty-array immediate)
  ())

(defclass non-empty-non-immediate (non-empty-array non-immediate)
  ((%refcount :initform 0 :accessor refcount)))

(defclass array-immediate (non-empty-immediate)
  ((%reusablep :initarg :reusablep :initform nil :reader reusablep)
   (%storage :initarg :storage :reader storage)))

(defclass range-immediate (non-empty-immediate)
  ())

(defclass application (non-empty-non-immediate)
  ((%operator :initarg :operator :reader operator)
   (%value-n :initarg :value-n :reader value-n :type (integer 0 #.multiple-values-limit))))

(defclass reduction (non-empty-non-immediate)
  ((%operator :initarg :operator :reader operator)
   (%value-n :initarg :value-n :reader value-n :type (integer 0 #.multiple-values-limit))))

(defclass fusion (non-empty-non-immediate)
  ())

(defclass reference (non-empty-non-immediate)
  ((%transformation :initarg :transformation :reader transformation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod initialize-instance :after
    ((non-immediate non-immediate) &key &allow-other-keys)
  (mapc #'increment-refcount (inputs non-immediate)))

(defmethod coerce-to-lazy-array ((lazy-array lazy-array))
  lazy-array)

(defmethod coerce-to-lazy-array ((array array))
  (make-array-immediate array))

(defmethod coerce-to-lazy-array ((object t))
  (make-scalar-immediate object))

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
  (load-time-value (make-shape '())))

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

(defmethod refcount ((object t))
  0)

(defmethod increment-refcount ((object t))
  (declare (ignore object))
  (values))

(defmethod increment-refcount ((non-empty-non-immediate non-empty-non-immediate))
  (incf (refcount non-empty-non-immediate)))

(defmethod print-object ((lazy-array lazy-array) stream)
  (print-unreadable-object (lazy-array stream :type t)
    (format stream "~S ~S" (element-type lazy-array) (shape lazy-array))))

(defmethod print-object ((array-immediate array-immediate) stream)
  (print-unreadable-object (array-immediate stream :type t)
    (princ (storage array-immediate) stream)))

(defmethod print-object ((range-immediate range-immediate) stream)
  (print-unreadable-object (range-immediate stream :type t)
    (format stream ":SHAPE ~A" (shape range-immediate))))

;; TODO remove this function?
(defmethod transform ((lazy-array lazy-array) (transformation transformation))
  (make-reference
   lazy-array
   (transform (shape lazy-array) transformation)
   (invert-transformation transformation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Immediate Constructors

(defun make-scalar-immediate (object)
  (make-instance 'array-immediate
    :shape (~)
    :ntype (petalisp.type-inference:ntype-of object)
    :storage (petalisp.type-inference:make-rank-zero-array object)))

(defun make-array-immediate (array &optional reusablep)
  (check-type array array)
  (if (zerop (array-total-size array))
      (empty-array)
      (make-instance 'array-immediate
        :shape (shape array)
        :storage array
        :reusablep reusablep
        :ntype (petalisp.type-inference:array-element-ntype array))))

(defun make-range-immediate (range)
  (make-instance 'range-immediate
    :shape (make-shape (list range))
    :ntype
    (petalisp.type-inference:ntype-union
     (petalisp.type-inference:ntype-of (range-start range))
     (petalisp.type-inference:ntype-of (range-end range)))))

(defun indices (array-or-shape &optional (axis 0))
  (cond ((null array-or-shape)
         (empty-array))
        ((shapep array-or-shape)
         (let ((rank (shape-rank array-or-shape)))
           (unless (<= 0 axis (1- rank))
             (error "~@<Invalid axis ~A for a shape with rank ~D.~:@>" axis rank))
           (make-reference
            (make-range-immediate (nth axis (shape-ranges array-or-shape)))
            array-or-shape
            (make-transformation
             :input-rank rank
             :output-mask (vector axis)))))
        (t (indices (shape array-or-shape)))))

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
