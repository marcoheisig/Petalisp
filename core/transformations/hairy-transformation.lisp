;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/transformations/hairy-transformation
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/transformation
   :petalisp/core/transformations/invertible-transformation)
  (:export
   #:hairy-transformation
   #:hairy-invertible-transformation))

(in-package :petalisp/core/transformations/hairy-transformation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass hairy-transformation (transformation)
  ((%input-dimension :initarg :input-dimension
                     :reader input-dimension
                     :type unsigned-byte)
   (%output-dimension :initarg :output-dimension
                      :reader output-dimension
                      :type unsigned-byte)
   ;; The slots %INPUT-CONSTRAINTS, %TRANSLATION, %PERMUTATION and %SCALING
   ;; are either nil or a suitable simple vector. The number of slots of
   ;; many transformations could be reduced by introducing separate classes
   ;; for the nil case and the simple vector case. However, this would
   ;; amount to 2^4 = 16 classes and a lot of added complexity. So we
   ;; remain with a single class HAIRY-TRANSFORMATION to cover all these
   ;; cases.
   (%input-constraints :initarg :input-constraints
                       :reader input-constraints
                       :initform nil
                       :type (or null simple-vector))
   (%translation :initarg :translation
                 :reader translation
                 :initform nil
                 :type (or null simple-vector))
   (%permutation :initarg :permutation
                 :reader permutation
                 :initform nil
                 :type (or null simple-vector))
   (%scaling :initarg :scaling
             :reader scaling
             :initform nil
             :type (or null simple-vector)))
  (:metaclass funcallable-standard-class))

(defclass hairy-invertible-transformation
    (hairy-transformation
     invertible-transformation
     cached-inverse-transformation-mixin)
  ()
  (:metaclass funcallable-standard-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Handling of (or null simple-vector) Arrays

(defmacro with-duplicate-body (condition definitions &body body)
  (loop for (name lambda-list true-body false-body) in definitions
        collect `(,name ,lambda-list
                        (declare (ignorable ,@lambda-list))
                        ,true-body)
          into true-defs
        collect `(,name ,lambda-list
                        (declare (ignorable ,@lambda-list))
                        ,false-body)
          into false-defs
        finally
           (return
             `(if ,condition
                  (macrolet ,true-defs ,@body)
                  (macrolet ,false-defs ,@body)))))

;;; Replicate BODY 16 times for all the different possible array states.
(defmacro with-hairy-transformation-refs
    ((&key
        ((:input-constraints iref))
        ((:translation tref))
        ((:permutation pref))
        ((:scaling sref)))
     transformation &body body)
  (once-only (transformation)
    (with-gensyms (input-constraints translation permutation scaling)
      `(let ((,input-constraints (input-constraints ,transformation))
             (,translation (translation ,transformation))
             (,permutation (permutation ,transformation))
             (,scaling (scaling ,transformation)))
         (with-duplicate-body (null ,input-constraints)
             ((,iref (index) nil `(the (or null integer) (aref ,',input-constraints ,index))))
           (with-duplicate-body (null ,translation)
               ((,tref (index) 0 `(the rational (aref ,',translation ,index))))
             (with-duplicate-body (null ,permutation)
                 ((,pref (index) index `(the array-index (aref ,',permutation ,index))))
               (with-duplicate-body (null ,scaling)
                   ((,sref (index) 1 `(the rational (aref ,',scaling ,index))))
                 ,@body))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod transformation-equal
    ((transformation-1 hairy-transformation)
     (transformation-2 hairy-transformation))
  (and (= (input-dimension transformation-1)
          (input-dimension transformation-2))
       (= (output-dimension transformation-1)
          (output-dimension transformation-2))
       (equalp (input-constraints transformation-1)
               (input-constraints transformation-2))
       (equalp (translation transformation-1)
               (translation transformation-2))
       (equalp (permutation transformation-1)
               (permutation transformation-2))
       (equalp (scaling transformation-1)
               (scaling transformation-2))))

;; This function is just a temporary hack to simplify the transition to the
;; new implementation of transformations. It will be removed once matrices
;; are no longer a dependency.
(defun linear-operator (transformation)
  (scaled-permutation-matrix
   (output-dimension transformation)
   (input-dimension transformation)
   (or (permutation transformation)
       (apply #'vector (iota (output-dimension transformation))))
   (or (scaling transformation)
       (make-array (output-dimension transformation)
                   :initial-element 1))))

(defmethod compose-transformations
    ((g hairy-transformation) (f hairy-transformation))
  ;; A2 (A1 x + b1) + b2 = A2 A1 x + A2 b1 + b2
  (let ((A1 (linear-operator f))
        (A2 (linear-operator g))
        (b1 (or (translation f)
                (make-array (output-dimension f) :initial-element 0)))
        (b2 (or (translation g)
                (make-array (output-dimension g) :initial-element 0))))
    (let ((input-constraints (or (input-constraints f)
                                 (make-array (input-dimension f) :initial-element nil)))
          (linear-operator (matrix-product A2 A1))
          (translation (map 'vector #'+ (matrix-product A2 b1) b2)))
      (make-transformation
       :input-constraints input-constraints
       :permutation (spm-column-indices linear-operator)
       :scaling (spm-values linear-operator)
       :translation translation))))

(defmethod invert-transformation
    ((object hairy-invertible-transformation))
  ;;    f(x) = (Ax + b)
  ;; f^-1(x) = A^-1(x - b) = A^-1 x - A^-1 b
  (let ((A (linear-operator object))
        (b (or (translation object)
               (make-array (output-dimension object) :initial-element 0)))
        (input-constraints (make-array (output-dimension object)
                                       :initial-element nil
                                       :element-type '(or null integer))))
    ;; the new input constraints are the values of b whenever the
    ;; corresponding row of A is zero
    (loop for value across (spm-values A)
          for translation across b
          for row-index from 0 do
            (when (zerop value)
              (setf (aref input-constraints row-index) translation)))
    (let* ((linear-operator (matrix-inverse A))
           (translation (matrix-product linear-operator b)))
      (map-into translation #'- translation) ; negate b
      (loop for index below (length translation)
            for input-constraint across (or (input-constraints object)
                                            (make-array (length translation)
                                                        :initial-element nil)) do
              (when input-constraint
                (assert (= (aref translation index) 0))
                (setf (aref translation index) input-constraint)))
      (make-transformation
       :input-constraints input-constraints
       :permutation (spm-column-indices linear-operator)
       :scaling (spm-values linear-operator)
       :translation translation))))

(defmethod enlarge-transformation
    ((transformation hairy-transformation) scale offset)
  (let ((input-dimension (input-dimension transformation))
        (output-dimension (output-dimension transformation))
        (matrix (linear-operator transformation)))
    (let ((input-constraints (make-array (1+ input-dimension)))
          (permutation       (make-array (1+ output-dimension)))
          (scaling           (make-array (1+ output-dimension)))
          (translation       (make-array (1+ output-dimension))))
      (replace input-constraints (input-constraints transformation))
      (replace permutation       (spm-column-indices matrix))
      (replace scaling           (spm-values matrix))
      (replace translation       (translation transformation))
      (setf (aref input-constraints input-dimension) nil)
      (setf (aref permutation       output-dimension) input-dimension)
      (setf (aref scaling           output-dimension) scale)
      (setf (aref translation       output-dimension) offset)
      (make-transformation
       :input-constraints input-constraints
       :permutation permutation
       :scaling scaling
       :translation translation))))

(defmethod generic-unary-funcall
    ((transformation hairy-transformation)
     (s-expressions list))
  (map 'list (lambda (Ax b)
               (cond ((eql Ax 0) b)
                     ((numberp Ax) (+ Ax b))
                     ((eql b 0) Ax)
                     (t `(+ ,Ax ,b))))
       (matrix-product (linear-operator transformation) s-expressions)
       (or (translation transformation)
           (make-array (output-dimension transformation) :initial-element 0))))

(defmethod map-transformation-outputs
    ((transformation hairy-transformation)
     (function function))
  (let ((output-dimension (output-dimension transformation)))
    (with-hairy-transformation-refs
        (:input-constraints iref
         :scaling sref
         :permutation pref
         :translation tref)
        transformation
      (loop for output-index below output-dimension
            for input-index = (pref output-index)
            for scaling = (sref output-index)
            for offset = (tref output-index) do
              (funcall function output-index input-index scaling offset)))))

(defmethod print-object
    ((transformation hairy-transformation) stream)
  (let* ((variables
           (loop for index below (input-dimension transformation)
                 collect (format-symbol :keyword "I~D" index)))
         (inputs
           (if (null (input-constraints transformation))
               variables
               (loop for input-constraint across (input-constraints transformation)
                     for variable in variables
                     collect (or input-constraint variable)))))
    (princ `(τ ,inputs ,(funcall transformation inputs))
           stream)))

