;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass hairy-transformation (transformation)
  ((%input-rank :initarg :input-rank
                     :reader input-rank
                     :type unsigned-byte)
   (%output-rank :initarg :output-rank
                      :reader output-rank
                      :type unsigned-byte)
   ;; The slots %INPUT-CONSTRAINTS, %TRANSLATION, %PERMUTATION and %SCALING
   ;; are either nil or a suitable simple vector.  The number of slots of
   ;; many transformations could be reduced by introducing separate classes
   ;; for the nil case and the simple vector case.  However, this would
   ;; amount to 2^4 = 16 classes and a lot of added complexity.  So we
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
             :type (or null simple-vector))))

(defclass hairy-invertible-transformation
    (hairy-transformation
     invertible-transformation
     cached-inverse-transformation-mixin)
  ())

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
        ((:input-constraints cref))
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
             ((,cref (index) nil `(the (or null integer) (aref ,',input-constraints ,index))))
           (with-duplicate-body (null ,translation)
               ((,tref (index) 0 `(the rational (aref ,',translation ,index))))
             (with-duplicate-body (null ,permutation)
                 ((,pref (index) index `(the (or null array-index) (aref ,',permutation ,index))))
               (with-duplicate-body (null ,scaling)
                   ((,sref (index) 1 `(the rational (aref ,',scaling ,index))))
                 ,@body))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod transformation-equal
    ((transformation-1 hairy-transformation)
     (transformation-2 hairy-transformation))
  (and (= (input-rank transformation-1)
          (input-rank transformation-2))
       (= (output-rank transformation-1)
          (output-rank transformation-2))
       (equalp (input-constraints transformation-1)
               (input-constraints transformation-2))
       (equalp (translation transformation-1)
               (translation transformation-2))
       (equalp (permutation transformation-1)
               (permutation transformation-2))
       (equalp (scaling transformation-1)
               (scaling transformation-2))))

(defmethod compose-transformations
    ((g hairy-transformation) (f hairy-transformation))
  ;; A2 (A1 x + b1) + b2 = A2 A1 x + A2 b1 + b2
  (let ((input-rank (input-rank f))
        (output-rank (output-rank g)))
    (let ((input-constraints
            (if-let ((input-constraints (input-constraints f)))
              (copy-array input-constraints)
              (make-array input-rank :initial-element nil)))
          (permutation
            (make-array output-rank :initial-element nil))
          (scaling
            (make-array output-rank :initial-element 0))
          (translation
            (make-array output-rank :initial-element 0)))
      (with-hairy-transformation-refs
          (:input-constraints iref
           :permutation pref
           :scaling sref
           :translation tref)
          f
        (flet ((set-output (output-index input-index a b)
                 (cond ((null input-index)
                        (setf (aref scaling output-index) b))
                       ((and)
                        (setf (aref permutation output-index)
                              (pref input-index))
                        (setf (aref scaling output-index)
                              (* a (sref input-index)))
                        (setf (aref translation output-index)
                              (+ (* a (tref input-index)) b))))))
          (map-transformation-outputs g #'set-output)))
      (make-transformation
       :input-rank input-rank
       :output-rank output-rank
       :input-constraints input-constraints
       :permutation permutation
       :scaling scaling
       :translation translation))))

(defmethod invert-transformation
    ((transformation hairy-invertible-transformation))
  ;;    f(x) = (Ax + b)
  ;; f^-1(x) = A^-1(x - b) = A^-1 x - A^-1 b
  (let ((output-rank (input-rank transformation))
        (input-rank (output-rank transformation))
        (original-input-constraints (input-constraints transformation)))
    (let ((input-constraints
            (make-array input-rank :initial-element nil))
          (permutation
            (make-array output-rank :initial-element nil))
          (scaling
            (make-array output-rank :initial-element 0))
          (translation
            (if (not original-input-constraints)
                (make-array output-rank :initial-element 0)
                (copy-array (input-constraints transformation)))))
      (flet ((set-inputs (output-index input-index a b)
               (cond
                 ((not input-index)
                  (setf (aref input-constraints output-index) b))
                 ((/= 0 a)
                  (setf (aref permutation input-index) output-index)
                  (setf (aref scaling input-index) (/ a))
                  (setf (aref translation input-index) (- (/ b a)))))))
        (map-transformation-outputs transformation #'set-inputs))
      (make-transformation
       :input-rank input-rank
       :output-rank output-rank
       :input-constraints input-constraints
       :permutation permutation
       :scaling scaling
       :translation translation))))

(defmethod enlarge-transformation
    ((transformation hairy-transformation) scale offset)
  (let ((input-rank (1+ (input-rank transformation)))
        (output-rank (1+ (output-rank transformation)))
        (old-constraints (input-constraints transformation))
        (old-translation (translation transformation))
        (old-scaling (scaling transformation))
        (old-permutation (permutation transformation)))
    (let ((input-constraints (make-array input-rank))
          (permutation       (make-array output-rank))
          (scaling           (make-array output-rank))
          (translation       (make-array output-rank)))
      (if (not old-constraints)
          (fill input-constraints nil)
          (replace input-constraints old-constraints :start1 1))
      (if (not old-permutation)
          (loop for index below output-rank do
            (setf (aref permutation index) index))
          (replace permutation old-permutation :start1 1))
      (if (not old-scaling)
          (loop for index below output-rank
                for p across permutation do
            (setf (aref scaling index)
                  (if (not p) 0 1)))
          (replace scaling old-scaling :start1 1))
      (if (not old-translation)
          (fill translation 0)
          (replace translation old-translation))
      (setf (aref input-constraints 0) nil)
      (setf (aref permutation       0) (1- input-rank))
      (setf (aref scaling           0) scale)
      (setf (aref translation       0) offset)
      (make-transformation
       :input-rank input-rank
       :output-rank output-rank
       :input-constraints input-constraints
       :permutation permutation
       :scaling scaling
       :translation translation))))

(defmethod transform :before ((sequence sequence) (transformation hairy-transformation))
  (when-let ((input-constraints (input-constraints transformation)))
    (map nil (lambda (constraint element)
               (when (and (numberp constraint)
                          (numberp element)
                          (/= constraint element))
                 (error "~@<The number ~S violates the input constraint ~S ~
                            of the transformation ~S.~:@>"
                        element constraint transformation)))
         input-constraints sequence)))

(defmethod transform ((list list) (transformation hairy-transformation))
  (let ((result '()))
    (flet ((push-output-expression (output-index input-index a b)
             (declare (ignore output-index))
             (let* ((x (if (not input-index)
                           0
                           (elt list input-index)))
                    (a*x (cond ((= 1 a) x)
                               ((numberp x) (* a x))
                               ((and) `(* ,a ,x))))
                    (a*x+b (cond ((eql a*x 0) b)
                                 ((= b 0) a*x)
                                 ((numberp a*x) (+ a*x b))
                                 ((= b  1) `(1+ ,a*x))
                                 ((= b -1) `(1- ,a*x))
                                 ((and) `(+ ,a*x ,b)))))
               (push A*x+b result))))
      (map-transformation-outputs transformation #'push-output-expression)
      (nreverse result))))

(defmethod map-transformation-outputs
    ((transformation hairy-transformation)
     (function function)
     &key from-end)
  (let ((output-rank (output-rank transformation)))
    (with-hairy-transformation-refs
        (:input-constraints cref
         :scaling sref
         :permutation pref
         :translation tref)
        transformation
      (if (not from-end)
          (loop for output-index below output-rank
                for input-index = (pref output-index)
                for scaling = (sref output-index)
                for offset = (tref output-index) do
                  (funcall function output-index input-index scaling offset))
          (loop for output-index downfrom (1- output-rank) to 0
                for input-index = (pref output-index)
                for scaling = (sref output-index)
                for offset = (tref output-index) do
                  (funcall function output-index input-index scaling offset))))))
