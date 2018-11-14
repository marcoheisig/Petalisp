;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass hairy-transformation (transformation)
  ((%input-mask :initarg :input-mask
                :reader input-mask
                :initform nil
                :type simple-vector)
   (%output-mask :initarg :output-mask
                 :reader output-mask
                 :initform nil
                 :type simple-vector)
   (%offsets :initarg :offsets
             :reader offsets
             :initform nil
             :type simple-vector)
   (%scalings :initarg :scalings
              :reader scalings
              :initform nil
              :type  simple-vector)))

(defclass hairy-invertible-transformation
    (hairy-transformation
     invertible-transformation
     cached-inverse-transformation-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod transformation-equal ((t1 hairy-transformation) (t2 hairy-transformation))
  (and (equalp (input-mask t1)
               (input-mask t2))
       (equalp (output-mask t1)
               (output-mask t2))
       (equalp (offsets t1)
               (offsets t2))
       (equalp (scalings t1)
               (scalings t2))))

(defmethod input-rank ((hairy-transformation hairy-transformation))
  (length (input-mask hairy-transformation)))

(defmethod output-rank ((hairy-transformation hairy-transformation))
  (length (output-mask hairy-transformation)))

(defmethod compose-transformations
    ((g hairy-transformation) (f hairy-transformation))
  ;; A2 (A1 x + b1) + b2 = A2 A1 x + A2 b1 + b2
  (let ((input-rank (input-rank f))
        (output-rank (output-rank g)))
    ;; TODO check the input mask of G.
    (let ((f-input-mask (input-mask f))
          (f-output-mask (output-mask f))
          (f-offsets (offsets f))
          (f-scalings (scalings f))
          (input-mask (copy-array (input-mask f)))
          (output-mask (make-array output-rank :initial-element nil))
          (scalings (make-array output-rank :initial-element 0))
          (offsets (make-array output-rank :initial-element 0)))
      (map-transformation-outputs
       (lambda (output-index input-index a b)
         (if (null input-index)
             (setf (svref scalings output-index) b)
             (progn
               (setf (svref output-mask output-index)
                     (svref f-output-mask input-index))
               (setf (svref scalings output-index)
                     (* a (svref f-scalings input-index)))
               (setf (svref offsets output-index)
                     (+ (* a (svref f-offsets input-index)) b)))))
       g)
      (make-transformation
       :input-rank input-rank
       :output-rank output-rank
       :input-mask input-mask
       :output-mask output-mask
       :scalings scalings
       :offsets offsets))))

(defmethod invert-transformation
    ((transformation hairy-invertible-transformation))
  ;;    f(x) = (Ax + b)
  ;; f^-1(x) = A^-1(x - b) = A^-1 x - A^-1 b
  (let* ((output-rank (input-rank transformation))
         (input-rank (output-rank transformation))
         (input-mask
           (make-array input-rank :initial-element nil))
         (output-mask
           (make-array output-rank :initial-element nil))
         (scalings
           (make-array output-rank :initial-element 0))
         (offsets
           (copy-array (input-mask transformation))))
    (map-transformation-outputs
     (lambda (output-index input-index a b)
       (if (not input-index)
           (progn (setf (aref input-mask output-index) b))
           (progn (setf (aref output-mask input-index) output-index)
                  (setf (aref scalings input-index) (/ a))
                  (setf (aref offsets input-index) (- (/ b a))))))
     transformation)
    (make-transformation
     :input-rank input-rank
     :output-rank output-rank
     :input-mask input-mask
     :output-mask output-mask
     :scalings scalings
     :offsets offsets)))

(defmethod enlarge-transformation
    ((transformation hairy-transformation) scaling offset)
  (let ((input-rank (1+ (input-rank transformation)))
        (output-rank (1+ (output-rank transformation))))
    (let ((input-mask (make-array input-rank))
          (output-mask (make-array output-rank))
          (scalings (make-array output-rank))
          (offsets (make-array output-rank)))
      (replace input-mask (input-mask transformation) :start1 1)
      (replace output-mask (output-mask transformation) :start1 1)
      (replace scalings (scalings transformation) :start1 1)
      (replace offsets (offsets transformation) :start1 1)
      (setf (aref input-mask 0) nil)
      (setf (aref output-mask 0) (1- input-rank))
      (setf (aref scalings 0) scaling)
      (setf (aref offsets 0) offset)
      (make-transformation
       :input-rank input-rank
       :output-rank output-rank
       :input-mask input-mask
       :output-mask output-mask
       :scalings scalings
       :offsets offsets))))

(defmethod transform :before ((sequence sequence) (transformation hairy-transformation))
  (when-let ((input-mask (input-mask transformation)))
    (map nil (lambda (constraint element)
               (when (and (numberp constraint)
                          (numberp element)
                          (/= constraint element))
                 (error "~@<The number ~S violates the input constraint ~S ~
                            of the transformation ~S.~:@>"
                        element constraint transformation)))
         input-mask sequence)))

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
      (map-transformation-outputs #'push-output-expression transformation)
      (nreverse result))))

(defmethod map-transformation-inputs
    ((function function)
     (transformation transformation)
     &key from-end)
  (let ((input-mask (the simple-vector (input-mask transformation))))
    (if (not from-end)
        (loop for input-index below (input-rank transformation) do
          (funcall function input-index (aref input-mask input-index)))
        (loop for input-index downfrom (1- (input-rank transformation)) to 0 do
          (funcall function input-index (aref input-mask input-index))))))

(defmethod map-transformation-outputs
    ((function function)
     (transformation hairy-transformation)
     &key from-end)
  (let ((output-rank (output-rank transformation))
        (output-mask (the simple-vector (output-mask transformation)))
        (offsets (the simple-vector (offsets transformation)))
        (scalings (the simple-vector (scalings transformation))))
    (flet ((process (output-index)
             (let ((input-index (aref output-mask output-index))
                   (scalings (aref scalings output-index))
                   (offset (aref offsets output-index)))
               (funcall function output-index input-index scalings offset))))
      (if (not from-end)
          (loop for output-index below output-rank do
            (process output-index))
          (loop for output-index downfrom (1- output-rank) to 0 do
            (process output-index))))))
