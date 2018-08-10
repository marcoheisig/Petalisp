;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; STRIDED-ARRAY, its subclasses and corresponding constructors

(defclass strided-array (data-structure) ())

(defclass strided-array-application (strided-array application) ())

(defclass strided-array-fusion (strided-array fusion) ())

(defclass strided-array-reduction (strided-array reduction) ())

(defclass strided-array-reference (strided-array reference) ())

(defmethod make-application (operator (first-input strided-array) inputs)
  (multiple-value-bind (element-type function-designator)
    (infer-type operator (mapcar #'element-type inputs))
    (make-instance 'strided-array-application
      :operator function-designator
      :element-type element-type
      :inputs inputs
      :shape (shape first-input))))

(defmethod make-fusion ((first-input strided-array) all-inputs)
  (make-instance 'strided-array-fusion
    :element-type (atomic-type
                   (upgraded-array-element-type
                    `(or ,@(mapcar #'element-type all-inputs))))
    :inputs all-inputs
    :shape (apply #'shape-union (mapcar #'shape all-inputs))))

(defmethod make-reduction (binary-operator unary-operator
                           (strided-array strided-array)
                           order)
  (multiple-value-bind (unary-element-type unary-designator)
      (dx-let ((argument-types (list (element-type strided-array))))
        (infer-type unary-operator argument-types))
    (multiple-value-bind (element-type binary-designator)
        (dx-let ((argument-types (list (element-type strided-array) unary-element-type)))
          (infer-type binary-operator argument-types))
      (make-instance 'strided-array-reduction
        :binary-operator binary-designator
        :unary-operator unary-designator
        :order order
        :element-type element-type
        :inputs (list strided-array)
        :shape (shape-from-ranges (cdr (ranges (shape strided-array))))))))

(defmethod make-reference ((object strided-array)
                           (shape shape)
                           (transformation transformation))
  (make-instance 'strided-array-reference
    :element-type (element-type object)
    :inputs (list object)
    :shape shape
    :transformation transformation))

(defmethod broadcast ((object strided-array) (shape shape))
  ;; The goal is to find a transformation that maps SHAPE to the shape of
  ;; OBJECT.
  (let ((transformation
          (let* ((input-ranges (ranges shape))
                 (output-ranges (ranges (shape object)))
                 (input-dimension (length input-ranges))
                 (output-dimension (length output-ranges))
                 (translation (make-array output-dimension :initial-element 0))
                 (scaling (make-array output-dimension :initial-element 1))
                 (input-constraints (map 'vector (lambda (range)
                                                   (when (size-one-range-p range)
                                                     (range-start range)))
                                         input-ranges)))
            (loop for index below output-dimension
                  unless (>= index input-dimension) do
                    (let* ((output-range (elt output-ranges index))
                           (input-range (elt input-ranges index))
                           (output-size (set-size output-range))
                           (input-size (set-size input-range)))
                      (cond ( ;; Select
                             (> output-size input-size)
                             (setf (aref translation index) 0)
                             (setf (aref scaling index) 1))
                            ( ;; Move
                             (= output-size input-size)
                             (setf (aref translation index)
                                   (- (range-start output-range)
                                      (range-start input-range)))
                             (setf (aref scaling index)
                                   (if (size-one-range-p output-range)
                                       0
                                       (/ (range-step output-range)
                                          (range-step input-range)))))
                            ( ;; Broadcast
                             (= 1 output-size)
                             (setf (aref translation index)
                                   (- (range-start output-range)
                                      (range-start input-range)))
                             (setf (aref scaling index) 0)))))
            (make-transformation
             :output-dimension output-dimension
             :input-dimension input-dimension
             :translation translation
             :scaling scaling
             :input-constraints input-constraints))))
    (make-reference object shape transformation)))

(defmethod canonicalize-shape ((array array))
  (let* ((rank (array-rank array))
         (ranges (make-array rank)))
    (loop for axis below rank do
      (setf (aref ranges axis)
            (make-range 0 1 (1- (array-dimension array axis)))))
    (shape-from-ranges ranges)))

(defmethod canonicalize-shape ((strided-array strided-array))
  (shape strided-array))
