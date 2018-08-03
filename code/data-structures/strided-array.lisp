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
      :index-space (index-space first-input))))

(defmethod make-fusion ((first-input strided-array) all-inputs)
  (make-instance 'strided-array-fusion
    :element-type (atomic-type
                   (upgraded-array-element-type
                    `(or ,@(mapcar #'element-type all-inputs))))
    :inputs all-inputs
    :index-space (apply #'index-space-union (mapcar #'index-space all-inputs))))

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
        :index-space
        (let ((ranges (ranges (index-space strided-array))))
          (make-instance 'strided-array-index-space
            :ranges (subseq ranges 1 (length ranges))))))))

(defmethod make-reference ((object strided-array)
                           (space strided-array-index-space)
                           (transformation transformation))
  (make-instance 'strided-array-reference
    :element-type (element-type object)
    :inputs (list object)
    :index-space space
    :transformation transformation))

(defmethod broadcast ((object strided-array) (space strided-array-index-space))
  ;; The goal is to find a transformation that maps SPACE to the index
  ;; space of OBJECT.
  (let ((transformation
          (let* ((input-ranges (ranges space))
                 (output-ranges (ranges (index-space object)))
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
                    (let* ((output-range (aref output-ranges index))
                           (input-range (aref input-ranges index))
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
    (make-reference object space transformation)))

(defmethod canonicalize-index-space ((array array))
  (let* ((rank (array-rank array))
         (ranges (make-array rank)))
    (loop for axis below rank do
      (setf (aref ranges axis)
            (make-range 0 1 (1- (array-dimension array axis)))))
    (make-instance 'strided-array-index-space
      :ranges ranges)))

(defmethod canonicalize-index-space ((strided-array strided-array))
  (index-space strided-array))
