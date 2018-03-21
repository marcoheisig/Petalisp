;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/data-structures/strided-array
  (:use :closer-common-lisp :alexandria :iterate)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/all
   :petalisp/core/type-inference/all
   :petalisp/core/data-structures/index-space
   :petalisp/core/data-structures/data-structure
   :petalisp/core/data-structures/strided-array-index-space)
  (:export
   #:strided-array
   #:strided-array-immediate
   #:strided-array-application
   #:strided-array-reduction
   #:strided-array-fusion
   #:strided-array-reference))

(in-package :petalisp/core/data-structures/strided-array)

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
    :element-type (element-type first-input) ;; TODO compute type union
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
          (index-space
           (subseq ranges 0 (1- (length ranges)))))))))

(defmethod make-reference ((object strided-array)
                           (space strided-array-index-space)
                           (transformation transformation))
  (make-instance 'strided-array-reference
    :element-type (element-type object)
    :inputs (list object)
    :index-space space
    :transformation transformation))

(defmethod broadcast ((object strided-array) (space strided-array-index-space))
  (let ((transformation
          (let ((input-dimension (dimension space))
                (output-dimension (dimension object)))
            (let ((translation (make-array output-dimension :initial-element 0))
                  (permutation (make-array output-dimension :initial-element 0))
                  (scaling (make-array output-dimension :initial-element 0)))
              (iterate (for input-range in-vector (ranges (index-space object)))
                       (for output-range in-vector (ranges space))
                       (for index from 0)
                       (setf (aref permutation index) index)
                       (cond ((size-one-range-p output-range)
                              (setf (aref translation index) (range-start output-range)))
                             ((equalp input-range output-range)
                              (setf (aref scaling index) 1))))
              (make-transformation
               :output-dimension output-dimension
               :input-dimension input-dimension
               :permutation permutation
               :scaling scaling
               :translation translation)))))
    (make-reference object space transformation)))
