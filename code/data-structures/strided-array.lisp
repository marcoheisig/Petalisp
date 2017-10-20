;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array (data-structure)
  (index-space))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; subclasses of the class STRIDED-ARRAY and corresponding constructors

(define-class strided-array-application (strided-array application) ())

(defmethod application ((operator function) (object strided-array)
                        &rest more-objects)
  (let ((objects (cons object more-objects)))
    (make-instance 'strided-array-application
      :operator operator
      :element-type (apply #'result-type operator
                           (mapcar #'element-type objects))
      :inputs objects
      :index-space (index-space object))))

(define-class strided-array-fusion (strided-array fusion) ())

(defmethod fusion ((object strided-array) &rest more-objects)
  (let ((objects (cons object more-objects)))
    (make-instance 'strided-array-fusion
      :element-type (element-type object)
      :inputs objects
      :index-space (apply #'fusion (mapcar #'index-space objects)))))

(define-class strided-array-reduction (strided-array reduction) ())

(defmethod reduction ((operator function) (object strided-array))
  (let ((ranges (ranges (index-space object))))
    (make-instance 'strided-array-reduction
      :operator operator
      :element-type (element-type object)
      :inputs (list object)
      :index-space (make-strided-array-index-space
                    (subseq ranges 0 (1- (length ranges)))))))

(define-class strided-array-reference (strided-array reference) ())

(defmethod reference ((object strided-array)
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
            (let ((translation-vector (make-array output-dimension :initial-element 0))
                  (column-indices (make-array output-dimension :initial-element 0))
                  (values (make-array output-dimension :initial-element 0)))
              (iterate (for input-range in-vector (ranges (index-space object)))
                       (for output-range in-vector (ranges space))
                       (for index from 0)
                       (setf (aref column-indices index) index)
                       (cond ((unary-range? output-range)
                              (setf (aref translation-vector index) (range-start output-range)))
                             ((equal? input-range output-range)
                              (setf (aref values index) 1))))
              (make-affine-transformation
               (make-array input-dimension :initial-element nil)
               (scaled-permutation-matrix
                output-dimension
                input-dimension
                column-indices
                values)
               translation-vector)))))
    (reference object space transformation)))

