;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array (data-structure) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; subclasses of the class STRIDED-ARRAY and corresponding constructors

(define-class strided-array-application (strided-array application) ())

(define-class strided-array-fusion (strided-array fusion) ())

(define-class strided-array-reduction (strided-array reduction) ())

(define-class strided-array-reference (strided-array reference) ())

(defmethod make-application ((operator function) (object strided-array)
                             &rest more-objects)
  (let* ((objects (cons object more-objects))
         (operator-metadata
           (apply #'compute-operator-metadata
                  operator (mapcar #'element-type objects))))
    (make-instance 'strided-array-application
      :operator operator
      :element-type (result-type operator-metadata)
      :operator-metadata operator-metadata
      :inputs objects
      :index-space (index-space object))))

(defmethod make-fusion ((object strided-array) &rest more-objects)
  (let ((objects (cons object more-objects)))
    (make-instance 'strided-array-fusion
      :element-type (element-type object)
      :inputs objects
      :index-space (apply #'union (mapcar #'index-space objects)))))

(defmethod make-reduction ((binary-operator function)
                           (unary-operator function)
                           (strided-array strided-array)
                           order)
  (let* ((unary-operator-metadata
           (compute-operator-metadata
            unary-operator (element-type strided-array)))
         (binary-operator-metadata
           (compute-operator-metadata
            binary-operator (element-type strided-array)
            (result-type unary-operator-metadata)))
         (element-type (result-type binary-operator-metadata)))
    (make-instance 'strided-array-reduction
      :binary-operator binary-operator
      :binary-operator-metadata binary-operator-metadata
      :unary-operator unary-operator
      :unary-operator-metadata unary-operator-metadata
      :order order
      :element-type element-type
      :inputs (list strided-array)
      :index-space
      (let ((ranges (ranges (index-space strided-array))))
        (index-space
         (subseq ranges 0 (1- (length ranges))))))))

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
              (affine-transformation
               (make-array input-dimension :initial-element nil)
               (scaled-permutation-matrix
                output-dimension
                input-dimension
                column-indices
                values)
               translation-vector)))))
    (reference object space transformation)))

(defmethod equal? ((a strided-array) (b strided-array))
  (and (equal? (index-space a) (index-space b))
       (equalp (storage a) (storage b))))
