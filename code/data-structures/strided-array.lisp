;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp-internals)

(define-class strided-array (data-structure) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; subclasses of the class STRIDED-ARRAY and corresponding constructors

(define-class strided-array-application (strided-array application) ())

(define-class strided-array-fusion (strided-array fusion) ())

(define-class strided-array-reduction (strided-array reduction) ())

(define-class strided-array-reference (strided-array reference) ())

(defmethod make-application ((operator function) (first-input strided-array) all-inputs)
  (let ((operator-metadata
          (apply #'compute-operator-metadata
                 operator (mapcar #'element-type all-inputs))))
    (make-instance 'strided-array-application
      :operator operator
      :element-type (result-type operator-metadata)
      :operator-metadata operator-metadata
      :inputs all-inputs
      :index-space (index-space first-input))))

(defmethod make-fusion ((first-input strided-array) all-inputs)
  (make-instance 'strided-array-fusion
    :element-type (element-type first-input) ;; TODO compute type union
    :inputs all-inputs
    :index-space (apply #'union (mapcar #'index-space all-inputs))))

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
            (let ((translation (make-array output-dimension :initial-element 0))
                  (permutation (make-array output-dimension :initial-element 0))
                  (scaling (make-array output-dimension :initial-element 0)))
              (iterate (for input-range in-vector (ranges (index-space object)))
                       (for output-range in-vector (ranges space))
                       (for index from 0)
                       (setf (aref permutation index) index)
                       (cond ((unary-range? output-range)
                              (setf (aref translation index) (range-start output-range)))
                             ((equal? input-range output-range)
                              (setf (aref scaling index) 1))))
              (affine-transformation
               :output-dimension output-dimension
               :input-dimension input-dimension
               :permutation permutation
               :scaling scaling
               :translation translation)))))
    (reference object space transformation)))

(defmethod equal? ((a strided-array) (b strided-array))
  (and (equal? (index-space a) (index-space b))
       (equalp (storage a) (storage b))))
