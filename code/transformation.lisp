;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; transformations on structured operands
;;;
;;; the concept of transformations deserves special explanation. All
;;; transformations in Petalisp are deriveable from the following five
;;; elementary transformations:
;;;
;;; (1) shifting the indices by a constant
;;; (2) multiplying the indices by a constant
;;; (3) permuting the indices
;;; (4) introducing indices with only one element
;;; (5) removing indices with only one element
;;;
;;; The class TRANSFORMATION contains all objects that are formed by
;;; functional composition of the five elementary operations. A beautiful
;;; property is that each of these transformations is an isomorphism. In
;;; particular this means they can be inverted with INVERT.

(define-class transformation ()
  ((input-dimension
    :initarg :input-dimension :reader input-dimension
    :type integer
    :documentation
    "An integer denoting the necessary dimension of the spaces to which
   the transformation can be applied.")
   (permutation
    :initarg :permutation :reader permutation
    :type '(simple-array integer (*))
    :documentation
    "A vector of integers denoting the position of the Nth input index in
 the output index. It may be longer or shorter tha INPUT-DIMENSION in which
 case the superfluous or missing indices are dropped, or introduced with a
 sole index of zero.")
   (affine-coefficients
    :initarg :affine-coefficients :reader affine-coefficients
    :type '(simple-array integer (* 2))
    :documentation
    "An array with the same zeroth dimension as PERMUTATION and a first
    dimension of two. The (X 0) column contains the scaling factors and
    the (X 1) column contains the offsets that specify the affine linear
    function that is applied to the Xth output index.")))

(defmethod dimension ((object transformation))
  (length (permutation object)))

(defmethod equalp ((object-1 transformation) (object-2 transformation))
  (and (= (input-dimension object-1) (input-dimension object-2))
       (equalp (permutation object-1) (permutation object-2))
       (equalp (affine-coefficients object-1) (affine-coefficients object-2))))

(defmethod compose ((object-1 transformation) (object-2 transformation))
  TODO)

(defmethod invert ((object transformation))
  TODO)

;;; several ways to initialize affine transformations:
;;; (transformation :permutation '(0 2 1) :input-dimension 2)
;;; (transformation :permutation '(0 2) :input-dimension 3)
;;; (transformation :translation '(0 0) :scaling '(0 2))
;;; (transformation :affine-mappings (list #'identity (lambda (x) (* 2 x))))
(defmethod initialize-instance
    :around ((instance transformation)
             &key translation scaling affine-mappings permutation input-dimension)
  (let ((explicit-coefficients (or translation scaling))
        (implicit-coefficients affine-mappings)
        (lengths (list (length translation)
                       (length scaling)
                       (length affine-mappings)
                       (length permutation))))
    (let ((dimension (apply #'max lengths)))
      (assert (every (lambda (x) (or (zerop x) (= x dimension)))
                     lengths))
      (let ((affine-coefficients
              (cond
                ((and explicit-coefficients (not implicit-coefficients))
                 (let ((translation (or translation
                                        (make-list dimension :initial-element 0)))
                       (scaling (or scaling
                                    (make-list dimension :initial-element 1))))
                   (make-array `(,dimension 2)
                               :initial-contents
                               (mapcar #'list translation scaling))))
                ((and implicit-coefficients (not explicit-coefficients))
                 (make-array `(,dimension 2)
                             :initial-contents
                             (loop for mapping in affine-mappings
                                   collect
                                   (let ((b (funcall mapping 0))
                                         (a+b (funcall mapping 1))
                                         (2a+b (funcall mapping 2)))
                                     (let ((a (- a+b b)))
                                       (assert (= 2a+b (+ (* 2 a) b)))
                                       (list a b)))))))))
        (call-next-method
         instance
         :affine-coefficients affine-coefficients
         :permutation (or permutation (iota dimension))
         :growth growth)))))
