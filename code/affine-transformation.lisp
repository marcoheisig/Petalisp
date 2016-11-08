;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; affine transformations
;;;
;;; (1) translating the indices by a constant
;;; (2) multiplying the indices by a constant
;;; (3) permuting the dimensions
;;; (4) introducing dimensions with only one element
;;; (5) removing dimensions with only one element
;;;
;;; The class AFFINE-TRANSFORMATION contains all objects that are formed by
;;; functional composition of these five elementary operations. A beautiful
;;; property is that each of these transformations is an automorhism. In
;;; particular this means they can always be inverted with INVERT and the
;;; inverse is again such a transformation.

(defmethod initialize-instance :after ((instance affine-transformation)
                                       &key input-dimension output-dimension
                                         permutation affine-coefficients)
  (when (and
         (not (identity-transformation? instance))
         (= input-dimension output-dimension)
         (loop for p across permutation and i from 0
               always
               (and (= (aref affine-coefficients i 0) 1)
                    (= (aref affine-coefficients i 1) 0)
                    (= p i))))
    (change-class instance 'identity-transformation)))

(defmethod equal? ((object-1 affine-transformation)
                   (object-2 affine-transformation))
  (and (= (input-dimension object-1)
          (input-dimension object-2))
       (equalp (permutation object-1)
               (permutation object-2))
       (equalp (affine-coefficients object-1)
               (affine-coefficients object-2))))

(defmethod compose ((g affine-transformation) (f affine-transformation))
  (let ((input-dimension (input-dimension f))
        (dimension (output-dimension g)))
    (let ((permutation (make-array dimension))
          (affine-coefficients (make-array `(,dimension 2))))
      (assert (= (output-dimension f) (input-dimension g)))
      (loop for gp across (permutation g) and i from 0 do
        (setf (aref permutation i) (aref (permutation f) gp))
        (let ((fa (aref (affine-coefficients f) gp 0))
              (fb (aref (affine-coefficients f) gp 1))
              (ga (aref (affine-coefficients g) i 0))
              (gb (aref (affine-coefficients g) i 1)))
          (setf (aref affine-coefficients i 0) (* fa ga))
          (setf (aref affine-coefficients i 1) (+ (* ga fb) gb))))
      (make-instance
       'affine-transformation
       :affine-coefficients affine-coefficients
       :permutation permutation
       :input-dimension input-dimension
       :output-dimension dimension))))

(defmethod invert ((object affine-transformation))
  (let ((input-dimension (output-dimension object))
        (output-dimension (input-dimension object))
        (old-coefficients (affine-coefficients object)))
    (let ((permutation (make-array output-dimension))
          (affine-coefficients (make-array `(,output-dimension 2))))
      (loop for i below output-dimension do
        (setf (aref permutation i)
              (position i (permutation object))))
      (loop for p across permutation
            and i from 0 do
              (let ((a (aref old-coefficients p 0))
                    (b (aref old-coefficients p 1)))
                (setf (aref affine-coefficients i 0) (/ a))
                (setf (aref affine-coefficients i 1) (/ (- b) a))))
      (make-instance
       'affine-transformation
       :affine-coefficients affine-coefficients
       :permutation permutation
       :input-dimension input-dimension
       :output-dimension output-dimension))))

(defmethod print-object ((object affine-transformation) stream)
  (loop for i below (output-dimension object)
        with coefficients = (affine-coefficients object)
        collect
        (let* ((p (aref (permutation object) i))
               (a (aref coefficients i 0))
               (b (aref coefficients i 1))
               (var (intern (format nil "i~a" p)))
               (mul-form (cond ((zerop a) 0)
                               ((= a 1) var)
                               (t `(* ,a ,var)))))
          (cond ((zerop b) mul-form)
                ((eql mul-form 0) b)
                (t `(+ ,b ,mul-form))))
          into expressions
        finally
           (format stream "(τ (~{i~d~^ ~}) ~a)"
                   (iota (input-dimension object))
                   expressions)))
