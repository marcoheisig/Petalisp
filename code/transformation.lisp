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
  ((domain-dimension
    :initarg :domain-dimension :reader domain-dimension
    :type integer
    :documentation
    "An integer denoting the necessary dimension of the spaces to which
   the transformation can be applied.")
   (permutation
    :initarg :permutation :reader permutation
    :type '(simple-array integer (*))
    :documentation
    "A vector of integers denoting the position of the Nth input index in
 the output index. It may be longer or shorter tha DOMAIN-DIMENSION in which
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

(defmethod initialize-instance
    :around ((instance transformation)
             &key affine-coefficients affine-mappings permutation domain-dimension)
  (assert (or (xor affine-coefficients affine-mappings) permutation))
  (assert (non-negative-integer-p domain-dimension))
  (let ((dimension
          (or (and permutation (length permutation))
              (and affine-mappings (length affine-mappings))
              (and affine-coefficients (array-dimension affine-coefficients 0)))))
    (unless affine-coefficients
      (setf affine-coefficients
            (make-array
             `(,dimension 2)
             :initial-contents
             (loop for mapping in affine-mappings
                   collect
                   (let ((b (funcall mapping 0))
                         (a+b (funcall mapping 1))
                         (2a+b (funcall mapping 2)))
                     (let ((a (- a+b b)))
                       (assert (= 2a+b (+ (* 2 a) b)))
                       (list a b)))))))
    (unless permutation
      (setf permutation
            (make-array dimension :initial-contents (iota dimension))))
    (assert (and (= 1 (array-rank permutation))
                 (= 2 (array-rank affine-coefficients))
                 (= 2 (array-dimension affine-coefficients 1))
                 (= (array-dimension permutation 0)
                    (array-dimension affine-coefficients 0))))
    (call-next-method
     instance
     :affine-coefficients affine-coefficients
     :permutation permutation
     :domain-dimension domain-dimension)))

(defmethod dimension ((object transformation))
  (length (permutation object)))

(defmethod equalp ((object-1 transformation) (object-2 transformation))
  (and (= (domain-dimension object-1) (domain-dimension object-2))
       (equalp (permutation object-1) (permutation object-2))
       (equalp (affine-coefficients object-1) (affine-coefficients object-2))))

(defmethod compose ((g transformation) (f transformation))
  (let ((domain-dimension (domain-dimension f))
        (dimension (dimension g)))
    (let ((permutation (make-array dimension))
          (affine-coefficients (make-array `(,dimension 2))))
      (assert (= (dimension f) (domain-dimension g)))
      (loop for gp across (permutation g) and i from 0 do
        (setf (aref permutation i) (aref (permutation f) gp))
        (let ((fa (aref (affine-coefficients f) gp 0))
              (fb (aref (affine-coefficients f) gp 1))
              (ga (aref (affine-coefficients g) i 0))
              (gb (aref (affine-coefficients g) i 1)))
          (setf (aref affine-coefficients i 0) (* fa ga))
          (setf (aref affine-coefficients i 1) (+ (* ga fb) gb))))
      (make-instance
       'transformation
       :affine-coefficients affine-coefficients
       :permutation permutation
       :domain-dimension domain-dimension))))

(defmethod invert ((object transformation))
  (let ((domain-dimension (dimension object))
        (dimension (domain-dimension object))
        (old-coefficients (affine-coefficients object)))
    (let ((permutation (make-array dimension))
          (affine-coefficients (make-array `(,dimension 2))))
      (loop for i below dimension do
        (setf (aref permutation i)
              (position i (permutation object))))
      (loop for p across permutation
            and i from 0 do
        (let ((a (aref old-coefficients p 0))
              (b (aref old-coefficients p 1)))
          (setf (aref affine-coefficients i 0) (/ a))
          (setf (aref affine-coefficients i 1) (/ (- b) a))))
      (make-instance
       'transformation
       :affine-coefficients affine-coefficients
       :permutation permutation
       :domain-dimension domain-dimension))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Quick notation of index space transformations
;;;
;;; Example: #t((m n) ((+ 2 n) (* 9 m)))

(defmacro expand-transformation (symbols mappings)
  (let* ((domain-dimension (length symbols))
         (dim-counter (1- domain-dimension))
         (permuted-symbols
           (loop for mapping in mappings
                 collect
                 (tree-find-if
                  (lambda (x)
                    (member x symbols :test #'eq))
                  mapping)))
         (permutation
           (map 'vector
                (lambda (psym)
                  (or (position psym symbols)
                      (incf dim-counter)))
                permuted-symbols))
         (affine-mappings
           (loop for sym in permuted-symbols
                 and body in mappings
                 collect
                 (if sym
                     `(lambda (,sym) ,body)
                     (with-gensyms (x)
                       `(lambda (,x)
                          (declare (ignore ,x))
                          ,body))))))
    `(make-instance
      'transformation
      :affine-mappings (list ,@affine-mappings)
      :permutation ',permutation
      :domain-dimension ,domain-dimension)))

(defun |#t-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  `(expand-transformation ,@(read stream t nil t)))

(set-dispatch-macro-character #\# #\t #'|#t-reader|)
