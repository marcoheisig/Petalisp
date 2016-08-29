;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; transformations on structured operands
;;;
;;; the concept of transformations deserves special explanation. All
;;; transformations on structured operands are deriveable from the
;;; following five elementary transformations:
;;;
;;; (1) translating the indices by a constant
;;; (2) multiplying the indices by a constant
;;; (3) permuting the dimensions
;;; (4) introducing dimensions with only one element
;;; (5) removing dimensions with only one element
;;;
;;; The class TRANSFORMATION contains all objects that are formed by
;;; functional composition of the five elementary operations. A beautiful
;;; property is that each of these transformations is an automorhism. In
;;; particular this means they can always be inverted with INVERT and the
;;; inverse is again such a transformation.

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
             &rest rest
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
    (apply
     #'call-next-method
     instance
     :affine-coefficients affine-coefficients
     :permutation permutation
     :domain-dimension domain-dimension
     rest)))

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

(define-memo-function identity-transformation (dimension)
  (make-instance
   'transformation
   :affine-coefficients
   (make-array
    `(,dimension 2)
    :initial-contents (make-list dimension :initial-element '(1 0)))
   :permutation (make-array dimension :initial-contents (iota dimension))
   :domain-dimension dimension))

(defmethod reference :around ((object structured-operand) (space index-space)
                              &optional transformation)
  (call-next-method
   object space
   (or transformation (identity-transformation (dimension object)))))

(defmethod transform :before ((object t) (transformation transformation))
  (assert (= (dimension object) (domain-dimension transformation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Quick notation of index space transformations
;;;
;;; Examples:
;;; #2t((+ 2 i0) (* 9 (+ 3 i1)))
;;; #t((m n) (n m))

(defmacro expand-transformation (symbols &rest mappings)
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
  (declare (ignore subchar))
  (let ((rest (read stream t nil t)))
    (if arg
        `(expand-transformation
          ,(loop for i below arg collect (intern (format nil "I~d" i)))
          ,@rest)
        `(expand-transformation ,@rest))))

(set-dispatch-macro-character #\# #\t #'|#t-reader|)

(defmethod print-object ((object transformation) stream)
  (let ((coefficients (affine-coefficients object))
        (dim-counter (1- (dimension object))))
    (format
     stream "#~dt(~{~a~^ ~})"
     (domain-dimension object)
     (loop for d below (dimension object)
           collect
           (let* ((p (or (position d (permutation object))
                         (incf dim-counter)))
                  (a (aref coefficients d 0))
                  (b (aref coefficients d 1))
                  (var (intern (format nil "i~d" p)))
                  (mul-form (cond ((zerop a) nil)
                                 ((= a 1) var)
                                 (t `(* ,a ,var)))))
             (cond ((zerop b) mul-form)
                   ((not mul-form) b)
                   (t `(+ ,b ,mul-form))))))))
