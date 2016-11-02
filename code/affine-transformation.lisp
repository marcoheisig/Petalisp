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

(define-class affine-transformation (transformation)
  ((permutation
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

(defmethod initialize-instance
    :around ((instance affine-transformation)
             &rest rest
             &key affine-coefficients affine-mappings permutation input-dimension)
  (assert (or (xor affine-coefficients affine-mappings) permutation))
  (assert (non-negative-integer-p input-dimension))
  (let ((output-dimension
          (or (and permutation (length permutation))
              (and affine-mappings (length affine-mappings))
              (and affine-coefficients (array-dimension affine-coefficients 0)))))
    (unless affine-coefficients
      (setf affine-coefficients
            (make-array
             `(,output-dimension 2)
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
            (make-array output-dimension :initial-contents (iota output-dimension))))
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
     :input-dimension input-dimension
     :output-dimension output-dimension
     rest)))

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
       :input-dimension input-dimension))))

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
       :input-dimension input-dimension))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Quick notation of index space transformations
;;;
;;; Examples:
;;; #2t((+ 2 i0) (* 9 (+ 3 i1)))
;;; #t((m n) (n m))

(defmacro expand-transformation (symbols &rest mappings)
  (let* ((input-dimension (length symbols))
         (dim-counter (1- input-dimension))
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
      :input-dimension ,input-dimension)))

(defun |#t-reader| (stream subchar arg)
  (declare (ignore subchar))
  (let ((rest (read stream t nil t)))
    (if arg
        `(expand-transformation
          ,(loop for i below arg collect (intern (format nil "I~d" i)))
          ,@rest)
        `(expand-transformation ,@rest))))

(set-dispatch-macro-character #\# #\t #'|#t-reader|)

(defmethod print-object ((object affine-transformation) stream)
  (let ((coefficients (affine-coefficients object))
        (dim-counter (1- (output-dimension object))))
    (format
     stream "#~dt(~{~a~^ ~})"
     (input-dimension object)
     (loop for d below (input-dimension object)
           collect
           (let* ((p (or (position d (permutation object))
                         (incf dim-counter)))
                  (a (aref coefficients d 0))
                  (b (aref coefficients d 1))
                  (var (intern (format nil "i~d" p)))
                  (mul-form (cond ((zerop a) 0)
                                  ((= a 1) var)
                                  (t `(* ,a ,var)))))
             (cond ((zerop b) mul-form)
                   ((eql mul-form 0) b)
                   (t `(+ ,b ,mul-form))))))))
