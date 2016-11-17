;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;; the valid index space transformations in Petalisp

(in-package :petalisp)

;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;;  affine transformations
;;; _________________________________________________________________
;;;
;;; (1) translating the indices by a constant
;;; (2) multiplying the indices by a constant
;;; (3) permuting the dimensions
;;; (4) introducing dimensions with an unary range
;;; (5) removing dimensions with an unary range
;;;
;;; The class AFFINE-TRANSFORMATION contains all objects that are formed by
;;; functional composition of these five elementary operations. A beautiful
;;; property is that each of these transformations is an automorhism. In
;;; particular this means they can always be inverted with INVERT and the
;;; inverse is again such a transformation.

(define-class affine-transformation (transformation)
  (permutation affine-coefficients input-constraints))

(defmethod input-dimension ((instance affine-transformation))
  (length (input-constraints instance)))

(defmethod output-dimension ((instance affine-transformation))
  (length (permutation instance)))

(defmethod initialize-instance :before ((instance affine-transformation)
                                        &key input-constraints permutation
                                          affine-coefficients)
  (let ((input-dimension (array-dimension input-constraints 0))
        (output-dimension (array-dimension permutation 0)))
    (assert (= output-dimension (array-dimension affine-coefficients 0)))
    (let ((constant-outargs
            (loop for p across permutation and i from 0
                  with c = affine-coefficients
                  count (and (not p) (= (aref c i 0) 0))))
          (dropped-inargs
            (count-if #'integerp input-constraints)))
      (assert (= (- input-dimension dropped-inargs)
                 (- output-dimension constant-outargs))))))

(defmethod classify-transformation ((f function)
                                    (input-constraints vector)
                                    (output-dimension integer))
  (multiple-value-bind (zeroes ones twos)
      (loop for input-constraint across input-constraints
            collect (or input-constraint 0) into zeroes
            collect (or input-constraint 1) into ones
            collect (or input-constraint 2) into twos
            finally (return (values zeroes ones twos)))
    (let* ((permuted-translation
             (multiple-value-list
              (apply f zeroes)))
           (args zeroes) ; shamelessly reusing memory here
           (permuted-scaling-1
             (mapcar #'-
                     (multiple-value-list (apply f ones))
                     permuted-translation))
           (permuted-scaling-2
             (mapcar (lambda (a b) (/ (- a b) 2))
                     (multiple-value-list (apply f twos))
                     permuted-translation))
           (permutation (make-array output-dimension :initial-element nil))
           (affine-coefficients (make-array `(,output-dimension 2))))
      (unless (every #'= permuted-scaling-1 permuted-scaling-2)
        (error "The transformation is not linear."))
      ;; determine the permutation
      (flet ((outpos (inpos) ; the output corresponding to the nth input
               (setf (nth inpos args) 1)
               (let ((occurences
                       (loop for i below output-dimension
                             and a in (multiple-value-list (apply f args))
                             and b in permuted-translation
                             when (/= a b) collect i)))
                 (setf (nth inpos args) 0)
                 (cond
                   ((null occurences) nil)
                   ((= 1 (length occurences)) (car occurences))
                   (t (error "Input ~d affects more than one output." inpos))))))
        (loop for input-constraint across input-constraints
              and inpos from 0 do
          (let ((outpos (if input-constraint nil (outpos inpos))))
            (when outpos
              (when (aref permutation outpos)
                (error "Output argument ~d depends on more than one variable." outpos))
              (setf (aref permutation outpos) inpos)))))
      ;; determine the affine coefficients
      (loop for outpos below output-dimension do
        (setf (aref affine-coefficients outpos 0)
              (nth outpos permuted-scaling-1))
        (setf (aref affine-coefficients outpos 1)
              (nth outpos permuted-translation)))
      (make-instance
       'affine-transformation
       :affine-coefficients affine-coefficients
       :permutation permutation
       :input-constraints input-constraints))))

(defmethod equal? ((t1 affine-transformation)
                   (t2 affine-transformation))
  (and (= (input-dimension t1)
          (input-dimension t2))
       (equalp (permutation t1)
               (permutation t2))
       (equalp (affine-coefficients t1)
               (affine-coefficients t2))
       (equalp (input-constraints t1)
               (input-constraints t2))))

(defmethod compose ((g affine-transformation) (f affine-transformation))
  (let ((compose-input-constraints
          (input-constraints f))
        (compose-permutation
          (make-array (output-dimension g)))
        (compose-affine-coefficients
          (make-array `(,(output-dimension g) 2))))
    (loop for f-pos across (permutation g)
          and g-pos from 0
          with g-coeffs = (affine-coefficients g)
          and  f-coeffs = (affine-coefficients f)
          when (not f-pos) do
            (setf (aref compose-permutation g-pos) nil)
            (setf (aref compose-affine-coefficients g-pos 0)
                  (aref g-coeffs g-pos 0))
            (setf (aref compose-affine-coefficients g-pos 1)
                  (aref g-coeffs g-pos 1))
          else do
            (setf (aref compose-permutation g-pos)
                  (aref (permutation f) f-pos))
            (setf (aref compose-affine-coefficients g-pos 0)
                  (* (aref f-coeffs f-pos 0)
                     (aref g-coeffs g-pos 0)))
            (setf (aref compose-affine-coefficients g-pos 1)
                  (+ (* (aref g-coeffs g-pos 0)
                        (aref f-coeffs f-pos 1))
                     (aref g-coeffs g-pos 1))))
    (make-instance
     'affine-transformation
     :input-constraints compose-input-constraints
     :permutation compose-permutation
     :affine-coefficients compose-affine-coefficients)))

(defmethod invert ((object affine-transformation))
  (let ((inverse-input-constraints
          (make-array (output-dimension object)))
        (inverse-permutation
          (make-array (input-dimension object)))
        (inverse-affine-coefficients
          (make-array `(,(input-dimension object) 2))))
    ;; determine the input constraints
    (loop for p across (permutation object)
          and i from 0
          with c = (affine-coefficients object) do
      (setf (aref inverse-input-constraints i)
            (unless p (aref c i 1))))
    ;; determine the permutation
    (loop for input-constraint across (input-constraints object)
          and i from 0
          with p = (permutation object) do
            (cond (input-constraint
                   (setf (aref inverse-permutation i) nil)
                   (setf (aref inverse-affine-coefficients i 0) 0)
                   (setf (aref inverse-affine-coefficients i 1) input-constraint))
                  (t
                   (setf (aref inverse-permutation i) (position i p)))))
      ;; determine the affine coefficients
    (loop for p across inverse-permutation
          and i from 0
          with c = (affine-coefficients object)
          when p do
            (let ((a (aref c p 0))
                  (b (aref c p 1)))
              (setf (aref inverse-affine-coefficients i 0) (/ a))
              (setf (aref inverse-affine-coefficients i 1) (/ (- b) a))))
    (make-instance
     'affine-transformation
     :input-constraints inverse-input-constraints
     :affine-coefficients inverse-affine-coefficients
     :permutation inverse-permutation)))

(defmethod print-object ((object affine-transformation) stream)
  (let* ((abc '(a b c d e f g h i j k l m n o p q r s t. u v w x y z))
         (variables (if (<= (input-dimension object) (length abc))
                        (subseq abc 0 (input-dimension object))
                        (loop for inpos below (input-dimension object)
                              collect (intern (format nil "V~d" inpos)))))
         (input-forms
           (loop for input-constraint across (input-constraints object)
                 and variable in variables
                 collect (or input-constraint variable)))
         (output-forms
           (loop for outpos below (output-dimension object)
                 with coefficients = (affine-coefficients object)
                 collect
                 (let* ((p (aref (permutation object) outpos))
                        (a (aref coefficients outpos 0))
                        (b (aref coefficients outpos 1))
                        (var (and p (nth p variables)))
                        (mul-form (cond ((zerop a) 0)
                                        ((= a 1) var)
                                        (t `(* ,a ,var)))))
                   (cond ((zerop b) mul-form)
                         ((eql mul-form 0) b)
                         ((plusp b) `(+ ,mul-form ,b))
                         ((minusp b) `(- ,mul-form ,(abs b))))))))
    (format stream "(τ ~a ~a)" input-forms output-forms)))

;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;;  the special case of identity transformations
;;; _________________________________________________________________

(define-class identity-transformation (affine-transformation) ())

(defmethod invert ((tr identity-transformation)) tr)

(defmethod compose ((g transformation) (f identity-transformation)) g)

(defmethod compose ((g identity-transformation) (f transformation)) f)

(defmethod equal? ((a identity-transformation) (b identity-transformation))
  (= (input-dimension a) (input-dimension b)))

(defmethod transform ((object structured-operand) (f identity-transformation))
  object)

(define-memo-function identity-transformation (dimension)
  (make-instance
   'identity-transformation
   :input-constraints (make-array dimension :initial-element nil)
   :permutation (apply #'vector (iota dimension))
   :affine-coefficients
   (make-array
    `(,dimension 2)
    :initial-contents
    (loop for i below dimension collect '(1 0)))))

(defmethod initialize-instance :after ((instance affine-transformation)
                                       &key &allow-other-keys)
  (and
   (not (identity-transformation? instance))
   (= (input-dimension instance) (output-dimension instance))
   (every #'null (input-constraints instance))
   (loop for p across (permutation instance) and i from 0
         with c = (affine-coefficients instance)
         always
         (and (= (aref c i 0) 1)
              (= (aref c i 1) 0)
              (= p i)))
   (change-class instance 'identity-transformation)))
