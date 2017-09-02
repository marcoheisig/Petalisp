;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; definitions of externally visible Petalisp symbols

(in-package :petalisp)

(defgeneric broadcast (space-1 space-2)
  (:method ((space-1 strided-array-index-space)
            (space-2 strided-array-index-space))
    (when (>= (dimension space-1) (dimension space-2))
      (rotatef space-1 space-2))
    (let ((ranges-1 (ranges space-1))
          (ranges-2 (ranges space-2)))
      (make-instance
       'strided-array-index-space
       :ranges
       (concatenate
        'vector
        (map 'vector #'broadcast ranges-1 ranges-2)
        (subseq ranges-2 (length ranges-1))))))
  (:method ((range-1 range) (range-2 range))
    (cond
      ((unary-range? range-1) range-2)
      ((unary-range? range-2) range-1)
      ((equalp range-1 range-2) range-1)
      (t (error "Ranges not upgradeable.")))))

(test |(broadcast)|
  (flet ((? (a b result)
           (is (equal? result (broadcast a b)))))
    (? (σ (0 9)) (σ (9 9)) (σ (0 9)))
    (? (σ (9 9)) (σ (0 9)) (σ (0 9)))
    (? (σ (-5 5)) (σ (-5 5)) (σ (-5 5)))
    (? (σ) (σ (0 100) (0 100)) (σ (0 100) (0 100)))
    (? (σ (0 100) (0 100)) (σ) (σ (0 100) (0 100)))
    (signals error (broadcast (σ (2 4)) (σ (1 3))))))

(defun α (function object &rest more-objects)
  "Apply FUNCTION element-wise to OBJECT and MORE-OBJECTS, like a CL:MAPCAR
for Petalisp data structures. When the dimensions of some of the inputs
mismatch, the smaller objects are broadcasted where possible."
  (let* ((objects
           (mapcar #'petalispify (cons object more-objects)))
         (index-space
           (reduce (compose #'broadcast #'index-space) objects))
         (objects
           (mapcar
            (lambda (object)
              (repetition object index-space))
            objects)))
    (apply #'application function objects)))

(defun β (function object)
  "Reduce the last dimension of OBJECT with FUNCTION."
  (reduction function (petalispify object)))

(defun fuse (&rest objects)
  "Combine OBJECTS into a single petalisp data structure. It is an error if
some of the inputs overlap, or if there exists no suitable data structure
to represent the fusion."
  (apply #'fusion (mapcar #'petalispify objects)))

(defun fuse* (&rest objects)
  "Combine OBJECTS into a single petalisp data structure. When some OBJECTS
overlap partially, the value of the rightmost object is used."
  (let* ((objects (mapcar #'petalispify objects))
         (pieces (apply #'subdivision objects)))
    (apply
     #'fusion
     (mapcar
      (lambda (piece)
        (reference
         (find piece objects :from-end t :test #'subspace?)
         (index-space piece)
         (make-instance 'identity-transformation
                        :dimension (dimension piece))))
      pieces))))

(defun -> (data-structure &rest modifiers)
  "Manipulate DATA-STRUCTURE depending on the individual MODIFIERS. The
MODIFIERS are applied from left to right, the result of the first
modification is used as the argument to the second one and so on. The result
of the last modification is returned.

When a modifier is of type INDEX-SPACE, it denotes a selection of the given
data structure. For example the modifier (σ (7 9)) would select only the
elements with the keys 7, 8 and 9 from the given argument.

When a modifier is of type TRANSFORMATION, the argument is permuted
accordingly. For example applying the transformation (τ (m n) (n m) to a
3x10 array would result in a 10x3 array."
  (labels ((apply-modification (x modifier)
             (etypecase modifier
               (data-structure
                (if (or (/= (dimension x) (dimension modifier))
                        (subspace? x modifier))
                    (repetition x modifier)
                    (reference x (intersection modifier x)
                               (make-instance 'identity-transformation
                                              :dimension (dimension x)))))
               (transformation
                (reference x (index-space x) modifier)))))
    (reduce #'apply-modification modifiers
            :initial-value (petalispify data-structure))))

