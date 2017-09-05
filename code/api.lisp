;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; definitions of externally visible Petalisp symbols

(in-package :petalisp)

(defgeneric broadcast-to (object space)
  (:method :before ((object data-structure) (space index-space))
    (assert (<= (dimension object) (dimension space))))
  (:method ((object strided-array) (space strided-array-index-space))
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
      (reference object space transformation))))

(defgeneric broadcast (object &rest more-objects)
  (:method ((object strided-array) &rest more-objects)
    (let/de ((objects (cons object more-objects)))
      (let ((dimension (iterate (for object in objects)
                                (maximize (dimension object)))))
        (let ((upgraded-ranges (make-array dimension :initial-element nil)))
          (iterate (for object in objects)
                   (for arg-index from 0)
                   (for ranges = (ranges (index-space object)))
                   (iterate (for range in-vector ranges)
                            (for upgraded-range in-vector upgraded-ranges)
                            (for index from 0)
                            (if (not upgraded-range)
                                (setf (aref upgraded-ranges index) range)
                                (if (unary-range? upgraded-range)
                                    (setf (aref upgraded-ranges index) range)
                                    (assert (equal? range upgraded-range)
                                            (object)
                                            "Illegal broadcasting in dimension ~D of argument ~D."
                                            index arg-index)))))
          (let ((index-space (make-strided-array-index-space upgraded-ranges)))
            (mapcar (λ x (broadcast-to x index-space)) objects)))))))

(defun α (function object &rest more-objects)
  "Apply FUNCTION element-wise to OBJECT and MORE-OBJECTS, like a CL:MAPCAR
for Petalisp data structures. When the dimensions of some of the inputs
mismatch, the smaller objects are broadcast."
  (let/de ((objects (cons object more-objects)))
    (apply #'application
           function
           (apply #'broadcast
                  (mapcar #'petalispify objects)))))

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
  (let ((objects (mapcar #'petalispify objects)))
    (let ((pieces (apply #'subdivision objects)))
      (flet ((reference-origin (piece)
               (reference
                (find piece objects :from-end t :test #'subspace?)
                (index-space piece)
                (make-identity-transformation (dimension piece)))))
        (apply #'fusion (mapcar #'reference-origin pieces))))))

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
  (labels ((recurse (data-structure modifiers)
             (if (null modifiers)
                 data-structure
                 (recurse
                  (modify data-structure (first modifiers))
                  (rest modifiers))))
           (modify (data-structure modifier)
             (etypecase modifier
               (index-space
                (if (or (<= (dimension data-structure) (dimension modifier))
                        (subspace? (index-space data-structure) modifier))
                    (broadcast-to data-structure modifier)
                    (reference
                     data-structure
                     (intersection modifier data-structure)
                     (make-identity-transformation
                      (dimension data-structure)))))
               (transformation
                (reference
                 data-structure
                 (index-space data-structure)
                 (inverse modifier))))))
    (recurse (petalispify data-structure) modifiers)))
