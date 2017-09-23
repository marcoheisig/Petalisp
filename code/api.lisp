;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; definitions of externally visible Petalisp symbols

(in-package :petalisp)

(defun α (function object &rest more-objects)
  "Apply FUNCTION element-wise to OBJECT and MORE-OBJECTS, like a CL:MAPCAR
for Petalisp data structures. When the dimensions of some of the inputs
mismatch, the smaller objects are broadcast."
  (let ((objects (cons (petalispify object) (mapcar #'petalispify more-objects))))
    (let ((space (apply #'common-broadcast-space (mapcar #'index-space objects))))
      (apply #'application
             function
             (mapcar (λ _ (broadcast _ space)) objects)))))

(defun β (function object)
  "Reduce the last dimension of OBJECT with FUNCTION."
  (reduction function (petalispify object)))

(defun compute (&rest objects)
  "Return the computed value of OBJECTS."
  (apply #'schedule objects)
  ;; TODO
  )

(defun fuse (&rest objects)
  "Combine OBJECTS into a single petalisp data structure. It is an error if
some of the inputs overlap, or if there exists no suitable data structure
to represent the fusion."
  (apply #'fusion (mapcar #'petalispify objects)))

(defun fuse* (&rest objects)
  "Combine OBJECTS into a single petalisp data structure. When some OBJECTS
overlap partially, the value of the rightmost object is used."
  (let ((objects (mapcar #'petalispify objects)))
    (let ((pieces (apply #'subdivision (mapcar #'index-space objects))))
      (flet ((reference-origin (piece)
               (reference
                (find piece objects :from-end t :key #'index-space :test #'subspace?)
                piece
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
                (if (or (< (dimension data-structure) (dimension modifier))
                        (subspace? (index-space data-structure) modifier))
                    (broadcast data-structure modifier)
                    (reference
                     data-structure
                     (intersection modifier (index-space data-structure))
                     (make-identity-transformation
                      (dimension data-structure)))))
               (transformation
                (reference
                 data-structure
                 (funcall modifier (index-space data-structure))
                 (inverse modifier))))))
    (recurse (petalispify data-structure) modifiers)))

(defun schedule (&rest objects)
  "Instruct Petalisp to compute all given OBJECTS asynchronously."
  (global-evaluator-evaluate-data-structures (map 'vector #'petalispify objects))
  (values))

(test API
  ;; 2D Multigrid
  (flet ((red-black-gauss-seidel (u rhs iterations)
           (let ((r1 (σ* u ((+ start 2) 2 (1- end)) ((+ start 2) 2 (1- end))))
                 (r2 (σ* u ((+ start 1) 2 (1- end)) ((+ start 1) 2 (1- end))))
                 (b1 (σ* u ((+ start 2) 2 (1- end)) ((+ start 1) 2 (1- end))))
                 (b2 (σ* u ((+ start 1) 2 (1- end)) ((+ start 2) 2 (1- end))))
                 (h (/ (1- (sqrt (size u))))))
             (labels ((update (u what)
                        (α #'* 0.25
                           (α #'+
                              (-> u (τ (i j) (1+ i) j) what)
                              (-> u (τ (i j) (1- i) j) what)
                              (-> u (τ (i j) i (1+ j)) what)
                              (-> u (τ (i j) i (1- j)) what)
                              (-> (α #'* (* h h) rhs) what)))))
               (iterate (repeat iterations)
                        (setf u (fuse* u (update u r1) (update u r2)))
                        (setf u (fuse* u (update u b1) (update u b2))))
               u))))
    (red-black-gauss-seidel (-> 0.0 (σ (0 9) (0 9)))
                            (-> 0.0 (σ (0 9) (0 9))) 1)))

