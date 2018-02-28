;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/api
  (:nicknames :petalisp)
  (:use :closer-common-lisp :alexandria :trivia)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/all
   :petalisp/core/virtual-machines/all
   :petalisp/core/visualization)
  (:export
   #:α
   #:β
   #:->
   #:τ
   #:σ
   #:σ*
   #:fuse
   #:fuse*
   #:compute
   #:schedule
   #:size
   #:dimension
   #:*virtual-machine*
   #:reference-virtual-machine
   #:common-lisp-virtual-machine
   #:testing-virtual-machine))

(in-package :petalisp/core/api)

(defparameter *virtual-machine*
  (make-instance 'common-lisp-virtual-machine))

(defun α (function object &rest more-objects)
  "Apply FUNCTION element-wise to OBJECT and MORE-OBJECTS, like a CL:MAPCAR
for Petalisp data structures. When the dimensions of some of the inputs
mismatch, the smaller objects are broadcast."
  (let* ((objects (cons (make-immediate object)
                        (mapcar #'make-immediate more-objects)))
         (space (apply #'common-broadcast-space (mapcar #'index-space objects)))
         (inputs (mapcar (lambda (object) (broadcast object space)) objects)))
    (make-application function (first inputs) inputs)))

(defun β (f &rest args)
  "Reduce the last dimension of OBJECT with F, using G to convert single
values to the appropriate result type."
  (ematch args
    ((list g object)
     (make-reduction f g (make-immediate object) :up))
    ((list object)
     (make-reduction f #'identity (make-immediate object) :up))))

(defun fuse (&rest objects)
  "Combine OBJECTS into a single petalisp data structure. It is an error if
some of the inputs overlap, or if there exists no suitable data structure
to represent the fusion."
  (let ((immediates (mapcar #'make-immediate objects)))
    (make-fusion (first immediates) immediates)))

(defun fuse* (&rest objects)
  "Combine OBJECTS into a single petalisp data structure. When some OBJECTS
overlap partially, the value of the rightmost object is used."
  (declare (optimize (debug 3)))
  (let ((objects (mapcar #'make-immediate objects)))
    (flet ((reference-origin (piece)
             (make-reference
              (find piece objects :from-end t :key #'index-space :test #'subspace-p)
              piece
              (identity-transformation (dimension piece)))))
      (let ((inputs
              (mapcar #'reference-origin
                      (subdivision (mapcar #'index-space objects)))))
        (make-fusion (first inputs) inputs)))))

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
                        (subspace-p (index-space data-structure) modifier))
                    (broadcast data-structure modifier)
                    (make-reference
                     data-structure
                     (index-space-intersection modifier (index-space data-structure))
                     (identity-transformation
                      (dimension data-structure)))))
               (transformation
                (make-reference
                 data-structure
                 (funcall modifier (index-space data-structure))
                 (inverse modifier))))))
    (recurse (make-immediate data-structure) modifiers)))

(defun schedule (&rest objects)
  "Instruct Petalisp to compute all given OBJECTS asynchronously."
  (let* ((recipes (map 'vector #'shallow-copy objects))
         (targets (map 'vector #'make-immediate! objects)))
    (vm/schedule *virtual-machine* targets recipes)
    (values-list objects)))

(defun compute (&rest objects)
  "Return the computed values of all OBJECTS."
  (let* ((recipes (map 'vector #'shallow-copy objects))
         (targets (map 'vector #'make-immediate! objects)))
    (wait (vm/schedule *virtual-machine* targets recipes))
    (flet ((lispify (immediate)
             (let ((array (storage immediate)))
               (if (zerop (array-rank array))
                   (aref array)
                   array))))
      (values-list (map 'list #'lispify targets)))))

