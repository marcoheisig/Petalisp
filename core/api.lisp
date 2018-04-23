;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/api
  (:nicknames :petalisp)
  (:use :closer-common-lisp :alexandria :trivia)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/all
   :petalisp/core/backends/all)
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
   #:*backend*
   #:reference-backend
   #:common-lisp-backend
   #:testing-backend))

(in-package :petalisp/core/api)

(defparameter *backend*
  (make-instance 'common-lisp-backend)
  "This special variable decides on which backend Petalisp programs are
executed.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Index Spaces

(defmethod print-object ((object strided-array-index-space) stream)
  (flet ((range-list (range)
           (list (range-start range)
                 (range-step range)
                 (range-end range))))
    (prin1 `(σ ,@(map 'list #'range-list (ranges object)))
           stream)))

(defmacro expand-range-specifier (range-specifier)
  (match range-specifier
    ((list start step end)
     `(make-range ,start ,step ,end))
    ((list start end)
     `(make-range ,start 1 ,end))
    ((list start)
     (once-only (start)
       `(make-range ,start 1 ,start)))
    (length
     `(make-range 0 1 (1- ,length)))))

(defmacro σ (&rest range-specifiers)
  `(index-space
    (vector
     ,@(loop for range-specifier in range-specifiers
             collect `(expand-range-specifier ,range-specifier)))))

(defmacro σ* (space-form &body range-specifiers)
  (with-gensyms (dim ranges)
    `(let ((,ranges (ranges (index-space (make-immediate ,space-form)))))
       (declare (type (simple-array range (*)) ,ranges)
                (ignorable ,ranges))
       (symbol-macrolet
           ((,(intern "START") (range-start (aref ,ranges ,dim)))
            (,(intern "STEP") (range-step (aref ,ranges ,dim)))
            (,(intern "END") (range-end (aref ,ranges ,dim))))
         (index-space
          (vector
           ,@(loop for range-specifier in range-specifiers
                   for index from 0
                   collect
                   `(let ((,dim ,index))
                      (declare (ignorable ,dim))
                      (expand-range-specifier ,range-specifier)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transformations

(defmethod print-object
    ((transformation transformation) stream)
  (let* ((variables
           (loop for index below (input-dimension transformation)
                 collect (format-symbol :keyword "I~D" index)))
         (inputs
           (if (null (input-constraints transformation))
               variables
               (loop for input-constraint across (input-constraints transformation)
                     for variable in variables
                     collect (or input-constraint variable)))))
    (princ `(τ ,inputs ,(funcall transformation inputs))
           stream)))

(defmacro τ (input-forms output-forms)
  (flet ((constraint (input-form)
           (etypecase input-form
             (integer input-form)
             (symbol nil)))
         (variable (input-form)
           (etypecase input-form
             (integer (gensym))
             (symbol input-form))))
    (let* ((input-constraints
             (map 'vector #'constraint input-forms))
           (variables
             (map 'list #'variable input-forms)))
      `(make-transformation-from-function
        (lambda ,variables
          (declare (ignorable ,@variables))
          (values ,@output-forms))
        ,input-constraints))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Data Combination and Reordering

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
                     (make-identity-transformation
                      (dimension data-structure)))))
               (transformation
                (make-reference
                 data-structure
                 (funcall modifier (index-space data-structure))
                 (invert-transformation modifier))))))
    (recurse (make-immediate data-structure) modifiers)))

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
              (make-identity-transformation (dimension piece)))))
      (let ((inputs
              (mapcar #'reference-origin
                      (subdivision (mapcar #'index-space objects)))))
        (make-fusion (first inputs) inputs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parallel MAP and REDUCE

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Evaluation

(defun compute (&rest objects)
  "Return the computed values of all OBJECTS."
  (let* ((recipes (map 'vector #'shallow-copy objects))
         (targets (map 'vector #'make-immediate! objects)))
    (wait (vm/schedule *backend* targets recipes))
    (flet ((lispify (immediate)
             (let ((array (storage immediate)))
               (if (zerop (array-rank array))
                   (aref array)
                   array))))
      (values-list (map 'list #'lispify targets)))))

(defun schedule (&rest objects)
  "Instruct Petalisp to compute all given OBJECTS asynchronously."
  (let* ((recipes (map 'vector #'shallow-copy objects))
         (targets (map 'vector #'make-immediate! objects)))
    (vm/schedule *backend* targets recipes)
    (values-list objects)))
