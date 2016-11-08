;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; the building blocks of Petalisp

(define-class operator () (name lisp-function))

(define-class structured-operand () (element-type predecessors))

(define-class index-space (structured-operand) ())

(defmacro define-node (name lambda-list slots)
  `(progn
     (define-class ,name (structured-operand) ,slots)
     (defgeneric ,name ,lambda-list)))

(define-node application (operator object &rest more-objects) (operator))

(define-node reduction (operator object) (operator))

(define-node repetition (object space) ())

(define-node fusion (object &rest more-objects) ())

(define-node reference (object space transformation) (transformation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; classes and methods concerning index spaces

(defgeneric index-space (object))

(defgeneric broadcast (space-1 space-2))

(defgeneric intersection (space-1 space-2))

(defgeneric difference (space-1 space-2))

(defgeneric subdivision (object &rest more-objects)
  (:documentation
   "Return a list of disjoint objects. Each resulting objects is a proper
subspace of one or more of the arguments and their fusion covers all
arguments."))

(defgeneric subspace? (space-1 space-2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; classes and methods concerning transformations

(define-class transformation () (input-dimension output-dimension))

(define-class affine-transformation (transformation) (permutation affine-coefficients))

(define-class identity-transformation (affine-transformation) ())

(defgeneric compose (transformation-1 transformation-2))

(defgeneric invert (transformation))

(defgeneric transform (object transformation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; miscellaneous petalisp functions

(defgeneric dimension (object))

(defgeneric size (object))

(defgeneric equal? (object-1 object-2))

(defgeneric result-type (function &rest arguments))

(defgeneric compute (&rest objects))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; input and output

(defgeneric lisp->petalisp (object))

(defgeneric petalisp->lisp (object &optional storage))

(defgeneric hdf5->petalisp (&rest arguments))

(defgeneric petalisp->hdf5 (&rest arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; argument checking

(defmethod application :before ((operator function)
                                (object structured-operand)
                                &rest more-objects)
  (assert (identical (list* object more-objects)
                     :test #'equal? :key #'index-space)))

(defmethod reduction :before ((operator function)
                              (object structured-operand))
  (assert (plusp (dimension object))))

(defmethod repetition :before (object space)
  (assert
   (or
    (< (dimension object) (dimension space))
    (subspace? (index-space object) space))))

(defmethod fusion :before ((object structured-operand) &rest more-objects)
  (assert (identical (list* object more-objects)
                     :test #'= :key #'dimension)))

(defmethod reference :before (object space transformation)
  (assert (and (subspace? space object)
               (= (dimension space) (input-dimension transformation)))))

(defmethod intersection :before ((space-1 index-space) (space-2 index-space))
  (assert (= (dimension space-1) (dimension space-2))))

(defmethod difference :before ((space-1 index-space) (space-2 index-space))
  (assert (= (dimension space-1) (dimension space-2))))

(defmethod compose :before ((t1 transformation) (t2 transformation))
  (assert (= (input-dimension t1) (output-dimension t2))))

(defmethod transform :before ((object structured-operand)
                              (transformation transformation))
  (assert (= (dimension object) (input-dimension transformation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; default behavior

(defmethod predecessors ((node t)) (declare (ignore node)) nil)

(defmethod equal? ((object-1 t) (object-2 t))
  (equalp object-1 object-2))

(defmethod compose ((g function) (f function))
  (alexandria:compose g f))

(defmethod index-space ((object index-space)) object)

(defmethod subspace? (space-1 space-2)
  (equal? space-1 (intersection space-1 space-2)))

(defmethod intersection ((object-1 structured-operand)
                         (object-2 structured-operand))
  (intersection (index-space object-1) (index-space object-2)))

(defmethod difference ((object-1 structured-operand)
                       (object-2 structured-operand))
  (difference (index-space object-1) (index-space object-2)))

(defmethod invert ((tr identity-transformation)) tr)

(defmethod compose ((g transformation) (f identity-transformation)) g)

(defmethod compose ((g identity-transformation) (f transformation)) f)

(defmethod equal? ((a identity-transformation) (b identity-transformation))
  (= (input-dimension a) (input-dimension b)))

(defmethod transform ((object structured-operand) (_ identity-transformation))
  object)

(define-memo-function identity-transformation (dimension)
  (make-instance
   'identity-transformation
   :permutation (apply #'vector (iota dimension))
   :affine-coefficients
   (make-array
    `(,dimension 2)
    :initial-contents
    (loop for i below dimension collect '(1 0)))
   :input-dimension dimension
   :output-dimension dimension))

(defmethod lisp->petalisp ((object structured-operand)) object)

(defmethod result-type ((f t) &rest arguments)
  (declare (ignore f arguments))
  t)

(defmethod print-object ((object identity-transformation) stream)
  (format stream "(τ (~{i~d~^ ~}) ~:*(~{i~d~^ ~}))"
          (iota (input-dimension object))))

(defmethod subdivision ((object structured-operand) &rest more-objects)
  (flet ((shatter (dust object)
           (let ((object-w/o-dust (list object)))
             (nconc
              (loop for particle in dust do
                (setf object-w/o-dust
                      (loop for object in object-w/o-dust
                            append (difference object particle)))
                    append (difference particle object)
                    when (intersection particle object) collect it)
              object-w/o-dust))))
    (reduce #'shatter more-objects :initial-value (list object))))
