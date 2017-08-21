;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; the fundamental building blocks of Petalisp

(in-package :petalisp) (in-suite petalisp)

(define-class data-structure () (element-type predecessors))

(define-class constant () ())

(defmacro define-node (name lambda-list slots)
  `(progn
     (define-class ,name (data-structure) ,slots)
     (defgeneric ,name ,lambda-list)))

(define-node application (operator object &rest more-objects) (operator))

(define-node reduction (operator object) (operator))

(define-node repetition (object space) ())

(define-node fusion (object &rest more-objects) ())

(define-node reference (object space transformation) (transformation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  classes and methods concerning index spaces

(define-class index-space (data-structure) ())

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  miscellaneous Petalisp functions

(defgeneric generic-unary-funcall (operator argument))

(defgeneric generic-binary-funcall (operator argument-1 argument-2))

(defgeneric name (object))

(defgeneric dimension (object))

(defgeneric size (object))

(defgeneric equal? (object-1 object-2)
  (:documentation "Two objects are EQUAL? if their use in Petalisp will
  always result in identical behavior."))

(defgeneric result-type (function &rest arguments))

(defgeneric compute (&rest objects))

(defgeneric evaluate (node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  classes and methods concerning transformations

(define-class transformation () ())

(defgeneric classify-transformation (function input-constraints nargout))

(defgeneric compose (transformation-1 transformation-2))

(defgeneric invert (transformation))

(defgeneric input-dimension (transformation))

(defgeneric output-dimension (transformation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  the special case of identity transformations

(define-class identity-transformation (transformation) (dimension))

(defmethod input-dimension ((tr identity-transformation)) (dimension tr))

(defmethod output-dimension ((tr identity-transformation)) (dimension tr))

(defmethod compose ((g transformation) (f identity-transformation)) g)

(defmethod compose ((g identity-transformation) (f transformation)) f)

(defmethod invert ((tr identity-transformation)) tr)

(defmethod equal? ((a identity-transformation) (b identity-transformation))
  (= (dimension a) (dimension b)))

(defmethod generic-unary-funcall ((op identity-transformation) (arg data-structure)) arg)

(defmethod print-object ((object identity-transformation) stream)
  (let ((symbols (list-of-symbols (input-dimension object))))
    (prin1 `(τ ,symbols ,@symbols))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  input and output

(defgeneric lisp->petalisp (object))

(defgeneric petalisp->lisp (object &optional storage))

(defgeneric hdf5->petalisp (&rest arguments))

(defgeneric petalisp->hdf5 (&rest arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  argument checking

(defmethod application :before ((operator function)
                                (object data-structure)
                                &rest more-objects)
  (assert (identical (cons object more-objects)
                     :test #'equal? :key #'index-space)))

(defmethod reduction :before ((operator function)
                              (object data-structure))
  (assert (plusp (dimension object))))

(defmethod repetition :before (object space)
  (assert
   (<= (dimension object) (dimension space))))

(defmethod fusion :before ((object data-structure) &rest more-objects)
  (assert (identical (cons object more-objects)
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

(defmethod transform :before ((object data-structure)
                              (transformation transformation))
  (assert (= (dimension object) (input-dimension transformation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  default behavior

(defmethod predecessors ((node t)) (declare (ignore node)) nil)

(defun predecessor (node) (first (predecessors node)))

(defmethod equal? ((object-1 t) (object-2 t))
  (equalp object-1 object-2))

(defmethod compose ((g function) (f function))
  (alexandria:compose g f))

(defmethod index-space ((object index-space)) object)

(defmethod subspace? (space-1 space-2)
  (equal? space-1 (intersection space-1 space-2)))

(defmethod intersection ((object-1 data-structure)
                         (object-2 data-structure))
  (intersection (index-space object-1) (index-space object-2)))

(defmethod difference ((object-1 data-structure)
                       (object-2 data-structure))
  (difference (index-space object-1) (index-space object-2)))

(defmethod lisp->petalisp ((object data-structure)) object)

(defmethod subdivision ((object data-structure) &rest more-objects)
  (flet ((shatter (dust object)
           (let ((object-w/o-dust (list object)))
             (nconc
              (loop for particle in dust do
                (setf object-w/o-dust
                      (loop for x in object-w/o-dust
                            append (difference x particle)))
                    append (difference particle object)
                    when (intersection particle object) collect it)
              object-w/o-dust))))
    (reduce #'shatter more-objects :initial-value (list object))))

(defmethod evaluate ((node constant)) node)

(defmethod evaluate (node) node)

(defparameter *constant-fold-threshold* 99)
