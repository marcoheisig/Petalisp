;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/data-structures/index-space
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/all)
  (:export
   #:index-space
   #:common-broadcast-space
   #:index-space-difference
   #:index-space-intersection
   #:index-space-intersection?
   #:index-space-union
   #:index-space-equality
   #:dimension
   #:enlarge-index-space
   #:size
   #:subspace?
   #:subdivision))

(in-package :petalisp/core/data-structures/index-space)

(define-class index-space () ()
  (:documentation
   "An index space of dimension D is a set of D-tuples i1,...,iD."))

(defgeneric index-space (object)
  (:documentation
   "Return the index space of OBJECT.")
  (:method ((index-space index-space)) index-space))

(defgeneric common-broadcast-space (space &rest more-spaces)
  (:documentation
   "Return a space such that all objects whose index space is SPACE or in
MORE-SPACES can be broadcast to this space. Signal an error if there is no
such space."))

(defmethod generic-unary-funcall :before ((transformation transformation)
                                          (object index-space))
  (assert (= (input-dimension transformation) (dimension object))))

(defgeneric index-space-difference (space-1 space-2)
  (:documentation
   "Return a list of index spaces that denote exactly those indices of
SPACE-1 that are not indices of SPACE-2.")
  (:method :before ((space-1 index-space) (space-2 index-space))
    (assert (= (dimension space-1) (dimension space-2)))))

(defgeneric index-space-intersection (space-1 space-2)
  (:documentation
   "Return an index space containing all indices that occur both in SPACE-1
and SPACE-2.")
  (:method :before ((space-1 index-space) (space-2 index-space))
    (assert (= (dimension space-1) (dimension space-2)))))

(defgeneric index-space-intersection? (space-1 space-2)
  (:documentation
   "Return whether some indices occur both in SPACE-1 and SPACE-2.")
  (:method :before ((space-1 index-space) (space-2 index-space))
    (assert (= (dimension space-1) (dimension space-2))))
  (:method (space-1 space-2)
    (and (index-space-intersection space-1 space-2) t)))

(defgeneric index-space-union (set &rest more-sets)
  (:documentation
   "Return the set of all elements of all supplied sets."))

(defgeneric index-space-equality (space-1 space-2)
  (:documentation
   "Return whether the two spaces denote the same set of indices."))

(defgeneric size (object)
  (:documentation
   "The size of a compound object, such as an array or hash-table, is the
number of its elements.")
  (:method ((object array)) (array-total-size object))
  (:method ((object hash-table)) (hash-table-count object)))

(defgeneric subspace? (space-1 space-2)
  (:documentation
   "Return true if every index in SPACE-1 also occurs in SPACE-2.")
  (:method ((space-1 t) (space-2 t))
    (if-let ((intersection (index-space-intersection space-1 space-2)))
      (index-space-equality space-1 intersection))))

(defgeneric enlarge-index-space (from to)
  (:documentation
   "Given an index space FROM of dimension N and an index space TO of
dimension N+1, return an index space whose first dimensions are taken from
FROM, but with the last dimension of TO.")
  (:method :before ((from index-space) (to index-space))
    (assert (< (dimension from) (dimension to)))))

(defun subdivision (objects)
  "Return a list of disjoint objects. Each resulting object is a proper
subspace of one or more of the arguments and their fusion covers all
arguments."
  (flet ((shatter (dust object) ; dust is a list of disjoint objects
           (let* ((object-w/o-dust (list object))
                  (new-dust
                    (loop for particle in dust do
                      (setf object-w/o-dust
                            (loop for x in object-w/o-dust
                                  append (index-space-difference x particle)))
                          append (index-space-difference particle object)
                          when (index-space-intersection particle object) collect it)))
             (append object-w/o-dust new-dust))))
    (cond ((emptyp objects) nil)
          ((= 1 (length objects)) (list (elt objects 0)))
          (t (reduce #'shatter objects :initial-value nil)))))
