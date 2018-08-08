;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;; An index space of dimension D is a set of D-tuples i1,...,iD.
(defclass index-space () ())

(defgeneric dimension (object))

;;; Return a space such that all objects whose index space is SPACE or in
;;; MORE-SPACES can be broadcast to this space. Signal an error if there is no
;;; such space.
(defgeneric common-broadcast-space (space &rest more-spaces))

(defgeneric canonicalize-index-space (index-space-designator)
  (:method ((index-space index-space)) index-space))

(defmethod transform :before ((index-space index-space)
                              (transformation transformation))
  (demand (= (dimension index-space) (input-dimension transformation))
    "~@<Cannot apply the transformation ~A with input dimension ~R ~
        to the index space ~A with dimension ~R.~:@>"
    transformation
    (input-dimension transformation)
    index-space
    (dimension index-space)))

;;; Return a list of index spaces that denote exactly those indices of
;;; SPACE-1 that are not indices of SPACE-2.
(defgeneric index-space-difference (space-1 space-2)
  (:method :before ((space-1 index-space) (space-2 index-space))
    (demand (= (dimension space-1) (dimension space-2))
      "~@<Can only determine the difference of index spaces with ~
          equal dimension. The supplied spaces ~S and ~S have ~
          dimension ~R and ~R, respectively.~:@>"
      space-1 space-2 (dimension space-1) (dimension space-2))))

(defgeneric index-space-intersection (space-1 space-2)
  (:method :before ((space-1 index-space) (space-2 index-space))
    (demand (= (dimension space-1) (dimension space-2))
      "~@<Can only determine the intersection of index spaces with ~
          equal dimension. The supplied spaces ~S and ~S have ~
          dimension ~R and ~R, respectively.~:@>"
      space-1 space-2 (dimension space-1) (dimension space-2))))

(defgeneric index-space-intersection-p (space-1 space-2)
  (:method :before ((space-1 index-space) (space-2 index-space))
    (assert (= (dimension space-1) (dimension space-2))
            (space-1 space-2)
            'intersection-of-index-spaces-with-different-dimensions
            :index-spaces (list space-1 space-2)))
  (:method (space-1 space-2)
    (and (index-space-intersection space-1 space-2) t)))

(defgeneric index-space-union (space-1 &rest more-spaces)
  (:method :before ((space-1 index-space) &rest more-spaces)
    (let ((dimension-1 (dimension space-1)))
      (demand (every (lambda (space) (= dimension-1 (dimension space)))
                     more-spaces)
        "~@<Can only determine the union of index spaces with ~
            equal dimension. The index spaces ~
            ~{~#[~;and ~S~;~S ~:;~S, ~]~} violate this requirement.~:@>"
        (list* space-1 more-spaces)))))

(defgeneric index-space-equality (space-1 space-2))

(defgeneric size (object)
  (:method ((object array)) (array-total-size object))
  (:method ((object hash-table)) (hash-table-count object)))

(defgeneric subspace-p (space-1 space-2)
  (:method ((space-1 t) (space-2 t))
    (let ((intersection (index-space-intersection space-1 space-2)))
      (if (set-emptyp intersection)
          nil
          (index-space-equality space-1 intersection)))))

;;; Given an index space FROM of dimension N and an index space TO of
;;; dimension N+1, return an index space whose first dimensions are taken from
;;; FROM, but with the last dimension of TO.
(defgeneric enlarge-index-space (from to)
  (:method :before ((from index-space) (to index-space))
    (assert (< (dimension from) (dimension to)))))

;;; Return a list of disjoint index-spaces. Each resulting object is a proper
;;; subspace of one or more of the arguments and their fusion covers all
;;; arguments.
(defun subdivision (index-spaces)
  (labels ((subtract (spaces what)
             (loop for space in spaces
                   append (index-space-difference space what)))
           (shatter (dust object) ; dust is a list of disjoint index-spaces
             (let* ((object-w/o-dust (list object))
                    (new-dust '()))
               (loop for particle in dust do
                 (setf object-w/o-dust (subtract object-w/o-dust particle))
                 (loop for space in (index-space-difference particle object) do
                   (push space new-dust))
                 (let ((it (index-space-intersection particle object)))
                   (unless (set-emptyp it)
                     (push it new-dust))))
               (append object-w/o-dust new-dust))))
    (cond ((emptyp index-spaces) '())
          ((= 1 (length index-spaces)) (list (elt index-spaces 0)))
          (t (reduce #'shatter index-spaces :initial-value nil)))))
