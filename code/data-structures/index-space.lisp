;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

(defmethod transform :before ((shape shape) (transformation transformation))
  (demand (= (dimension shape) (input-dimension transformation))
    "~@<Cannot apply the transformation ~A with input dimension ~R ~
        to the index shape ~A with dimension ~R.~:@>"
    transformation (input-dimension transformation)
    shape (dimension shape)))

;;; Return a list of index shapes that denote exactly those indices of
;;; SHAPE-1 that are not indices of SHAPE-2.
(defgeneric shape-difference (shape-1 shape-2)
  (:method :before ((shape-1 shape) (shape-2 shape))
    (demand (= (dimension shape-1) (dimension shape-2))
      "~@<Can only determine the difference of index shapes with ~
          equal dimension. The supplied shapes ~S and ~S have ~
          dimension ~R and ~R, respectively.~:@>"
      shape-1 shape-2 (dimension shape-1) (dimension shape-2))))

(defgeneric shape-intersection (shape-1 shape-2)
  (:method :before ((shape-1 shape) (shape-2 shape))
    (demand (= (dimension shape-1) (dimension shape-2))
      "~@<Can only determine the intersection of index shapes with ~
          equal dimension. The supplied shapes ~S and ~S have ~
          dimension ~R and ~R, respectively.~:@>"
      shape-1 shape-2 (dimension shape-1) (dimension shape-2))))

(defgeneric shape-union (shape-1 &rest more-shapes)
  (:method :before ((shape-1 shape) &rest more-shapes)
    (let ((dimension-1 (dimension shape-1)))
      (demand (every (lambda (shape) (= dimension-1 (dimension shape)))
                     more-shapes)
        "~@<Can only determine the union of index shapes with ~
            equal dimension. The index shapes ~
            ~{~#[~;and ~S~;~S ~:;~S, ~]~} violate this requirement.~:@>"
        (list* shape-1 more-shapes)))))

(defgeneric subspace-p (shape-1 shape-2)
  (:method ((shape-1 t) (shape-2 t))
    (let ((intersection (set-intersection shape-1 shape-2)))
      (if (set-emptyp intersection)
          nil
          (set-equal shape-1 intersection)))))

;;; Given an index shape FROM of dimension N and an index shape TO of
;;; dimension N+1, return an index shape whose first dimensions are taken from
;;; FROM, but with the last dimension of TO.
(defgeneric enlarge-shape (from to)
  (:method :before ((from shape) (to shape))
    (assert (< (dimension from) (dimension to)))))

;;; Return a list of disjoint shapes. Each resulting object is a proper
;;; subshape of one or more of the arguments and their fusion covers all
;;; arguments.
(defun subdivision (shapes)
  (labels ((subtract (shapes what)
             (loop for shape in shapes
                   append (shape-difference-list shape what)))
           (shatter (dust object) ; dust is a list of disjoint shapes
             (let* ((object-w/o-dust (list object))
                    (new-dust '()))
               (loop for particle in dust do
                 (setf object-w/o-dust (subtract object-w/o-dust particle))
                 (loop for shape in (shape-difference-list particle object) do
                   (push shape new-dust))
                 (let ((it (set-intersection particle object)))
                   (unless (set-emptyp it)
                     (push it new-dust))))
               (append object-w/o-dust new-dust))))
    (cond ((emptyp shapes) '())
          ((= 1 (length shapes)) (list (elt shapes 0)))
          (t (reduce #'shatter shapes :initial-value nil)))))
