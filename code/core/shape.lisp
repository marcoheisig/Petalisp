;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;; A shape of rank D is the Cartesian product of D ranges.  That means
;;; each element of a shape is a D-tuple of integers, such that the first
;;; integer is an element of the first range, the second integer is an
;;; element of the second range and so on.  If any of the ranges of a shape
;;; is empty, that shape is the empty shape.  The empty shape must not be
;;; confused with the shape that is the product of zero ranges, which has a
;;; size of one.

(deftype rank ()
  `(integer 0 (,array-rank-limit)))

(defstruct (shape
            (:predicate shapep)
            (:copier nil)
            (:constructor nil))
  (ranges nil :type list :read-only t)
  (rank nil :type rank :read-only t)
  (size nil :type unsigned-byte :read-only t))

(defstruct (empty-shape
            (:include shape)
            (:predicate empty-shape-p)
            (:copier nil)
            (:constructor make-empty-shape
                (rank
                 &aux
                   (size 0)
                   (ranges (make-list rank :initial-element (make-empty-range)))))))

(defstruct (non-empty-shape
            (:include shape)
            (:predicate non-empty-shape-p)
            (:copier nil)
            (:constructor make-non-empty-shape (ranges rank size))))

(defun make-shape (ranges)
  (let ((size 1)
        (rank 0))
    (declare (unsigned-byte size)
             (rank rank))
    (loop for range in ranges do
      (setf size (* size (range-size range)))
      (incf rank))
    (if (zerop size)
        (make-empty-shape rank)
        (make-non-empty-shape ranges rank size))))

(defun shape-equal (shape1 shape2)
  (declare (shape shape1 shape2))
  (and (= (shape-rank shape1)
          (shape-rank shape2))
       (= (shape-size shape1)
          (shape-size shape2))
       (or (zerop (shape-size shape1))
           (every #'range-equal
                  (shape-ranges shape1)
                  (shape-ranges shape2)))))

(defun shape-intersection (shape1 shape2)
  (if (/= (shape-rank shape1)
          (shape-rank shape2))
      (make-empty-shape (shape-rank shape1)) ; TODO should we signal an error here?
      (make-shape
       (loop with rank = (shape-rank shape1)
             for range1 in (shape-ranges shape1)
             for range2 in (shape-ranges shape2)
             for intersection = (range-intersection range1 range2)
             when (empty-range-p intersection) do
               (return-from shape-intersection (make-empty-shape rank))
             collect intersection))))

(defun shape-intersectionp (shape1 shape2)
  (declare (shape shape1 shape2))
  (and (= (shape-rank shape1)
          (shape-rank shape2))
       (every #'range-intersectionp
              (shape-ranges shape1)
              (shape-ranges shape2))))

(defun shape-difference-list (shape1 shape2)
  (declare (shape shape1 shape2))
  (let ((intersection (shape-intersection shape1 shape2)))
    (if (empty-shape-p intersection)
        (list shape1)
        (let ((intersection-ranges (shape-ranges intersection))
              (result '()))
          (loop for (range1 . tail) on (shape-ranges shape1)
                for range2 in (shape-ranges shape2)
                for i from 0
                for head = (subseq intersection-ranges 0 i) do
                  (loop for difference in (range-difference-list range1 range2) do
                    (push (make-shape (append head (cons difference tail)))
                          result)))
          result))))

(defun map-shape (function shape)
  (declare (function function)
           (shape shape))
  (labels ((rec (ranges indices function)
             (if (null ranges)
                 (funcall function indices)
                 (map-range
                  (lambda (index)
                    (rec (rest ranges)
                         (cons index indices)
                         function))
                  (first ranges)))))
    (rec (reverse (shape-ranges shape)) '() function))
  shape)

(defun shape-contains (shape index)
  (declare (shape shape)
           (list index))
  (if (empty-shape-p shape)
      nil
      (loop for integer in index
            for range in (shape-ranges shape)
            always (range-contains range integer))))

(defun shrink-shape (shape)
  (declare (shape shape))
  (assert (plusp (shape-rank shape)))
  (let ((ranges (shape-ranges shape)))
    (values (make-shape (rest ranges))
            (first ranges))))

(defun enlarge-shape (shape range)
  (declare (shape shape)
           (range range))
  (make-shape
   (list* range (shape-ranges shape))))

(defun subshapep (shape1 shape2)
  (declare (shape shape1 shape2))
  (and (= (shape-rank shape1)
          (shape-rank shape2))
       (loop for range1 in (shape-ranges shape1)
             for range2 in (shape-ranges shape2)
             always (subrangep range1 range2))))

(defun fuse-shapes (shape &rest more-shapes)
  (declare (shape shape))
  (let ((rank (shape-rank shape)))
    (make-shape
     (apply #'mapcar #'fuse-ranges
            (shape-ranges shape)
            (loop for other-shape in more-shapes
                  do (assert (= rank (shape-rank other-shape)))
                  collect (shape-ranges other-shape))))))

(defun shape-dimensions (shape)
  (declare (shape shape))
  (loop for range in (shape-ranges shape)
        collect (range-size range)
        unless (empty-range-p range)
          do (assert (= 0 (range-start range)))
             (assert (= 1 (range-step range)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subdivide

(defun subdivide (arrays)
  (reduce #'subdivide-aux
          (loop for array in arrays
                for bitmask = 1 then (ash bitmask 1)
                collect (cons (array-shape array) bitmask))
          :initial-value '()))

;; A fragment is a cons whose car is a shape and whose cdr is the
;; corresponding bitmask. This function takes a list of fragments whose
;; shapes are disjoint and a new fragment, and returns a list of disjoint
;; fragments that partition both the old fragments and the new fragment.
(defun subdivide-aux (old-fragments new-fragment)
  (let ((intersections
          (loop for old-fragment in old-fragments
                append (fragment-intersections old-fragment new-fragment))))
    (append
     intersections
     (loop for old-fragment in old-fragments
           append
           (fragment-difference-list old-fragment new-fragment))
     (subtract-fragment-lists (list new-fragment) intersections))))

(defun fragment-intersections (fragment1 fragment2)
  (destructuring-bind (shape1 . mask1) fragment1
    (destructuring-bind (shape2 . mask2) fragment2
      (let ((intersection (shape-intersection shape1 shape2)))
        (if (empty-shape-p intersection)
            '()
            (list (cons intersection (logior mask1 mask2))))))))

(defun fragment-difference-list (fragment1 fragment2)
  (destructuring-bind (shape1 . mask1) fragment1
    (destructuring-bind (shape2 . mask2) fragment2
      (declare (ignore mask2))
      (loop for shape in (shape-difference-list shape1 shape2)
            collect (cons shape mask1)))))

(defun subtract-fragment-lists (fragment-list1 fragment-list2)
  (if (null fragment-list2)
      fragment-list1
      (subtract-fragment-lists
       (loop for fragment in fragment-list1
             append
             (fragment-difference-list fragment (first fragment-list2)))
       (rest fragment-list2))))
