;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;; A shape of rank D is the Cartesian product of D ranges.  That means
;;; each element of a shape is a D-tuple of integers, such that the first
;;; integer is an element of the first range, the second integer is an
;;; element of the second range and so on.

(deftype rank ()
  `(integer 0 (,array-rank-limit)))

(deftype axis ()
  `(integer 0 (,(1- array-rank-limit))))

(defstruct (shape
            (:predicate shapep)
            (:copier nil)
            (:constructor %make-shape (ranges rank size)))
  (ranges nil :type list :read-only t)
  (rank nil :type rank :read-only t)
  (size nil :type unsigned-byte :read-only t))

(defun make-shape (ranges)
  (let ((size 1)
        (rank 0))
    (declare (unsigned-byte size)
             (rank rank))
    (loop for range in ranges do
      (setf size (* size (range-size range)))
      (incf rank))
    (%make-shape ranges rank size)))

(declaim (inline shape-emptyp))
(defun shape-emptyp (shape)
  (declare (shape shape))
  (zerop (shape-size shape)))

(declaim (inline shape-with-size-one-p))
(defun shape-with-size-one-p (shape)
  (declare (shape shape))
  (= 1 (shape-size shape)))

(declaim (inline shape-range))
(defun shape-range (shape &optional (axis 0))
  (declare (shape shape) (rank axis))
  (unless (<= 0 axis (1- (shape-rank shape)))
    (error "Invalid axis ~D for shape ~S." axis shape))
  (nth axis (shape-ranges shape)))

(defun shape= (shape1 shape2)
  (declare (shape shape1 shape2))
  (and (= (shape-rank shape1)
          (shape-rank shape2))
       (= (shape-size shape1)
          (shape-size shape2))
       (or (zerop (shape-size shape1))
           (every #'range=
                  (shape-ranges shape1)
                  (shape-ranges shape2)))))

(defun shape< (shape1 shape2)
  (declare (shape shape1 shape2))
  (block nil
    (flet ((cmp (p1 p2)
             (cond ((< p1 p2) (return t))
                   ((> p1 p2) (return nil)))))
      (cmp (shape-size shape1)
           (shape-size shape2))
      (cmp (shape-rank shape1)
           (shape-rank shape2))
      (loop for range1 in (shape-ranges shape1)
            for range2 in (shape-ranges shape2)
            do (cmp (range-size range1)
                    (range-size range2)))
      nil)))

(defun shape-intersection (shape1 shape2)
  (declare (shape shape1 shape2))
  (unless (= (shape-rank shape1)
             (shape-rank shape2))
    (error "~@<Can only compute the intersection of shapes with equal rank. ~
               The supplied shapes are ~S, with rank ~D, and ~S, with rank ~D.~:@>"
           shape1 (shape-rank shape1)
           shape2 (shape-rank shape2)))
  (make-shape
   (mapcar
    #'range-intersection
    (shape-ranges shape1)
    (shape-ranges shape2))))

(defun shape-intersectionp (shape1 shape2)
  (declare (shape shape1 shape2))
  (assert (= (shape-rank shape1)
             (shape-rank shape2)))
  (every #'range-intersectionp
           (shape-ranges shape1)
           (shape-ranges shape2)))

(defun shape-difference-list (shape1 shape2)
  (declare (shape shape1 shape2))
  (let ((intersection (shape-intersection shape1 shape2)))
    (if (shape-emptyp intersection)
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
  nil)

(defun shape-contains (shape index)
  (declare (shape shape)
           (list index))
  (assert (= (shape-rank shape) (length index)))
  (if (shape-emptyp shape)
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

(defun inflate-shape (shape n)
  (check-type n unsigned-byte "a non-negative integer")
  (if (zerop n)
      shape
      (%make-shape
       (append (shape-ranges shape)
               (make-list n :initial-element (range 1)))
       (+ (shape-rank shape) n)
       (shape-size shape))))

(defun shape-subseq (shape start &optional end)
  (declare (shape shape))
  (check-type start rank "a valid lazy array rank")
  (let* ((rank (shape-rank shape))
         (end (or end rank)))
    (check-type end rank "a valid lazy array rank")
    (assert (<= end rank) (start end))
    (assert (<= start end) (start end))
    (if (and (zerop start) (= end rank))
        shape
        (make-shape
         (subseq (shape-ranges shape) start end)))))

(defun shape-prefix (shape n)
  (declare (shape shape))
  (shape-subseq shape 0 n))

(defun shape-suffix (shape n)
  (declare (shape shape))
  (shape-subseq shape (- (shape-rank shape) n)))

(defun subshapep (shape1 shape2)
  (declare (shape shape1 shape2))
  (unless (= (shape-rank shape1) (shape-rank shape2))
    (error "~@<The shapes ~S and ~S don't have the same rank.~:@>"
           shape1 shape2))
  (loop for range1 in (shape-ranges shape1)
        for range2 in (shape-ranges shape2)
        always (subrangep range1 range2)))

(defun fuse-shapes (shapes)
  "Returns a shape that covers all the supplied shapes.  Signals an error if the
supplied shapes aren't disjoint."
  (declare (list shapes))
  (let ((predicted-result (superimpose-shapes shapes)))
    (dolist (shape shapes)
      (assert (= (shape-rank predicted-result)
                 (shape-rank shape))))
    (assert (= (reduce #'+ shapes :key #'shape-size)
               (shape-size predicted-result)))
    predicted-result))

(defun superimpose-shapes (shapes)
  "Returns the smallest possible shape that covers all the supplied shapes."
  (declare (list shapes))
  (let ((vector-of-ranges (map 'vector #'shape-ranges shapes))
        (maxrank (reduce #'max shapes :key #'shape-rank :initial-value 0)))
    (make-shape
     (loop repeat maxrank
           collect
           (superimpose-ranges
            (loop for index below (length vector-of-ranges)
                  when (consp (svref vector-of-ranges index))
                    collect (pop (svref vector-of-ranges index))))))))

(defun shape-dimensions (shape)
  (declare (shape shape))
  (loop for range in (shape-ranges shape)
        collect (range-size range)))

(defun shape-dimension (shape axis)
  (declare (shape shape) (axis axis))
  (range-size
   (shape-range shape axis)))

(defun array-shape (array)
  (declare (array array))
  (make-shape
   (loop for axis below (array-rank array)
         collect
         (range (array-dimension array axis)))))

(defgeneric shape-designator-shape (shape-designator)
  (:method ((object t))
    (make-shape '()))
  (:method ((shape shape))
    shape)
  (:method ((array array))
    (array-shape array)))

(defun array-has-shape-p (array shape)
  (and (= (array-rank array)
          (shape-rank shape))
       (loop for range in (shape-ranges shape)
             for axis below (array-rank array)
             for d = (array-dimension array axis)
             always
             (and (= 0 (range-start range))
                  (= 1 (range-step range))
                  (= d (range-end range))))))

(defun split-shape (shape axis &optional position)
  (multiple-value-bind (left-range right-range)
      (split-range (shape-range shape axis) position)
    (let* ((ranges (shape-ranges shape))
           (prefix (subseq ranges 0 axis))
           (suffix (subseq ranges (1+ axis))))
      (values
       (make-shape `(,@prefix ,left-range ,@suffix))
       (make-shape `(,@prefix ,right-range ,@suffix))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subdivide

(defun subdivide-shapes (shapes)
  (reduce #'subdivide-fragments
          (loop for shape in shapes
                for bitmask = 1 then (ash bitmask 1)
                collect (cons shape bitmask))
          :initial-value '()))

;;; A fragment is a cons whose car is a shape and whose cdr is the
;;; corresponding bitmask.

(defun subdivide-fragments (old-fragments new-fragment)
  "Takes a list of fragments whose shapes are disjoint, and one new
fragment, and returns a list of disjoint fragments that partition both the old
fragments and the new fragment. The resulting list consists of three parts:

1. One fragment for each old fragment that has an intersection with the new
   fragment.  Its shape is that intersection, and its mask is the LOGIOR of the
   mask of the old fragment and the mask of the new fragment.

2. All elements of the fragment difference list of any old fragment and the new
   fragment.  Their masks are the same as those of the old fragment they were
   derived from.

3. Fragments that, together, cover every part of the new fragment that is not
   already covered by the intersections from the first step.  Each one has the
   same mask as the new fragment."
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
        (if (shape-emptyp intersection)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Shape Table
;;;
;;; The shape table provides O(1) lookup and deletion of entries whose keys
;;; are shapes.

(defstruct (shape-table
            (:copier nil)
            (:predicate shape-table-p)
            (:constructor make-shape-table ()))
  (ht (make-hash-table :test #'equalp)
   :type hash-table
   :read-only t))

(defun shape-table-value (shape-table shape &optional (default nil))
  (declare (shape-table shape-table)
           (shape shape))
  (gethash shape (shape-table-ht shape-table) default))

(defun (setf shape-table-value) (value shape-table shape &optional (default nil))
  (declare (shape-table shape-table)
           (shape shape))
  (setf (gethash shape (shape-table-ht shape-table) default)
        value))

(defun remove-shape-table-entry (shape-table shape)
  (declare (shape-table shape-table)
           (shape shape))
  (remhash shape (shape-table-ht shape-table)))

(defun clear-shape-table (shape-table)
  (declare (shape-table shape-table))
  (clrhash (shape-table-ht shape-table)))
