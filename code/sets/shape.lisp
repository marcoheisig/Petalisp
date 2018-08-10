;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;; A shape of dimension D is the Cartesian product of D ranges.  That
;;; means each element of a shape is a D-tuple of integers, such that the
;;; first integer is an element of the first range, the second integer is
;;; an element of the second range and so on.  Ranges are never empty, so
;;; there can also be no empty shapes.  However, there is one shape, that
;;; is the product of zero ranges, which has the empty tuple as its sole
;;; element.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric make-shape (shape-designator))

(defgeneric shape-from-ranges (ranges))

(defgeneric dimension (shape))

(defgeneric ranges (shape))

(defgeneric shape-difference-list (shape-1 shape-2))

(defgeneric broadcast-shapes (shapes))

(defgeneric shapep (shape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass shape (finite-set)
  ((%ranges :initarg :ranges :reader ranges :type list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod shapep ((object t))
  nil)

(defmethod shapep ((range range))
  t)

(defmethod dimension ((shape shape))
  (length (ranges shape)))

(defmethod set-elements ((shape shape))
  (if (null (ranges shape))
      (list '())
      (apply #'map-product #'list
             (mapcar #'set-elements (ranges shape)))))

(defmethod set-size ((shape shape))
  (reduce #'* (ranges shape) :key #'set-size))

(defmethod set-contains ((shape shape) (tuple list))
  (loop for integer in tuple
        for range in (ranges shape)
        always (set-contains range integer)))

(defmethod set-equal ((shape-1 shape) (shape-2 shape))
  (and (= (dimension shape-1)
          (dimension shape-2))
       (every #'set-equal (ranges shape-1) (ranges shape-2))))

(defmethod shape-difference-list ((shape-1 shape) (shape-2 shape))
  (let ((intersection (set-intersection shape-1 shape-2)))
    (if (set-emptyp intersection)
        (list shape-1)
        (let ((intersection-ranges (ranges intersection))
              (result '()))
          (loop for (range-1 . tail) on (ranges shape-1)
                for range-2 in (ranges shape-2)
                for i from 0
                for head = (subseq intersection-ranges 0 i) do
                  (loop for difference in (range-difference-list range-1 range-2) do
                    (push (shape-from-ranges
                           (append head (cons difference tail)))
                          result)))
          result))))

(defmethod set-difference ((shape-1 shape) (shape-2 shape))
  (trivia:match (shape-difference-list shape-1 shape-2)
    ((list) (empty-set))
    ((list shape) shape)
    ((list* shapes)
     (set-from-sequence
      (apply #'append (mapcar #'set-elements shapes))))))

(defmethod set-intersection ((shape-1 shape) (shape-2 shape))
  (block nil
    (flet ((range-intersection (a b)
             (let ((a∩b (set-intersection a b)))
               (if (set-emptyp a∩b)
                   (return (empty-set))
                   a∩b))))
      (shape-from-ranges
       (mapcar #'range-intersection (ranges shape-1) (ranges shape-2))))))

(defmethod set-intersectionp ((shape-1 shape) (shape-2 shape))
  (every #'set-intersectionp (ranges shape-1) (ranges shape-2)))

(defmethod print-object ((shape shape) stream)
  (flet ((represent (range)
           (trivia:match (multiple-value-list (range-start-step-end range))
             ((list 0 1 a) (1+ a))
             ((trivia:guard (list a 1 b) (= a b)) (list a))
             ((list a 1 b) (list a b))
             ((list a b c) (list a b c)))))
    (print-unreadable-object (shape stream :type t)
      (format stream "~{~S~^ ~}" (mapcar #'represent (ranges shape))))))

(defmethod make-shape ((shape shape))
  shape)

(defmethod make-shape ((shape-designator list))
  (flet ((parse-range (range-designator)
           (trivia:match range-designator
             ((list start step end) (make-range start step end))
             ((list start end)      (make-range start 1 end))
             ((list start)          (make-range start 1 start))
             (length                (make-range 0 1 (1- length))))))
    (shape-from-ranges
     (mapcar #'parse-range shape-designator))))

(defmethod shape-from-ranges ((ranges list))
  (assert (every #'rangep ranges))
  (make-instance 'shape :ranges ranges))

(defmethod broadcast-shapes ((shapes list))
  ;; Each stack is of the form (LENGTH . RANGES).
  (let* ((stacks (mapcar #'ranges shapes))
         (stack-depths (mapcar #'length stacks))
         (maxdim (apply #'max stack-depths)))
    (flet ((broadcast-ranges (ranges)
             (let ((result (first ranges)))
               (loop for range in (rest ranges) do
                 (cond
                   ((set-equal range result)
                    (values))
                   ((size-one-range-p range)
                    (values))
                   ((size-one-range-p result)
                    (setf result range))
                   (t
                    (demand nil
                      "~@<There is no common broadcast shape for the shapes ~
                                 ~{~#[~;and ~S~;~S ~:;~S, ~]~}.~:@>"
                      shapes))))
               result)))
      (shape-from-ranges
       (loop for dim below maxdim
             collect
             (broadcast-ranges
              (loop for cons on stacks
                    for stack-depth in stack-depths
                    collect (if (> (- maxdim stack-depth) dim)
                                (load-time-value (make-range 0 1 0))
                                (pop (car cons))))))))))
