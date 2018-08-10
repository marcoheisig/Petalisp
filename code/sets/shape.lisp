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

(defgeneric dimension (shape))

(defgeneric ranges (shape))

(defgeneric shape-difference-list (shape-1 shape-2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass shape (finite-set)
  ((%ranges :initarg :ranges :reader ranges)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

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
        (apply #'map-product #'make-shape
               (mapcar #'range-difference-list
                       (ranges shape-1)
                       (ranges shape-2))))))

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
      (make-instance 'strided-array-index-space
        :ranges (map 'list #'range-intersection
                     (ranges shape-1)
                     (ranges shape-2))))))

(defmethod set-intersectionp ((shape-1 shape) (shape-2 shape))
  (every #'set-intersectionp (ranges shape-1) (ranges shape-2)))

(defmethod print-object ((shape shape) stream)
  (flet ((range-list (range)
           (trivia:match
               (list (range-start range)
                     (range-step range)
                     (range-end range))
             ((list 0 1 a) a)
             ((list a 1 b) (list a b))
             ((list a b c) (list a b c)))))
    (print-unreadable-object (shape stream :type t)
      (princ (map 'list #'range-list (ranges shape)) stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions

(defmethod make-shape ((shape shape))
  shape)

(defmethod make-shape ((shape-designator list))
  (flet ((parse-range (range-designator)
           (trivia:match range-designator
             ((list start step end) (make-range start step end))
             ((list start end)      (make-range start 1 end))
             ((list start)          (make-range start 1 start))
             (length                (make-range 0 1 (1- length))))))
    (make-instance 'shape
      :ranges (mapcar #'parse-range shape-designator))))
