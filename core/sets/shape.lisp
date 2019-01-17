;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package :petalisp-core)

;;; A shape of rank D is the Cartesian product of D ranges.  That means
;;; each element of a shape is a D-tuple of integers, such that the first
;;; integer is an element of the first range, the second integer is an
;;; element of the second range and so on.  Ranges are never empty, so
;;; there can also be no empty shapes.  However, there is one shape, that
;;; is the product of zero ranges, which has the empty tuple as its sole
;;; element.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric rank (shape))

(defgeneric ranges (shape))

(defgeneric shape-difference-list (shape-1 shape-2))

(defgeneric shape-union (shapes))

(defgeneric enlarge-shape (shape range))

(defgeneric shrink-shape (shape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass shape (finite-set)
  ((%rank :initarg :rank :reader rank :type array-rank)
   (%ranges :initarg :ranges :reader ranges :type list)))

(defun make-shape (&rest ranges)
  (assert (every #'rangep ranges))
  (make-instance 'shape :ranges ranges :rank (length ranges)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(define-class-predicate shape)

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
                    (push (apply #'make-shape (append head (cons difference tail)))
                          result)))
          result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Shapes as Sets

(defmethod set-for-each ((function function) (shape shape))
  (labels ((rec (ranges indices function)
             (if (null ranges)
                 (funcall function indices)
                 (set-for-each
                  (lambda (index)
                    (rec (rest ranges)
                         (cons index indices)
                         function))
                  (first ranges)))))
    (rec (reverse (ranges shape)) '() function)))

(defmethod set-size ((shape shape))
  (reduce #'* (ranges shape) :key #'set-size))

(defmethod set-contains ((shape shape) (tuple list))
  (loop for integer in tuple
        for range in (ranges shape)
        always (set-contains range integer)))

(defmethod set-difference ((shape-1 shape) (shape-2 shape))
  (trivia:match (shape-difference-list shape-1 shape-2)
    ((list) (empty-set))
    ((list shape) shape)
    ((list* shapes)
     (set-from-sequence
      (apply #'append (mapcar #'set-elements shapes))))))

(defmethod set-equal ((shape-1 shape) (shape-2 shape))
  (and (= (rank shape-1)
          (rank shape-2))
       (every #'set-equal (ranges shape-1) (ranges shape-2))))

(defmethod set-intersection ((shape-1 shape) (shape-2 shape))
  (if (/= (rank shape-1) (rank shape-2))
      (empty-set)
      (block nil
        (apply #'make-shape
               (mapcar (lambda (range-1 range-2)
                         (let ((intersection (set-intersection range-1 range-2)))
                           (if (set-emptyp intersection)
                               (return (empty-set))
                               intersection)))
                       (ranges shape-1)
                       (ranges shape-2))))))

(defmethod set-intersectionp ((shape-1 shape) (shape-2 shape))
  (every #'set-intersectionp (ranges shape-1) (ranges shape-2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Shape Union

(defmethod shape-union ((null null))
  (empty-set))

(defmethod shape-union :before ((shapes cons))
  (demand (identical shapes :key #'rank)
    "~@<Can only determine the union of index shapes with ~
            equal rank. The index shapes ~
            ~{~#[~;and ~S~;~S ~:;~S, ~]~} violate this requirement.~:@>"
    shapes))

(defmethod shape-union ((shapes cons))
  (apply #'make-shape
         (apply #'mapcar #'shape-union-range-oracle
                (mapcar #'ranges shapes))))

(defmethod shape-union :around ((shapes cons))
  (let ((union (call-next-method)))
    (flet ((proper-subspace-p (shape)
             (set-subsetp shape union)))
      (assert (every #'proper-subspace-p shapes))
      union)))

(defun shape-union-range-oracle (&rest ranges)
  ;; determine the bounding box
  (loop for range in ranges
        minimize (range-start range) into global-start
        maximize (range-end range) into global-end
        finally
           (return
             (if (= global-start global-end)
                 (first ranges)
                 ;; now determine the step size
                 (let ((step-size (- global-end global-start)))
                   (dolist (range ranges)
                     (flet ((check (n)
                              (setf step-size
                                    (min step-size
                                         (- n global-start)))))
                       (if (> (range-start range) global-start)
                           (check (range-start range))
                           (unless (size-one-range-p range)
                             (check (+ (range-start range)
                                       (range-step range)))))))
                   (make-range global-start step-size global-end))))))

(defmethod enlarge-shape ((shape shape) (range range))
  (apply #'make-shape range (ranges shape)))

(defmethod shrink-shape ((shape shape))
  (let ((ranges (ranges shape)))
    (values (apply #'make-shape (rest ranges))
            (first ranges))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenient Notation for Shapes

(trivia:defpattern shape (&rest ranges)
  (with-gensyms (it)
    `(trivia:guard1 ,it (shapep ,it)
                    (ranges ,it) (list ,@ranges))))

(trivia:defpattern make-shape (&rest ranges)
  `(shape ,@ranges))

(defmethod print-object ((shape shape) stream)
  (print-unreadable-object (shape stream :type t)
    (format stream "~{~S~^ ~}" (ranges shape))))
