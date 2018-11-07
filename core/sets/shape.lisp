;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

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

(defgeneric make-shape (shape-designator))

(defgeneric shape-from-ranges (ranges))

(defgeneric rank (shape))

(defgeneric ranges (shape))

(defgeneric shape-difference-list (shape-1 shape-2))

(defgeneric broadcast-shapes (shapes))

(defgeneric shapep (shape))

(defgeneric shape-union (shapes))

(defgeneric enlarge-shape (shape range))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass shape (finite-set)
  ((%ranges :initarg :ranges :reader ranges :type list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(define-class-predicate shape)

(defmethod make-shape ((shape shape))
  shape)

(defmethod make-shape ((shape-designator list))
  (parse-shape-designator shape-designator))

(defmethod make-shape ((integer integer))
  (assert (plusp integer))
  (shape-from-ranges
   (list
    (make-range 0 1 (1- integer)))))

(defmethod shape-from-ranges ((ranges list))
  (assert (every #'rangep ranges))
  (make-instance 'shape :ranges ranges))

(defmethod rank ((object t))
  0)

(defmethod rank ((array array))
  (array-rank array))

(defmethod rank ((shape shape))
  (length (ranges shape)))

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

(defmethod broadcast-shapes ((shapes list))
  ;; Each stack is of the form (LENGTH . RANGES).
  (let* ((stacks (mapcar #'ranges shapes))
         (stack-depths (mapcar #'length stacks))
         (maxdim (apply #'max stack-depths)))
    (flet ((broadcast-ranges (ranges)
             (let ((result (load-time-value (make-range 0 1 0))))
               (loop for range in ranges do
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
        (shape-from-ranges
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
  (shape-from-ranges
   (apply #'mapcar #'shape-union-range-oracle (mapcar #'ranges shapes))))

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
  (shape-from-ranges (cons range (ranges shape))))

;;; Return a list of disjoint shapes. Each resulting object is a proper
;;; subspace of one or more of the arguments and their fusion covers all
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenient Notation for Shapes

(defun parse-shape-designator (shape-designator)
  (if (integerp shape-designator)
      (shape-from-ranges (list (make-range 0 1 (1- shape-designator))))
      (shape-from-ranges
       (mapcar (lambda (range-designator)
                 (multiple-value-call #'make-range
                   (parse-range-designator range-designator)))
               shape-designator))))

(trivia:defpattern list-of-ranges (&rest range-designators)
  (if (null range-designators)
      `(null)
      (multiple-value-bind (start step end)
          (parse-range-designator (first range-designators))
        `(cons (range ,start ,step ,end)
               (list-of-ranges ,@(rest range-designators))))))

(trivia:defpattern shape (&rest range-designators)
  (with-gensyms (it)
    `(trivia:guard1 ,it (shapep ,it)
                    (ranges ,it) (list-of-ranges ,@range-designators))))

(defmethod print-object ((shape shape) stream)
  (flet ((represent (range)
           (trivia:match (multiple-value-list (range-start-step-end range))
             ((list 0 1 a) (1+ a))
             ((trivia:guard (list a 1 b) (= a b)) (list a))
             ((list a 1 b) (list a b))
             ((list a b c) (list a b c)))))
    (print-unreadable-object (shape stream :type t)
      (format stream "~{~S~^ ~}" (mapcar #'represent (ranges shape))))))
