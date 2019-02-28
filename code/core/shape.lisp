;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;; A shape of rank D is the Cartesian product of D ranges.  That means
;;; each element of a shape is a D-tuple of integers, such that the first
;;; integer is an element of the first range, the second integer is an
;;; element of the second range and so on.  Ranges are never empty, so
;;; there can also be no empty shapes.  However, there is one shape - the
;;; product of zero ranges - which has the empty tuple as its sole element.

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

(defun make-shape (ranges)
  (assert (every #'rangep ranges))
  (make-instance 'shape :ranges ranges :rank (length ranges)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(petalisp.utilities:define-class-predicate shape)

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
                    (push (make-shape (append head (cons difference tail)))
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
     (make-explicit-set
      (apply #'append (mapcar #'set-elements shapes))))))

(defmethod set-equal ((shape-1 shape) (shape-2 shape))
  (and (= (rank shape-1)
          (rank shape-2))
       (every #'set-equal (ranges shape-1) (ranges shape-2))))

(defmethod set-intersection ((shape-1 shape) (shape-2 shape))
  (if (/= (rank shape-1) (rank shape-2))
      (empty-set)
      (block nil
        (make-shape
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
  (assert (petalisp.utilities:identical shapes :key #'rank) ()
          "~@<Can only determine the union of index shapes with ~
              equal rank. The index shapes ~
              ~{~#[~;and ~S~;~S ~:;~S, ~]~} violate this requirement.~:@>"
    shapes))

(defmethod shape-union ((shapes cons))
  (make-shape
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
  (make-shape (list* range (ranges shape))))

(defmethod shrink-shape ((shape shape))
  (let ((ranges (ranges shape)))
    (values (make-shape (rest ranges))
            (first ranges))))

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
    (trivia:ematch shapes
      ((list) '())
      ((list _) shapes)
      ((list* _ _ _)
       (reduce #'shatter shapes :initial-value nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenient Notation for Shapes

(defmethod print-object ((shape shape) stream)
  (if *read-eval*
      (format stream "#.(~~~{~^ ~{~D~^ ~}~^ ~~~})"
              (mapcar
               (lambda (range)
                 (multiple-value-bind (start step end)
                     (range-start-step-end range)
                   (cond ((= start end) (list start))
                         ((= step 1) (list start end))
                         ((list start step end)))))
               (ranges shape)))
      (print-unreadable-object (shape stream :type t)
        (format stream "~{~S~^ ~}" (ranges shape)))))

(defconstant ~ '~)

(defun ~ (&rest tilde-separated-range-designators)
  (if (null tilde-separated-range-designators)
      (make-shape '())
      (let ((range-designators
              (split-sequence:split-sequence '~ tilde-separated-range-designators)))
        (make-shape
         (loop for range-designator in range-designators
               collect (apply #'range range-designator))))))

(define-compiler-macro ~ (&whole form &rest tilde-separated-range-designators)
  (if (null tilde-separated-range-designators)
      `(make-shape '())
      (let* ((bindings '())
             (values
               (reverse
                (mapcar
                 (lambda (form)
                   (if (constantp form)
                       form
                       (let ((g (gensym)))
                         (push (list g form) bindings)
                         g)))
                 (reverse tilde-separated-range-designators))))
             (subsequences (split-sequence:split-sequence '~ values)))
        (alexandria:with-gensyms (ranges)
          `(,(if (null bindings) 'load-time-value 'progn)
            (let (,@bindings (,ranges '()))
              ,@(mapcar
                 (trivia:lambda-match
                   ((list start)
                    `(push (range ,start) ,ranges))
                   ((list start end)
                    `(push (range ,start ,end) ,ranges))
                   ((list start step end)
                    `(if (eq ,step '~)
                         (progn
                           (push (range ,end) ,ranges)
                           (push (range ,start) ,ranges))
                         (push (range ,start ,step ,end) ,ranges)))
                   (_ (return-from ~ form)))
                 (reverse subsequences))
              (make-shape ,ranges)))))))

(trivia:defpattern shape (&rest ranges)
  (with-gensyms (it)
    `(trivia:guard1 ,it (shapep ,it)
                    (ranges ,it) (list ,@ranges))))

(trivia:defpattern ~ (&rest tilde-separated-range-designators)
  (if (null tilde-separated-range-designators)
      `(shape)
      (let ((range-designators
              (split-sequence:split-sequence '~ tilde-separated-range-designators)))
        `(shape
          ,@(loop for range-designator in range-designators
                  collect `(range ,@range-designator))))))
