;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(deftype rank ()
  `(integer 0 (,array-rank-limit)))

;;; A shape of rank D is the Cartesian product of D ranges.  That means
;;; each element of a shape is a D-tuple of integers, such that the first
;;; integer is an element of the first range, the second integer is an
;;; element of the second range and so on.  Ranges are never empty, so
;;; there can also be no empty shapes.  However, there is one shape - the
;;; product of zero ranges - which has the empty tuple as its sole element.

(defstruct (shape
            (:predicate shapep)
            (:constructor %make-shape (ranges rank)))
  (rank nil :type array-rank :read-only t)
  (ranges nil :type list :read-only t))

(defun shape-size (shape)
  (declare (shape shape))
  (reduce #'* (shape-ranges shape) :key #'range-size))

(defun shape-equal (shape-1 shape-2)
  (declare (shape shape-1 shape-2))
  (and (= (shape-rank shape-1)
          (shape-rank shape-2))
       (every #'range-equal
              (shape-ranges shape-1)
              (shape-ranges shape-2))))

(defun shape-difference-list (shape-1 shape-2)
  (declare (shape shape-1 shape-2))
  (let ((intersection (shape-intersection shape-1 shape-2)))
    (if (null intersection)
        (list shape-1)
        (let ((intersection-ranges (shape-ranges intersection))
              (result '()))
          (loop for (range-1 . tail) on (shape-ranges shape-1)
                for range-2 in (shape-ranges shape-2)
                for i from 0
                for head = (subseq intersection-ranges 0 i) do
                  (loop for difference in (range-difference-list range-1 range-2) do
                    (push (%make-shape
                           (append head (cons difference tail))
                           (shape-rank shape-1))
                          result)))
          result))))

(defun shape-intersection (shape-1 shape-2)
  (if (/= (shape-rank shape-1)
          (shape-rank shape-2))
      nil
      (%make-shape
       (loop for range-1 in (shape-ranges shape-1)
             for range-2 in (shape-ranges shape-2)
             for intersection = (range-intersection range-1 range-2)
             when (null intersection) do
               (return-from shape-intersection nil)
               collect intersection)
       (shape-rank shape-1))))

(defun shape-intersectionp (shape-1 shape-2)
  (declare (shape shape-1 shape-2))
  (and (= (shape-rank shape-1)
          (shape-rank shape-2))
       (every #'range-intersectionp
              (shape-ranges shape-1)
              (shape-ranges shape-2))))

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
    (rec (reverse (shape-ranges shape)) '() function)))

(defun shape-contains (shape index)
  (declare (shape shape)
           (list index))
  (loop for integer in index
        for range in (shape-ranges shape)
        always (range-contains range integer)))

(defun shrink-shape (shape)
  (declare (shape shape))
  (assert (plusp (shape-rank shape)))
  (let ((ranges (shape-ranges shape)))
    (values (%make-shape
             (rest ranges)
             (1- (shape-rank shape)))
            (first ranges))))

(defun enlarge-shape (shape range)
  (declare (shape shape)
           (range range))
  (%make-shape
   (list* range (shape-ranges shape))
   (1+ (shape-rank shape))))

(defun subshapep (shape-1 shape-2)
  (declare (shape shape-1 shape-2))
  (and (= (shape-rank shape-1)
          (shape-rank shape-2))
       (loop for range-1 in (shape-ranges shape-1)
             for range-2 in (shape-ranges shape-2)
             always (subrangep range-1 range-2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The ~ Notation for Shapes

(defmethod print-object ((shape shape) stream)
  (flet ((listify-shape (shape)
           (mapcar
            (lambda (range)
              (multiple-value-bind (start step end)
                  (range-start-step-end range)
                (cond ((= start end) (list start))
                      ((= step 1) (list start end))
                      ((list start step end)))))
            (shape-ranges shape))))
    (cond ((and *print-readably* *read-eval*)
           (format stream "#.(~~~{~^ ~{~D~^ ~}~^ ~~~})"
                   (listify-shape shape)))
          ((not *print-readably*)
           (format stream "(~~~{~^ ~{~D~^ ~}~^ ~~~})"
                   (listify-shape shape)))
          (t (print-unreadable-object (shape stream :type t)
               (format stream "~{~S~^ ~}" (shape-ranges shape)))))))

(macrolet ((define-shape-builder-function (name)
             `(progn
                (defconstant ,name ',name)
                (declaim (inline ,name))
                (defun ,name (&rest range-designators &aux (whole (cons ,name range-designators)))
                  (declare (dynamic-extent range-designators whole))
                  (build-shape whole)))))
  (define-shape-builder-function ~)
  (define-shape-builder-function ~l)
  (define-shape-builder-function ~r)
  (define-shape-builder-function ~s))

(defun range-designator-separator-p (x)
  (member x '(~ ~l ~r ~s)))

(defun build-shape (range-designators)
  (petalisp.utilities:with-collectors ((ranges collect))
    (labels ((process (range-designators rank)
               (trivia:match range-designators
                 ((or (list) (list ~))
                  (%make-shape (ranges) rank))
                 ((list* ~ (and start (type integer)) (and step (type integer)) (and end (type integer)) rest)
                  (collect (range start step end))
                  (process rest (1+ rank)))
                 ((list* ~ (and start (type integer)) (and end (type integer)) rest)
                  (collect (range start end))
                  (process rest (1+ rank)))
                 ((list* ~ (and start (type integer)) rest)
                  (collect (range start))
                  (process rest (1+ rank)))
                 ((list* ~l (and ranges (type list)) rest)
                  (let ((counter 0))
                    (dolist (range ranges (process rest (+ rank counter)))
                      (check-type range range)
                      (collect range)
                      (incf counter))))
                 ((list* ~r (and range (type range)) rest)
                  (collect range)
                  (process rest (1+ rank)))
                 ((list* ~s (and shape (type shape)) rest)
                  (mapc #'collect (shape-ranges shape))
                  (process rest (+ rank (shape-rank shape))))
                 (_
                  (error "Invalid range designator~P: ~A"
                         (count-if #'range-designator-separator-p (butlast range-designators))
                         range-designators)))))
      (process range-designators 0))))

(trivia:defpattern shape (&rest ranges)
  (alexandria:with-gensyms (it)
    `(trivia:guard1 ,it (shapep ,it)
                    (shape-ranges ,it) (list ,@ranges))))

(trivia:defpattern ~l (&rest range-designators)
  (build-shape-pattern (cons ~l range-designators)))

(trivia:defpattern ~ (&rest range-designators)
  (build-shape-pattern (cons ~ range-designators)))

(trivia:defpattern non-~ ()
  `(not (satisfies range-designator-separator-p)))

(defun build-shape-pattern (range-designators)
  (petalisp.utilities:with-collectors ((range-patterns collect))
    (labels ((process (range-designators)
               (trivia:match range-designators
                 ((or (list) (list ~))
                  `(list ,@(range-patterns)))
                 ((list* ~ (and start (non-~)) (and step (non-~)) (and end (non-~)) rest)
                  (collect `(range ,start ,step ,end))
                  (process rest))
                 ((list* ~ (and start (non-~)) (and end (non-~)) rest)
                  (collect `(range ,start ,end))
                  (process rest))
                 ((list* ~ (and start (non-~)) rest)
                  (collect `(range ,start))
                  (process rest))
                 ((list* ~l ranges rest)
                  (unless (null rest)
                    (error "~S must only appear at the last clause of a shape pattern."
                           ~l))
                  `(list* ,@(range-patterns) ,ranges))
                 ((list* ~s _ _)
                  (error "~S must not appear in a shape pattern."
                         ~s))
                 ((list* ~r range rest)
                  (collect range)
                  (process rest))
                 (_
                  (error "Invalid range designator~P: ~A"
                         (count-if #'range-designator-separator-p (butlast range-designators))
                         range-designators)))))
      (alexandria:with-gensyms (it)
        `(trivia:guard1
          ,it (shapep ,it)
          (shape-ranges ,it) ,(process range-designators))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subdivide

(defun subdivide (arrays)
  (reduce #'subdivide-aux
          (loop for array in arrays
                for bitmask = 1 then (ash bitmask 1)
                collect (cons (shape array) bitmask))
          :initial-value '()))

;; A fragment is a cons whose car is a shape and whose cdr is the
;; corresponding bitmask. This function takes a list of disjoint fragments
;; (fragments whose shapes are disjoint) and a new fragment and returns a
;; list of disjoint fragments that partition both the old fragments and the
;; new fragment.
(defun subdivide-aux (old-fragments new-fragment)
  (let ((intersections
          (loop for old-fragment in old-fragments
                append (fragment-intersections old-fragment new-fragment))))
    (append
     intersections
     (loop for old-fragment in old-fragments
           append
           (fragment-difference-list old-fragment new-fragment))
     (labels ((subtract (list-1 list-2)
                (if (null list-2)
                    list-1
                    (subtract
                     (loop for fragment in list-1
                           append
                           (fragment-difference-list fragment (first list-2)))
                     (rest list-2)))))
       (subtract (list new-fragment) intersections)))))

(defun fragment-intersections (fragment-1 fragment-2)
  (destructuring-bind (shape-1 . mask-1) fragment-1
    (destructuring-bind (shape-2 . mask-2) fragment-2
      (let ((intersection (shape-intersection shape-1 shape-2)))
        (if (null intersection)
            '()
            (list (cons intersection (logior mask-1 mask-2))))))))

(defun fragment-difference-list (fragment-1 fragment-2)
  (destructuring-bind (shape-1 . mask-1) fragment-1
    (destructuring-bind (shape-2 . mask-2) fragment-2
      (declare (ignore mask-2))
      (loop for shape in (shape-difference-list shape-1 shape-2)
            collect (cons shape mask-1)))))
