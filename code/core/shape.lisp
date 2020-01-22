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

(defun make-shape (ranges)
  (let ((rank 0))
    (declare (rank rank))
    (dolist (range ranges (%make-shape ranges rank))
      (check-type range range)
      (incf rank))))

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
                 (let ((it (shape-intersection particle object)))
                   (unless (null it)
                     (push it new-dust))))
               (append object-w/o-dust new-dust))))
    (trivia:ematch shapes
      ((list) '())
      ((list _) shapes)
      ((list* _ _ _)
       (reduce #'shatter shapes :initial-value '())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenient Notation for Shapes

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
  (alexandria:with-gensyms (it)
    `(trivia:guard1 ,it (shapep ,it)
                    (shape-ranges ,it) (list ,@ranges))))

(trivia:defpattern ~ (&rest tilde-separated-range-designators)
  (if (null tilde-separated-range-designators)
      `(shape)
      (let ((range-designators
              (split-sequence:split-sequence '~ tilde-separated-range-designators)))
        `(shape
          ,@(loop for range-designator in range-designators
                  collect `(range ,@range-designator))))))
