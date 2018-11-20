;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric range-start-step-end (range))

(defgeneric range-start (range))

(defgeneric range-step (range))

(defgeneric range-end (range))

(defgeneric range-difference-list (range-1 range-2))

(defgeneric make-range (start step end))

(defgeneric split-range (range))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass range (finite-set)
  ())

(defclass non-contiguous-range (range)
  ((%start :initarg :start :reader range-start)
   (%step :initarg :step :reader range-step)
   (%end :initarg :end :reader range-end)))

(defclass contiguous-range (range)
  ())

(defclass size-one-range (contiguous-range)
  ((%element :initarg :start :reader range-start
             :initarg :end :reader range-end)))

(defclass non-size-one-contiguous-range (contiguous-range)
  ((%start :initarg :start :reader range-start)
   (%end :initarg :end :reader range-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(define-class-predicate range)

(define-class-predicate size-one-range :hyphenate t)

(defmethod range-step ((range contiguous-range))
  1)

(defmethod range-start-step-end ((range range))
  (values (range-start range)
          (range-step range)
          (range-end range)))

(defmethod range-start-step-end ((range contiguous-range))
  (values (range-start range)
          1
          (range-end range)))

(defmethod range-start-step-end ((range size-one-range))
  (let ((element (range-start range)))
    (values element 1 element)))

(defmethod set-for-each ((function function) (range range))
  (multiple-value-bind (start step end)
      (range-start-step-end range)
    (loop for element from start by step to end do
      (funcall function element))))

(defmethod set-size ((range range))
  (1+ (/ (- (range-end range) (range-start range))
         (range-step range))))

(defmethod set-size ((range contiguous-range))
  (1+ (- (range-end range) (range-start range))))

(defmethod set-size ((range size-one-range))
  1)

(defmethod set-contains ((range range) (integer integer))
  (and (<= (range-start range) integer (range-end range))
       (zerop (rem (- integer (range-start range))
                   (range-step range)))))

(defmethod set-contains ((range contiguous-range) (integer integer))
  (<= (range-start range) integer (range-end range)))

(defmethod set-contains ((range size-one-range) (integer integer))
  (= integer (range-start range)))

(defmethod set-equal ((range-1 range) (range-2 range))
  (and (= (range-start range-1)
          (range-start range-2))
       (= (range-step range-1)
          (range-step range-2))
       (= (range-end range-1)
          (range-end range-2))))

(defmethod set-equal ((range-1 size-one-range) (range-2 size-one-range))
  (= (range-start range-1) (range-start range-2)))

(define-method-pair set-equal ((range-1 size-one-range) (range-2 range))
  (declare (ignore range-1 range-2))
  nil)

(define-method-pair set-equal ((range range) (explicit-set explicit-set))
  (let ((table (set-element-table explicit-set)))
    (and (= (hash-table-count table) (set-size range))
         (loop for integer from (range-start range) by (range-step range) to (range-end range)
               always (gethash integer table)))))

(defmethod split-range ((range range))
  (multiple-value-bind (start step end)
      (range-start-step-end range)
    (let ((middle (floor (+ start end) 2)))
      (values (make-range start step middle)
              (make-range end step (+ middle step))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Difference of Ranges

(defmethod range-difference-list ((range-1 range) (range-2 range))
  ;; We only care about the part of range-2 that intersects with range-1.
  (let ((range-2 (set-intersection range-1 range-2)))
    (if (set-emptyp range-2)
        (list range-1)
        (multiple-value-bind (start-1 step-1 end-1) (range-start-step-end range-1)
          (multiple-value-bind (start-2 step-2 end-2) (range-start-step-end range-2)
            (declare (integer start-1 end-1 start-2 end-2)
                     (positive-integer step-1 step-2))
            ;; The new range-2 is now a proper sub-range of range-1, i.e. we
            ;; have (<= start-1 start-2 end-2 end-1).  Furthermore, step-2 is
            ;; now either a multiple of step-1, or one, if range-2 has only a
            ;; single element.
            (cond
              (;; If range-2 does not intersect range-1, we are done.
               (set-emptyp range-2) (list range-1))
              ;; Now we pick off the five special cases where range-2 has only
              ;; a single element.
              ((= start-2 end-2)
               (range-difference-list--single start-1 step-1 end-1 start-2))
              ;; At this point, we know that step-2 is a multiple of step-1.
              ;; Using a coordinate transformation, we simplify this case such
              ;; that range-1 is contiguous.
              (t
               (range-difference-list--contiguous
                0
                (/ (- end-1 start-1) step-1)
                (/ (- start-2 start-1) step-1)
                (/ step-2 step-1)
                (/ (- end-2 start-1) step-1)
                (lambda (start step end)
                  (make-range
                   (+ (* start step-1) start-1)
                   (* step step-1)
                   (+ (* end step-1) start-1)))))))))))

(defmethod range-difference-list ((range-1 size-one-range) (range-2 size-one-range))
  (if (= (range-start range-1) (range-start range-2))
      (list)
      (list range-1)))

(defmethod range-difference-list ((range-1 range) (range-2 size-one-range))
  (multiple-value-call #'range-difference-list--single
    (range-start-step-end range-1)
    (range-start range-2)))

(defmethod range-difference-list ((range-1 contiguous-range) (range-2 range))
  (multiple-value-call #'range-difference-list--contiguous
    (values (range-start range-1))
    (values (range-end range-1))
    (range-start-step-end range-2)
    #'make-range))

(defun range-difference-list--contiguous
    (start-1 end-1 start-2 step-2 end-2 make-range-fn)
  (declare (integer start-1 end-1 start-2 end-2)
           (positive-integer step-2)
           (function make-range-fn))
  ;; There are two strategies to partition the contiguous indices
  ;; start-1..end-1 into ranges.  The first one is to create strided ranges
  ;; and possibly a contiguous range for the first and last elements, the
  ;; other strategy is to create only contiguous ranges.
  (let* ((strategy-1-lb (- start-2 step-2))
         (strategy-1-ub (+ end-2 step-2))
         (strategy-1-lb-p (>= strategy-1-lb start-1))
         (strategy-1-ub-p (<= strategy-1-ub end-1))
         (strategy-1 (+ (1- step-2)
                        (if strategy-1-lb-p 1 0)
                        (if strategy-1-ub-p 1 0)))
         (strategy-2-lb-p (/= start-2 start-1))
         (strategy-2-ub-p (/= end-2 end-1))
         (strategy-2 (+ (/ (- end-2 start-2) step-2)
                        (if strategy-2-lb-p 1 0)
                        (if strategy-2-ub-p 1 0))))
    ;; We pick the strategy that produces fewer ranges.
    (let ((ranges '()))
      (flet ((push-range (start step end)
               (push (funcall make-range-fn start step end) ranges)))
        (if (< strategy-1 strategy-2)
            ;; Strategy 1
            (loop for offset from 1 below step-2
                  for start = (let ((high (+ start-2 offset))
                                    (low (+ start-2 (- step-2) offset)))
                                (if (>= low start-1) low high))
                  for end = (let ((high (+ end-2 offset))
                                  (low (+ end-2 (- step-2) offset)))
                              (if (<= high end-1) high low))
                  do (push-range start step-2 end)
                  finally
                     (when strategy-1-lb-p
                       (push-range start-1 1 strategy-1-lb))
                     (when strategy-1-ub-p
                       (push-range strategy-1-ub 1 end-1)))
            ;; Strategy 2
            (let ((block-size (1- step-2)))
              (loop for start from (1+ start-2) by step-2 below end-2
                    for end = (+ start block-size)
                    do (push-range start 1 end)
                    finally
                       (when strategy-2-lb-p
                         (push-range start-1 1 (1- start-2)))
                       (when strategy-2-ub-p
                         (push-range (1+ end-2) 1 end-1)))))
        ranges))))

;;; Remove a single (valid) index from the given range.
(defun range-difference-list--single (start-1 step-1 end-1 index)
  (declare (integer start-1 end-1 index)
           (positive-integer step-1))
  (cond ((= start-1 end-1)
         '())
        ((= index start-1)
         (list
          (make-range (+ start-1 step-1) step-1 end-1)))
        ((= index end-1)
         (list
          (make-range start-1 step-1 (- end-1 step-1))))
        ((= (+ start-1 step-1) index (- end-1 step-1))
         (list
          (make-range start-1 (* 2 step-1) end-1)))
        (t
         (list
          (make-range start-1 step-1 (- index step-1))
          (make-range (+ index step-1) step-1 end-1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Intersection of Ranges

(defmethod set-intersection ((range-1 range) (range-2 range))
  (multiple-value-bind (start step end)
      (range-intersection-start-step-end range-1 range-2)
    (if (null start)
        (empty-set)
        (make-range start step end))))

(defmethod set-intersection ((range-1 contiguous-range) (range-2 contiguous-range))
  (let ((start (max (range-start range-1)
                    (range-start range-2)))
        (end (min (range-end range-1)
                  (range-end range-2))))
    (if (<= start end)
        (make-range start 1 end)
        (empty-set))))

(defun range-intersection-start-step-end (range-1 range-2)
  (let ((lb (max (range-start range-1) (range-start range-2)))
        (ub (min (range-end   range-1) (range-end   range-2))))
    (let ((a (range-step range-1))
          (b (range-step range-2))
          (c (- (range-start range-2) (range-start range-1))))
      (multiple-value-bind (s gcd) (extended-euclid a b)
        (when (integerp (/ c gcd))
          (let ((x (+ (* s (/ c gcd) a)
                      (range-start range-1)))
                (lcm (/ (* a b) gcd)))
            (let ((start (+ x (* lcm (ceiling (- lb x) lcm))))
                  (end   (+ x (* lcm (floor   (- ub x) lcm)))))
              (when (<= lb start end ub)
                (values start lcm end)))))))))

(defmethod set-intersectionp ((range-1 range) (range-2 range))
  (and (range-intersection-start-step-end range-1 range-2) t))

(defmethod make-range ((start integer) (step integer) (end integer))
  (if (zerop step)
      (if (= start end)
          (make-instance 'size-one-range :start start)
          (error "Bad step size 0 for range with start ~d and end ~d" start end))
      (let ((steps (truncate (- end start) step)))
        (if (= steps 0)
            (make-instance 'size-one-range :start start)
            (let ((congruent-end (+ start (* step steps))))
              (let ((step (abs step))
                    (start (min start congruent-end))
                    (end (max start congruent-end)))
                (if (= 1 step)
                    (make-instance 'non-size-one-contiguous-range :start start :end end)
                    (make-instance 'non-contiguous-range :start start :step step :end end))))))))

(defun range-fusion (ranges)
  ;; Assuming that all supplied RANGES are non-overlapping, the only
  ;; possible fusion is obtained by summing the number of elements,
  ;; determining the smallest and largest element of all sequences and
  ;; choosing a step size to yield the correct number of elements.
  (loop for range in ranges
        summing (set-size range) into number-of-elements
        minimizing (range-start range) into start
        maximizing (range-end range) into end
        finally
           (flet ((fail ()
                    (simple-program-error
                     "Unable to fuse ranges:~%~{~A~%~}" ranges)))
             (let ((step (if (= number-of-elements 1) 1
                             (/ (- end start) (1- number-of-elements)))))
               (unless (integerp step) (fail))
               (let ((result (make-range start step end)))
                 (when (notevery (lambda (range) (set-intersectionp range result)) ranges)
                   (fail))
                 (return result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenient Notation for Ranges

(defun range (start &optional (step-or-end 1 two-args-p) (end start three-args-p))
  (cond (three-args-p (make-range start step-or-end end))
        (two-args-p (make-range start 1 step-or-end))
        (t (make-range start step-or-end end))))

(trivia:defpattern range (&rest start-step-end)
  (with-gensyms (it tmp)
    (multiple-value-bind (start step end)
        (trivia:ematch start-step-end
          ((list start step end) (values start step end))
          ((list start end) (values start 1 end))
          ((list start) (values `(and ,start ,tmp) 1 `(= ,tmp))))
      `(trivia:guard1 ,it (rangep ,it)
                      (range-start ,it) ,start
                      (range-step ,it) ,step
                      (range-end ,it) ,end))))

(defmethod print-object ((range range) stream)
  (print-unreadable-object (range stream)
    (format stream "RANGE ~D ~D ~D"
            (range-start range)
            (range-step range)
            (range-end range))))
