;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric range-start (range))

(defgeneric range-step (range))

(defgeneric range-end (range))

(defgeneric make-range (start step end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass range (finite-set)
  ())

(defclass contiguous-range (range)
  ((%start :initarg :start :reader range-start)
   (%end :initarg :end :reader range-end)))

(defclass strided-range (range)
  ((%start :initarg :start :reader range-start)
   (%step :initarg :step :reader range-step)
   (%end :initarg :end :reader range-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod set-elements ((range range))
  (loop for x from (range-start range) by (range-step range) to (range-end range)
        collect x))

(defmethod set-size ((range range))
  (1+ (/ (- (range-end range)
            (range-start range))
         (range-step range))))

(defmethod set-equal ((range-1 range) (range-2 range))
  (and (= (range-start range-1)
          (range-start range-2))
       (= (range-step range-1)
          (range-step range-2))
       (= (range-end range-1)
          (range-end range-2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Difference of Ranges

(defmethod set-difference ((range-1 range) (range-2 range))
  ;; we only care about the part of range-2 that intersects with range-1
  (let ((range-2 (set-intersection range-1 range-2)))
    ;; range-2 is now a proper sub-range of range-1
    (if (not range-2)
        (list range-1)
        (let ((start-1 (range-start range-1))
              (step-1 (range-step range-1))
              (end-1 (range-end range-1))
              (start-2 (range-start range-2))
              (step-2 (range-step range-2))
              (end-2 (range-end range-2))
              result)
          (flet ((maybe-push-range (start step end)
                   (when (<= start-1 start end end-1)
                     (push (make-range start step end) result))))
            (if (and
                 ;; by the definition of INTERSECTION, step-2 is now a
                 ;; multiple of step-1, except when it contains only a
                 ;; single element, where it is normalized to 1
                 (/= step-2 1)
                 (<= (the positive-integer (/ step-2 step-1))
                     (set-size range-2)))
                ;; Case 1: create ranges with step size step-2
                (iterate:iterate
                  (iterate:for offset from step-1 by step-1 below step-2)
                  (iterate:for start = (let ((high (+ start-2 offset))
                                             (low (+ start-2 (- step-2) offset)))
                                         (if (>= low start-1) low high)))
                  (iterate:for end = (let ((high (+ end-2 offset))
                                           (low (+ end-2 (- step-2) offset)))
                                       (if (<= high end-1) high low)))
                  (iterate:accumulate start by #'min initial-value start-2 into interior-start)
                  (iterate:accumulate end by #'max initial-value end-2 into interior-end)
                  (maybe-push-range start step-2 end)
                  ;; process the boundaries
                  (iterate:finally
                   (maybe-push-range start-1 step-1 (- interior-start step-1))
                   (maybe-push-range (+ interior-end step-1) step-1 end-1)))
                ;; Case 2: create ranges with step size step-1
                (let ((lower-max (- start-2 step-1))
                      (upper-min (+ end-2 step-1)))
                  (cond
                    ;; if there is exactly one upper and lower boundary
                    ;; element, both can be viewed as a single strided
                    ;; array with large step size
                    ((and (= lower-max start-1) (= upper-min end-1))
                     (maybe-push-range start-1 (- end-1 start-1) end-1))
                    ;; otherwise, treat both boundaries separately
                    (t
                     (maybe-push-range start-1 step-1 lower-max)
                     (maybe-push-range upper-min step-1 end-1)))
                  (when (> step-2 step-1) ; i.e. there is an interior
                    ;; process the interior
                    (let ((interval-length (- step-2 (* 2 step-1))))
                      (loop for start from (+ start-2 step-1) below end-2 by step-2
                            for end = (+ start interval-length) do
                              (maybe-push-range start step-1 end)))))))
          result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Intersection of Ranges

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

(defmethod set-intersection ((range-1 range) (range-2 range))
  (multiple-value-bind (start step end)
      (range-intersection-start-step-end range-1 range-2)
    (when start (make-range start step end))))

(defmethod set-intersection ((range-1 contiguous-range) (range-2 contiguous-range))
  (let ((start (max (range-start range-1)
                    (range-start range-2)))
        (end (min (range-end range-1)
                  (range-end range-2))))
    (when (<= start end)
      (make-range start 1 end))))

(defmethod range-intersectionp ((range-1 range) (range-2 range))
  (and (range-intersection-start-step-end range-1 range-2) t))

(defmethod range-step ((range contiguous-range))
  1)

(defmethod make-range ((start integer) (step integer) (end integer))
  (if (zerop step)
      (if (= start end)
          (make-instance 'contiguous-range :start start :end start)
          (error "Bad step size 0 for range with start ~d and end ~d" start end))
      (let ((steps (truncate (- end start) step)))
        (if (= steps 0)
            (make-instance 'contiguous-range :start start :end start)
            (let ((congruent-end (+ start (* step steps))))
              (let ((step (abs step))
                    (start (min start congruent-end))
                    (end (max start congruent-end)))
                (if (= 1 step)
                    (make-instance 'contiguous-range :start start :end end)
                    (make-instance 'strided-range :start start :step step :end end))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Legacy Functions

(declaim (inline size-one-range-p))
(defun size-one-range-p (range)
  (= (range-start range)
     (range-end range)))

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
