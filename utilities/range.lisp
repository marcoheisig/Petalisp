;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/utilities/range
  (:use :closer-common-lisp :alexandria :iterate)
  (:use
   :petalisp/utilities/extended-euclid)
  (:export
   #:range
   #:make-range
   #:rangep
   #:range-start
   #:range-step
   #:range-end
   #:range-difference
   #:range-intersection
   #:range-intersection?
   #:range-fusion
   #:range-size
   #:size-one-range-p))

(in-package :petalisp/utilities/range)

(defstruct (range
            (:constructor %make-range (start step end))
            (:copier nil)
            (:predicate rangep))
  "A range denotes the set {x | ∃n ∈ N0, x = START + n STEP ∧ x <= END}."
  (start nil :type integer          :read-only t)
  (step  nil :type positive-integer :read-only t)
  (end   nil :type integer          :read-only t))

(defun make-range (start step end)
  "Return a normalized range. If the specified end is not congruent to
start with respect to the step size, it is replaced by the next congruent
integer closer to start.

After normalization, start will be the smallest integer of the range, end
the largest integer and step a non-negative integer. If start equals end,
step is unconditionally set to one."
  (declare (integer start step end))
  (when (and (zerop step) (/= start end))
    (simple-program-error
     "Bad step size 0 for range with start ~d and end ~d"
     start end))
  (let ((step (if (= start end) 1 (abs step))))
    ;; ensure START and END are congruent relative to STEP
    (let ((end (+ start (* step (truncate (- end start) step)))))
      (%make-range (min start end) step (max start end)))))

(defun range-difference (range-1 range-2)
  (declare (type range range-1 range-2))
  ;; we only care about the part of range-2 that intersects with range-1
  (let ((range-2 (range-intersection range-1 range-2)))
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
                     (range-size range-2)))
                ;; Case 1: create ranges with step size step-2
                (iterate (for offset from step-1 by step-1 below step-2)
                         (for start = (let ((high (+ start-2 offset))
                                            (low (+ start-2 (- step-2) offset)))
                                        (if (>= low start-1) low high)))
                         (for end = (let ((high (+ end-2 offset))
                                          (low (+ end-2 (- step-2) offset)))
                                      (if (<= high end-1) high low)))
                         (accumulate start by #'min initial-value start-2 into interior-start)
                         (accumulate end by #'max initial-value end-2 into interior-end)
                         (maybe-push-range start step-2 end)
                         ;; process the boundaries
                         (finally
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
                      (iterate (for start from (+ start-2 step-1) below end-2 by step-2)
                               (for end = (+ start interval-length))
                               (maybe-push-range start step-1 end)))))))
          result))))

(defun range-fusion (ranges)
  ;; Assuming that all supplied RANGES are non-overlapping, the only
  ;; possible fusion is obtained by summing the number of elements,
  ;; determining the smallest and largest element of all sequences and
  ;; choosing a step size to yield the correct number of elements.
  (loop for range in ranges
        summing (range-size range) into number-of-elements
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
                 (when (notevery (lambda (range) (range-intersection? range result)) ranges)
                   (fail))
                 (return result))))))

(defun range-intersection-start-step-end (range-1 range-2)
  (declare (range range-1 range-2))
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

(defun range-intersection (range-1 range-2)
  (multiple-value-bind (start step end)
      (range-intersection-start-step-end range-1 range-2)
    (when start (make-range start step end))))

(defun range-intersection? (range-1 range-2)
  (and (range-intersection-start-step-end range-1 range-2) t))

(defun range-size (range)
  (declare (type range range))
  (1+ (the integer (/ (- (range-end range) (range-start range))
                      (range-step range)))))

(declaim (inline size-one-range-p))
(defun size-one-range-p (range)
  (= (range-start range)
     (range-end range)))
