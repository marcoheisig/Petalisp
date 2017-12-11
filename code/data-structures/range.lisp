;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp-internals)

(defstruct (range
            (:constructor %make-range (start step end))
            (:copier nil)
            (:predicate range?))
  "A range denotes the set {x | ∃n ∈ N0, x = START + n STEP ∧ x <= END}."
  (start nil :type integer          :read-only t)
  (step  nil :type positive-integer :read-only t)
  (end   nil :type integer          :read-only t))

(defun range (start step-or-end &optional end)
  "Return a normalized range. If END is specified, STEP-OR-END denotes the
  step size. If not, the step size defaults to one and STEP-OR-END denotes
  the interval end. If the specified end is not congruent to start with
  respect to the step size, it is replaced by the next congruent integer
  closer to start.

  After normalization, start will be the smallest integer of the range, end
  the largest integer and step a non-negative integer. If start equals
  end, step is unconditionally set to one."
  (let ((end (or end step-or-end))
        (step (if end step-or-end 1)))
    (when (and (zerop step) (/= start end))
      (simple-program-error
       "Bad step size 0 for range with start ~d and end ~d"
       start end))
    (let ((step (if (= start end) 1 (abs step))))
      ;; ensure START and END are congruent relative to STEP
      (let ((end (+ start (* step (truncate (- end start) step)))))
        (%make-range (min start end) step (max start end))))))

(defmethod generator ((result-type (eql 'range))
                      &key
                        (max-extent (floor most-positive-fixnum 4/5))
                        (max-size (floor (sqrt max-extent)))
                        intersecting)
  "Return a random range with at most MAX-SIZE elements, bounded by
MAX-EXTENT. If another range INTERSECTING is given, the result will
intersect it (potentially violating MAX-EXTENT)."
  (assert (and (plusp max-extent)
               (plusp max-size)
               (<= max-size max-extent)))
  (let ((max-step (floor max-extent (1- max-size))))
    (lambda ()
      (let ((step (1+ (random max-step)))
            (size (1+ (random max-size)))
            (sign (- (* (random 2) 2) 1)))
        (let ((extent (floor (* size step) 2)))
          (let ((offset (* sign (random (max (- max-extent extent) 1)))))
            (let ((start (- offset extent)))
              (let ((range (range start step (+ start (* (1- size) step)))))
                (if (not intersecting)
                    range
                    (flet ((random-element (range)
                             (+ (range-start range)
                                (* (range-step range)
                                   (random (size range))))))
                      (let ((offset (- (random-element intersecting)
                                       (random-element range))))
                        (range (+ (range-start range) offset)
                               (range-step range)
                               (+ (range-end range) offset)))))))))))))

(defmethod difference ((space-1 range) (space-2 range))
  ;; we only care about the part of space-2 that intersects with space-1
  (let ((space-2 (intersection space-1 space-2)))
    ;; now space-2 is a proper subspace of space-1
    (if (not space-2)
        (list space-1)
        (let ((start-1 (range-start space-1))
              (step-1 (range-step space-1))
              (end-1 (range-end space-1))
              (start-2 (range-start space-2))
              (step-2 (range-step space-2))
              (end-2 (range-end space-2))
              result)
          (flet ((maybe-push-range (start step end)
                   (when (<= start-1 start end end-1)
                     (push (range start step end) result))))
            (if (and
                 ;; by the definition of INTERSECTION, step-2 is now a
                 ;; multiple of step-1, except when it contains only a
                 ;; single element, where it is normalized to 1
                 (/= step-2 1)
                 (<= (the positive-integer (/ step-2 step-1))
                     (size space-2)))
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

(defmethod union ((range range) &rest more-ranges)
  (let ((ranges (list* range more-ranges)))
    (flet ((fail ()
             (simple-program-error
              "Unable to fuse ranges:~%~{~A~%~}" ranges)))
      ;; another generic method of UNION asserts that the given ranges are
      ;; non-overlapping. Relying on this, the only possible fusion is
      ;; obtained by summing the number of elements, determining the
      ;; smallest and largest element of all sequences and choosing a step
      ;; size to yield the correct number of elements.
      (iterate (for range in ranges)
               (sum (size range) into number-of-elements)
               (maximize (range-end range) into end)
               (minimize (range-start range) into start)
               (finally
                (let ((step (if (= number-of-elements 1) 1
                                (/ (- end start) (1- number-of-elements)))))
                  (unless (integerp step) (fail))
                  (let ((fusion (range start step end)))
                    (when (notevery (λ range (subspace? range fusion)) ranges) (fail))
                    (return fusion))))))))

(defmethod intersection ((range-1 range) (range-2 range))
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
                (range start lcm end)))))))))

(defmethod size ((object range))
  (1+ (the integer (/ (- (range-end object) (range-start object))
                      (range-step object)))))

(declaim (inline unary-range?))
(defun unary-range? (range)
  (= (range-start range)
     (range-end range)))
