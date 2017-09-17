;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defstruct (range
            (:constructor %make-range (start step end))
            (:copier nil)
            (:predicate range?))
  "A range denotes the set {x | ∃n ∈ N0, x = START + n STEP ∧ x <= END}."
  (start nil :type integer          :read-only t)
  (step  nil :type positive-integer :read-only t)
  (end   nil :type integer          :read-only t))

(defun range (start step-or-end &optional end)
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

(defun range-generator (&key
                          (max-extent #.(floor most-positive-fixnum 4/5))
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

(test |(range-generator)|
  (let ((max-size 10)
        (max-extent 100))
    (for-all ((range (range-generator :max-extent max-extent
                                      :max-size max-size)))
      (is (<= (abs (range-start range)) max-extent))
      (is (<= (abs (range-end range)) max-extent))
      (is (<= (size range) max-size)))))

(defmethod difference ((space-1 range) (space-2 range))
  ;; we only care about the part of space-2 that intersects with space-1
  (let ((space-2 (intersection space-1 space-2)))
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
            (if (and (/= step-2 1)
                     (<= (the positive-integer (/ step-2 step-1))
                         (size space-2)))
                ;; Case 1: create ranges with step size step-2
                (let ((include-lowers (> start-1 (- start-2 step-2)))
                      (include-uppers (< end-1 (+ end-2 step-2))))
                  ;; process the boundaries
                  (unless include-lowers
                    (maybe-push-range start-1 step-1 (- start-2 step-1)))
                  (unless include-uppers
                    (maybe-push-range (+ end-2 step-1) step-1 end-1))
                  ;; process the interior
                  (iterate (for offset from step-1 by step-1 below step-2)
                           (for start = (if include-lowers
                                            (+ start-2 (- step-2) offset)
                                            (+ start-2 offset)))
                           (for end = (if include-uppers
                                          (+ end-2 offset)
                                          (+ end-2 (- step-2) offset)))
                           (maybe-push-range
                            (if (< start start-1) (+ start step-2) start)
                            step-2
                            (if (< end-1 end) (- end step-2) end))))
                ;; Case 2: create ranges with step size step-1
                (let ((lower-max (- start-2 step-1))
                      (upper-min (+ end-2 step-1)))
                  ;; process the boundaries
                  (cond ((and (= lower-max start-1) (= upper-min end-1))
                         (maybe-push-range start-1 (- end-1 start-1) end-1))
                        (t
                         (maybe-push-range start-1 step-1 lower-max)
                         (maybe-push-range upper-min step-1 end-1)))
                  (when (> step-2 step-1) ; i.e. there is an interior
                    ;; process the interior
                    (let ((end-offset (- step-2 step-1 step-1)))
                      (iterate (for start from (+ start-2 step-1) below end-2 by step-2)
                               (for end = (+ start end-offset))
                               (maybe-push-range start step-1 end)))))))
          result))))

(test |(difference range)|
  (for-all ((a (range-generator :max-extent 1000)))
    (for-all ((b (range-generator :max-extent 1000
                                  :intersecting a)))
      (is (equal? a (apply #'fusion
                           (intersection a b)
                           (difference a b)))))))

(defmethod fusion ((range range) &rest more-ranges)
  (let ((ranges (list* range more-ranges)))
    (iterate (for range in ranges)
             (sum (size range) into number-of-elements)
             (maximize (range-end range) into end)
             (minimize (range-start range) into start)
             (finally
              (let ((step (if (= number-of-elements 1) 1
                              (/ (- end start) (1- number-of-elements)))))
                (assert (integerp step)
                        (ranges)
                        "Unable to fuse ranges:~%~{~A~%~}"
                        ranges)
                (return (range start step end)))))))

(defmethod intersection ((range-1 range) (range-2 range))
  (let ((lb (max (range-start range-1) (range-start range-2)))
        (ub (min (range-end   range-1) (range-end   range-2)))
        (a (range-step range-1))
        (b (range-step range-2))
        (c (- (range-start range-2) (range-start range-1))))
    (multiple-value-bind (s gcd)
        (extended-euclid a b)
      (when (integerp (/ c gcd))
        (let ((x (+ (* s (/ c gcd) a)
                    (range-start range-1)))
              (lcm (/ (* a b) gcd)))
          (let ((smallest (+ x (* lcm (ceiling (- lb x) lcm))))
                (biggest  (+ x (* lcm (floor (- ub x) lcm)))))
            (when (<= lb smallest biggest ub)
              (range smallest lcm biggest))))))))

(test |(intersection range)|
  (let ((fiveam::*num-trials* (ceiling (sqrt fiveam::*num-trials*))))
    (for-all ((a (range-generator :max-extent 10000)))
      (for-all ((b (range-generator :max-extent 10000
                                    :intersecting a)))
        (let ((intersection (intersection a b)))
          (is-true (subspace? intersection a))
          (is-true (subspace? intersection b))
          (is (not (difference intersection a)))
          (is (not (difference intersection b))))))))

(defmethod size ((object range))
  (1+ (the integer (/ (- (range-end object) (range-start object))
                      (range-step object)))))

(declaim (inline unary-range?))
(defun unary-range? (range)
  (= (range-start range)
     (range-end range)))

(test range
  (is (range? (range 0 0 0)))
  (signals error (range 0 0 1))
  (for-all ((start (integer-generator))
            (step (integer-generator 1))
            (end (integer-generator)))
    (is (range? (range start step end)))
    (is (= (size (range start step end))
           (1+ (floor (abs (- start end)) (abs step)))))
    (is (equal? (range start step end)
                (range start (- step) end)))))
