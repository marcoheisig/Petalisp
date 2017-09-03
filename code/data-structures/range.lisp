;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defstruct (range
            (:constructor %make-range (start step end))
            (:copier nil)
            (:predicate range?))
  (start 0 :type integer       :read-only t)
  (step  1 :type unsigned-byte :read-only t)
  (end   0 :type integer       :read-only t))

(defun range (&rest spec)
  (declare (dynamic-extent spec))
  (multiple-value-bind (start step end)
      (ematch spec
        ((list start step end) (values start step end))
        ((list start end) (values start 1 end)))
    (if (zerop step)
        (if (= start end)
            (range start 1 end)
            (simple-program-error
             "Bad step size 0 for range with start ~d and end ~d"
             start end))
        ;; ensure START and END are congruent relative to STEP
        (let ((end (+ start (* step (truncate (- end start) step)))))
          (%make-range
           (min start end)
           (if (= start end) 1 (abs step))
           (max start end))))))

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
                ;; Case 1: ranges with step size step-2
                (let ((include-lowers (> start-1 (- start-2 step-2)))
                      (include-uppers (< end-1 (+ end-2 step-2))))
                  (unless include-lowers
                    (maybe-push-range start-1 step-1 (- start-2 step-1)))
                  (unless include-uppers
                    (maybe-push-range (+ end-2 step-1) step-1 end-1))
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
                ;; Case 2: ranges with step-size step-1
                (let ((lower-max (- start-2 step-1))
                      (upper-min (+ end-2 step-1)))
                  ;; process the boundaries
                  (cond ((and (= lower-max start-1) (= upper-min end-1))
                         (maybe-push-range start-1 (- end-1 start-1) end-1))
                        (t
                         (maybe-push-range start-1 step-1 lower-max)
                         (maybe-push-range upper-min step-1 end-1)))
                  (unless (= 1 step-2)
                    ;; process the interior
                    (iterate (for start from (+ start-2 step-1) below end-2 by step-2)
                             (for end = (+ start step-2 (- step-1)))
                             (maybe-push-range start step-1 end))))))
          result))))

(test |(difference range)|
  (flet ((? (a b)
           (if (intersection a b)
               (is (equal? a (apply #'fusion
                                    (intersection a b)
                                    (difference a b))))
               (is (equal? a (first (difference a b)))))))
    (? (range 1 3 7) (range 8 1 22))
    (? (range 1 1 5) (range 6 1 10))
    (? (range 1 3 7) (range 1 3 10))
    (? (range 1 3 13) (range 4 2 10))
    (? (range 0 1 9) (range 2 2 4))
    (? (range 1 2 23) (range 3 10 23))))

(defmethod fusion ((range range) &rest more-ranges)
  (let/de ((ranges (cons range more-ranges)))
    (iterate (for range in ranges)
             (sum (size range) into number-of-elements)
             (maximize (range-end range) into end)
             (minimize (range-start range) into start)
             (finally
              (let ((step (ceiling (1+ (- end start)) number-of-elements)))
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
  (flet ((? (a b result)
           (is (equal? result (intersection a b)))))
    (is (intersection (range 0 0 0) (range 0 0 0)))
    (is (null (intersection (range 1 1 5) (range 6 1 10))))
    (? (range -5 1 5) (range 0 1 10) (range 0 1 5))
    (? (range 0 3 12) (range 0 2 12) (range 0 6 12))
    (? (range 1 2 23) (range 3 10 23) (range 3 10 23))
    (? (range 0 55 1000) (range 0 143 1000) (range 0 715 1000))
    (? (range -10 40902 50000000) (range 24 24140 50000000)
       (range 13783964 (lcm 40902 24140) 42824384))
    (? (range 0 (expt 6 40) (expt 2 200)) (range 0 (expt 9 40) (expt 2 200))
       (range 0 (expt 18 40) (expt 2 200)))))

(defmethod size ((object range))
  (1+ (the integer (/ (- (range-end object) (range-start object))
                      (range-step object)))))

(declaim (inline unary-range?))
(defun unary-range? (range)
  (= (range-start range)
     (range-end range)))

(test range
  (is (range? (range 0 1 10)))
  (is (equal? (range 0 5 0) (range 0 0 0)))
  (is (equal? (range 5 3 12) (range 5 3 11)))
  (is (equal? (range -2 2 2) (range 2 2 -2)))
  (is (unary-range? (range 99 -5 103)))
  (is (= (size (range 0 1 100))
         (size (range 5 5 505))))
  (for-all ((start (integer-generator))
            (step (integer-generator))
            (end (integer-generator)))
    (let ((step (if (zerop step) 1 step)))
      (is (range? (range start step end)))
      (is (= (size (range start step end))
             (1+ (floor (abs (- start end)) (abs step)))))
      (is (equal? (range start step end)
                  (range start (- step) end))))))
