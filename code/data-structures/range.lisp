;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defstruct (range
            (:constructor %make-range (start step end))
            (:copier nil)
            (:predicate range?))
  (start 0 :type integer          :read-only t)
  (step  1 :type positive-integer :read-only t)
  (end   0 :type integer          :read-only t))

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

(defun range-generator (&optional (max-size #.(* 3 most-positive-fixnum)))
  "Return a random range, whose start and end are at most MAX-SIZE apart."
  (let ((random-integer (integer-generator
                         (- (round max-size 2))
                         (round max-size 2))))
    (lambda ()
      (let ((start (funcall random-integer))
            (end (funcall random-integer)))
        (let ((step (apply #'* (random-selection
                                (prime-factors
                                 (abs (- end start))
                                 :less-than #.(expt 10 6))))))
          (range start step end))))))

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
                  (when (> step-2 step-1) ; i.e. there is an interior
                    ;; process the interior
                    (let ((end-offset (- step-2 step-1 step-1)))
                      (iterate (for start from (+ start-2 step-1) below end-2 by step-2)
                               (for end = (+ start end-offset))
                               (maybe-push-range start step-1 end)))))))
          result))))

(test |(difference range)|
  (for-all ((a (range-generator 1000))
            (b (range-generator 1000)))
    (if (intersection a b)
        (is (equal? a (apply #'fusion
                             (intersection a b)
                             (difference a b))))
        (is (equal? a (first (difference a b)))))))

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
  (for-all ((a (range-generator 10000))
            (b (range-generator 10000))
            (rand-a (integer-generator 0))
            (rand-b (integer-generator 0)))
    ;; make both ranges intersect by selecting a random point in each one
    ;; and moving b such that these points coincide
    (let ((pa (+ (range-start a)
                 (* (range-step a)
                    (mod rand-a (size a)))))
          (pb (+ (range-start b)
                 (* (range-step b)
                    (mod rand-b (size b))))))
      (let ((offset (- pa pb)))
        (let ((b (range (+ (range-start b) offset)
                        (range-step b)
                        (+ (range-end b) offset))))
          (let ((intersection (intersection a b)))
            (is-true (subspace? intersection a))
            (is-true (subspace? intersection b))
            (is (not (difference intersection a)))
            (is (not (difference intersection b)))))))))

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
