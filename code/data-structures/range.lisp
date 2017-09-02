;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (assert (not (and (zerop step) (/= start end))))
    ;; ensure that STEP is positive
    (when (minusp step) (setf step (- step)))
    ;; normalize step
    (when (= start end) (setf step 1))
    ;; ensure START and END are congruent relative to STEP
    (setf end (+ start (* step (truncate (- end start) step))))
    ;; ensure START is bigger than END
    (when (> start end) (rotatef start end))
    ;; normalize step again
    (when (= start end) (setf step 1))
    (%make-range start step end)))

(defmethod difference ((space-1 range) (space-2 range))
  ;; we only care about the part of space-2 that intersects with space-1
  (let ((space-2 (intersection space-1 space-2)))
    (if (not space-2) `(,space-1)
        (let ((start-1 (range-start space-1))
              (step-1 (range-step space-1))
              (end-1 (range-end space-1))
              (start-2 (range-start space-2))
              (step-2 (range-step space-2))
              (end-2 (range-end space-2)))
          ;; There are two options to form the difference of two
          ;; ranges. Either by creating ranges with the step size of
          ;; space-1, or by creating ranges with the step size of
          ;; space-2. Which one is more efficient depends on the situation.
          (flet ((range (start step end)
                   (when (<= start-1 start end end-1)
                     (range start step end))))
            (loop for x from start-2 below end-2 by step-2
                  when (range (+ x step-1) step-1 (- (+ x step-2) step-1))
                    collect it into result
                  finally
                     (if-let (it (range start-1 step-1 (- start-2 step-1)))
                       (push it result))
                     (if-let (it (range (+ end-2 step-1) step-1 end-1))
                       (push it result))
                     (return result)))))))

(defmethod fusion ((range range) &rest more-ranges)
  (let ((ranges (cons range more-ranges)))
    (loop for range in ranges
          sum (size range) into number-of-elements
          maximize (range-end range) into end
          minimize (range-start range) into start
          finally
             (let ((step (ceiling (1+ (- end start)) number-of-elements)))
               (return (range start step end))))))

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

(defmethod size ((object range))
  (1+ (the integer (/ (- (range-end object) (range-start object))
                      (range-step object)))))

(declaim (inline unary-range?))
(defun unary-range? (range)
  (= (range-start range)
     (range-end range)))
