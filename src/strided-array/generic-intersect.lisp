;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod generic-intersect ((space-1 strided-array-index-space)
                              (space-2 strided-array-index-space))
  (when (= (dimension space-1)
           (dimension space-2))
    (catch 'no-intersection
      (make-instance
       'strided-array-index-space
       :ranges
       (mapcar #'generic-intersect
               (ranges space-1)
               (ranges space-2))))))

(defmethod generic-intersect ((range-1 range)
                              (range-2 range))
  (let ((start-1 (range-start range-1))
        (start-2 (range-start range-2))
        (step-1 (range-step range-1))
        (step-2 (range-step range-2))
        (end-1 (range-end range-1))
        (end-2 (range-end range-2)))
    (multiple-value-bind (a b gcd)
        (kuṭṭaka step-1 step-2 (- start-2 start-1))
      (declare (ignore b))
      (unless a (throw 'no-intersection nil))
      (let ((lb (max start-1 start-2))
            (ub (min end-1 end-2))
            (lcm (/ (* step-1 step-2) gcd))
            (x (+ (* a step-1) start-1)))
        (let ((smallest (+ x (* lcm (ceiling (- lb x) lcm))))
              (biggest  (+ x (* lcm (floor (- ub x) lcm)))))
          (unless (<= lb smallest biggest ub)
            (throw 'no-intersection nil))
          (range smallest lcm biggest))))))

(defun kuṭṭaka (d1 d2 c)
  "Returns A, B and GCD(d1,d2) such that A * d1 - B * d2 = c. Returns NIL
if no solution exists."
  (declare (integer d1 d2 c))
  ;; The first part is just Euclids algorithm to determine the GCD, where
  ;; we additionally keep track of all the quotients
  (let* ((quotients ())
         (gcd
           (loop with u of-type integer = (abs d1)
                 and  v of-type integer = (abs d2) do
             (when (= v 0) (return u))
             (multiple-value-bind (quot rem) (floor u v)
               (push quot quotients)
               (psetf v rem u v)))))
    ;; If C cannot be divided by GCD, there is no solution
    (let ((c (/ c gcd)))
      (unless (integerp c) (return-from kuṭṭaka nil))
      ;; now comes the algorithm of Aryabhata
      (let* ((a 0)
             (b (if (evenp (length quotients)) c (- c))))
        (mapc
         (lambda (x)
           (psetf a b b (the integer (+ (* x b) a))))
         (cdr quotients))
        (values a b gcd)))))
