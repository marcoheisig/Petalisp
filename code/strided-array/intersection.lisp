;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod intersection ((space-1 strided-array-index-space)
                         (space-2 strided-array-index-space))
  (make-instance
   'strided-array-index-space
   :ranges (map 'vector
                (lambda (a b)
                  (or (intersection a b)
                      (return-from intersection nil)))
                (ranges space-1)
                (ranges space-2))))

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

