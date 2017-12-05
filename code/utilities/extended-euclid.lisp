;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defun extended-euclid (u v)
  "Given nonnegative integers u and v, return the values u1 and u3 such
  that u*u1 + v*u2 = u3 = gcd(u,v)."
  (declare (non-negative-integer u v))
  (macrolet
      ((typed-euclid (type)
         (let ((fname (symbolicate type "-EUCLID")))
           ;; This is a variant of Knuth's Algorithm X from TAOCP Volume 2,
           ;; but using tail recursion instead of iteration and dropping
           ;; the redundant computation of u2
           `(named-lambda ,fname (u1 u3 v1 v3)
              (declare (type ,type u1 u3 v1 v3))
              (if (zerop v3)
                  (values u1 u3)
                  (let ((q (floor u3 v3)))
                    (,fname v1 v3
                            (- u1 (the ,type (* q v1)))
                            (- u3 (the ,type (* q v3))))))))))
    ;; the absolute values of all coefficients in this algorithm are
    ;; bounded by the least common multiple of U and V. Since computing
    ;; the latter is quite expensive, the product of U and V is used
    ;; instead. To see why (* u v) is unconditionally greater than (lcm u
    ;; v), consider the prime factors of both numbers.
    (with-unsafe-optimizations
      (if (<= (* u v) most-positive-fixnum)
          (funcall (typed-euclid fixnum ) 1 u 0 v)
          (funcall (typed-euclid integer) 1 u 0 v)))))
