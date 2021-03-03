;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

(deftype small-fixnum ()
  `(integer
    ,(- (isqrt most-positive-fixnum))
    ,(isqrt most-positive-fixnum)))

(deftype small-non-negative-fixnum ()
  `(integer
    0
    ,(isqrt most-positive-fixnum)))

(defun extended-euclid (u v)
  "Given nonnegative integers u and v, return the values u1 and u3 such
that u*u1 + v*u2 = u3 = gcd(u,v)."
  (declare (alexandria:non-negative-integer u v))
  ;; This is a variant of Knuth's Algorithm X from TAOCP Volume 2, but
  ;; using tail recursion instead of iteration and dropping the redundant
  ;; computation of u2.
  (labels ((fixnum-euclid (u1 u3 v1 v3)
             (declare (small-fixnum u1 v1)
                      (small-non-negative-fixnum u3 v3)
                      (optimize (speed 3) (safety 0)))
             (if (zerop v3)
                 (values u1 u3)
                 (let ((q (floor u3 v3)))
                   (declare (small-non-negative-fixnum q))
                   (fixnum-euclid
                    v1 v3
                    (- u1 (* q v1))
                    (- u3 (* q v3))))))
           (bignum-euclid (u1 u3 v1 v3)
             (declare (integer u1 v1)
                      (alexandria:non-negative-integer u3 v3))
             (if (zerop v3)
                 (values u1 u3)
                 (let ((q (floor u3 v3)))
                   (bignum-euclid
                    v1 v3
                    (- u1 (* q v1))
                    (- u3 (* q v3)))))))
    ;; The absolute values of all coefficients in this algorithm are
    ;; bounded by the least common multiple of U and V.  Computing the
    ;; least common multiple is quite expensive, so instead we use the
    ;; product of U and V as a conservative estimate.  To see why (* u v)
    ;; is unconditionally greater than (lcm u v), consider the prime
    ;; factors of both numbers.
    (if (typep (* u v) 'small-non-negative-fixnum)
        (fixnum-euclid 1 u 0 v)
        (bignum-euclid 1 u 0 v))))
