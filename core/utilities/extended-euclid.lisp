;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package :petalisp-core)

(defun extended-euclid (u v)
  "Given nonnegative integers u and v, return the values u1 and u3 such
that u*u1 + v*u2 = u3 = gcd(u,v)."
  (declare (non-negative-integer u v))
  ;; This is a variant of Knuth's Algorithm X from TAOCP Volume 2, but
  ;; using tail recursion instead of iteration and dropping the redundant
  ;; computation of u2.
  (labels ((fixnum-euclid (u1 u3 v1 v3)
             (declare (fixnum u1 v1)
                      (non-negative-fixnum u3 v3)
                      (optimize (speed 3) (safety 0)))
             (if (zerop v3)
                 (values u1 u3)
                 (let ((q (floor u3 v3)))
                   (fixnum-euclid
                    v1 v3
                    (- u1 (the fixnum (* q v1)))
                    (- u3 (the fixnum (* q v3)))))))
           (bignum-euclid (u1 u3 v1 v3)
             (declare (integer u1 v1)
                      (non-negative-integer u3 v3))
             (if (zerop v3)
                 (values u1 u3)
                 (let ((q (floor u3 v3)))
                   (bignum-euclid
                    v1 v3
                    (- u1 (the integer (* q v1)))
                    (- u3 (the integer (* q v3))))))))
    ;; The absolute values of all coefficients in this algorithm are
    ;; bounded by the least common multiple of U and V.  Computing the
    ;; least common multiple is quite expensive, so instead we use the
    ;; product of U and V as a conservative estimate.  To see why (* u v)
    ;; is unconditionally greater than (lcm u v), consider the prime
    ;; factors of both numbers.
    (if (typep (* u v) 'non-negative-fixnum)
        (fixnum-euclid 1 u 0 v)
        (bignum-euclid 1 u 0 v))))
