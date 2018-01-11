;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/utilities/extended-euclid
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/macros)
  (:export #:extended-euclid))

(in-package :petalisp/utilities/extended-euclid)

(defun extended-euclid (u v)
  "Given nonnegative integers u and v, return the values u1 and u3 such
that u*u1 + v*u2 = u3 = gcd(u,v)."
  (declare (non-negative-integer u v))
  ;; This is a variant of Knuth's Algorithm X from TAOCP Volume 2, but
  ;; using tail recursion instead of iteration and dropping the redundant
  ;; computation of u2
  (labels ((fixnum-euclid (u1 u3 v1 v3)
             (declare (fixnum u1 v1)
                      (non-negative-fixnum u3 v3))
             (with-unsafe-optimizations
               (if (zerop v3)
                   (values u1 u3)
                   (let ((q (floor u3 v3)))
                     (fixnum-euclid
                      v1 v3
                      (- u1 (the fixnum (* q v1)))
                      (- u3 (the fixnum (* q v3))))))))
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
    ;; the absolute values of all coefficients in this algorithm are
    ;; bounded by the least common multiple of U and V. Since computing the
    ;; latter is quite expensive, the product of U and V is used
    ;; instead. To see why (* u v) is unconditionally greater than (lcm u
    ;; v), consider the prime factors of both numbers.
    (if (<= (* u v) most-positive-fixnum)
        (fixnum-euclid 1 u 0 v)
        (bignum-euclid 1 u 0 v))))
