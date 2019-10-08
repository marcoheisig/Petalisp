;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

(defun make-table-of-primes (size)
  (let ((table (make-array size))
        (index 0))
    (flet ((register (n)
             (cond ((not (array-in-bounds-p table index))
                    (return-from make-table-of-primes table))
                   (t
                    (setf (aref table index) n)
                    (incf index))))
           (primep (n)
             (let ((limit (isqrt n)))
               (loop for prime across table do
                 (cond ((> prime limit)
                        (return t))
                       ((zerop (mod n prime))
                        (return nil)))))))
      (register 2)
      (loop for number from 3 by 2 do
        (when (primep number)
          (register number))))))

(defparameter *table-of-primes* (make-table-of-primes 25000))

(defun prime-factors (integer)
  "Return the list of prime factors of INTEGER in ascending order."
  (declare (integer integer))
  (let ((factors '()))
    (loop for prime across *table-of-primes* do
      (tagbody retry
         (if (> (* prime prime) integer)
             (return-from prime-factors
               (nreverse (cons integer factors)))
             (multiple-value-bind (mod rem)
                 (floor integer prime)
               (when (zerop rem)
                 (setf integer mod)
                 (push prime factors)
                 (go retry))))))
    ;; TODO: Use a fallback algorithm.  Not that we are likely to end up
    ;; here, given that our table of primes is large enough to factorize
    ;; numbers up to 2^36.
    (error "The integer ~S is too large for factorization." integer)))
