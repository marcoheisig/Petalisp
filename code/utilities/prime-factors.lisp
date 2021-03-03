;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun nth-prime-upper-bound (n)
    (* n (+ (log n) (log (log n)))))

  (defun nth-prime-max-bits (n)
    (ceiling (log (nth-prime-upper-bound n) 2)))

  (defun make-table-of-primes (size)
    (let ((table (make-array size :element-type `(unsigned-byte ,(nth-prime-max-bits size))))
          (index 0))
      (flet ((register (n)
               (unless (array-in-bounds-p table index)
                 (return-from make-table-of-primes table))
               (setf (aref table index) n)
               (incf index))
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

  ;; Memory is cheap these days, so instead of implementing a second
  ;; algorithm for factoring large integers, we simply make the table large
  ;; enough.
  (defconstant +table-of-primes+
    (if (boundp '+table-of-primes+)
        (symbol-value '+table-of-primes+)
        (make-table-of-primes 90000))))

(defun prime-factors (integer)
  "Return the list of prime factors of INTEGER in ascending order."
  (declare (integer integer))
  (let ((factors '()))
    (loop for prime across +table-of-primes+ do
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
    ;; We aren't very likely to end up here, given that our table of primes
    ;; is large enough to factorize numbers up to 2^40.
    (error "The integer ~S is too large for factorization." integer)))

(defun primep (n)
  (let* ((imax (1- (array-total-size +table-of-primes+)))
         (pmax (aref +table-of-primes+ imax)))
    (if (< n (* pmax pmax))
        (find n +table-of-primes+)
        (error "The integer ~S is too large for checking prime-ness."
               n))))
