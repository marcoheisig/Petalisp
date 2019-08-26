;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (/= cmpneq.integer) (generalized-boolean) (integer integer))
(define-simple-instruction (/= cmpneq.rational) (generalized-boolean) (rational rational))
(define-simple-instruction (/= cmpneq.short-float) (generalized-boolean) (short-float short-float))
(define-simple-instruction (/= cmpneq.single-float) (generalized-boolean) (single-float single-float))
(define-simple-instruction (/= cmpneq.double-float) (generalized-boolean) (double-float double-float))
(define-simple-instruction (/= cmpneq.long-float) (generalized-boolean) (long-float long-float))
(define-simple-instruction (/= cmpneq.complex-short-float) (generalized-boolean) (complex-short-float complex-short-float))
(define-simple-instruction (/= cmpneq.complex-single-float) (generalized-boolean) (complex-single-float complex-single-float))
(define-simple-instruction (/= cmpneq.complex-double-float) (generalized-boolean) (complex-double-float complex-double-float))
(define-simple-instruction (/= cmpneq.complex-long-float) (generalized-boolean) (complex-long-float complex-long-float))
(define-instruction (= cmpneq) (generalized-boolean) (a b)
  (ntype-subtypecase
      (numeric-contagion
       (wrapper-ntype a)
       (wrapper-ntype b))
    ((not number) (abort-specialization))
    (integer
     (rewrite-as
      (cmpneq.integer
       (the-integer a)
       (the-integer b))))
    (rational
     (rewrite-as
      (cmpneq.rational
       (the-rational a)
       (the-rational b))))
    (short-float
     (rewrite-as
      (cmpneq.short-float
       (coerce-to-short-float a)
       (coerce-to-short-float b))))
    (single-float
     (rewrite-as
      (cmpneq.single-float
       (coerce-to-single-float a)
       (coerce-to-single-float b))))
    (double-float
     (rewrite-as
      (cmpneq.double-float
       (coerce-to-double-float a)
       (coerce-to-double-float b))))
    (long-float
     (rewrite-as
      (cmpneq.long-float
       (coerce-to-long-float a)
       (coerce-to-long-float b))))
    ((complex short-float)
     (rewrite-as
      (cmpneq.complex-short-float
       (coerce-to-complex-short-float a)
       (coerce-to-complex-short-float b))))
    ((complex single-float)
     (rewrite-as
      (cmpneq.complex-single-float
       (coerce-to-complex-single-float a)
       (coerce-to-complex-single-float b))))
    ((complex double-float)
     (rewrite-as
      (cmpneq.complex-double-float
       (coerce-to-complex-double-float a)
       (coerce-to-complex-double-float b))))
    ((complex long-float)
     (rewrite-as
      (cmpneq.complex-long-float
       (coerce-to-complex-long-float a)
       (coerce-to-complex-long-float b))))
    (t
     (rewrite-default
      (ntype 'generalized-boolean)))))

(defun map-unique-pairs (fn list)
  (loop for sublist on list
        for a = (first sublist) do
          (loop for b in (rest sublist) do
                (funcall fn a b))))

(define-rule /= (number &rest more-numbers)
  (if (null more-numbers)
      (rewrite-as
       (prog2-fn
        (the-number number)
        t))
      ;; This code produces (N^2-N)/2 comparisons for N supplied numbers.
      ;; But whoever calls /= with more than two arguments deserves it.
      (let ((value (rewrite-as t)))
        (map-unique-pairs
         (lambda (a b)
           (setf value
                 (rewrite-as
                  (and-fn value (cmpneq a b)))))
         (list* number more-numbers))
        value)))
