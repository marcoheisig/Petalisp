;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (/= short-float/=) (generalized-boolean) (short-float short-float))
(define-simple-instruction (/= single-float/=) (generalized-boolean) (single-float single-float))
(define-simple-instruction (/= double-float/=) (generalized-boolean) (double-float double-float))
(define-simple-instruction (/= long-float/=) (generalized-boolean) (long-float long-float))
(define-simple-instruction (/= complex-short-float/=) (generalized-boolean) (complex-short-float complex-short-float))
(define-simple-instruction (/= complex-single-float/=) (generalized-boolean) (complex-single-float complex-single-float))
(define-simple-instruction (/= complex-double-float/=) (generalized-boolean) (complex-double-float complex-double-float))
(define-simple-instruction (/= complex-long-float/=) (generalized-boolean) (complex-long-float complex-long-float))
(define-instruction (= cmpneq) (generalized-boolean) (a b)
  (ntype-subtypecase
      (numeric-contagion
       (wrapper-ntype a)
       (wrapper-ntype b))
    ((not number) (abort-specialization))
    (short-float
     (rewrite-as
      (short-float/=
       (coerce-to-short-float a)
       (coerce-to-short-float b))))
    (single-float
     (rewrite-as
      (single-float/=
       (coerce-to-single-float a)
       (coerce-to-single-float b))))
    (double-float
     (rewrite-as
      (double-float/=
       (coerce-to-double-float a)
       (coerce-to-double-float b))))
    (long-float
     (rewrite-as
      (long-float/=
       (coerce-to-long-float a)
       (coerce-to-long-float b))))
    ((complex short-float)
     (rewrite-as
      (complex-short-float/=
       (coerce-to-complex-short-float a)
       (coerce-to-complex-short-float b))))
    ((complex single-float)
     (rewrite-as
      (complex-single-float/=
       (coerce-to-complex-single-float a)
       (coerce-to-complex-single-float b))))
    ((complex double-float)
     (rewrite-as
      (complex-double-float/=
       (coerce-to-complex-double-float a)
       (coerce-to-complex-double-float b))))
    ((complex long-float)
     (rewrite-as
      (complex-long-float/=
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
