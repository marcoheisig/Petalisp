;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(defun one-ntype-p (ntype)
  (and (numberp ntype)
       (= ntype 1)))

(define-specializer * (&rest numbers)
  (trivia:match numbers
    ((list)
     (wrap 1))
    ((list number)
     (wrap
      (the-number number)))
    (numbers
     (reduce
      (lambda (a b)
        (let* ((ntype-of-a (wrapper-ntype a))
               (ntype-of-b (wrapper-ntype b))
               (result-ntype (numeric-contagion ntype-of-a ntype-of-b)))
          (cond
            ((or (zero-ntype-p ntype-of-a)
                 (zero-ntype-p ntype-of-b))
             (wrap-constant (coerce 0 (type-specifier result-ntype))))
            ((one-ntype-p ntype-of-a)
             (funcall (specializer 'coerce)
                      b
                      (wrap-constant (type-specifier result-ntype))))
            ((one-ntype-p ntype-of-b)
             (funcall (specializer 'coerce)
                      a
                      (wrap-constant (type-specifier result-ntype))))
            (t
             (ntype-subtypecase
                 (numeric-contagion
                  (wrapper-ntype a)
                  (wrapper-ntype b))
               ((not number) (abort-specialization))
               (short-float
                (wrap
                 (short-float*
                  (coerce-to-short-float a)
                  (coerce-to-short-float b))))
               (single-float
                (wrap
                 (single-float*
                  (coerce-to-single-float a)
                  (coerce-to-single-float b))))
               (double-float
                (wrap
                 (double-float*
                  (coerce-to-double-float a)
                  (coerce-to-double-float b))))
               (long-float
                (wrap
                 (long-float*
                  (coerce-to-long-float a)
                  (coerce-to-long-float b))))
               ((complex short-float)
                (wrap
                 (complex-short-float*
                  (coerce-to-complex-short-float a)
                  (coerce-to-complex-short-float b))))
               ((complex single-float)
                (wrap
                 (complex-single-float*
                  (coerce-to-complex-single-float a)
                  (coerce-to-complex-single-float b))))
               ((complex double-float)
                (wrap
                 (complex-double-float*
                  (coerce-to-complex-double-float a)
                  (coerce-to-complex-double-float b))))
               ((complex long-float)
                (wrap
                 (complex-long-float*
                  (coerce-to-complex-long-float a)
                  (coerce-to-complex-long-float b))))
               (t
                (wrap-default (ntype 'number))))))))
      numbers))))

(define-simple-instruction (* short-float*) (short-float) (short-float short-float))
(define-simple-instruction (* single-float*) (single-float) (single-float single-float))
(define-simple-instruction (* double-float*) (double-float) (double-float double-float))
(define-simple-instruction (* long-float*) (long-float) (long-float long-float))
(define-simple-instruction (* complex-short-float*) (complex-short-float) (complex-short-float complex-short-float))
(define-simple-instruction (* complex-single-float*) (complex-single-float) (complex-single-float complex-single-float))
(define-simple-instruction (* complex-double-float*) (complex-double-float) (complex-double-float complex-double-float))
(define-simple-instruction (* complex-long-float*) (complex-long-float) (complex-long-float complex-long-float))

(define-differentiator * (&rest numbers) index
  (apply (specializer '*)
         (loop for number in numbers
               for position from 0
               unless (= position index)
                 collect number)))
