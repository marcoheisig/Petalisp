;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)


(define-differentiator / (number &rest more-numbers) index
  (cond ((null more-numbers)
         (wrap (* -1 (expt number 2))))
        ((zerop index)
         (apply (specializer '/) (wrap 1) more-numbers))
        (t
         (apply (specializer '/)
                (wrap (- number))
                (loop for number in more-numbers
                      for position from 1
                      collect
                      (if (= position index)
                          (wrap (expt number 2))
                          number))))))

(define-simple-instruction (/ short-float/) (short-float) (short-float short-float))
(define-simple-instruction (/ single-float/) (single-float) (single-float single-float))
(define-simple-instruction (/ double-float/) (double-float) (double-float double-float))
(define-simple-instruction (/ long-float/) (long-float) (long-float long-float))
(define-simple-instruction (/ complex-short-float/) (complex-short-float) (complex-short-float complex-short-float))
(define-simple-instruction (/ complex-single-float/) (complex-single-float) (complex-single-float complex-single-float))
(define-simple-instruction (/ complex-double-float/) (complex-double-float) (complex-double-float complex-double-float))
(define-simple-instruction (/ complex-long-float/) (complex-long-float) (complex-long-float complex-long-float))

(define-specializer / (number &rest more-numbers)
  (cond ((null more-numbers)
         (wrap
          (/ 1 number)))
        (t
         (flet ((two-arg-/ (a b)
                  (let* ((ntype-of-a (wrapper-ntype a))
                         (ntype-of-b (wrapper-ntype b))
                         (result-ntype (numeric-contagion ntype-of-a ntype-of-b)))
                    (cond
                      ((and (eql-ntype-p ntype-of-b)
                            (numberp ntype-of-b)
                            (= ntype-of-b 1))
                       (funcall (specializer 'coerce)
                                a
                                (wrap-constant (type-specifier result-ntype))))
                      ;; Multiplication by a reciprocal is always
                      ;; faster than the division, as long as we can
                      ;; statically determine the reciprocal.
                      ((and (eql-ntype-p ntype-of-b)
                            (numberp ntype-of-b)
                            (not (eql-ntype-p ntype-of-a)))
                       (wrap (* a (/ b))))
                      (t
                       (ntype-subtypecase (numeric-contagion ntype-of-a ntype-of-b)
                         ((not number) (abort-specialization))
                         ((or integer rational)
                          (wrap-default (ntype 'rational)))
                         (short-float
                          (wrap
                           (short-float/
                            (coerce-to-short-float a)
                            (coerce-to-short-float b))))
                         (single-float
                          (wrap
                           (single-float/
                            (coerce-to-single-float a)
                            (coerce-to-single-float b))))
                         (double-float
                          (wrap
                           (double-float/
                            (coerce-to-double-float a)
                            (coerce-to-double-float b))))
                         (long-float
                          (wrap
                           (long-float/
                            (coerce-to-long-float a)
                            (coerce-to-long-float b))))
                         ((complex short-float)
                          (wrap
                           (complex-short-float/
                            (coerce-to-complex-short-float a)
                            (coerce-to-complex-short-float b))))
                         ((complex single-float)
                          (wrap
                           (complex-single-float/
                            (coerce-to-complex-single-float a)
                            (coerce-to-complex-single-float b))))
                         ((complex double-float)
                          (wrap
                           (complex-double-float/
                            (coerce-to-complex-double-float a)
                            (coerce-to-complex-double-float b))))
                         ((complex long-float)
                          (wrap
                           (complex-long-float/
                            (coerce-to-complex-long-float a)
                            (coerce-to-complex-long-float b))))
                         (t
                          (wrap-default
                           (ntype 'number)))))))))
           (reduce #'two-arg-/ more-numbers :initial-value number)))))
