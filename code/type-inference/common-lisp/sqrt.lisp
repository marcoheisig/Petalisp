;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (sqrt short-float-sqrt) (short-float) ((or short-float (complex short-float))))
(define-simple-instruction (sqrt single-float-sqrt) (single-float) ((or single-float (complex single-float))))
(define-simple-instruction (sqrt double-float-sqrt) (double-float) ((or double-float (complex double-float))))
(define-simple-instruction (sqrt long-float-sqrt) (long-float) ((or long-float (complex long-float))))

(define-specializer sqrt (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not number)
     (abort-specialization))
    (short-float
     (wrap (short-float-sqrt x)))
    (single-float
     (wrap (single-float-sqrt x)))
    (double-float
     (wrap (double-float-sqrt x)))
    (long-float
     (wrap (long-float-sqrt x)))
    (complex
     (wrap
      (exp (/ (log x) 2))))
    (t
     (wrap-default (ntype 'number)))))

(define-differentiator sqrt (x) index
  (wrap (/ (* 2 (sqrt x)))))
