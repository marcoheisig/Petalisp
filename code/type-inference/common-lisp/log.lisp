;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(declaim (inline ln))
(defun ln (number)
  (log number))

(define-simple-instruction (ln short-float-ln) (short-float) ((or short-float (complex short-float))))
(define-simple-instruction (ln single-float-ln) (single-float) ((or single-float (complex single-float))))
(define-simple-instruction (ln double-float-ln) (double-float) ((or double-float (complex double-float))))
(define-simple-instruction (ln long-float-ln) (long-float) ((or long-float (complex long-float))))

(define-differentiator ln (x) _
  (wrap (/ x)))

(define-specializer ln (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not number) (abort-specialization))
    (short-float (wrap (short-float-ln x)))
    (single-float (wrap (single-float-ln x)))
    (double-float (wrap (double-float-ln x)))
    (long-float (wrap (long-float-ln x)))
    (t (wrap-default (ntype 'number)))))

(define-differentiator log (number &optional (base nil base-supplied-p)) index
  (if (not base-supplied-p)
      (wrap (/ number))
      (ecase index
        (0 (wrap (/ (* (ln base) number))))
        (1 (wrap (- (/ (ln number) (* base (expt (ln base) 2)))))))))

(define-specializer log (number &optional (base nil base-supplied-p))
  (if (not base-supplied-p)
      (wrap (ln number))
      (wrap (/ (ln number) (ln base)))))
