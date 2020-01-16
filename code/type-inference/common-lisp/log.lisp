;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(declaim (inline ln))
(defun ln (number)
  (log number))

(define-differentiator ln (x) _
  (wrap (/ x)))

(define-specializer ln (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not number)
     (abort-specialization))
    (t
     (wrap-default (ntype 'number)))))

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
