;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPLEX

(define-simple-instruction (complex complex.short-float) (complex-short-float) (short-float short-float))
(define-simple-instruction (complex complex.single-float) (complex-single-float) (single-float single-float))
(define-simple-instruction (complex complex.double-float) (complex-double-float) (double-float double-float))
(define-simple-instruction (complex complex.long-float) (complex-long-float) (long-float long-float))

(define-rule complex (realpart &optional (imagpart nil imagpart-supplied-p))
  (let* ((realpart-ntype (wrapper-ntype realpart))
         (imagpart-ntype (if imagpart-supplied-p
                             (wrapper-ntype imagpart)
                             (coerce 0 (type-specifier (generalize-ntype realpart-ntype))))))
    (with-constant-folding (complex (realpart-ntype real) (imagpart-ntype real))
      (if (and (eql-ntype-p imagpart)
               (eql imagpart 0))
          (rewrite-as realpart)
          (ntype-subtypecase (numeric-contagion realpart-ntype imagpart-ntype)
            (short-float
             (rewrite-as
              (complex.short-float
               (coerce-to-short-float realpart)
               (coerce-to-short-float imagpart))))
            (single-float
             (rewrite-as
              (complex.single-float
               (coerce-to-single-float realpart)
               (coerce-to-single-float imagpart))))
            (double-float
             (rewrite-as
              (complex.double-float
               (coerce-to-double-float realpart)
               (coerce-to-double-float imagpart))))
            (long-float
             (rewrite-as
              (complex.long-float
               (coerce-to-long-float realpart)
               (coerce-to-long-float imagpart))))
            (t
             (ntype-subtypecase imagpart-ntype
               ((not rational) (rewrite-default 'complex))
               (t (rewrite-default 'number)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; REALPART

(define-simple-instruction (realpart realpart.complex-short-float) (short-float) (complex-short-float))
(define-simple-instruction (realpart realpart.complex-single-float) (single-float) (complex-single-float))
(define-simple-instruction (realpart realpart.complex-double-float) (double-float) (complex-double-float))
(define-simple-instruction (realpart realpart.complex-long-float) (long-float) (complex-long-float))

(define-rule realpart (number)
  (let ((ntype (wrapper-ntype number)))
    (with-constant-folding (realpart (ntype number))
      (ntype-subtypecase ntype
        (complex-short-float (rewrite-as (realpart.complex-short-float number)))
        (complex-single-float (rewrite-as (realpart.complex-single-float number)))
        (complex-double-float (rewrite-as (realpart.complex-double-float number)))
        (complex-long-float (rewrite-as (realpart.complex-long-float number)))
        (real (rewrite-as number))
        (t (rewrite-default (ntype 'real)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IMAGPART

(define-simple-instruction (imagpart imagpart.complex-short-float) (short-float) (complex-short-float))
(define-simple-instruction (imagpart imagpart.complex-single-float) (single-float) (complex-single-float))
(define-simple-instruction (imagpart imagpart.complex-double-float) (double-float) (complex-double-float))
(define-simple-instruction (imagpart imagpart.complex-long-float) (long-float) (complex-long-float))

(define-rule imagpart (number)
  (let ((ntype (wrapper-ntype number)))
    (with-constant-folding (imagpart (ntype number))
      (ntype-subtypecase ntype
        (complex-short-float (rewrite-as (imagpart.complex-short-float number)))
        (complex-single-float (rewrite-as (imagpart.complex-single-float number)))
        (complex-double-float (rewrite-as (imagpart.complex-double-float number)))
        (complex-long-float (rewrite-as (imagpart.complex-long-float number)))
        (real (rewrite-as (* 0 number)))
        (t (rewrite-default (ntype 'real)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CIS

(define-simple-instruction (cis cis.short-float) (complex-short-float) (short-float))
(define-simple-instruction (cis cis.single-float) (complex-single-float) (single-float))
(define-simple-instruction (cis cis.double-float) (complex-double-float) (double-float))
(define-simple-instruction (cis cis.long-float) (complex-long-float) (long-float))

(define-rule cis (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not real)
     (abort-specialization))
    (short-float
     (rewrite-as
      (cis.short-float x)))
    (single-float
     (rewrite-as
      (cis.single-float x)))
    (double-float
     (rewrite-as
      (cis.double-float x)))
    (long-float
     (rewrite-as
      (cis.long-float x)))
    (t
     (rewrite-default
      (ntype 'complex)))))
