;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPLEX

(define-simple-instruction (complex short-float-complex) (complex-short-float) (short-float short-float))
(define-simple-instruction (complex single-float-complex) (complex-single-float) (single-float single-float))
(define-simple-instruction (complex double-float-complex) (complex-double-float) (double-float double-float))
(define-simple-instruction (complex long-float-complex) (complex-long-float) (long-float long-float))

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
              (short-float-complex
               (coerce-to-short-float realpart)
               (coerce-to-short-float imagpart))))
            (single-float
             (rewrite-as
              (single-float-complex
               (coerce-to-single-float realpart)
               (coerce-to-single-float imagpart))))
            (double-float
             (rewrite-as
              (double-float-complex
               (coerce-to-double-float realpart)
               (coerce-to-double-float imagpart))))
            (long-float
             (rewrite-as
              (long-float-complex
               (coerce-to-long-float realpart)
               (coerce-to-long-float imagpart))))
            (t
             (ntype-subtypecase imagpart-ntype
               ((not rational) (rewrite-default 'complex))
               (t (rewrite-default 'number)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; REALPART

(define-simple-instruction (realpart complex-short-float-realpart) (short-float) (complex-short-float))
(define-simple-instruction (realpart complex-single-float-realpart) (single-float) (complex-single-float))
(define-simple-instruction (realpart complex-double-float-realpart) (double-float) (complex-double-float))
(define-simple-instruction (realpart complex-long-float-realpart) (long-float) (complex-long-float))

(define-rule realpart (number)
  (let ((ntype (wrapper-ntype number)))
    (with-constant-folding (realpart (ntype number))
      (ntype-subtypecase ntype
        (complex-short-float (rewrite-as (complex-short-float-realpart number)))
        (complex-single-float (rewrite-as (complex-single-float-realpart number)))
        (complex-double-float (rewrite-as (complex-double-float-realpart number)))
        (complex-long-float (rewrite-as (complex-long-float-realpart number)))
        (real (rewrite-as number))
        (t (rewrite-default (ntype 'real)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IMAGPART

(define-simple-instruction (imagpart complex-short-float-imagpart) (short-float) (complex-short-float))
(define-simple-instruction (imagpart complex-single-float-imagpart) (single-float) (complex-single-float))
(define-simple-instruction (imagpart complex-double-float-imagpart) (double-float) (complex-double-float))
(define-simple-instruction (imagpart complex-long-float-imagpart) (long-float) (complex-long-float))

(define-rule imagpart (number)
  (let ((ntype (wrapper-ntype number)))
    (with-constant-folding (imagpart (ntype number))
      (ntype-subtypecase ntype
        (complex-short-float (rewrite-as (complex-short-float-imagpart number)))
        (complex-single-float (rewrite-as (complex-single-float-imagpart number)))
        (complex-double-float (rewrite-as (complex-double-float-imagpart number)))
        (complex-long-float (rewrite-as (complex-long-float-imagpart number)))
        (real (rewrite-as (* 0 number)))
        (t (rewrite-default (ntype 'real)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONJUGATE

(define-rule conjugate (number)
  (let ((ntype (wrapper-ntype number)))
    (if (and (eql-ntype-p number)
             (numberp number))
        (wrap-constant
         (conjugate ntype))
        (ntype-subtypecase ntype
          ((not number) (abort-specialization))
          (real (rewrite-as number))
          (complex (rewrite-as (complex (realpart number) (- (imagpart number)))))
          (t (rewrite-default (ntype 'number)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PHASE

(define-rule phase (number)
  (let ((ntype (wrapper-ntype number)))
    (if (and (eql-ntype-p ntype)
             (numberp ntype))
        (wrap-constant
         (phase ntype))
        (ntype-subtypecase ntype
          ((not number) (abort-specialization))
          ((float 0e0 *) (rewrite-as (float 0 number)))
          ((rational 0 *) (rewrite-as 0))
          ((float * (0e0)) (rewrite-as (float #.pi number)))
          ((rational * (0)) (wrap-constant (coerce pi 'single-float)))
          ((complex float) (rewrite-default (complex-part-ntype ntype)))
          (t (rewrite-default (ntype 'number)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CIS

(define-simple-instruction (cis short-float-cis) (complex-short-float) (short-float))
(define-simple-instruction (cis single-float-cis) (complex-single-float) (single-float))
(define-simple-instruction (cis double-float-cis) (complex-double-float) (double-float))
(define-simple-instruction (cis long-float-cis) (complex-long-float) (long-float))

(define-rule cis (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not real)
     (abort-specialization))
    (short-float
     (rewrite-as
      (short-float-cis x)))
    (single-float
     (rewrite-as
      (single-float-cis x)))
    (double-float
     (rewrite-as
      (double-float-cis x)))
    (long-float
     (rewrite-as
      (long-float-cis x)))
    (t
     (rewrite-default
      (ntype 'complex)))))
