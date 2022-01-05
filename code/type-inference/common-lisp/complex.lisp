;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPLEX

(define-simple-instruction (complex short-float-complex) (complex-short-float) (short-float short-float))
(define-simple-instruction (complex single-float-complex) (complex-single-float) (single-float single-float))
(define-simple-instruction (complex double-float-complex) (complex-double-float) (double-float double-float))
(define-simple-instruction (complex long-float-complex) (complex-long-float) (long-float long-float))

(define-specializer complex (realpart &optional (imagpart nil imagpart-supplied-p))
  (let* ((realpart-ntype (wrapper-ntype realpart))
         (imagpart-ntype (if imagpart-supplied-p
                             (wrapper-ntype imagpart)
                             (coerce 0 (type-specifier (generalize-ntype realpart-ntype))))))
    (with-constant-folding (complex (realpart-ntype real) (imagpart-ntype real))
      (if (and (eql-ntype-p imagpart)
               (eql imagpart 0))
          (wrap realpart)
          (ntype-subtypecase (numeric-contagion realpart-ntype imagpart-ntype)
            (short-float
             (wrap
              (short-float-complex
               (coerce-to-short-float realpart)
               (coerce-to-short-float imagpart))))
            (single-float
             (wrap
              (single-float-complex
               (coerce-to-single-float realpart)
               (coerce-to-single-float imagpart))))
            (double-float
             (wrap
              (double-float-complex
               (coerce-to-double-float realpart)
               (coerce-to-double-float imagpart))))
            (long-float
             (wrap
              (long-float-complex
               (coerce-to-long-float realpart)
               (coerce-to-long-float imagpart))))
            (t
             (ntype-subtypecase imagpart-ntype
               ((not rational) (wrap-default 'complex))
               (t (wrap-default 'number)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; REALPART

(define-simple-instruction (realpart complex-short-float-realpart) (short-float) (complex-short-float))
(define-simple-instruction (realpart complex-single-float-realpart) (single-float) (complex-single-float))
(define-simple-instruction (realpart complex-double-float-realpart) (double-float) (complex-double-float))
(define-simple-instruction (realpart complex-long-float-realpart) (long-float) (complex-long-float))

(define-specializer realpart (number)
  (let ((ntype (wrapper-ntype number)))
    (with-constant-folding (realpart (ntype number))
      (ntype-subtypecase ntype
        (complex-short-float (wrap (complex-short-float-realpart number)))
        (complex-single-float (wrap (complex-single-float-realpart number)))
        (complex-double-float (wrap (complex-double-float-realpart number)))
        (complex-long-float (wrap (complex-long-float-realpart number)))
        (real (wrap number))
        (t (wrap-default (ntype 'real)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IMAGPART

(define-simple-instruction (imagpart complex-short-float-imagpart) (short-float) (complex-short-float))
(define-simple-instruction (imagpart complex-single-float-imagpart) (single-float) (complex-single-float))
(define-simple-instruction (imagpart complex-double-float-imagpart) (double-float) (complex-double-float))
(define-simple-instruction (imagpart complex-long-float-imagpart) (long-float) (complex-long-float))

(define-specializer imagpart (number)
  (let ((ntype (wrapper-ntype number)))
    (with-constant-folding (imagpart (ntype number))
      (ntype-subtypecase ntype
        (complex-short-float (wrap (complex-short-float-imagpart number)))
        (complex-single-float (wrap (complex-single-float-imagpart number)))
        (complex-double-float (wrap (complex-double-float-imagpart number)))
        (complex-long-float (wrap (complex-long-float-imagpart number)))
        (real (wrap (* 0 number)))
        (t (wrap-default (ntype 'real)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONJUGATE

(define-specializer conjugate (number)
  (let ((ntype (wrapper-ntype number)))
    (if (and (eql-ntype-p number)
             (numberp number))
        (wrap-constant
         (conjugate ntype))
        (ntype-subtypecase ntype
          ((not number) (abort-specialization))
          (real (wrap number))
          (complex (wrap (complex (realpart number) (- (imagpart number)))))
          (t (wrap-default (ntype 'number)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PHASE

(define-specializer phase (number)
  (let ((ntype (wrapper-ntype number)))
    (if (and (eql-ntype-p ntype)
             (numberp ntype))
        (wrap-constant
         (phase ntype))
        (ntype-subtypecase ntype
          ((not number) (abort-specialization))
          ((float 0e0 *) (wrap (float 0 number)))
          ((rational 0 *) (wrap 0))
          ((float * (0e0)) (wrap (float #.pi number)))
          ((rational * (0)) (wrap-constant (coerce pi 'single-float)))
          ((complex float) (wrap-default (complex-part-ntype ntype)))
          (t (wrap-default (ntype 'number)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CIS

(define-simple-instruction (cis short-float-cis) (complex-short-float) (short-float))
(define-simple-instruction (cis single-float-cis) (complex-single-float) (single-float))
(define-simple-instruction (cis double-float-cis) (complex-double-float) (double-float))
(define-simple-instruction (cis long-float-cis) (complex-long-float) (long-float))

(define-specializer cis (x)
  (ntype-subtypecase (wrapper-ntype x)
    ((not real)
     (abort-specialization))
    (short-float
     (wrap
      (short-float-cis x)))
    (single-float
     (wrap
      (single-float-cis x)))
    (double-float
     (wrap
      (double-float-cis x)))
    (long-float
     (wrap
      (long-float-cis x)))
    (t
     (wrap-default
      (ntype 'complex)))))
