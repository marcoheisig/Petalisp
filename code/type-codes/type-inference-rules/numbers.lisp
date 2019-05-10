;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Numeric Contagion

(defun slow-numeric-contagion (&rest type-codes)
  (labels ((initial-state ()
             (type-code-subtypecase (pop type-codes)
               ((not number) (abort-type-inference))
               (short-float (short-float-state))
               (single-float (single-float-state))
               (double-float (double-float-state))
               (long-float (long-float-state))
               (float (float-state))
               (integer (integer-state))
               (rational (rational-state))
               (real (real-state))
               ((complex short-float) (complex-short-float-state))
               ((complex single-float) (complex-single-float-state))
               ((complex double-float) (complex-double-float-state))
               ((complex long-float) (complex-long-float-state))
               (complex (complex-state))
               (t (number-state))))
           (short-float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'short-float)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (abort-type-inference))
                   (short-float (short-float-state))
                   (single-float (single-float-state))
                   (double-float (double-float-state))
                   (long-float (long-float-state))
                   (float (float-state))
                   (integer (short-float-state))
                   (rational (short-float-state))
                   (real (real-state))
                   ((complex short-float) (complex-short-float-state))
                   ((complex single-float) (complex-single-float-state))
                   ((complex double-float) (complex-double-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (single-float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'single-float)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (abort-type-inference))
                   (short-float (single-float-state))
                   (single-float (single-float-state))
                   (double-float (double-float-state))
                   (long-float (long-float-state))
                   (float (float-state))
                   (integer (single-float-state))
                   (rational (single-float-state))
                   (real (real-state))
                   ((complex short-float) (complex-single-float-state))
                   ((complex single-float) (complex-single-float-state))
                   ((complex double-float) (complex-double-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (double-float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'double-float)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (abort-type-inference))
                   (short-float (double-float-state))
                   (single-float (double-float-state))
                   (double-float (double-float-state))
                   (long-float (long-float-state))
                   (float (float-state))
                   (integer (double-float-state))
                   (rational (double-float-state))
                   (real (real-state))
                   ((complex short-float) (complex-double-float-state))
                   ((complex single-float) (complex-double-float-state))
                   ((complex double-float) (complex-double-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (long-float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'long-float)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (abort-type-inference))
                   (short-float (long-float-state))
                   (single-float (long-float-state))
                   (double-float (long-float-state))
                   (long-float (long-float-state))
                   (float (float-state))
                   (integer (long-float-state))
                   (rational (long-float-state))
                   (real (real-state))
                   ((complex short-float) (complex-long-float-state))
                   ((complex single-float) (complex-long-float-state))
                   ((complex double-float) (complex-long-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'float)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (abort-type-inference))
                   (short-float (float-state))
                   (single-float (float-state))
                   (double-float (float-state))
                   (long-float (float-state))
                   (float (float-state))
                   (integer (float-state))
                   (rational (float-state))
                   (real (real-state))
                   ((complex short-float) (complex-state))
                   ((complex single-float) (complex-state))
                   ((complex double-float) (complex-state))
                   ((complex long-float) (complex-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (integer-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'integer)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (abort-type-inference))
                   (short-float (short-float-state))
                   (single-float (single-float-state))
                   (double-float (double-float-state))
                   (long-float (long-float-state))
                   (float (float-state))
                   (integer (integer-state))
                   (rational (rational-state))
                   (real (real-state))
                   ((complex short-float) (complex-short-float-state))
                   ((complex single-float) (complex-single-float-state))
                   ((complex double-float) (complex-double-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (rational-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'rational)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (abort-type-inference))
                   (short-float (short-float-state))
                   (single-float (single-float-state))
                   (double-float (double-float-state))
                   (long-float (long-float-state))
                   (float (float-state))
                   (integer (rational-state))
                   (rational (rational-state))
                   (real (real-state))
                   ((complex short-float) (complex-short-float-state))
                   ((complex single-float) (complex-single-float-state))
                   ((complex double-float) (complex-double-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (real-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'real)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (abort-type-inference))
                   (short-float (real-state))
                   (single-float (real-state))
                   (double-float (real-state))
                   (long-float (real-state))
                   (float (real-state))
                   (integer (real-state))
                   (rational (real-state))
                   (real (real-state))
                   ((complex short-float) (complex-state))
                   ((complex single-float) (complex-state))
                   ((complex double-float) (complex-state))
                   ((complex long-float) (complex-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (complex-short-float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier '(complex short-float))
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (abort-type-inference))
                   (short-float (complex-short-float-state))
                   (single-float (complex-single-float-state))
                   (double-float (complex-double-float-state))
                   (long-float (complex-long-float-state))
                   (float (complex-state))
                   (integer (complex-short-float-state))
                   (rational (complex-short-float-state))
                   (real (complex-state))
                   ((complex short-float) (complex-short-float-state))
                   ((complex single-float) (complex-single-float-state))
                   ((complex double-float) (complex-double-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (complex-single-float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier '(complex single-float))
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (abort-type-inference))
                   (short-float (complex-single-float-state))
                   (single-float (complex-single-float-state))
                   (double-float (complex-double-float-state))
                   (long-float (complex-long-float-state))
                   (float (complex-state))
                   (integer (complex-single-float-state))
                   (rational (complex-single-float-state))
                   (real (complex-state))
                   ((complex short-float) (complex-single-float-state))
                   ((complex single-float) (complex-single-float-state))
                   ((complex double-float) (complex-double-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (complex-double-float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier '(complex double-float))
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (abort-type-inference))
                   (short-float (complex-double-float-state))
                   (single-float (complex-double-float-state))
                   (double-float (complex-double-float-state))
                   (long-float (complex-long-float-state))
                   (float (complex-state))
                   (integer (complex-double-float-state))
                   (rational (complex-double-float-state))
                   (real (complex-state))
                   ((complex short-float) (complex-double-float-state))
                   ((complex single-float) (complex-double-float-state))
                   ((complex double-float) (complex-double-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (complex-long-float-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier '(complex long-float))
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (abort-type-inference))
                   (short-float (complex-long-float-state))
                   (single-float (complex-long-float-state))
                   (double-float (complex-long-float-state))
                   (long-float (complex-long-float-state))
                   (float (complex-state))
                   (integer (complex-long-float-state))
                   (rational (complex-long-float-state))
                   (real (complex-state))
                   ((complex short-float) (complex-long-float-state))
                   ((complex single-float) (complex-long-float-state))
                   ((complex double-float) (complex-long-float-state))
                   ((complex long-float) (complex-long-float-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (complex-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'complex)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (abort-type-inference))
                   (float (complex-state))
                   (integer (complex-state))
                   (rational (complex-state))
                   (real (complex-state))
                   (complex (complex-state))
                   (t (number-state)))))
           (number-state ()
             (if (null type-codes)
                 (type-code-from-type-specifier 'number)
                 (type-code-subtypecase (pop type-codes)
                   ((not number) (abort-type-inference))
                   (t (number-state))))))
    (initial-state)))

(defun numeric-contagion (type-code-1 type-code-2)
  (with-type-code-caching (type-code-1 type-code-2)
    (with-type-inference-barrier
      (slow-numeric-contagion type-code-1 type-code-2))))

(defun complex-part-type-code (type-code)
  (type-code-subtypecase type-code
    ((not complex) (abort-type-inference))
    ((complex short-float) (type-code-from-type-specifier 'short-float))
    ((complex single-float) (type-code-from-type-specifier 'single-float))
    ((complex double-float) (type-code-from-type-specifier 'double-float))
    ((complex long-float) (type-code-from-type-specifier 'long-float))
    (t (type-code-from-type-specifier 'real))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Numbers Dictionary (CLHS 12.2)

(flet ((inference (numbers)
         (dolist (number numbers)
           (check-type-code number number))
         (type-code-from-type-specifier 't)))
  (define-type-inference-rule = (&rest numbers) (inference numbers))
  (define-type-inference-rule /= (&rest numbers) (inference numbers)))

(flet ((inference (numbers)
         (dolist (number numbers)
           (check-type-code number real))
         (type-code-from-type-specifier 't)))
  (define-type-inference-rule < (&rest numbers) (inference numbers))
  (define-type-inference-rule > (&rest numbers) (inference numbers))
  (define-type-inference-rule <= (&rest numbers) (inference numbers))
  (define-type-inference-rule >= (&rest numbers) (inference numbers)))

(define-type-inference-rule max (&rest reals)
  (dolist (real reals)
    (check-type-code real real))
  (reduce #'type-code-union reals))

(define-type-inference-rule min (&rest reals)
  (dolist (real reals)
    (check-type-code real real))
  (reduce #'type-code-union reals))

(define-type-inference-rule minusp (real)
  (check-type-code real real)
  (type-code-from-type-specifier 't))

(define-type-inference-rule plusp (real)
  (check-type-code real real)
  (type-code-from-type-specifier 't))

(define-type-inference-rule zerop (number)
  (check-type-code number number)
  (type-code-from-type-specifier 't))

(flet ((inference (number divisor)
         (check-type-code number real)
         (check-type-code divisor real)
         (values
          (type-code-from-type-specifier 'integer)
          (numeric-contagion number divisor))))
  (define-type-inference-rule floor (number &optional (divisor (type-code-of 1)))
    (inference number divisor))
  (define-type-inference-rule ceiling (number &optional (divisor (type-code-of 1)))
    (inference number divisor))
  (define-type-inference-rule truncate (number &optional (divisor (type-code-of 1)))
    (inference number divisor))
  (define-type-inference-rule round (number &optional (divisor (type-code-of 1)))
    (inference number divisor)))

(flet ((inference (number divisor)
         (check-type-code number real)
         (values
          (numeric-contagion
           number
           (type-code-subtypecase divisor
             ((not real) (abort-type-inference))
             (float divisor)
             ((not float) (type-code-from-type-specifier 'single-float))
             (t (type-code-from-type-specifier 'float))))
          (numeric-contagion number divisor))))
  (define-type-inference-rule ffloor (number &optional (divisor (type-code-of 1)))
    (inference number divisor))
  (define-type-inference-rule fceiling (number &optional (divisor (type-code-of 1)))
    (inference number divisor))
  (define-type-inference-rule ftruncate (number &optional (divisor (type-code-of 1)))
    (inference number divisor))
  (define-type-inference-rule fround (number &optional (divisor (type-code-of 1)))
    (inference number divisor)))

(flet ((inference (number)
         (type-code-subtypecase number
           ((not number) (abort-type-inference))
           ((complex float) number)
           (t (type-code-from-type-specifier 'number)))))
  (define-type-inference-rule sin (radians) (inference radians))
  (define-type-inference-rule cos (radians) (inference radians))
  (define-type-inference-rule tan (radians) (inference radians))
  (define-type-inference-rule asin (radians) (inference radians))
  (define-type-inference-rule acos (radians) (inference radians))
  (define-type-inference-rule sinh (radians) (inference radians))
  (define-type-inference-rule cosh (radians) (inference radians))
  (define-type-inference-rule tanh (radians) (inference radians))
  (define-type-inference-rule asinh (radians) (inference radians))
  (define-type-inference-rule acosh (radians) (inference radians))
  (define-type-inference-rule atanh (radians) (inference radians))
  (define-type-inference-rule atan (number-1 &optional (number-2 nil number-2-supplied-p))
    (if (not number-2-supplied-p)
        (inference number-1)
        (progn
          (check-type-code number-1 real)
          (check-type-code number-2 real)
          (inference (numeric-contagion number-1 number-2))))))

(define-type-inference-rule * (&rest type-codes)
  (if (null type-codes)
      (type-code-of 1)
      (reduce #'numeric-contagion type-codes)))

(define-type-inference-rule + (&rest type-codes)
  (if (null type-codes)
      (type-code-of 0)
      (reduce #'numeric-contagion type-codes)))

(define-type-inference-rule - (minuend &rest subtrahends)
  (if (null subtrahends)
      (type-code-subtypecase minuend
        ((not number) (abort-type-inference))
        (integer (type-code-from-type-specifier 'integer))
        (t minuend))
      (reduce #'numeric-contagion subtrahends :initial-value minuend)))

(define-type-inference-rule / (numerator &rest denominators)
  (let ((initial-type
          (type-code-subtypecase numerator
            ((not number) (abort-type-inference))
            (rational (type-code-from-type-specifier 'rational))
            ((or float (complex float)) numerator)
            (t (type-code-from-type-specifier 'number)))))
    (reduce #'numeric-contagion denominators :initial-value initial-type)))

(define-type-inference-rule 1+ (number)
  (values-type-codes #'+ number (type-code-of 1)))

(define-type-inference-rule 1- (number)
  (values-type-codes #'- number (type-code-of 1)))

(define-type-inference-rule abs (number)
  (type-code-subtypecase number
    ((not number) (abort-type-inference))
    (real number)
    (complex (complex-part-type-code number))
    (t (type-code-from-type-specifier 'real))))

(define-type-inference-rule evenp (integer)
  (check-type-code integer integer)
  (type-code-from-type-specifier 't))

(define-type-inference-rule oddp (integer)
  (check-type-code integer integer)
  (type-code-from-type-specifier 't))

(define-type-inference-rule exp (number)
  (type-code-subtypecase number
    ((not number) (abort-type-inference))
    ((or float (complex float)) number)
    (rational (type-code-from-type-specifier 'single-float))
    (t (type-code-from-type-specifier 'number))))

(define-type-inference-rule expt (base power)
  (check-type-code base number)
  (type-code-subtypecase power
    (integer
     (type-code-subtypecase base
       (integer (type-code-from-type-specifier 'integer))
       (rational (type-code-from-type-specifier 'rational))
       (t base)))
    (complex
     (numeric-contagion base power))
    (t
     (type-code-subtypecase base
       (complex base)
       (t (type-code-from-type-specifier 'number))))))

(define-type-inference-rule gcd (&rest integers)
  (dolist (integer integers)
    (check-type-code integer integer))
  (type-code-from-type-specifier '(integer 0 *)))

(define-type-inference-rule lcm (&rest integers)
  (dolist (integer integers)
    (check-type-code integer integer))
  (type-code-from-type-specifier '(integer 0 *)))

(define-type-inference-rule log
    (number &optional (base (type-code-subtypecase number
                              (float number)
                              (t (type-code-from-type-specifier 'single-float)))))
  (check-type-code number number)
  (check-type-code base number)
  ;; The trouble here is that LOG can return either a float, or a complex
  ;; float, depending on the sign of NUMBER.  Since type codes do not track
  ;; the sign, we cannot really perform any meaningful inference.
  (type-code-from-type-specifier 'number))

(define-type-inference-rule mod (number divisor)
  (nth-value 1 (values-type-codes #'floor number divisor)))

(define-type-inference-rule rem (number divisor)
  (nth-value 1 (values-type-codes #'truncate number divisor)))

(define-type-inference-rule signum (number)
  (type-code-subtypecase number
    ((not number) (abort-type-inference))
    (rational (type-code-from-type-specifier '(member -1 0 1)))
    ((or float (complex float)) number)
    (complex (type-code-from-type-specifier 'complex))
    (t (type-code-from-type-specifier 'number))))

(define-type-inference-rule sqrt (number)
  (type-code-subtypecase number
    ((not number) (abort-type-inference))
    (complex number)
    (t (type-code-from-type-specifier 'number))))

(define-type-inference-rule isqrt (natural)
  (check-type-code natural (integer 0 *))
  (type-code-from-type-specifier '(integer 0 *)))

(define-type-inference-rule random (limit &optional (random-state (type-code-from-type-specifier 'random-state)))
  (check-type-code random-state random-state)
  (type-code-subtypecase limit
    ((not (or integer float)) (abort-type-inference))
    (integer limit)
    (float limit)
    (t (type-code-from-type-specifier 'real))))

(define-predicate-type-inference-rule random-state-p random-state)

(define-predicate-type-inference-rule numberp number)

(define-predicate-type-inference-rule realp real)

(define-predicate-type-inference-rule rationalp rational)

(define-predicate-type-inference-rule integerp integer)

(define-predicate-type-inference-rule floatp float)

(define-predicate-type-inference-rule complexp complex)

(define-type-inference-rule cis (radians)
  (type-code-subtypecase radians
    ((not real) (abort-type-inference))
    (short-float (type-code-from-type-specifier '(complex short-float)))
    (single-float (type-code-from-type-specifier '(complex single-float)))
    (double-float (type-code-from-type-specifier '(complex double-float)))
    (long-float (type-code-from-type-specifier '(complex long-float)))
    (t (type-code-from-type-specifier 'complex))))

(define-type-inference-rule complex (realpart &optional (imagpart realpart))
  (type-code-subtypecase (numeric-contagion realpart imagpart)
    ((not real) (abort-type-inference))
    (short-float (type-code-from-type-specifier '(complex short-float)))
    (single-float (type-code-from-type-specifier '(complex single-float)))
    (double-float (type-code-from-type-specifier '(complex double-float)))
    (long-float (type-code-from-type-specifier '(complex long-float)))
    (t (type-code-from-type-specifier 'complex))))

(define-type-inference-rule conjugate (number)
  (type-code-subtypecase number
    ((not number) (abort-type-inference))
    ((or real complex) number)
    (t (type-code-from-type-specifier 'number))))

(define-type-inference-rule phase (number)
  (type-code-subtypecase number
    ((not number) (abort-type-inference))
    (float number)
    ((or rational (complex rational)) (type-code-from-type-specifier 'single-float))
    ((complex float) (complex-part-type-code number))
    (t (type-code-from-type-specifier 'number))))

(define-type-inference-rule realpart (number)
  (type-code-subtypecase number
    ((not number) (abort-type-inference))
    (real number)
    (complex (complex-part-type-code number))
    (t (type-code-from-type-specifier 'real))))

(define-type-inference-rule imagpart (number)
  (type-code-subtypecase number
    ((not number) (abort-type-inference))
    (real (numeric-contagion (type-code-of 0) number))
    (complex (complex-part-type-code number))
    (t (type-code-from-type-specifier 'real))))

(define-type-inference-rule numerator (rational)
  (check-type-code rational rational)
  (type-code-from-type-specifier 'integer))

(define-type-inference-rule denominator (rational)
  (check-type-code rational rational)
  (type-code-from-type-specifier '(integer 1 *)))

(define-type-inference-rule rational (real)
  (check-type-code real real)
  (type-code-from-type-specifier 'rational))

(define-type-inference-rule rationalize (real)
  (check-type-code real real)
  (type-code-from-type-specifier 'rational))

(define-type-inference-rule ash (integer count)
  (check-type-code integer integer)
  (check-type-code count integer)
  (type-code-from-type-specifier 'integer))

(define-type-inference-rule integer-length (integer)
  (check-type-code integer integer)
  (type-code-from-type-specifier '(integer 0 *)))

(define-type-inference-rule boole (op integer-1 integer-2)
  (check-type-code op t)
  (check-type-code integer-1 integer)
  (check-type-code integer-2 integer)
  (type-code-from-type-specifier 'integer))

(flet ((inference (&rest integers)
         ;; TODO use a more fine-grained analysis that is also able to
         ;; distinguish integer subtypes such as (unsigned-byte 8).
         (dolist (integer integers)
           (check-type-code integer integer))
         (type-code-from-type-specifier 'integer)))
  (define-type-inference-rule logand (&rest integers) (apply #'inference integers))
  (define-type-inference-rule logandc1 (i1 i2) (inference i1 i2))
  (define-type-inference-rule logandc2 (i1 i2) (inference i1 i2))
  (define-type-inference-rule logeqv (&rest integers) (apply #'inference integers))
  (define-type-inference-rule logior (&rest integers) (apply #'inference integers))
  (define-type-inference-rule lognand (i1 i2) (inference i1 i2))
  (define-type-inference-rule lognor (i1 i2) (inference i1 i2))
  (define-type-inference-rule lognot (integer) (inference integer))
  (define-type-inference-rule logorc1 (i1 i2) (inference i1 i2))
  (define-type-inference-rule logorc2 (i1 i2) (inference i1 i2))
  (define-type-inference-rule logxor (&rest integers) (apply #'inference integers)))

(define-type-inference-rule logbitp (index integer)
  (check-type-code index (integer 0 *))
  (check-type-code integer integer)
  (type-code-from-type-specifier 't))

(define-type-inference-rule logcount (integer)
  (check-type-code integer integer)
  (type-code-from-type-specifier '(integer 0 *)))

(define-type-inference-rule logtest (integer-1 integer-2)
  (check-type-code integer-1 integer)
  (check-type-code integer-2 integer)
  (type-code-from-type-specifier 't))

;; The representation of byte specifiers is implementation-dependent.
;; However, under the assumption that each implementation consistently uses
;; a uniform representation, we still might be able to infer something.
(deftype byte-specifier ()
  `(or ,(type-of (byte 0 0))
       ,(type-of (byte 16 253))))

(define-type-inference-rule byte (size position)
  (check-type-code size (integer 0 *))
  (check-type-code position (integer 0 *))
  (type-code-from-type-specifier 'byte-specifier))

(define-type-inference-rule byte-size (bytespec)
  (check-type-code bytespec byte-specifier)
  (type-code-from-type-specifier '(integer 0 *)))

(define-type-inference-rule byte-position (bytespec)
  (check-type-code bytespec byte-specifier)
  (type-code-from-type-specifier '(integer 0 *)))

(define-type-inference-rule dbp (newbyte bytespec integer)
  (check-type-code newbyte integer)
  (check-type-code bytespec byte-specifier)
  (check-type-code integer integer)
  (type-code-from-type-specifier 'integer))

(define-type-inference-rule ldb (bytespec integer)
  (check-type-code bytespec byte-specifier)
  (check-type-code integer integer)
  (type-code-from-type-specifier '(integer 0 *)))

(define-type-inference-rule deposit-field (newbyte bytespec integer)
  (check-type-code newbyte integer)
  (check-type-code bytespec byte-specifier)
  (check-type-code integer integer)
  (type-code-from-type-specifier 'integer))

(define-type-inference-rule mask-field (bytespec integer)
  (check-type-code bytespec byte-specifier)
  (check-type-code integer integer)
  (type-code-from-type-specifier '(integer 0 *)))

(define-type-inference-rule decode-float (float)
  (type-code-subtypecase float
    ((not float) (abort-type-inference))
    (float (values
            float
            (type-code-from-type-specifier 'integer)
            float))
    (t (values
        (type-code-from-type-specifier 'float)
        (type-code-from-type-specifier 'integer)
        (type-code-from-type-specifier 'float)))))

(define-type-inference-rule scale-float (float integer)
  (type-code-subtypecase float
    ((not float) (abort-type-inference))
    (float float)
    (t (type-code-from-type-specifier 'float))))

(define-type-inference-rule float-radix (float)
  (type-code-from-type-specifier 'integer))

(define-type-inference-rule float-sign (float-1 &optional (float-2 float-1))
  (check-type-code float-1 float)
  (type-code-subtypecase float-2
    ((not float) (abort-type-inference))
    (float float)
    (t (type-code-from-type-specifier 'float))))

(define-type-inference-rule float-digits (float)
  (check-type-code float float)
  (type-code-from-type-specifier '(integer 0 *)))

(define-type-inference-rule float-precision (float)
  (check-type-code float float)
  (type-code-from-type-specifier '(integer 0 *)))

(define-type-inference-rule integer-decode-float (float)
  (check-type-code float float)
  (values (type-code-from-type-specifier 'integer)
          (type-code-from-type-specifier 'integer)
          (type-code-from-type-specifier 'integer)))

(define-type-inference-rule float
    (number &optional (prototype nil prototype-supplied-p))
  (check-type-code number real)
  (if (not prototype-supplied-p)
      (type-code-subtypecase number
        (float number)
        ((not float) (type-code-from-type-specifier 'single-float))
        (t (type-code-from-type-specifier 'float)))
      (type-code-subtypecase prototype
        ((not float) (abort-type-inference))
        (float prototype)
        (t (type-code-from-type-specifier 'float)))))

(define-type-inference-rule float-sign (type-code-1 &optional (type-code-2 type-code-1))
  (check-type-code type-code-1 float)
  (check-type-code type-code-2 float)
  type-code-2)
