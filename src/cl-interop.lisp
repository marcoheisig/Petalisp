;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Petalisp / Common Lisp interaction

(in-package :petalisp)

(defparameter *petalisp-operators*
  '(* + - /
    conjugate lcm gcd
    abs    cos    signum
    acos   cosh   sin
    acosh  exp    sinh
    asin   expt   sqrt
    asinh  isqrt  tan
    atan   log    tanh
    atanh  phase
    cis    pi
    /=  >=      oddp
    <   evenp   plusp
    <=  max     zerop
    =   min
    >   minusp
    ceiling          float-radix           rational
    complex          float-sign            rationalize
    decode-float
    floor            realpart
    denominator      fround                rem
    fceiling         ftruncate             round
    ffloor           imagpart              scale-float
    float            integer-decode-float  truncate
    float-digits     mod
    float-precision  numerator
    ))

(defun cl-to-petalisp (operation &rest args)
  (case operation
    (constant (match args ((list shape value) `(constant ,shape ,value))))
    (otherwise ''foo)))

(defmacro petalisp (&body body)
  (let ((macros (loop for op in *petalisp-operators* collect
                      `(,op (&rest args)
                            (apply #'cl-to-petalisp `,',op args)))))
    `(macrolet ,macros ,@body)))
