;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :cl-user)

(defpackage :petalisp
  (:use #:cl :optima)
  (:export petalisp))

(in-package :petalisp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *petalisp-operators*
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
      float-precision  numerator)))

(defpackage :petalisp-operators
  (:use #:cl #:optima)
  #.`(:shadow . ,*petalisp-operators*))
