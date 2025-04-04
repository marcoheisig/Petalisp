;;;; A simple APL implementation in Petalisp.

(in-package :common-lisp-user)

(defpackage #:petalisp.examples.apl
  (:export
   ;; Primitive Functions (https://help.dyalog.com/latest/#Language/Primitive%20Functions/Primitive%20Functions%20Table.htm)
   #:+ #:^ #:! #:⌈ #:○ #:? #:⊥ #:L #:× #:÷
   #:↓ #:⊂ #:⊤ #:= #:~ #:⍀ #:* #:⍷ #:⌊ #:⍕
   #:⍒ #:⍋ #:≥ #:⊢ #:⍳ #:⌷ #:∩ #:⍸ #:⊣ #:≤
   #:⍟ #:\| #:≡ #:⌹ #:∊ #:- #:⍲ #:⊆ #:⍱ #:≠
   #:≢ #:∨ #:⊃ #:\, #:⍴ #:⍪ #:↑ #:⍉ #:∪ #:^
   #:⌽
   ;; Primitive Operators (https://help.dyalog.com/latest/#Language/Primitive%20Operators/Primitive%20Operators%20Table.htm)
   #:[]← #:← #:← #:@ #:⍤ #:[] #:∘ #:⍨ #:¨ #:⌶
   #:. #:⌸ #:∘. #:⍥ #:⍣ #:⍤ #:⌿ #:/ #:⍀ #:\\
   #:& #:⌺ #:⍠))

(defpackage #:petalisp.examples.apldef
  (:use #:common-lisp #:petalisp)
  (:local-nicknames (#:apl #:petalisp.examples.apl))
  (:export #:define-apl-function))

(in-package #:petalisp.examples.apldef)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Primitive Functions

(defvar *scalar-dyadic-functions* (make-hash-table :test #'eq)
  "A hash-table from dyadic function names to the corresponding scalar function.")

(defun scalar-dyadic-function (apl-function-name)
  (or (gethash apl-function-name *scalar-dyadic-functions*)
      (error "Not a scalar dyadic function: ~S" apl-function-name)))

(defmacro define-apl-function
    (name &key monadic monadic-non-scalar dyadic dyadic-non-scalar)
  (assert (not (and monadic monadic-non-scalar)))
  (assert (not (and dyadic dyadic-non-scalar)))
  (let ((monadic-form
          (cond (monadic `(lazy ',monadic x))
                (monadic-non-scalar `(,monadic-non-scalar x))))
        (dyadic-form
          (cond (dyadic `(lazy ',dyadic x y))
                (dyadic-non-scalar `(,dyadic-non-scalar x y)))))
    `(progn
       ,(cond ((and monadic-form dyadic-form)
               `(defun ,name (x &optional (y nil y-supplied-p))
                  (if (not y-supplied-p) ,monadic-form ,dyadic-form)))
              ((and monadic-form (not dyadic-form))
               `(defun ,name (x) ,monadic-form))
              ((and (not monadic-form) dyadic-form)
               `(defun ,name (x y) ,dyadic-form))
              (t
               (error "APL function must have a monadic or dyadic variant.")))
       ,@(when dyadic
           `((setf (gethash ',name *scalar-dyadic-functions*)
                   ',dyadic))))))

(defun todo (&rest args)
  (declare (ignore args))
  (error "Not yet implemented."))

(define-apl-function apl:+
  ;; https://aplwiki.com/wiki/Identity
  :monadic conjugate
  ;; https://aplwiki.com/wiki/Plus
  :dyadic +)

(define-apl-function apl:^
  ;; https://aplwiki.com/wiki/And
  :dyadic lcm)

(define-apl-function !
  ;; https://aplwiki.com/wiki/Factorial
  :monadic alexandria:factorial
  ;; https://aplwiki.com/wiki/Binomial_coefficient
  :dyadic alexandria:binomial-coefficient)

(define-apl-function apl:⌈
  ;; https://aplwiki.com/wiki/Ceiling
  :monadic ceiling
  ;; https://aplwiki.com/wiki/Maximum
  :dyadic max)

(define-apl-function apl:○
  ;; https://aplwiki.com/wiki/Pi_times
  :monadic apl-pi-times
  ;; https://aplwiki.com/wiki/Circular
  :dyadic apl-circle)

(defun apl-pi-times (x)
  (* x (float pi x)))

(defun apl-circle (x y)
  (declare (integer x) (number y))
  (ecase x
    (-12 (exp (* y #C(0 1))))
    (-11 (* y #C(0 1)))
    (-10 (conjugate y))
    (-9 (identity y))
    (-8 ( - (sqrt (- -1 (* y y)))))
    (-7 (atanh y))
    (-6 (acosh y))
    (-5 (asinh y))
    (-4 (sqrt (- (* y y) 1)))
    (-3 (atan y))
    (-2 (acos y))
    (-1 (asin y))
    (+0 (sqrt (- 1 (* y y))))
    (+1 (sin y))
    (+2 (cos y))
    (+3 (tan y))
    (+4 (sqrt (+ 1 (* y y))))
    (+5 (sinh y))
    (+6 (cosh y))
    (+7 (tanh y))
    (+8 (sqrt (- -1 (* y y))))
    (+9 (realpart y))
    (+10 (abs y))
    (+11 (imagpart y))
    (+12 (phase y))))

(define-apl-function apl:?
  ;; https://aplwiki.com/wiki/Roll
  :monadic random
  ;; https://aplwiki.com/wiki/Deal
  :dyadic-non-scalar apl-deal)

(defun apl-deal (k n)
  (with-lazy-arrays (k n)
    (let ((k (compute k)))
      (unless (integerp k)
        (error "k must be an integer."))
      (lazy 'typo:the-integer
       (lazy 'aref
        (lazy-rearrange (lazy 'random-k-permutation k n) 0 (~ 1))
        (lazy-index-components (~ k)))))))

(defun random-k-permutation (k n)
  "Returns a random k-permutation of integers from 0 to n-1."
  (cond
    ((< n k) (error "n must be greater than or equal to k."))
    ((< k 0) (error "k must be non-negative."))
    (t
     (let ((nums (make-array n :initial-contents (loop for i from 0 below n collect i)))
           (result (make-array k)))
       ;; Fisher-Yates shuffle, but only for k elements
       (dotimes (i k)
         (let* ((j (+ i (random (- n i))))
                (temp (aref nums i)))
           (setf (aref nums i) (aref nums j)
                 (aref nums j) temp
                 (aref result i) (aref nums i))))
       result))))

(define-apl-function apl:⊥
  ;; https://aplwiki.com/wiki/Decode
  :dyadic-non-scalar todo)

#+(or) (define-apl-function apl:L)

(define-apl-function apl:×
  ;; https://aplwiki.com/wiki/Signum
  :monadic signum
  ;; https://aplwiki.com/wiki/Times
  :dyadic *)

(define-apl-function apl:÷
  ;; https://aplwiki.com/wiki/Reciprocal
  :monadic /
  ;; https://aplwiki.com/wiki/Divide
  :dyadic /)

(define-apl-function apl:↓
  ;; https://aplwiki.com/wiki/Drop
  :dyadic-non-scalar todo)

(define-apl-function apl:⊂
  ;; https://aplwiki.com/wiki/Enclose
  :monadic-non-scalar todo)

(define-apl-function apl:⊤
  ;; https://aplwiki.com/wiki/Encode
  :dyadic-non-scalar todo)

(define-apl-function apl:=
  ;; https://aplwiki.com/wiki/Equal_to
  :dyadic apl=)

(defun apl= (x y)
  (if (= x y) 1 0))

(define-apl-function apl:~
  ;; https://aplwiki.com/wiki/Not
  :monadic lognot
  ;; https://aplwiki.com/wiki/Without
  :dyadic-non-scalar todo)

#+(or) (define-apl-function apl:⍀ )

(define-apl-function apl:*
  ;; https://aplwiki.com/wiki/Exponential
  :monadic exp
  ;; https://aplwiki.com/wiki/Power
  :dyadic expt)

#+(or) (define-apl-function apl:⍷ )

(define-apl-function apl:⌊
  ;; https://aplwiki.com/wiki/Floor
  :monadic floor
  ;; https://aplwiki.com/wiki/Minimum
  :dyadic min)

#+(or) (define-apl-function apl:⍕ )

#+(or) (define-apl-function apl:⍒ )

#+(or) (define-apl-function apl:⍋ )

#+(or) (define-apl-function apl:≥ )

#+(or) (define-apl-function apl:⊢ )

(define-apl-function apl:⍳
  ;; https://aplwiki.com/wiki/Index_Generator
  :monadic-non-scalar apl-iota
  ;; https://aplwiki.com/wiki/Index_Of
  :dyadic-non-scalar todo)

(defun apl-iota (n)
  (with-lazy-arrays (n)
    (let* ((shape (coerce-array-to-shape n))
           (rank (shape-rank shape)))
      (case rank
        ((0 1)
         (lazy-index-components shape))
        (otherwise
         (apply 'lazy 'vector
                 (loop for axis below rank collect (lazy-index-components shape axis))))))))

#+(or) (define-apl-function apl:⌷ )

#+(or) (define-apl-function apl:∩ )

#+(or) (define-apl-function apl:⍸ )

#+(or) (define-apl-function apl:⊣ )

#+(or) (define-apl-function apl:≤ )

(define-apl-function apl:⍟
  ;; https://aplwiki.com/wiki/Natural_logarithm
  :monadic ln
  ;; https://aplwiki.com/wiki/Logarithm
  :dyadic log)

(define-apl-function apl:\|
  ;; https://aplwiki.com/wiki/Magnitude
  :monadic abs
  ;; https://aplwiki.com/wiki/Residue
  :dyadic rem)

#+(or) (define-apl-function apl:≡ )
#+(or) (define-apl-function apl:⌹ )
#+(or) (define-apl-function apl:∊ )

(define-apl-function apl:-
  ;; https://aplwiki.com/wiki/Negate
  :monadic -
  ;; https://aplwiki.com/wiki/Subtract
  :dyadic -)

#+(or) (define-apl-function apl:⍲ )

#+(or) (define-apl-function apl:⊆ )

#+(or) (define-apl-function apl:⍱ )

#+(or) (define-apl-function apl:≠ )

#+(or) (define-apl-function apl:≢ )

#+(or) (define-apl-function apl:∨ )

#+(or) (define-apl-function apl:⊃ )

#+(or) (define-apl-function apl:\, )

(define-apl-function apl:⍴
  ;; https://aplwiki.com/wiki/Shape
  :monadic-non-scalar apl-shape
  ;; https://aplwiki.com/wiki/Reshape
  :dyadic-non-scalar apl-reshape)

(defun apl-shape (x)
  (with-lazy-arrays (x)
    (let* ((shape (lazy-array-shape x))
           (rank (shape-rank shape))
           (result (make-array rank :element-type '(and (unsigned-byte 64) fixnum))))
      (dotimes (axis rank result)
        (setf (aref result axis)
              (shape-dimension shape axis))))))

(defun apl-reshape (x y)
  (with-lazy-arrays (x y)
    (let* ((s (coerce-array-to-shape x))
           (n (shape-size s))
           (k (lazy-array-size y))
           (x (lazy-rearrange y (lazy-array-rank y) (~ k))))
      (multiple-value-bind (q r) (floor n k)
        (lazy-rearrange
         (if (< q k)
             (lazy-stack
              (append (make-list q :initial-element x)
                      (list (lazy-reshape x (~ r)))))
             (apply #'lazy-fuse
                    (loop for i below k
                          collect
                          (lazy-reshape x (~ i (1+ i)) (~ i n k)))))
         1 s)))))

(defun coerce-array-to-shape (array)
  "Returns a shape whose extent in each axis is the corresponding element of the
supplied array."
  (with-lazy-arrays (array)
    (case (lazy-array-rank array)
      (0 (~ (compute array)))
      (1 (~* (map 'list #'range (compute array))))
      (otherwise
       (error "Can only interpret vectors as shape, got array of rank ~D."
              (lazy-array-rank array))))))

#+(or) (define-apl-function apl:⍪ )

#+(or) (define-apl-function apl:↑ )

#+(or) (define-apl-function apl:⍉ )

#+(or) (define-apl-function apl:∪ )

#+(or) (define-apl-function apl:^ )

(define-apl-function apl:⌽
  :dyadic-non-scalar apl-rotate)

(defun apl-rotate (array amount &optional (axis 0))
  (petalisp:with-lazy-arrays (array)
    (let* ((shape (lazy-array-shape array))
           (rank (shape-rank shape))
           (range (shape-range shape axis))
           (start (range-start range))
           (size (range-size range))
           (step (range-step range))
           (shift (mod (if (minusp amount)
                          (* (+ size amount) step)
                          (* amount step))
                       size))
           (position (+ start shift))
           (offsets (let ((v (make-array rank :initial-element 0)))
                      (setf (aref v axis) (- shift)) v))
           (transformation (make-transformation :offsets offsets)))
      (multiple-value-bind (lo hi)
          (split-shape shape axis position)
        (lazy-stack
         (list
          (lazy-reshape array hi transformation)
          (lazy-reshape array lo transformation))
         :axis axis)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Primitive Operators

(defun apl:. (f g)
  (lambda (x y)
     (lazy-reduce (scalar-dyadic-function f)
      (lazy (scalar-dyadic-function g) x y))))
