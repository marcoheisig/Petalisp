(in-package :common-lisp-user)

(defpackage #:petalisp.examples.linear-algebra
  (:use
   #:common-lisp
   #:petalisp)
  (:export
   #:coerce-to-matrix
   #:coerce-to-scalar
   #:zeros
   #:eye
   #:transpose
   #:norm
   #:dot
   #:asum
   #:max*
   #:matmul
   #:lu))

(in-package #:petalisp.examples.linear-algebra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Matrix Utilities

(defun coerce-to-matrix (x)
  (setf x (lazy-array x))
  (trivia:ematch (lazy-array-shape x)
    ((~* _ _)
     (lazy-reshape x (collapsing-reshaper 2)))
    ((~* range)
     (lazy-reshape x (~ (range-size range) ~ 1)))
    ((~*)
     (lazy-reshape x (~ 1 ~ 1)))))

(defun coerce-to-scalar (x)
  (setf x (lazy-array x))
  (trivia:ematch (lazy-array-shape x)
    ((~ i 1+i ~ j 1+j)
     (unless (and (= (1+ i) 1+i)
                  (= (1+ j) 1+j))
       (trivia.fail:fail))
     (lazy-reshape x (make-transformation :input-mask (vector i j) :output-rank 0)))
    ((~ i 1+i)
     (unless (= (1+ i) 1+i)
       (trivia.fail:fail))
     (lazy-reshape x (make-transformation :input-mask (vector i) :output-rank 0)))
    ((~*) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Linear Algebra Subroutines

(defun zeros (m &optional (n m))
  (lazy-reshape 0 (~ m ~ n)))

(declaim (inline δ))
(defun δ (i j)
  (declare (type integer i j))
  (if (= i j) 1 0))

(defun eye (m &optional (n m))
  (let ((shape (~ m ~ n)))
    (lazy #'δ
     (lazy-index-components shape 0)
     (lazy-index-components shape 1))))

(defun transpose (x)
  (lazy-reshape
   (coerce-to-matrix x)
   (transform m n to n m)))

(defun dot (x y)
  (coerce-to-scalar
   (matmul
    (transpose x)
    (coerce-to-matrix y))))

(defun norm (x)
  (lazy #'sqrt (dot x x)))

(defun asum (x)
  (coerce-to-scalar
   (lazy-reduce #'+ (lazy #'abs (coerce-to-matrix x)))))

(defun max* (x)
  (lazy-reduce
   (lambda (lv li rv ri)
     (if (> lv rv)
         (values lv li)
         (values rv ri)))
   x (lazy-index-components x)))

(defun matmul (A B)
  (lazy-reduce #'+
   (lazy #'*
    (lazy-reshape (coerce-to-matrix A) (transform m n to n m 0))
    (lazy-reshape (coerce-to-matrix B) (transform n k to n 0 k)))))

(defun pivot-and-value (A d)
  (setf A (coerce-to-matrix A))
  (trivia:ematch (lazy-array-shape A)
    ((~ m ~ n)
     (assert (= m n))
     (multiple-value-bind (v p)
         (max* (lazy #'abs (lazy-reshape A (~ d m ~ d (1+ d)))))
       (let ((p (coerce-to-scalar p))
             (v (coerce-to-scalar v)))
         (compute p v))))))

(defun swap-rows (A i j)
  (setf A (coerce-to-matrix A))
  (trivia:ematch (lazy-array-shape A)
    ((~ m ~ n)
     (assert (= m n))
     (assert (< -1 i m))
     (assert (< -1 j m))
     (if (= i j)
         A
         (let ((si (~ i (1+ i) ~ m))
               (sj (~ j (1+ j) ~ m)))
           (lazy-overwrite
            A
            (lazy-reshape A sj si)
            (lazy-reshape A si sj)))))))

(defun lu (A)
  (setf A (coerce-to-matrix A))
  (trivia:ematch (lazy-array-shape A)
    ((~ m ~ n)
     (assert (= m n))
     (labels
         ((rec (d P L U)
            (if (= (1+ d) m)
                (values (transpose P) L U)
                (multiple-value-bind (pivot value)
                    (pivot-and-value U d)
                  (assert (not (zerop value)))
                  (let* ((P (swap-rows P d pivot))
                         (L (swap-rows L d pivot))
                         (U (swap-rows U d pivot))
                         (S (lazy #'/ (lazy-reshape U (~ (1+ d) m ~ d (1+ d)))
                             (coerce-to-matrix value))))
                    (rec (1+ d)
                         P
                         (lazy-overwrite
                          L
                          S
                          (lazy-reshape 1 (~ d (1+ d) ~ d (1+ d))))
                         (lazy-overwrite
                          U
                          (lazy #'-
                           (lazy-reshape U (~ (1+ d) m ~ d m))
                           (lazy #'* S (lazy-reshape U (~ d (1+ d) ~ d m)))))))))))
       (rec 0 (eye m) (zeros m) A)))))
