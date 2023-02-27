;;;; Note: This file is not intended to be loaded directly from Lisp, but
;;;; to be executed expression by expression.  If you are using Emacs, you
;;;; can simply press C-c C-c while your cursor is hovering over an
;;;; expression.

;;; Load Petalisp and run its test suite:
(asdf:test-system :petalisp)

;;; Define and use a package for all the remaining examples:
(defpackage #:petalisp.examples.getting-started
  (:use #:common-lisp #:petalisp))

(in-package #:petalisp.examples.getting-started)

;;; A function that prints both inputs and outputs:
(defun present (&rest arrays)
  (format t "~{~& => ~A~}" (compute-list-of-arrays arrays)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lazy Map

(present
 (lazy-reshape 0 (~*))) ; the empty space

(defun zeros (shape)
  (lazy-reshape 0 shape))

(present
 (zeros (~ 10))) ; ten zeros

(present
 (lazy-indices (zeros (~ 10)))) ; the numbers from 0 to 9 (inclusive)

(present
 (lazy-reshape #2a((1 2 3 4) (5 6 7 8)) (~ 0 2 ~ 1 3))) ; selecting values

(present
 (lazy-reshape #2a((1 2 3 4) (5 6 7 8))
               (transform i j to j i))) ; transforming

;; arrays can be merged with fuse

(present
 (lazy-fuse
  (lazy-reshape 5 (~ 0 3))
  (lazy-reshape 1 (~ 3 6))))

;; arrays can be overwritten with lazy-overwrite

(present
 (lazy-overwrite
  (zeros (~ 10 ~ 10))
  (lazy-reshape 1 (~ 2 8 ~ 2 8))))

;; lazy arrays permit beautiful functional abstractions

(defun chessboard (h w)
  (lazy-fuse
   (lazy-reshape 0 (~ 0 h 2 ~ 0 w 2))
   (lazy-reshape 0 (~ 1 h 2 ~ 1 w 2))
   (lazy-reshape 1 (~ 0 h 2 ~ 1 w 2))
   (lazy-reshape 1 (~ 1 h 2 ~ 0 w 2))))

(present
 (chessboard 8 8))

;; lazy applies a Lisp function element-wise

(present
 (lazy #'+ #(1 2 3) #(1 1 1)))

(present
 (lazy #'* 2 3)) ; scalar are treated as rank zero arrays

(present
 (lazy #'* 2 #(1 2 3))) ; lazy broadcasts automatically

;; lazy-reduce reduces array elements

(present
 (lazy-reduce #'+ #(1 2 3 4 5 6 7 8 9 10)))

(present
 (lazy-reduce #'+
    ;; only the axis zero is reduced
    #2A((1 2 3) (4 5 6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Matrix multiplication

(defun matmul (A B)
  (lazy-reduce #'+
     (lazy #'*
        (lazy-reshape A (transform m n to n m 0))
        (lazy-reshape B (transform n k to n 0 k)))))

(defparameter MI #2a((1.0 0.0)
                     (0.0 1.0)))

(defparameter MA #2a((2.0 3.0)
                     (4.0 5.0)))

(present (matmul MI MI))

(present (matmul MI MA))

(present (matmul MA MA))

(present
 (matmul (lazy-reshape 3.0 (~ 4 ~ 8))
         (lazy-reshape 2.0 (~ 8 ~ 4))))

(defparameter M (lazy-reshape #(1 2 3 4 5 6) (~ 6 ~ 6)))

(present M)

(present
 (matmul M (lazy-reshape M (transform m n to n m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Jacobi's Method

(defun jacobi-2d (grid)
  (let ((interior (lazy-array-interior grid)))
    (lazy-overwrite
     grid
     (lazy #'* 1/4
           (lazy #'+
                 (lazy-reshape grid (transform i j to (1+ i) j) interior)
                 (lazy-reshape grid (transform i j to (1- i) j) interior)
                 (lazy-reshape grid (transform i j to i (1+ j)) interior)
                 (lazy-reshape grid (transform i j to i (1- j)) interior))))))

(defparameter domain
  (lazy-overwrite
   (lazy-reshape 1.0 (~ 10 ~ 10))
   (lazy-reshape 0.0 (~ 1 9 ~ 1 9))))

(present domain)

(present
 (jacobi-2d domain))

(present
 (jacobi-2d
  (jacobi-2d domain)))

;;; Finally, let's have a glimpse at the Petalisp data flow representation
;;; of Jacobi's algorithm and the corresponding IR.

(petalisp.graphviz:view
 (list
  (jacobi-2d
   (jacobi-2d domain))))

(petalisp.graphviz:view
 (petalisp.ir:ir-from-lazy-arrays
  (list
   (jacobi-2d
    (jacobi-2d domain)))))
