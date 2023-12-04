(in-package :common-lisp-user)

(defpackage #:petalisp.examples.wave-equation
  (:use
   #:common-lisp
   #:petalisp)
  (:export
   #:solve-wave-equation))

(in-package #:petalisp.examples.wave-equation)

;;; In this example, we will solve the 2D wave equation on a unit square,
;;; using a simple finite difference approximation and an explicit Euler
;;; time stepping scheme.

(defun solve-wave-equation (&key (n 20) (timesteps 0) (dt 0.01) (verbose nil))
  (let* ((dx (/ (1- n)))
         (shape (~ 0 (1- n) ~ 0 (1- n)))
         (X (lazy #'/ (lazy-index-components shape 0) (coerce n 'single-float)))
         (Y (lazy #'/ (lazy-index-components shape 1) (coerce n 'single-float)))
         (Z (lazy #'*
             (lazy #'sin (lazy #'* 2 X pi))
             (lazy #'sin (lazy #'* 2 Y pi)))))
    (let ((prev Z) (curr Z))
      (when verbose (print-domain curr *trace-output*))
      (loop repeat timesteps do
        (shiftf
         prev
         curr
         (simulate-one-step prev curr dx dt 1.0 1.0 1.0 1.0))
        (when verbose (print-domain curr *trace-output*)))
      curr)))

(defun simulate-one-step (prev curr dx dt N E S W)
  (let ((interior (lazy-reshape curr (peeler 1 1))))
    (lazy-overwrite
     curr
     (lazy #'+
      (lazy #'*
       (lazy #'+
        (lazy-reshape (lazy #'* curr (lazy #'+ W E) 0.5)
         (transform i j to (1- i) j) interior)
        (lazy-reshape (lazy #'* curr (lazy #'+ W E) 0.5)
         (transform i j to (1+ i) j) interior)
        (lazy-reshape (lazy #'* curr (lazy #'+ N S) 0.5)
         (transform i j to i (1- j)) interior)
        (lazy-reshape (lazy #'* curr (lazy #'+ N S) 0.5)
         (transform i j to i (1+ j)) interior)
        (lazy-reshape (lazy #'* curr (lazy #'- (lazy #'+ N E S W)) 1.0)
         interior))
       (lazy-reshape (lazy #'/ (lazy #'* dt dt) (lazy #'* dx dx))
        interior))
      (lazy #'- (lazy-reshape prev interior))
      (lazy #'* 2.0 (lazy-reshape curr interior))))))

(defun print-domain (domain stream)
  (setf domain (compute domain))
  (fresh-line stream)
  (loop for i below (array-dimension domain 0) do
    (loop for j below (array-dimension domain 1) do
      (write-char (scalar-char (aref domain i j)) stream))
    (terpri stream))
  (terpri stream)
  (finish-output stream)
  domain)

(defun scalar-char (scalar)
  (let* ((array " oO#Xx.")
         (index (floor (* (+ scalar 1.0) 0.5 (length array)))))
    (if (array-in-bounds-p array index)
        (aref array index)
        #\?)))
