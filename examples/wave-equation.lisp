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
         (X (α #'/ (indices shape 0) (coerce n 'single-float)))
         (Y (α #'/ (indices shape 1) (coerce n 'single-float)))
         (Z (α #'*
               (α #'sin (α #'* 2 X pi))
               (α #'sin (α #'* 2 Y pi)))))
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
  (let ((interior (interior curr)))
    (fuse*
     curr
     (α #'+
        (α #'*
           (α #'+
              (reshape (α #'* curr (α #'+ W E) 0.5)
                       (τ (i j) ((1- i) j)) interior)
              (reshape (α #'* curr (α #'+ W E) 0.5)
                       (τ (i j) ((1+ i) j)) interior)
              (reshape (α #'* curr (α #'+ N S) 0.5)
                       (τ (i j) (i (1- j))) interior)
              (reshape (α #'* curr (α #'+ N S) 0.5)
                       (τ (i j) (i (1+ j))) interior)
              (reshape (α #'* curr (α #'- (α #'+ N E S W)) 1.0)
                       interior))
           (reshape (α #'/ (α #'* dt dt) (α #'* dx dx))
                    interior))
        (α #'- (reshape prev interior))
        (α #'* 2.0 (reshape curr interior))))))

(defun interior (array)
  (flet ((range-interior (range)
           (multiple-value-bind (start step end)
               (range-start-step-end range)
             (range (+ start step) step (- end step)))))
    (~l (mapcar #'range-interior (shape-ranges (shape array))))))

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
