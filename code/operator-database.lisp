;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Petalisp type system

(defparameter petalisp-types
  '(single-float
    double-float
    (complex single-float)
    (complex double-float)
    t))

(defun petalisp-type (type)
  "Given a Lisp type, return its corresponding petalisp type."
  (find type petalisp-types :test #'subtypep))

#+nil
(defun numeric-supertype (a b)
  (let ((complex?
          (or
           (subtypep a 'complex)
           (subtypep b 'complex)))))
  ;; f32 f32 -> f32
  ;; c32 f64 -> c64
  ;; f32 f64 -> f64
  )

#+nil
(defmethod result-type ((f (eql #'+)) &rest arguments)
  (reduce #'numeric-supertype )
  'double-float)
