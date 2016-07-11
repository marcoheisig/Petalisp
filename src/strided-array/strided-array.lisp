;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(deftype strided-array-index () 'list)

(defclass strided-array (total-function)
  ((%ranges :initarg :ranges :reader ranges)
   (%domain-type :initform 'strided-array-index :allocation :class)))

(defclass strided-array-index-space
    (strided-array index-space)
  ((%codomain-type :initform 'strided-array-index :allocation :class)))

(defclass strided-array-constant
    (strided-array constant)
  ())

(defclass strided-array-index-space-transformation
    (strided-array affine-index-space-transformation)
  ((%codomain-type :initform 'strided-array-index :allocation :class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Working with ranges

(defstruct (range (:constructor %make-range (start step end)))
  (start 0 :type fixnum :read-only t)
  (step 1 :type fixnum :read-only t)
  (end 0 :type fixnum :read-only t))

(defun range (&rest spec)
  (multiple-value-bind (start step end)
      (ematch spec
        ((list start step end) (values start step end))
        ((list start end) (values start 1 end))
        ((list end) (values 1 1 end)))
    (assert (not (and (zerop step) (/= start end))))
    ;; ensure that STEP is positive
    (when (minusp step) (setf step (- step)))
    ;; normalize step
    (when (= start end) (setf step 1))
    ;; ensure START and END are congruent relative to STEP
    (setf end (+ start (* step (truncate (- end start) step))))
    ;; ensure START is bigger than END
    (when (> start end) (rotatef start end))
    (%make-range start step end)))
