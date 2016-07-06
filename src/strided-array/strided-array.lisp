;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The class of strided arrays

(in-package :petalisp)

(defclass strided-array (mapping)
  ((%ranges :initarg :ranges :reader ranges)))

(defclass strided-array-index-space
    (strided-array index-space)
  ())

(defclass strided-array-constant
    (strided-array constant)
  ())

(defclass strided-array-lisp-input
    (strided-array lisp-input)
  ())

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
    (when (zerop step) (setf step 1))
    ;; ensure START and END are congruent relative to STEP
    (setf end (+ start (* step (truncate (- end start) step))))
    ;; ensure START is bigger than END
    (when (> start end) (rotatef start end))
    (%make-range start step end)))

(defun range-elements (range)
  (1+ (the integer (/ (- (range-end range)
                         (range-start range))
                      (range-step range)))))

(defun unary-range-p (range)
  (= (range-start range) (range-end range)))

(defmethod generic-equalp ((object-1 strided-array-index-space)
                           (object-2 strided-array-index-space))
  (and (= (dimension object-1) (dimension object-2))
       (every #'equalp
              (ranges object-1)
              (ranges object-2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Working with index-spaces

;; TODO make this a #i... reader macro
(defmacro x (&rest specs)
  `(make-instance
    'strided-array-index-space
    :ranges
    ,@(loop for spec in specs
            collect
            (if (atom spec)
                `(range ,spec)
                `(range ,@spec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; strided array input objects

(defclass strided-array-constant (lisp-input) ()
  (:documentation
   "A Petalisp object that returns the same value for all indices."))

(defmethod generic-input ((object t) &rest arguments)
  (declare (ignore arguments))
  (make-instance
   'strided-array-constant
   :lisp-object object))
