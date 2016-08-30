;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array (structured-operand) (ranges index-space))

(define-class strided-array-index-space (strided-array index-space) ())

(defmethod dimension ((object strided-array))
  (length (ranges object)))

(defmethod size ((object strided-array))
  (reduce #'* (ranges object) :key #'size))

(defmethod equalp ((object-1 strided-array-index-space)
                   (object-2 strided-array-index-space))
  (and (= (dimension object-1) (dimension object-2))
       (every #'equalp
              (ranges object-1)
              (ranges object-2))))

(defmethod initialize-instance :after ((object strided-array)
                                       &key &allow-other-keys)
  (if (typep object 'strided-array-index-space)
      (setf (slot-value object 'index-space) object)
      (setf (slot-value object 'index-space)
            (make-instance
             'strided-array-index-space
             :ranges (ranges object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Working with ranges

(defstruct (range (:constructor %make-range (start step end)))
  (start 0 :type integer :read-only t)
  (step 1 :type integer :read-only t)
  (end 0 :type integer :read-only t))

(defun range (&rest spec)
  (multiple-value-bind (start step end)
      (ematch spec
        ((list start step end) (values start step end))
        ((list start end) (values start 1 end)))
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

(defmethod size ((range range))
  (1+ (the integer (/ (- (range-end range)
                         (range-start range))
                      (range-step range)))))

(defun unary-range-p (range)
  (= (range-start range)
     (range-end range)))

(defun make-index-space (&rest ranges)
  (make-instance
   'strided-array-index-space
   :ranges (make-array (length ranges) :initial-contents ranges)))

(defun |#i-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  `(make-index-space
    ,@(loop for form in (read stream t nil t) collect
            `(range ,@form))))

(set-dispatch-macro-character #\# #\i #'|#i-reader|)
