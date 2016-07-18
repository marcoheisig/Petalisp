;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(deftype strided-array-index () 'list)

(defclass strided-array (total-function)
  ((%ranges :initarg :ranges :reader ranges)
   (%domain-type :initform 'strided-array-index :allocation :class)
   (%index-space :initarg :index-space :reader index-space)))

(defclass strided-array-index-space
    (strided-array index-space)
  ((%codomain-type :initform 'strided-array-index :allocation :class)))

(defmethod dimension ((object strided-array))
  (length (ranges object)))

(defmethod size ((object strided-array))
  (reduce #'* (mapcar #'size (ranges object))))

(defmethod equalp ((object-1 strided-array-index-space)
                   (object-2 strided-array-index-space))
  (and (= (dimension object-1) (dimension object-2))
       (every #'equalp
              (ranges object-1)
              (ranges object-2))))

(defmethod initialize-instance :after ((object strided-array)
                                       &key &allow-other-keys)
  (setf (slot-value object 'index-space)
        (make-instance
         'strided-array-index-space
         :ranges (ranges object))))

(defmethod initialize-instance :after ((object strided-array-index-space)
                                       &key &allow-other-keys)
  (setf (slot-value object 'index-space) object))

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

(defun |#i-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  `(make-instance
    'strided-array-index-space
    :ranges
    (list ,@(loop for spec in (read stream t nil t)
                  collect
                  (if (atom spec)
                      `(range ,spec)
                      `(range ,@spec))))))

(set-dispatch-macro-character #\# #\i #'|#i-reader|)

(defmethod size ((range range))
  (1+ (the integer (/ (- (range-end range)
                         (range-start range))
                      (range-step range)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add magic keywords to the Lisp API

(dolist (sym '(start step end)) (pushnew sym *magic-symbols*))

(defmethod query ((symbol symbol) &key space dimension)
  (cond
    ((string= symbol 'start)
     (range-start (nth dimension (ranges space))))
    ((string= symbol 'step)
     (range-step (nth dimension (ranges space))))
    ((string= symbol 'end)
     (range-end (nth dimension (ranges space))))))
