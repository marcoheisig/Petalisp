;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defstruct (range
            (:constructor nil)
            (:predicate rangep)
            (:copier nil))
  (size nil :type (integer 0 *) :read-only t))

(defstruct (empty-range
            (:include range)
            (:constructor make-empty-range (&aux (size 0)))
            (:predicate empty-range-p)
            (:copier nil)))

(defstruct (non-empty-range
            (:include range)
            (:conc-name range-)
            (:constructor make-non-empty-range (start step size))
            (:predicate non-empty-range-p)
            (:copier nil))
  (step nil :type (integer 1 *) :read-only t)
  (start nil :type integer :read-only t))

(declaim (inline range-end))
(defun range-end (range)
  (declare (non-empty-range range))
  (+ (range-start range)
     (* (1- (range-size range))
        (range-step range))))

(defmethod print-object ((empty-range empty-range) stream)
  (print-unreadable-object (empty-range stream :identity t)
    (write (class-name (class-of empty-range)) :stream stream)))

(defmethod print-object ((range non-empty-range) stream)
  (with-accessors ((start range-start)
                   (step range-step)
                   (end range-end)
                   (size range-size)) range
    (print-unreadable-object (range stream)
      (write 'range :stream stream)
      (cond ((= size 1)
             (format stream "(~D)" start))
            ((= size 2)
             (format stream "(~D ~D)" start end))
            ((= size 3)
             (format stream "(~D ~D ~D)" start (+ start step) end))
            ((= size 4)
             (format stream "(~D ~D ~D ~D)" start (+ start step) (+ start step step) end))
            ((= step 1)
             (format stream "(~D ... ~D)" start end))
            (t
             (format stream "(~D ~D ... ~D)" start (+ start step) end))))))

(defun split-range (range)
  (declare (non-empty-range range))
  (assert (< 1 (range-size range)))
  (with-accessors ((start range-start)
                   (step range-step)
                   (size range-size)) range
    (let* ((size1 (ceiling size 2))
           (size2 (- size size1)))
      (values
       (make-non-empty-range start step size1)
       (make-non-empty-range (+ start (* size1 step)) step size2)))))

(defun make-range (start end step &aux (step (abs step)))
  (declare (integer start end step))
  (if (= start end)
      (make-empty-range)
      (let ((delta (if (< start end)
                       (- end start 1)
                       (- start end 1))))
        (if (zerop step)
            (if (zerop delta)
                (make-non-empty-range start 1 1)
                (error "~@<Bad step size 0 for range with start ~d and end ~d~:@>"
                       start end))
            (let ((n (truncate delta step)))
              (if (zerop n)
                  (make-non-empty-range start 1 1)
                  (make-non-empty-range
                   (min start (+ start (* n step)))
                   step
                   (1+ (abs n)))))))))

(defun range (first &optional (end nil endp) (step 1))
  (if (not endp)
      (make-range 0 first step)
      (make-range first end step)))

(define-compiler-macro range (&whole form &rest args)
  (case (length args)
    (1 `(make-range 0 ,(first args) 1))
    (2 `(make-range ,(first args) ,(second args) 1))
    (3 `(make-range ,(first args) ,(second args) ,(third args)))
    (otherwise form)))

(trivia:defpattern range (&rest start-end-step)
  (alexandria:with-gensyms (it)
    (multiple-value-bind (start end step)
        (trivia:ematch start-end-step
          ((list start end step) (values start end step))
          ((list start end) (values start end 1))
          ((list length) (values 0 length 1)))
      `(trivia:guard1 ,it (non-empty-range-p ,it)
                      (range-start ,it) ,start
                      (range-step ,it) ,step
                      (1+ (range-end ,it)) ,end))))

(declaim (inline size-one-range-p))
(defun size-one-range-p (range)
  (declare (range range))
  (= 1 (range-size range)))

(defun map-range (function range)
  (declare (function function)
           (range range))
  (unless (empty-range-p range)
    (loop for element from (range-start range)
          by (range-step range)
            to (range-end range) do
              (funcall function element)))
  range)

(declaim (inline range-contains))
(defun range-contains (range integer)
  (declare (range range)
           (integer integer))
  (if (empty-range-p range)
      nil
      (and (<= (range-start range) integer (range-end range))
           (zerop (rem (- integer (range-start range))
                       (range-step range))))))

(defun range-equal (range-1 range-2)
  (declare (range range-1 range-2))
  (if (empty-range-p range-1)
      (empty-range-p range-2)
      (and (= (range-start range-1)
              (range-start range-2))
           (= (range-step range-1)
              (range-step range-2))
           (= (range-size range-1)
              (range-size range-2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Difference of Ranges

(defun range-difference-list (range-1 range-2)
  (declare (range range-1 range-2))
  (when (empty-range-p range-1)
    (return-from range-difference-list '()))
  ;; For the remaining code, we only care about the part of range-2
  ;; that intersects with range-1.
  (let ((range-2 (range-intersection range-1 range-2)))
    (if (empty-range-p range-2)
        (list range-1)
        (with-accessors ((start-1 range-start)
                         (step-1 range-step)
                         (end-1 range-end)) range-1
          (with-accessors ((start-2 range-start)
                           (step-2 range-step)
                           (end-2 range-end)) range-2
            ;; The new range-2 is now a proper sub-range of range-1, i.e. we
            ;; have (<= start-1 start-2 end-2 end-1).  Furthermore, step-2 is
            ;; now either a multiple of step-1, or one, if range-2 has only a
            ;; single element.
            (cond
              ((= start-2 end-2)
               ;; First, we pick off the special case where range-2 has
               ;; only a single element.
               (range-difference-list--single start-1 step-1 end-1 start-2))
              (t
               ;; At this point, we know that step-2 is a multiple of
               ;; step-1.  Using a coordinate transformation, we
               ;; simplify this case such that range-1 is contiguous.
               (range-difference-list--contiguous
                0
                (1- (range-size range-1))
                (/ (- start-2 start-1) step-1)
                (/ step-2 step-1)
                (/ (- end-2 start-1) step-1)
                (lambda (start end step)
                  (make-range
                   (+ (* start step-1) start-1)
                   (+ (* end step-1) start-1 1)
                   (* step step-1)))))))))))

(defun range-difference-list--contiguous
    (start-1 end-1 start-2 step-2 end-2 make-range-fn)
  (declare (integer start-1 end-1 start-2 end-2)
           (alexandria:positive-integer step-2)
           (function make-range-fn))
  ;; There are two strategies to partition the contiguous indices
  ;; start-1..end-1 into ranges.  The first one is to create strided ranges
  ;; and possibly a contiguous range for the first and last elements, the
  ;; other strategy is to create only contiguous ranges.
  (let* ((strategy-1-lb (- start-2 step-2))
         (strategy-1-ub (+ end-2 step-2))
         (strategy-1-lb-p (>= strategy-1-lb start-1))
         (strategy-1-ub-p (<= strategy-1-ub end-1))
         (strategy-1 (- step-2
                        (if strategy-1-lb-p 0 1)
                        (if strategy-1-ub-p 0 1)))
         (strategy-2-lb-p (/= start-2 start-1))
         (strategy-2-ub-p (/= end-2 end-1))
         (strategy-2 (+ (/ (- end-2 start-2) step-2)
                        (if strategy-2-lb-p 1 0)
                        (if strategy-2-ub-p 1 0))))
    (let ((ranges '()))
      (flet ((push-range (start end step)
               (push (funcall make-range-fn start end step) ranges)))
        ;; We pick the strategy that produces fewer ranges.
        (if (< strategy-1 strategy-2)
            ;; Strategy 1
            (loop for offset from 1 below step-2
                  for start = (let ((high (+ start-2 offset))
                                    (low (+ start-2 (- step-2) offset)))
                                (if (>= low start-1) low high))
                  for end = (let ((high (+ end-2 offset))
                                  (low (+ end-2 (- step-2) offset)))
                              (if (<= high end-1) high low))
                  do (push-range start end step-2)
                  finally
                     (when strategy-1-lb-p
                       (push-range start-1 strategy-1-lb 1))
                     (when strategy-1-ub-p
                       (push-range strategy-1-ub end-1 1)))
            ;; Strategy 2
            (let ((block-size (- step-2 2)))
              (loop for start from (1+ start-2) by step-2 below end-2
                    for end = (+ start block-size)
                    do (push-range start end 1)
                    finally
                       (when strategy-2-lb-p
                         (push-range start-1 (1- start-2) 1))
                       (when strategy-2-ub-p
                         (push-range (1+ end-2) end-1 1)))))
        ranges))))

;;; Remove a single (valid) index from the given range.
(defun range-difference-list--single (start-1 step-1 end-1 index)
  (declare (integer start-1 end-1 index)
           (alexandria:positive-integer step-1))
  (cond ((= start-1 end-1)
         '())
        ((= index start-1)
         (list
          (range (+ start-1 step-1) (1+ end-1) step-1)))
        ((= index end-1)
         (list
          (range start-1 (1+ (- end-1 step-1)) step-1)))
        ((= (+ start-1 step-1) index (- end-1 step-1))
         (list
          (range start-1 (1+ end-1) (* 2 step-1))))
        (t
         (list
          (range start-1 (1+ (- index step-1)) step-1)
          (range (+ index step-1) (1+ end-1) step-1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Intersection of Ranges

(defun range-intersection (range-1 range-2)
  (declare (range range-1 range-2))
  (multiple-value-bind (start end step)
      (range-intersection-start-end-step range-1 range-2)
    (if (not start)
        (make-empty-range)
        (range start (1+ end) step))))

(defun range-intersection-start-end-step (range-1 range-2)
  (declare (range range-1 range-2))
  (if (or (empty-range-p range-1)
          (empty-range-p range-2))
      (values nil nil nil)
      (let ((lb (max (range-start range-1) (range-start range-2)))
            (ub (min (range-end   range-1) (range-end   range-2))))
        (let ((a (range-step range-1))
              (b (range-step range-2))
              (c (- (range-start range-2) (range-start range-1))))
          (multiple-value-bind (s gcd)
              (petalisp.utilities:extended-euclid a b)
            (let ((c/gcd (/ c gcd)))
              (if (not (integerp c/gcd))
                  (values nil nil nil)
                  (let ((x (+ (* s c/gcd a)
                              (range-start range-1)))
                        (lcm (/ (* a b) gcd)))
                    (let ((start (+ x (* lcm (ceiling (- lb x) lcm))))
                          (end   (+ x (* lcm (floor   (- ub x) lcm)))))
                      (if (<= lb start end ub)
                          (values start end lcm)
                          (values nil nil nil)))))))))))

(defun range-intersectionp (range-1 range-2)
  (declare (range range-1 range-2))
  (and (range-intersection-start-end-step range-1 range-2) t))

(defun subrangep (range-1 range-2)
  (declare (range range-1 range-2))
  (cond ((empty-range-p range-1)
         t)
        ((size-one-range-p range-1)
         (range-contains range-2 (range-start range-1)))
        (t
         (with-accessors ((start range-start)
                          (end range-end)
                          (step range-step)) range-1
           (and (range-contains range-2 start)
                (range-contains range-2 end)
                (range-contains range-2 (+ start step)))))))

(defun fuse-ranges (&rest ranges)
  (let ((start nil)
        (end nil))
    (loop for range in ranges
          unless (empty-range-p range)
            do (cond
                 ((null start)
                  (setf start (range-start range))
                  (setf end (range-end range)))
                 (t
                  (setf start (min start (range-start range)))
                  (setf end (max end (range-end range))))))
    (if (null start)
        (make-empty-range)
        (let ((step (- end start)))
          (loop for range in ranges
                unless (empty-range-p range)
                  do (let ((delta (- (range-start range) start)))
                       (if (size-one-range-p range)
                           (setf step (gcd step delta))
                           (setf step (gcd step delta (range-step range))))))
          (range start (1+ end) step)))))
