;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defstruct (range (:constructor %make-range (start step size))
                  (:predicate rangep)
                  (:copier nil))
  (start 0 :type integer :read-only t)
  (step 1 :type (integer 1 *) :read-only t)
  (size 1 :type (integer 1 *) :read-only t))

(declaim (inline range-end))
(defun range-end (range)
  (declare (range range))
  (+ (range-start range)
     (* (1- (range-size range))
        (range-step range))))

(defmethod print-object ((range range) stream)
  (with-accessors ((start range-start)
                   (step range-step)
                   (end range-end)
                   (size range-size))
      range
    (print-unreadable-object (range stream :type t)
      (cond ((= size 1)
             (format stream "(~D)" start))
            ((= size 2)
             (format stream "(~D ~D)" start end))
            ((= size 3)
             (format stream "(~D ~D ~D)" start (+ start step) end))
            ((= step 1)
             (format stream "(~D ... ~D)" start end))
            ((= size 4)
             (format stream "(~D ~D ~D ~D)" start (+ start step) (+ start step step) end))
            (t
             (format stream "(~D ~D ... ~D)" start (+ start step) end))))))

;; TODO This is a compatibility function that can be removed.
(declaim (inline range-start-step-end))
(defun range-start-step-end (range)
  (declare (range range))
  (values
   (range-start range)
   (range-step range)
   (range-end range)))

(defun split-range (range)
  (declare (range range))
  (assert (< 1 (range-size range)))
  (with-accessors ((start range-start)
                   (step range-step)
                   (size range-size))
      range
    (let* ((size-1 (ceiling size 2))
           (size-2 (- size size-1)))
      (values (%make-range start step size-1)
              (%make-range (+ start (* size-1 step)) step size-2)))))

(defun make-range (start step end)
  (declare (integer start end))
  (setf step (abs step))
  (if (= start end)
      (%make-range start 1 1)
      (if (zerop step)
          (error "~@<Bad step size 0 for range with start ~d and end ~d~:@>" start end)
          (let* ((n (truncate (- end start) step)))
            (if (zerop n)
                (%make-range start 1 1)
                (%make-range (min start (+ start (* n step))) step (1+ (abs n))))))))

(defun range (start &optional (step-or-end 1 two-args-p) (end start three-args-p))
  (cond (three-args-p (make-range start step-or-end end))
        (two-args-p (make-range start 1 step-or-end))
        (t (make-range start step-or-end end))))

(define-compiler-macro range (&whole form &rest args)
  (cond ((not (<= 1 (length args) 3))
         (return-from range form))
        ((and (every #'integerp args)
              (or (< (length args) 3)
                  (= (first args) (third args))
                  (not (zerop (second args)))))
         `(load-time-value
           (locally (declare (notinline range))
             (range ,@args))))
        ((let ((bindings (mapcar (lambda (arg)
                                   (let ((g (gensym)))
                                     (list g arg)))
                                 args)))
           `(let ,bindings
              ,(trivia:ematch (mapcar #'first bindings)
                 ((list start) `(make-range ,start 1 ,start))
                 ((list start end) `(make-range ,start 1 ,end))
                 ((list start step end) `(make-range ,start ,step ,end))))))))

(trivia:defpattern range (&rest start-step-end)
  (alexandria:with-gensyms (it tmp)
    (multiple-value-bind (start step end)
        (trivia:ematch start-step-end
          ((list start step end) (values start step end))
          ((list start end) (values start 1 end))
          ((list start) (values `(and ,start ,tmp) 1 `(= ,tmp))))
      `(trivia:guard1 ,it (rangep ,it)
                      (range-start ,it) ,start
                      (range-step ,it) ,step
                      (range-end ,it) ,end))))

(declaim (inline size-one-range-p))
(defun size-one-range-p (range)
  (declare (range range))
  (= 1 (range-size range)))

(defun map-range (function range)
  (declare (function function)
           (range range))
  (multiple-value-bind (start step end)
      (range-start-step-end range)
    (loop for element from start by step to end do
      (funcall function element))))

(declaim (inline range-contains))
(defun range-contains (range integer)
  (declare (range range)
           (integer integer))
  (and (<= (range-start range) integer (range-end range))
       (zerop (rem (- integer (range-start range))
                   (range-step range)))))

(defun range-equal (range-1 range-2)
  (declare (range range-1 range-2))
  (and (= (range-start range-1)
          (range-start range-2))
       (= (range-step range-1)
          (range-step range-2))
       (= (range-size range-1)
          (range-size range-2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Difference of Ranges

(defun range-difference-list (range-1 range-2)
  (declare (range range-1 range-2))
  ;; For the remaining code, we only care about the part of range-2
  ;; that intersects with range-1.
  (let ((range-2 (range-intersection range-1 range-2)))
    (if (not range-2)
        (list range-1)
        (multiple-value-bind (start-1 step-1 end-1) (range-start-step-end range-1)
          (multiple-value-bind (start-2 step-2 end-2) (range-start-step-end range-2)
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
                (lambda (start step end)
                  (make-range
                   (+ (* start step-1) start-1)
                   (* step step-1)
                   (+ (* end step-1) start-1)))))))))))

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
      (flet ((push-range (start step end)
               (push (funcall make-range-fn start step end) ranges)))
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
                  do (push-range start step-2 end)
                  finally
                     (when strategy-1-lb-p
                       (push-range start-1 1 strategy-1-lb))
                     (when strategy-1-ub-p
                       (push-range strategy-1-ub 1 end-1)))
            ;; Strategy 2
            (let ((block-size (- step-2 2)))
              (loop for start from (1+ start-2) by step-2 below end-2
                    for end = (+ start block-size)
                    do (push-range start 1 end)
                    finally
                       (when strategy-2-lb-p
                         (push-range start-1 1 (1- start-2)))
                       (when strategy-2-ub-p
                         (push-range (1+ end-2) 1 end-1)))))
        ranges))))

;;; Remove a single (valid) index from the given range.
(defun range-difference-list--single (start-1 step-1 end-1 index)
  (declare (integer start-1 end-1 index)
           (alexandria:positive-integer step-1))
  (cond ((= start-1 end-1)
         '())
        ((= index start-1)
         (list
          (make-range (+ start-1 step-1) step-1 end-1)))
        ((= index end-1)
         (list
          (make-range start-1 step-1 (- end-1 step-1))))
        ((= (+ start-1 step-1) index (- end-1 step-1))
         (list
          (make-range start-1 (* 2 step-1) end-1)))
        (t
         (list
          (make-range start-1 step-1 (- index step-1))
          (make-range (+ index step-1) step-1 end-1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Intersection of Ranges

(defun range-intersection (range-1 range-2)
  (declare (range range-1 range-2))
  (multiple-value-bind (start step end)
      (range-intersection-start-step-end range-1 range-2)
    (if (not start)
        nil
        (make-range start step end))))

(defun range-intersection-start-step-end (range-1 range-2)
  (declare (range range-1 range-2))
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
                      (values start lcm end)
                      (values nil nil nil))))))))))

(defun range-intersectionp (range-1 range-2)
  (declare (range range-1 range-2))
  (and (range-intersection-start-step-end range-1 range-2) t))

(defun subrangep (range-1 range-2)
  (declare (range range-1 range-2))
  (if (size-one-range-p range-1)
      (range-contains range-2 (range-start range-1))
      (multiple-value-bind (start step end)
          (range-start-step-end range-1)
        (and (range-contains range-2 start)
             (range-contains range-2 end)
             (range-contains range-2 (+ start step))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fusion of Ranges

(defun range-fusion (ranges)
  ;; Assuming that all supplied RANGES are non-overlapping, the only
  ;; possible fusion is obtained by summing the number of elements,
  ;; determining the smallest and largest element of all sequences and
  ;; choosing a step size to yield the correct number of elements.
  (loop for range in ranges
        summing (range-size range) into size
        minimizing (range-start range) into start
        maximizing (range-end range) into end
        finally
           (flet ((fail ()
                    (alexandria:simple-program-error
                     "Unable to fuse ranges:~%~{~A~%~}"
                     ranges)))
             (let ((step (if (= size 1)
                             1
                             (/ (- end start)
                                (1- size)))))
               (unless (integerp step)
                 (fail))
               (let ((result (%make-range start step size)))
                 (when (notevery (lambda (range) (range-intersectionp range result)) ranges)
                   (fail))
                 (return result))))))

