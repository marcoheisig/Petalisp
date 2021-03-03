;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

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

(declaim (inline range-last))
(defun range-last (range)
  (declare (non-empty-range range))
  (+ (range-start range)
     (* (1- (range-size range))
        (range-step range))))

(declaim (inline range-end))
(defun range-end (range)
  (declare (non-empty-range range))
  (1+ (range-last range)))

(defmethod print-object ((empty-range empty-range) stream)
  (print-unreadable-object (empty-range stream :identity t)
    (write (class-name (class-of empty-range)) :stream stream)))

(defmethod print-object ((range non-empty-range) stream)
  (with-accessors ((start range-start)
                   (step range-step)
                   (size range-size)
                   (last range-last)) range
    (if *print-readably*
        (call-next-method)
        (print-unreadable-object (range stream)
          (write 'range :stream stream)
          (cond ((= size 1)
                 (format stream "(~D)" start))
                ((= size 2)
                 (format stream "(~D ~D)" start last))
                ((= size 3)
                 (format stream "(~D ~D ~D)" start (+ start step) last))
                ((= size 4)
                 (format stream "(~D ~D ~D ~D)" start (+ start step) (+ start step step) last))
                ((= step 1)
                 (format stream "(~D ... ~D)" start last))
                (t
                 (format stream "(~D ~D ... ~D)" start (+ start step) last)))))))

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
                      (range-end ,it) ,end
                      (range-step ,it) ,step))))

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
            below (range-end range) do
              (funcall function element)))
  range)

(declaim (inline range-contains))
(defun range-contains (range integer)
  (declare (range range)
           (integer integer))
  (if (empty-range-p range)
      nil
      (and (<= (range-start range) integer (range-last range))
           (zerop (rem (- integer (range-start range))
                       (range-step range))))))

(defun range-equal (range1 range2)
  (declare (range range1 range2))
  (if (empty-range-p range1)
      (empty-range-p range2)
      (and (= (range-start range1)
              (range-start range2))
           (= (range-step range1)
              (range-step range2))
           (= (range-size range1)
              (range-size range2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Difference of Ranges

(defun range-difference-list (range1 range2)
  (declare (range range1 range2))
  (when (empty-range-p range1)
    (return-from range-difference-list '()))
  ;; For the remaining code, we only care about the part of range2
  ;; that intersects with range1.
  (let ((range2 (range-intersection range1 range2)))
    (if (empty-range-p range2)
        (list range1)
        (with-accessors ((start1 range-start)
                         (step1 range-step)
                         (end1 range-end)) range1
          (with-accessors ((start2 range-start)
                           (step2 range-step)
                           (end2 range-end)) range2
            ;; The new range2 is now a proper sub-range of range1, i.e. we
            ;; have (<= start1 start2 end2 end1).  Furthermore, step2 is
            ;; now either a multiple of step1, or one, if range2 has only a
            ;; single element.
            (cond
              ((size-one-range-p range2)
               ;; First, we pick off the special case where range2 has
               ;; only a single element.
               (range-difference-list--single range1 start2))
              (t
               ;; At this point, we know that step2 is a multiple of
               ;; step1.  Using a coordinate transformation, we
               ;; simplify this case such that range1 is contiguous.
               (range-difference-list--contiguous
                0
                (1- (range-size range1))
                (/ (- start2 start1) step1)
                (/ (- end2 1 start1) step1)
                (/ step2 step1)
                (lambda (start end step)
                  (make-range
                   (+ (* start step1) start1)
                   (+ (* end step1) start1)
                   (* step step1)))))))))))

(defun range-difference-list--contiguous
    (start1 last1 start2 last2 step2 make-range-fn)
  (declare (integer start1 last1 start2 last2)
           (alexandria:positive-integer step2)
           (function make-range-fn))
  ;; There are two strategies to partition the contiguous indices
  ;; start1..last1 into ranges.  The first one is to create strided ranges
  ;; and possibly a contiguous range for the first and last elements, the
  ;; other strategy is to create only contiguous ranges.
  (let* ((strategy1-lb (- start2 step2))
         (strategy1-ub (+ last2 step2))
         (strategy1-lb-p (>= strategy1-lb start1))
         (strategy1-ub-p (<= strategy1-ub last1))
         (strategy1 (- step2
                       (if strategy1-lb-p 0 1)
                       (if strategy1-ub-p 0 1)))
         (strategy2-lb-p (/= start2 start1))
         (strategy2-ub-p (/= last2 last1))
         (strategy2 (+ (/ (- last2 start2) step2)
                       (if strategy2-lb-p 1 0)
                       (if strategy2-ub-p 1 0))))
    (let ((ranges '()))
      (flet ((push-range (start last step)
               (push (funcall make-range-fn start (1+ last) step) ranges)))
        ;; We pick the strategy that produces fewer ranges.
        (if (< strategy1 strategy2)
            ;; Strategy 1
            (loop for offset from 1 below step2
                  for start = (let ((high (+ start2 offset))
                                    (low (+ start2 (- step2) offset)))
                                (if (>= low start1) low high))
                  for last = (let ((high (+ last2 offset))
                                   (low (+ last2 (- step2) offset)))
                               (if (<= high last1) high low))
                  do (push-range start last step2)
                  finally
                     (when strategy1-lb-p
                       (push-range start1 strategy1-lb 1))
                     (when strategy1-ub-p
                       (push-range strategy1-ub last1 1)))
            ;; Strategy 2
            (let ((block-size (- step2 2)))
              (loop for start from (1+ start2) by step2 below last2
                    for last = (+ start block-size)
                    do (push-range start last 1)
                    finally
                       (when strategy2-lb-p
                         (push-range start1 (1- start2) 1))
                       (when strategy2-ub-p
                         (push-range (1+ last2) last1 1)))))
        ranges))))

;;; Remove a single (valid) index from the given range.
(defun range-difference-list--single (range index)
  (declare (non-empty-range range)
           (integer index))
  (with-accessors ((start range-start)
                   (step range-step)
                   (last range-last)
                   (end range-end)) range
    (cond ((size-one-range-p range)
           '())
          ((= index start)
           ;; Remove the first element.
           (list
            (range (+ start step) end step)))
          ((= index last)
           ;; Remove the last element.
           (list
            (range start (- end step) step)))
          ((= (+ start step) index (- last step))
           ;; Remove the middle of three elements.
           (list
            (range start end (* 2 step))))
          (t
           ;; Return separate ranges for all integers below and above
           ;; INDEX.
           (list
            (range start (1+ (- index step)) step)
            (range (+ index step) end step))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Intersection of Ranges

(defun range-intersection (range1 range2)
  (declare (range range1 range2))
  (multiple-value-bind (start end step)
      (range-intersection-start-end-step range1 range2)
    (if (not start)
        (make-empty-range)
        (range start end step))))

(defun range-intersection-start-end-step (range1 range2)
  (declare (range range1 range2))
  (if (or (empty-range-p range1)
          (empty-range-p range2))
      (values nil nil nil)
      (let ((lb (max (range-start range1) (range-start range2)))
            (ub (min (range-last range1) (range-last range2))))
        (let ((a (range-step range1))
              (b (range-step range2))
              (c (- (range-start range2) (range-start range1))))
          (multiple-value-bind (s gcd)
              (petalisp.utilities:extended-euclid a b)
            (let ((c/gcd (/ c gcd)))
              (if (not (integerp c/gcd))
                  (values nil nil nil)
                  (let ((x (+ (* s c/gcd a)
                              (range-start range1)))
                        (lcm (/ (* a b) gcd)))
                    (let ((start (+ x (* lcm (ceiling (- lb x) lcm))))
                          (last  (+ x (* lcm (floor   (- ub x) lcm)))))
                      (if (<= lb start last ub)
                          (values start (1+ last) lcm)
                          (values nil nil nil)))))))))))

(defun range-intersectionp (range1 range2)
  (declare (range range1 range2))
  (and (range-intersection-start-end-step range1 range2) t))

(defun subrangep (range1 range2)
  (declare (range range1 range2))
  (cond ((empty-range-p range1)
         t)
        ((size-one-range-p range1)
         (range-contains range2 (range-start range1)))
        (t
         (with-accessors ((start range-start)
                          (last range-last)
                          (step range-step)) range1
           (and (range-contains range2 start)
                (range-contains range2 last)
                (range-contains range2 (+ start step)))))))

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
        (let ((step (- end start 1)))
          (loop for range in ranges
                unless (empty-range-p range)
                  do (let ((delta (- (range-start range) start)))
                       (if (size-one-range-p range)
                           (setf step (gcd step delta))
                           (setf step (gcd step delta (range-step range))))))
          (range start end step)))))
