;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.test-suite)

(defgenerator integer (&key (min -496) (max 496))
  "Generate an integer between MIN (inclusive) and MAX (exclusive)."
  (assert (> max min))
  (let ((delta (- max min)))
    (lambda ()
      (+ min (random delta)))))

(defmacro define-float-generator (type)
  (let ((zero (coerce 0 type))
        (one  (coerce 1 type))
        (two  (coerce 2 type)))
    ;; These generators use Marsaglia's polar method to convert the uniform
    ;; random numbers from RANDOM to a normal distribution.
    `(defgenerator ,type (&key (mean ,zero) (standard-deviation ,one))
       (let (cache)
         (declare (type (or null ,type) cache))
         (lambda ()
           (or (shiftf cache nil)
               (loop for u ,type = (- (random ,two) ,one)
                     for v ,type = (- (random ,two) ,one)
                     for s ,type = (+ (* u u) (* v v))
                     until (and (<= s ,one)
                                (/= s ,zero))
                     finally
                        (let ((m (sqrt (* (- ,two) (log s) (/ s)))))
                          (setf cache (+ (* v m standard-deviation) mean))
                          (return     (+ (* u m standard-deviation) mean))))))))))

(define-float-generator short-float)
(define-float-generator single-float)
(define-float-generator double-float)
(define-float-generator long-float)

(defgenerator range (&key (start-generator (make-integer-generator :min -20 :max 21))
                          (step-generator (make-integer-generator :min 1 :max 6))
                          (size-generator (make-integer-generator :min 1 :max 14)))
  (lambda ()
    (let* ((start (funcall start-generator))
           (step (funcall step-generator))
           (size (funcall size-generator))
           (end (+ start (* step (1- size)))))
      (range start (if (< end start) (1- end) (1+ end)) step))))

(defgenerator shape (&key (rank-generator (make-integer-generator :min 0 :max 5))
                          (range-generator (make-range-generator)))
  (lambda ()
    (apply ~* (loop repeat (funcall rank-generator)
                     collect (funcall range-generator)))))

(defgenerator lazy-array
    (&key (element-type 'single-float)
          (element-generator (make-single-float-generator))
          (shape-generator (make-shape-generator)))
  (lambda ()
    (let* ((shape (funcall shape-generator))
           (array (make-array (mapcar #'range-size (shape-ranges shape))
                              :element-type element-type)))
      (loop for index below (array-total-size array) do
        (setf (row-major-aref array index)
              (funcall element-generator)))
      (lazy-reshape array shape))))

(defun ndarray (rank)
  (generate-lazy-array
   :shape-generator
   (make-shape-generator
    :rank-generator (constantly rank)
    :range-generator
    (make-range-generator
     :size-generator (make-integer-generator :min 8 :max 14)
     :step-generator (constantly 1)))))

(defgenerator matrix
    (&key (element-type 'single-float)
          (element-generator (make-single-float-generator))
          (size-generator (make-integer-generator :min 1 :max 10)))
  (make-lazy-array-generator
   :element-type element-type
   :element-generator element-generator
   :shape-generator
   (make-shape-generator
    :rank-generator (constantly 2)
    :range-generator
    (make-range-generator
     :start-generator (constantly 0)
     :step-generator (constantly 1)
     :size-generator size-generator))))

(defun reshape-randomly (array)
  (let* ((lazy-array (lazy-array array))
         (rank (shape-rank lazy-array))
         (generator (make-integer-generator :min -20 :max 21)))
    (lazy-reshape
     lazy-array
     (make-transformation
      :input-rank rank
      :output-rank rank
      :scalings (loop repeat rank collect (funcall generator))
      :output-mask (alexandria:shuffle (alexandria:iota rank))))))
