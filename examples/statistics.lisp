(in-package :common-lisp-user)

(defpackage #:petalisp.examples.statistics
  (:use #:common-lisp #:petalisp)
  (:export))

(in-package #:petalisp.examples.statistics)

(defun lazy-mean (series)
  (with-lazy-arrays  (series)
    (lazy #'/
     (lazy-reduce '+ series)
     (lazy-array-dimension series 0))))

(defun lazy-median (series)
  (with-lazy-arrays (series)
    (let* ((data (lazy-sort series #'<))
           (size (lazy-array-dimension data 0)))
      (if (oddp size)
          (lazy-reshape data (slicer (/ (1- size) 2)))
          (lazy-mean
           (lazy-stack
            (list
             (lazy-reshape data (slicer (1- (/ size 2))))
             (lazy-reshape data (slicer (/ size 2))))))))))

(defun lazy-variance (series)
  (with-lazy-arrays (series)
    (let* ((mean (lazy-mean series))
           (diff (lazy #'- series (lazy-reshape mean (transform to 0)))))
      (lazy-mean (lazy #'expt diff 2)))))

(defun lazy-standard-deviation (series)
  (with-lazy-arrays (series)
    (lazy #'sqrt (lazy-variance series))))

(defun lazy-rolling-sum (data window-size)
  (declare (type (integer 1) window-size))
  (with-lazy-arrays (data)
    (let* ((x (lazy-reshape data (deflater 1)))
           (n (lazy-array-dimension data 0))
           (w window-size))
      (cond ((or (<= w 0) (> w n))
             (error "Invalid window size ~D." w))
            ((= w 1)
             x)
            ((= w 2)
             (lazy #'+
              (lazy-reshape x (~ 0 (1- n)))
              (lazy-reshape x (~ 1 n) (deflater 1))))
            ((oddp w)
             (lazy #'+
              (lazy-rolling-sum (lazy-reshape x (~ (1- n))) (1- w))
              (lazy-reshape x (~ (1- w) n) (deflater 1))))
            ((evenp w)
             (let* ((a (lazy-rolling-sum
                        (lazy #'+
                         (lazy-reshape x (~ 0 (1- n) 2) (deflater 1))
                         (lazy-reshape x (~ 1 n 2) (deflater 1)))
                        (/ w 2)))
                    (k (- (1+ (- n w)) (lazy-array-dimension a 0)))
                    (b
                      (lazy #'+
                       (lazy #'-
                        (lazy-reshape a (~ k))
                        (lazy-reshape x (~ 0 (* 2 k) 2) (deflater 1)))
                       (lazy-reshape x (~ w (+ w (* 2 k)) 2) (deflater 1)))))
               (lazy-fuse
                (lazy-reshape a (transform i to (* 2 i)))
                (lazy-reshape b (transform i to (1+ (* 2 i)))))))))))

(defun lazy-rolling-average (data window-size)
  (declare (type (integer 1) window-size))
  (with-lazy-arrays (data)
    (lazy #'/
     (lazy-rolling-sum data window-size)
     window-size)))
