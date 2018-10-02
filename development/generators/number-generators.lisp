;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

(defgenerator integer (&key (lower-limit -496) (upper-limit 496))
  "Generate an integer between LOWER-LIMIT (inclusive) and UPPER-LIMIT (exclusive)."
  (assert (> upper-limit lower-limit))
  (let ((delta (- upper-limit lower-limit)))
    (lambda ()
      (+ lower-limit (random delta)))))

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
