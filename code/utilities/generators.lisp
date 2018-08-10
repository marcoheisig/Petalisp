;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric generator (result-type &rest kwargs &key &allow-other-keys)
  (:documentation
   "Return a function that returns on each invocation a new, random object
of type RESULT-TYPE, with properties according to the supplied keyword
arguments."))

(defgeneric generate-instance (result-type &rest kwargs &key &allow-other-keys)
  (:documentation
   "Return a single, random object of type RESULT-TYPE, with properties
according to the supplied keyword arguments.")
  (:method ((result-type symbol) &rest arguments)
    (funcall (apply #'generator result-type arguments))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Random Number Generators

(defmethod generator ((result-type (eql 'integer))
                      &key (minimum -1000) (maximum 1000))
  (lambda ()
    (+ minimum (random (1+ (- maximum minimum))))))

(defmethod generator ((result-type (eql 'array))
                      &key
                        (element-type 'single-float)
                        (dimensions (loop repeat (random 4) collect (random 8)))
                        (element-generator (generator element-type)))
  (lambda ()
    (let ((result (make-array dimensions :element-type element-type)))
      (loop for index below (array-total-size result) do
        (setf (row-major-aref result index) (funcall element-generator)))
      result)))

(macrolet
    ((define-float-generator (type)
       (let ((zero (coerce 0 type))
             (one  (coerce 1 type))
             (two  (coerce 2 type)))
         ;; These generators use Marsaglia's polar method to convert the
         ;; uniform random numbers from RANDOM to a normal distribution.
         `(defmethod generator ((result-type (eql ',type))
                                &key
                                  (mean ,zero)
                                  (standard-deviation ,one))
            (let (cache)
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
                               (return     (+ (* u m standard-deviation) mean)))))))))))
  (define-float-generator short-float)
  (define-float-generator single-float)
  (define-float-generator double-float)
  (define-float-generator long-float))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Range Generator

(defmethod generator ((result-type (eql 'range))
                      &key
                        (max-extent (floor most-positive-fixnum 4/5))
                        (max-size (floor (sqrt max-extent)))
                        intersecting)
  "Return a random range with at most MAX-SIZE elements, bounded by
MAX-EXTENT. If another range INTERSECTING is given, the result will
intersect it (potentially violating MAX-EXTENT)."
  (assert (and (plusp max-extent)
               (plusp max-size)
               (<= max-size max-extent)))
  (let ((max-step (floor max-extent (1- max-size))))
    (lambda ()
      (let ((step (1+ (random max-step)))
            (size (1+ (random max-size)))
            (sign (- (* (random 2) 2) 1)))
        (let* ((extent (floor (* size step) 2))
               (offset (* sign (random (max (- max-extent extent) 1))))
               (start (- offset extent))
               (range (make-range start step (+ start (* (1- size) step)))))
          (if (not intersecting)
              range
              (flet ((random-element (range)
                       (+ (range-start range)
                          (* (range-step range)
                             (random (set-size range))))))
                (let ((offset (- (random-element intersecting)
                                 (random-element range))))
                  (make-range
                   (+ (range-start range) offset)
                   (range-step range)
                   (+ (range-end range) offset))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Shape Generators

(defmethod generator ((result-type (eql 'shape))
                      &key (dimension 3) (max-size 30) (max-extent 100) intersecting)
  (assert (or (not intersecting)
              (= dimension (dimension intersecting))))
  (let ((range-generators
          (if intersecting
              (mapcar (lambda (range)
                        (generator 'range :max-size max-size
                                          :max-extent max-extent
                                          :intersecting range))
                      (ranges intersecting))
              (make-list dimension :initial-element
                         (generator 'range :max-size max-size
                                           :max-extent max-extent)))))
    (lambda ()
      (shape-from-ranges
       (mapcar #'funcall range-generators)))))
