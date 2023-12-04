;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-reduce (function &rest arrays)
  (let ((n (length arrays)))
    (multiple-value-bind (lazy-arrays input-shape) (broadcast arrays)
      (dolist (function (alexandria:ensure-list function))
        (unless (plusp (shape-rank input-shape))
          (error "Cannot reduce arrays with rank zero."))
        (unless (plusp (range-size (shape-range input-shape 0)))
          (error "Cannot reduce along an empty axis."))
        (setf lazy-arrays (lazy-reduce-aux function n lazy-arrays))
        (setf input-shape (lazy-array-shape (first lazy-arrays))))
      (values-list lazy-arrays))))

(defun lazy-reduce-aux (function n-values inputs)
  (let ((range (lazy-array-range (first inputs) 0)))
    (if (range-with-size-one-p range)
        (let ((drop-first-axis
                (make-transformation
                 :input-mask (list (range-start range))
                 :output-rank 0)))
          (mapcar
           (lambda (input)
             (lazy-reshape input drop-first-axis))
           inputs))
        (with-accessors ((start range-start)
                         (end range-end)
                         (last range-last)
                         (step range-step)
                         (size range-size)) range
          (let* ((rem (rem (range-size range) 2))
                 (left  (~    start       (- end (* step rem)) (* 2 step)))
                 (right (~ (+ start step) (- end (* step rem)) (* 2 step)))
                 (rest  (if (zerop rem) (~ 0) (~ last (1+ last))))
                 (results
                   (multiple-value-list
                    (apply #'lazy-multiple-value n-values function
                           (petalisp.utilities:with-collectors ((arguments collect))
                             (dolist (input inputs)
                               (collect (lazy-reshape input left (deflater 1))))
                             (dolist (input inputs)
                               (collect (lazy-reshape input right (deflater 1))))
                             (arguments))))))
            (lazy-reduce-aux
             function
             n-values
             (loop for input in inputs
                   for result in results
                   collect
                   (lazy-stack (list result (lazy-reshape input rest))))))))))
