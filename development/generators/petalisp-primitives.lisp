;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

(defgenerator range (&key (start-generator (make-integer-generator :min -20 :max 21))
                          (step-generator (make-integer-generator :min 1 :max 6))
                          (size-generator (make-integer-generator :min 1 :max 14)))
  (lambda ()
    (let* ((start (funcall start-generator))
           (step (funcall step-generator))
           (size (funcall size-generator))
           (end (+ start (* step (1- size)))))
      (range start step end))))

(defgenerator shape (&key (rank-generator (make-integer-generator :min 0 :max 5))
                          (range-generator (make-range-generator)))
  (lambda ()
    (apply #'make-shape
           (loop repeat (funcall rank-generator)
                 collect (funcall range-generator)))))

(defgenerator strided-array
    (&key (element-type 'single-float)
          (element-generator (make-single-float-generator))
          (shape-generator (make-shape-generator)))
  (lambda ()
    (let* ((shape (funcall shape-generator))
           (array (make-array (mapcar #'set-size (ranges shape))
                              :element-type element-type)))
      (loop for index below (array-total-size array) do
        (setf (row-major-aref array index)
              (funcall element-generator)))
      (reshape array shape))))

(defun ndarray (rank)
  (generate-strided-array
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
  (make-strided-array-generator
   :element-type element-type
   :element-generator element-generator
   :shape-generator
   (make-shape-generator
    :rank-generator (constantly 2)
    :range-generator
    (make-range-generator
     :start-generator (constantly 1)
     :step-generator (constantly 1)
     :size-generator size-generator))))
