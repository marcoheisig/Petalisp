;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defvar *backend* (make-native-backend))

(defun flip (array axis-1 axis-2)
  (check-type axis-1 rank)
  (check-type axis-1 rank)
  (reshape array
           (make-transformation
            :output-mask
            (loop for axis below (rank array)
                  collect
                  (cond ((= axis axis-1) axis-2)
                        ((= axis axis-2) axis-1)
                        (t axis))))))

(defun β* (f z x &optional axis)
  (cond ((empty-array-p x) z)
        ((typep axis 'rank)
         (β f (flip x 0 axis)))
        ((null axis)
         (loop repeat (rank x)
               do (setf x (β f x))
               finally (return x)))
        (t (error "Not a valid axis: ~S" axis))))
