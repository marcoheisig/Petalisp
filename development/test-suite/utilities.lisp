;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

(defun ndarray (n &optional (length 10))
  "Create a LENGTH^N array of double floats."
  (generate-array
   :element-type 'double-float
   :dimensions (make-list n :initial-element length)
   :element-generator (make-double-float-generator)))

(defun reshape-randomly (array)
  (let* ((strided-array (strided-array array))
         (dimension (dimension strided-array))
         (generator (make-integer-generator :lower-limit -20 :upper-limit 21)))
    (reshape strided-array
             (make-transformation
              :input-dimension dimension
              :output-dimension dimension
              :translation (loop repeat dimension collect (funcall generator))
              :permutation (shuffle (iota dimension))))))
