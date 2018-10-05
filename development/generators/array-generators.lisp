;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

(defgenerator array
    (&key
     (adjustable nil)
     (fill-pointer nil)
     (element-type 'single-float)
     (dimensions (loop repeat (random 4) collect (random 8)))
     (element-generator (make-single-float-generator)))
  (lambda ()
    (let ((result (make-array dimensions :element-type element-type
                                         :adjustable adjustable
                                         :fill-pointer fill-pointer)))
      (loop for index below (array-total-size result) do
        (setf (row-major-aref result index) (funcall element-generator)))
      result)))
