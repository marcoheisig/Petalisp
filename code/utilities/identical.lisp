;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

(defun identical (sequence &key (test #'eql) (key #'identity))
  "Check whether the KEYs of SEQUENCE are identical with respect to TEST."
  (etypecase sequence
    (list
     (or (null sequence)
         (loop with reference-element = (funcall key (car sequence))
               for element in (cdr sequence)
               always (funcall test
                               reference-element
                               (funcall key element)))))
    (simple-vector #1=
     (or (= 0 (length sequence))
         (loop with reference-element = (funcall key (elt sequence 0))
               for i from 1 below (length sequence)
               always (funcall test
                               reference-element
                               (funcall key (elt sequence i))))))
    (sequence #1#)))
