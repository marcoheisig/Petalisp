;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-reduce (function &rest arrays)
  (let ((lazy-arrays (broadcast arrays)))
    (dolist (function (alexandria:ensure-list function))
      (setf lazy-arrays (lazy-reduce-aux function lazy-arrays)))
    (values-list lazy-arrays)))

(defun lazy-reduce-aux (function lazy-arrays)
  (labels ((multireshape (lazy-arrays &rest modifiers)
             (mapcar
              (lambda (lazy-array)
                (apply #'lazy-reshape lazy-array modifiers))
              lazy-arrays))
           (process (n k xs ys)
             (if (oddp n)
                 (process-odd n k xs ys)
                 (process-even n k xs ys)))
           (process-even (n k xs ys)
             (let* ((p (/ n 2))
                    (zs (multiple-value-list
                         (apply #'lazy-multiple-value k function
                                (append
                                 (multireshape xs (~ 0 n 2) (~ p))
                                 (multireshape xs (~ 1 n 2) (~ p)))))))
               (process p k zs ys)))
           (process-odd (n k xs ys)
             (if (not ys)
                 (if (= n 1)
                     (multireshape xs (transform 0 to))
                     (process-even
                      (1- n)
                      k
                      (multireshape xs (~ (1- n)))
                      (multireshape xs (~ (1- n) n))))
                 (process-even
                  (1+ n)
                  k
                  (mapcar (lambda (x y) (lazy-stack (list x y))) xs ys)
                  nil))))
    (cond ((null lazy-arrays)
           (if (null (multiple-value-list (funcall function)))
               (values)
               (error "~@<Invalid reduction with function ~S
                          on zero argument arrays.~:@>"
                      function)))
          ((zerop (lazy-array-rank (first lazy-arrays)))
           (error "Cannot reduce arrays with rank zero."))
          ((zerop (lazy-array-dimension (first lazy-arrays) 0))
           (let ((vs (multiple-value-list (funcall function)))
                 (s (lazy-array-shape (first lazy-arrays))))
             (unless (= (length vs) (length lazy-arrays))
               (error "~@<Failed to determine a default value for reducing ~
                          an empty sequence with the function ~S.~@:>"
                      function))
             (multireshape vs (make-shape (rest (shape-ranges s))))))
          (t
           (let ((n (lazy-array-dimension (first lazy-arrays) 0))
                 (k (length lazy-arrays)))
             (process n k (multireshape lazy-arrays (~ n)) nil))))))
