;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun collapsing-reshaper (rank)
  (lambda (shape)
    (petalisp.core:collapsing-transformation
     (petalisp.core:shape-subseq shape 0 rank))))

(defun peeling-reshaper
    (&key
       (layers 0)
       (lower-layers layers)
       (upper-layers layers)
       (strides 1))
  (declare (type (or null unsigned-byte sequence)
                 layers lower-layers upper-layers strides))
  (flet ((nth-thing (thing index default)
           (etypecase thing
             (unsigned-byte thing)
             (list #1=
              (if (< index (length thing))
                  (elt thing index)
                  default))
             (simple-vector #1#)
             (sequence #1#))))
    (lambda (shape)
      (make-shape
       (loop for range in (shape-ranges shape)
             for index from 0
             collect
             (let* ((lower (nth-thing lower-layers index 0))
                    (upper (nth-thing upper-layers index 0))
                    (total (+ lower upper))
                    (stride (nth-thing strides index 1))
                    (start (range-start range))
                    (end (range-end range))
                    (step (range-step range))
                    (size (range-size range)))
               (unless (<= total size)
                 (error "~@<Cannot peel ~R layers from the range ~S that ~
                          has only ~R elements.~:@>"
                        total range size))
               (range (+ start (* lower step))
                      (- end (* upper step))
                      (* step stride))))))))

(defun permuting-reshaper (&rest indices)
  (let ((transformation
          (make-transformation
           :output-mask indices)))
    (lambda (shape)
      (declare (ignore shape))
      transformation)))
