;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun collapsing-reshaper ()
  (lambda (shape)
    (collapsing-transformation shape)))

(defun peeling-reshaper
    (&key
       (layers 0)
       (lower-layers layers)
       (upper-layers layers))
  (declare (type (or null unsigned-byte sequence) layers lower-layers upper-layers))
  (flet ((number-of-layers (thing index)
           (etypecase thing
             (unsigned-byte thing)
             (list (or (nth index thing) 0))
             (sequence
              (if (< index (length thing))
                  0
                  (elt thing index))))))
    (lambda (shape)
      (make-shape
       (loop for range in (shape-ranges shape)
             for index from 0
             collect
             (let* ((lower-number-of-layers (number-of-layers lower-layers index))
                    (upper-number-of-layers (number-of-layers upper-layers index))
                    (total-number-of-layers (+ lower-number-of-layers upper-number-of-layers))
                    (start (range-start range))
                    (end (range-end range))
                    (step (range-step range))
                    (size (range-size range)))
               (unless (<= total-number-of-layers size)
                 (error "~@<Cannot peel ~R layers from the range ~S that ~
                          has only ~R elements.~:@>"
                        total-number-of-layers range size))
               (range (+ start (* lower-number-of-layers step))
                      (- end (* upper-number-of-layers step))
                      step)))))))

(defun permuting-reshaper (&rest indices)
  (let ((transformation
          (make-transformation
           :output-mask indices)))
    (lambda (shape)
      (declare (ignore shape))
      transformation)))
