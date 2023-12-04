;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun deflater (rank)
  (lambda (shape)
    (petalisp.core:deflating-transformation
     (petalisp.core:shape-subseq shape 0 (min (shape-rank shape) rank)))))

(defun peeler (&rest amount-specifiers)
  (let ((min-rank (length amount-specifiers))
        (range-peelers (mapcar #'range-peeler amount-specifiers)))
    (lambda (shape)
      (when (< (shape-rank shape) min-rank)
        (error "~@<Cannot apply a rank ~R peeler to the shape ~S ~
                   that has rank ~R.~:@>"
               min-rank shape (shape-rank shape)))
      (make-shape
       (mapcar #'funcall range-peelers (shape-ranges shape))))))

(defun range-peeler (amount)
  (multiple-value-bind (lo hi stride)
      (trivia:match amount
        ((list (and lo (type unsigned-byte))
               (and hi (type unsigned-byte))
               (and stride (type unsigned-byte)))
         (values lo hi stride))
        ((list (and lo (type unsigned-byte))
               (and hi (type unsigned-byte)))
         (values lo hi 1))
        ((or (list (and amount (type unsigned-byte)))
             (and amount (type unsigned-byte)))
         (values amount amount 1))
        ((list)
         (values 0 0 1))
        (_
         (error "~@<Malformed peel amount ~S. ~
                    Must be either an unsigned integer, ~
                    or a list of up to three unsigned integers.~:@>"
                amount)))
    (lambda (range)
      (declare (range range))
      (if (range-emptyp range)
          range
          (let* ((start (range-start range))
                 (end (range-end range))
                 (step (range-step range)))
            (range (+ start (* lo step))
                   (- end (* hi step))
                   (* step stride)))))))
