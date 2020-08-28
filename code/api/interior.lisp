;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun shape-interior (shape &optional (width 1))
  (make-shape
   (mapcar
    (lambda (range)
      (with-accessors ((start range-start)
                       (end range-end)
                       (step range-step)) range
        (let ((new-start (+ start (* step width)))
              (new-end (- end (* step width))))
          (range new-start new-end step))))
    (shape-ranges shape))))

(defun array-interior (array &optional (width 1))
  (check-type width unsigned-byte)
  (reshape array (shape-interior (array-shape array) width)))
