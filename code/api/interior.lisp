;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun shape-interior (shape width)
  (~l
   (mapcar
    (lambda (range)
      (with-accessors ((start range-start)
                       (end range-end)
                       (step range-step)) range
        (let ((new-start (+ start (* step width)))
              (new-end (- end (* step width) -1)))
          (range new-start new-end step))))
    (shape-ranges shape))))

(defun array-interior (array width)
  (check-type width unsigned-byte)
  (reshape array (shape-interior (shape array) width)))
