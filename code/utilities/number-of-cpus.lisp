;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

(defun number-of-cpus ()
  (let ((number-of-cpus
          (ignore-errors
           (parse-integer
            (with-output-to-string (stream)
              (uiop:run-program
               (list "getconf" "_NPROCESSORS_ONLN")
               :output stream))))))
    ;; We use CHECK-TYPE here, because it automatically installs a
    ;; STORE-VALUE restart.
    (check-type number-of-cpus (integer 1) "a positive integer")
    number-of-cpus))
