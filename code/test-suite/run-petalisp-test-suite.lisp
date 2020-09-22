;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.test-suite)

(defun run-petalisp-test-suite ()
  (format t "~&== Testing Petalisp ==~%")
  (print-platform-information)
  (format t "~&Git revision: ~a~%" (system-git-revision :petalisp))
  (print-system-statistics "petalisp.utilities")
  (print-system-statistics "petalisp.type-inference")
  (print-system-statistics "petalisp.core")
  (print-system-statistics "petalisp.ir")
  (print-system-statistics "petalisp.ir-backend")
  (print-system-statistics "petalisp.native-backend")
  (print-system-statistics "petalisp.api")
  (print-package-statistics :petalisp)
  (with-testing-backend
    (apply #'run-tests (all-tests))))
