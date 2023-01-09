;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.test-suite)

(defun run-tests (&rest tests)
  (with-test-harness
    (with-testing-backend
      (mapc #'funcall (alexandria:shuffle tests))))
  (values))

(defun run-petalisp-test-suite ()
  (format t "~&== Testing Petalisp ==~%")
  (print-platform-information)
  (format t "~&Git revision: ~a~%" (system-git-revision :petalisp))
  (print-system-statistics "petalisp.utilities")
  (print-system-statistics "petalisp.core")
  (print-system-statistics "petalisp.ir")
  (print-system-statistics "petalisp.xmas-backend")
  (print-system-statistics "petalisp.api")
  (print-package-statistics :petalisp)
  (apply #'run-tests (all-tests)))
