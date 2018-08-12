;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(uiop:define-package :petalisp-test-suite
  (:shadowing-import-from :petalisp :set-difference)
  (:use
   :closer-common-lisp
   :alexandria
   :fiveam
   :petalisp
   :petalisp/examples/jacobi
   :petalisp/examples/red-black-gauss-seidel
   :petalisp/examples/linear-algebra))

