;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(uiop:define-package :petalisp-development
  (:nicknames :petalisp-dev)
  (:shadowing-import-from :petalisp :set-difference)
  (:use
   :closer-common-lisp
   :alexandria
   :fiveam
   :petalisp
   :petalisp-iterative-methods
   :petalisp-linear-algebra)
  (:export
   #:run-test-suite))

