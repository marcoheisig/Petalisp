;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(uiop:define-package :petalisp/core/type-inference/all
  (:use :closer-common-lisp :alexandria)
  (:use-reexport :petalisp/core/type-inference/inference)
  (:use
   :petalisp/core/type-inference/numbers
   :petalisp/core/type-inference/data-and-control-flow))
