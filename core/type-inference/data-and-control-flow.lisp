;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(uiop:define-package :petalisp/core/type-inference/data-and-control-flow
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/type-inference/inference))

(in-package :petalisp/core/type-inference/data-and-control-flow)

(register-type-inferrer 'identity #'identity)
