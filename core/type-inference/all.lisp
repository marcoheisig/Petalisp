;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/type-inference/all
  (:use :closer-common-lisp :alexandria)
  (:use-reexport :petalisp/core/type-inference/inference)
  (:use
   :petalisp/core/type-inference/numbers))

(in-package :petalisp/core/type-inference/all)
