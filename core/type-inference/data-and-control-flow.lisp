;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/type-inference/data-and-control-flow
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/type-inference/inference))

(in-package :petalisp/core/type-inference/data-and-control-flow)

(register-type-inference-function 'identity #'identity)
