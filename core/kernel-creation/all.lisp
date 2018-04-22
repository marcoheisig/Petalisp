;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/kernel-creation/all
  (:use :closer-common-lisp :alexandria)
  (:use-reexport
   :petalisp/core/kernel-creation/kernel
   :petalisp/core/kernel-creation/kernelize))
