;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class virtual-machine ()
  ()
  (:documentation
   "A virtual machine is an abstraction over a set of hardware
   resources. All handling of kernels --- such as performance analysis,
   compilation and execution --- is done in the context of a particular
   virtual machine."))

(defparameter *virtual-machine* nil)

(defgeneric vm-bind-memory (virtual-machine immediate))

(defgeneric vm-free-memory (virtual-machine immediate))

(defgeneric vm-compile (virtual-machine kernel))

(defgeneric vm-execute (virtual-machine kernel))

(defun vm/bind-memory (immediate)
  (vm-bind-memory *virtual-machine* immediate))

(defun vm/free-memory (immediate)
  (vm-free-memory *virtual-machine* immediate))

(defun vm/compile (kernel)
  (vm-compile *virtual-machine* kernel))

(defun vm/execute (kernel)
  (vm-execute *virtual-machine* kernel))
