;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/virtual-machines/virtual-machine
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all)
  (:export
   #:virtual-machine
   #:schedule
   #:vm/schedule))

(in-package :petalisp/core/virtual-machines/virtual-machine)

(defclass virtual-machine () ()
  (:documentation
   "A virtual machine is an abstraction over a set of hardware
resources. All handling of kernels --- such as performance analysis,
compilation and execution --- is done in the context of a particular
virtual machine."))

(defgeneric vm/schedule (virtual-machine targets recipes)
  (:documentation
   "Instruct VIRTUAL-MACHINE to compute all given GRAPH-ROOTS
asynchronously. Return an object of type REQUEST that can be used to block
until the task is complete."))
