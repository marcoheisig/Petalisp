;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; For testing purposes, it is useful to compute the same recipes using
;;; different virtual machines and compare the result.
;;;
;;; The testing virtual machine is constructed from a sequence of other
;;; virtual machines. Each VM/SCHEDULE instruction is then dispatched among
;;; these and the results are compared. If there is a mismatch, an error is
;;; signaled.

(define-class testing-virtual-machine (virtual-machine)
  ((virtual-machines :type (simple-vector virtual-machine)
                     :initform (required-argument "virtual-machines"))))

(defmethod vm/schedule ((vm testing-virtual-machine) targets recipe)
  )
