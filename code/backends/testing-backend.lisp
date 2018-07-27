;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;; For testing purposes, it is useful to compute the same recipes using
;;; different backends and compare the result.
;;;
;;; The testing backend is constructed from a sequence of other
;;; backends. Each VM/SCHEDULE instruction is then dispatched among
;;; these and the results are compared. If there is a mismatch, an error is
;;; signaled.

(defclass testing-backend (backend)
  ((%backends :initarg :backends
              :reader backends
              :initform (required-argument "backends")
              :type sequence)))

(defmethod vm/schedule ((vm testing-backend) targets recipes)
  (let ((results
          (map 'vector
               (lambda (vm)
                 (let ((targets (map 'vector #'shallow-copy targets)))
                   (wait (vm/schedule vm targets recipes))
                   targets))
               (backends vm))))
    (unless (identical results :test (lambda (v1 v2)
                                       (every (lambda (a b) (data-structure-equality a b)) v1 v2)))
      (error "Different backends compute different results for the same recipes:~%~A"
             results))
    (loop for target across targets
          for vm-target across (elt results 0)
          do (setf (storage target) (storage vm-target)))
    (complete (make-request))))
