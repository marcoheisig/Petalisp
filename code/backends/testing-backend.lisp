;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;; For testing purposes, it is useful to compute the same recipes using
;;; different backends and compare the result.
;;;
;;; The testing backend is constructed from a sequence of other
;;; backends. Each COMPUTE-IMMEDIATES instruction is then dispatched among
;;; these and the results are compared. If there is a mismatch, an error is
;;; signaled.

(defclass testing-backend (backend)
  ((%backends :initarg :backends
              :reader backends
              :initform (required-argument "backends")
              :type sequence)))

(defmethod compute-immediates (data-structures (backend testing-backend))
  (let ((results
          (mapcar
           (lambda (backend)
             (compute-immediates data-structures backend))
           (backends backend))))
    (unless (identical results :test (lambda (v1 v2)
                                       (every (lambda (a b) (data-structure-equality a b)) v1 v2)))
      (error "Different backends compute different results for the same recipes:~%~A"
             results))
    (first results)))
