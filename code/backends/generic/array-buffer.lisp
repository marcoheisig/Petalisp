;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass array-buffer (buffer)
  (;; A mapping from indices of the array's shape to storage indices.
   (%transformation :initarg :transformation :accessor transformation)
   ;; The storage of the buffer.  Initially, the storage of a buffer is
   ;; NIL.  Before kernels can write to this buffer, its storage slot must
   ;; be set to an array of the appropriate size.
   (%storage :initarg :storage :initform nil :accessor storage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod initialize-instance ((buffer buffer) &key &allow-other-keys)
  (prog1 (call-next-method)
    (setf (transformation buffer)
          (collapsing-transformation (shape buffer)))))
