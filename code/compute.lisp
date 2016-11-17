;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;; compute does the actual work when running a Petalisp application

(in-package :petalisp)

(defparameter *node-table* nil)

(defun populate-node-table (object)
  (unless (gethash object *node-table*)
    (setf (gethash object *node-table*) object)
    (mapc #'populate-node-table (predecessors object))))

(defmethod compute (&rest objects)
  (let ((*node-table* (make-hash-table :test #'eq)))
    (mapc #'populate-node-table objects)
  ;; build graph
  ;; grouping
  ;; allocation and schedulting
  ;; execution
    ))
