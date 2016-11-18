;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;; compute does the actual work when running a Petalisp application

(in-package :petalisp)

(defparameter *successors* nil)

(defun successors (object)
  (gethash object *successors*))

(defun populate-successors (node &optional successor)
  (let ((first-visit (not (gethash node *successors*))))
    (when successor
      (pushnew successor (gethash node *successors*)))
    (when first-visit
      (mapc #'populate-successors (predecessors node) (forever node)))))

(defun work-node (x)
  (or (application? x) (reduction? x)))

(defmethod compute (&rest objects)
  (let ((*successors* (make-hash-table :test #'eq)))
    (mapc #'populate-successors objects)
    ;; identify iteration spaces
    (let ((foo *successors*))
      ;(break)
      foo)
    ;; allocation and scheduling

    ;; execution
    ))
