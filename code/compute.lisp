;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; compute does the actual work when running a Petalisp application

(in-package :petalisp)

(defparameter *successors* nil)

(defun successors (node)
  (gethash node *successors*))

(defun (setf successors) (value node)
  (setf (gethash node *successors*) value))

(defun populate-successors (node &optional successor)
  (let ((already-visited? (successors node)))
    (awhen successor
      (push it (successors node)))
    (unless already-visited?
        (mapc #'populate-successors (predecessors node) (forever node)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  a simple reference implementation of COMPUTE

(defparameter *value-cache* nil)

(defmethod evaluate :around ((node data-structure))
  (acond
    ((not (and *value-cache* *successors*)) (call-next-method))
    ((gethash node *value-cache*) it)
    ((<= (length (successors node)) 1) (call-next-method))
    (t (setf (gethash node *value-cache*) (call-next-method)))))

(defmethod compute (&rest objects)
  (let ((*successors*  (make-hash-table :test #'eq))
        (*value-cache* (make-hash-table :test #'eq)))
    (let ((objects (mapcar #'lisp->petalisp objects)))
      (mapc #'populate-successors objects)
      (apply #'values
             (mapcar
              (compose #'depetalispify #'evaluate)
              objects)))))
