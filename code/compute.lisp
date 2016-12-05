;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;; compute does the actual work when running a Petalisp application

(in-package :petalisp)

(defparameter *successors* nil)

(defun successors (object)
  (gethash object *successors*))

(defun populate-successors (node &optional successor)
  (let ((first-visit? (not (gethash node *successors*))))
    (when successor
      (pushnew successor (gethash node *successors*)))
    (when first-visit?
      (mapc #'populate-successors (predecessors node) (forever node)))))

;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;;  a simple reference implementation of COMPUTE
;;; _________________________________________________________________

(defparameter *value-cache* nil)

(defmethod evaluate :around ((node t))
  (if (not (and *value-cache* *successors*))
      (call-next-method)
      (aif (gethash node *value-cache*) it
           (if (<= (length (successors node)) 1)
               (call-next-method)
               (setf (gethash node *value-cache*)
                     (call-next-method))))))

(defmethod compute (&rest objects)
  (let ((*successors* (make-hash-table :test #'eq))
        (*value-cache* (make-hash-table :test #'eq)))
    (let ((objects (mapcar #'lisp->petalisp objects)))
      (mapc #'populate-successors objects)
      (apply #'values
             (mapcar
              (compose #'petalisp->lisp #'evaluate)
              objects)))))
