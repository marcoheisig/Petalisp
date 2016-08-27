;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defun α (operator object &rest more-objects)
  (let* ((objects
           (mapcar #'source (list* object more-objects)))
         (index-space
           (reduce #'broadcast objects))
         (objects
           (mapcar
            (lambda (object)
              (repetition object index-space))
            objects))
         (operator (find-operator operator)))
    (apply #'application operator objects)))

(defun β (operator object)
  (reduction operator object))

(defun fuse (object &rest more-objects)
  (apply #'fusion object more-objects))

(defun repeat (object space)
  (repetition object space))

(defun <- (object &rest subspaces-and-transformations)
  (let ((target-space (index-space object))
        (transformation (identity-transformation (dimension object))))
    (dolist (x subspaces-and-transformations)
      (etypecase x
        (transformation
         (zapf target-space (transform % x))
         (zapf transformation (compose x %)))
        (index-space
         (assert (subspace-p x target-space))
         (setf target-space x))))
    (let ((source-space (transform target-space (invert transformation))))
      (reference object source-space transformation))))
