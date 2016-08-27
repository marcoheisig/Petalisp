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

;;; fusion

;;; repetition

;;; source operations

(defmacro <- (object &rest subspaces-and-transformations)
  (apply #'reference object subspaces-and-transformations))
