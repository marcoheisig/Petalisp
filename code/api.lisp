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
  (let (non-overlapping-spaces)
    (dolist (b (list* object more-objects))
      (dolist (a non-overlapping-spaces)
        (let ((a∩b (intersection a b)))
          (cond
            ((not a∩b)
             (push b non-overlapping-spaces))
            (t
             (push (<- b a∩b) non-overlapping-spaces)
             (dolist (difference (difference a b))
               (push difference non-overlapping-spaces))
             (dolist (difference (difference b a))
               (push difference non-overlapping-spaces)))))))
    (apply #'fusion non-overlapping-spaces)))

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

(defmacro subspace (space &rest dimensions)
  (with-gensyms (dim)
    (once-only (space)
      `(symbol-macrolet
           ((,(intern "START") (range-start (aref (ranges ,space) ,dim)))
            (,(intern "STEP") (range-step (aref (ranges ,space) ,dim)))
            (,(intern "END") (range-end (aref (ranges ,space) ,dim))))
         (make-index-space
          ,@(loop for form in dimensions and d from 0
                  collect `(let ((,dim ,d)) (range ,@form))))))))
