;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defun normalize-kernel (kernel)
  (setf (recipe kernel) (normalize-kernel-recipe (recipe kernel)))
  kernel)

(defgeneric normalize-kernel-recipe (node)
  (:method ((node immediate)) node)
  (:method ((node application))
    (apply #'application (operator node) (mapcar #'normalize-kernel-recipe (inputs node))))
  (:method ((node reduction))
    (reduction (operator node) (normalize-kernel-recipe (input node))))
  (:method ((node fusion))
    (apply #'fusion (mapcar #'normalize-kernel-recipe (inputs node))))
  (:method ((node reference))
    (let ((predecessor (input node)))
      (etypecase predecessor
        (immediate
         (reference predecessor (index-space node) (transformation node)))
        (application
         (apply #'application (operator predecessor)
                (mapcar (λ predecessor
                           (normalize-kernel-recipe
                            (reference predecessor (index-space node) (transformation node))))
                        (inputs predecessor))))
        (reduction (error "TODO"))
        (fusion
         (apply #'fusion (mapcar (λ predecessor
                                    (normalize-kernel-recipe
                                     (reference predecessor
                                                (intersection (index-space predecessor)
                                                              (index-space node))
                                                (transformation node))))
                                 (inputs predecessor))))
        (reference (error "At this stage, there should be no consecutive references."))))))
