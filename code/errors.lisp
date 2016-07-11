;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod generic-apply :before ((operator total-function) (object total-function)
                                  &rest more-objects)
  (assert (= (generic-dimension operator) (1+ (length more-objects))))
  (assert (not (find (generic-index-space object)
                     (mapcar #'generic-index-space more-objects)
                     :test (complement #'generic-equalp)))))

(defmethod generic-reduce :before ((operator total-function) (object total-function))
  (assert (< 1 (generic-dimension object))))

(defmethod generic-select ((object total-function) (space total-function))
  (assert (generic-equalp (generic-index-space space)
                          (generic-intersect object space))))

(defmethod generic-fuse ((object total-function) &rest more-objects)
  (assert (apply #'= (mapcar #'generic-dimension
                             (list* object more-objects)))))
