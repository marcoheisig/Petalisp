;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defvar *substitutions*)

(defun copy-arrays (arrays)
  (substitute-arrays arrays '() '()))

(defun substitute-arrays (roots new-arrays old-arrays)
  (let ((*substitutions* (make-hash-table :test #'eq)))
    (loop for new-array in new-arrays
          for old-array in old-arrays do
            (assert (petalisp.type-inference:ntype=
                     (petalisp.type-inference:generalize-ntype
                      (element-ntype new-array))
                     (petalisp.type-inference:generalize-ntype
                      (element-ntype old-array))))
            (assert (shape-equal
                     (array-shape new-array)
                     (array-shape old-array)))
            (setf (gethash old-array *substitutions*) new-array))
    (mapcar #'substitute-array roots)))

(defgeneric substitute-array (array))

(defmethod substitute-array :around ((lazy-array lazy-array))
  (multiple-value-bind (substitution present-p)
      (gethash lazy-array *substitutions*)
    (if present-p
        substitution
        (setf (gethash lazy-array *substitutions*)
              (call-next-method)))))

(defmethod substitute-array ((lazy-map lazy-map))
  (make-instance 'lazy-map
    :operator (operator lazy-map)
    :shape (array-shape lazy-map)
    :ntype (element-ntype lazy-map)
    :inputs (mapcar #'substitute-array (inputs lazy-map))))

(defmethod substitute-array ((lazy-multiple-value-map lazy-multiple-value-map))
  (make-instance 'lazy-multiple-value-map
    :operator (operator lazy-map)
    :number-of-values (number-of-values lazy-map)
    :shape (array-shape lazy-map)
    :ntype (element-ntype lazy-map)
    :inputs (mapcar #'substitute-array (inputs lazy-map))))

(defmethod substitute-array ((lazy-multiple-value-ref lazy-multiple-value-ref))
  (make-instance 'lazy-multiple-value-map
    :value-n (value-n lazy-map)
    :shape (array-shape lazy-map)
    :ntype (element-ntype lazy-map)
    :inputs (mapcar #'substitute-array (inputs lazy-map))))

(defmethod substitute-array ((lazy-fuse lazy-fuse))
  (make-instance 'lazy-fuse
    :shape (array-shape lazy-fuse)
    :ntype (element-ntype lazy-fuse)
    :inputs (mapcar #'substitute-array (inputs lazy-fuse))))

(defmethod substitute-array ((lazy-reshape lazy-reshape))
  (make-instance 'lazy-reshape
    :ntype (element-ntype lazy-reshape)
    :shape (array-shape lazy-reshape)
    :transformation (transformation lazy-reshape)
    :inputs (list (substitute-array (input lazy-reshape)))))

(defmethod substitute-array ((lazy-array lazy-array))
  ;; All other kinds of lazy arrays (mostly immediates) are not copied.
  lazy-array)
