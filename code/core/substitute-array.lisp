;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defvar *substitutions*)

(defun copy-arrays (arrays)
  (substitute-arrays arrays '() '()))

(defun substitute-arrays (roots new-arrays old-arrays)
  (let ((*substitutions* (make-hash-table :test #'eq)))
    (loop for new-array in new-arrays
          for old-array in old-arrays do
            (setf (gethash old-array *substitutions*)
                  (reshape
                   (α #'coerce new-array (element-type old-array))
                   (shape old-array))))
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
    :value-n (value-n lazy-map)
    :shape (shape lazy-map)
    :ntype (element-ntype lazy-map)
    :inputs (mapcar #'substitute-array (inputs lazy-map))))

(defmethod substitute-array ((lazy-reduce lazy-reduce))
  (make-instance 'lazy-reduce
    :operator (operator lazy-reduce)
    :value-n (value-n lazy-reduce)
    :shape (shape lazy-reduce)
    :ntype (element-ntype lazy-reduce)
    :inputs (mapcar #'substitute-array (inputs lazy-reduce))))

(defmethod substitute-array ((fusion fusion))
  (make-instance 'fusion
    :shape (shape fusion)
    :ntype (element-ntype fusion)
    :inputs (mapcar #'substitute-array (inputs fusion))))

(defmethod substitute-array ((reference reference))
  (make-instance 'reference
    :ntype (element-ntype reference)
    :shape (shape reference)
    :transformation (transformation reference)
    :inputs (list (substitute-array (input reference)))))

(defmethod substitute-array ((lazy-array lazy-array))
  ;; All other kinds of lazy arrays (mostly immediates) are not copied.
  lazy-array)
