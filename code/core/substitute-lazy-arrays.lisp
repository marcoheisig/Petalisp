;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defvar *substitutions*)

(defun copy-lazy-arrays (lazy-arrays)
  (substitute-lazy-arrays lazy-arrays '() '()))

(defun substitute-lazy-arrays (roots new-lazy-arrays old-lazy-arrays)
  (let ((*substitutions* (make-hash-table :test #'eq)))
    (assert (= (length new-lazy-arrays) (length old-lazy-arrays)))
    (loop for new-lazy-array in new-lazy-arrays
          for old-lazy-array in old-lazy-arrays do
            (assert (shape-equal
                     (lazy-array-shape new-lazy-array)
                     (lazy-array-shape old-lazy-array)))
            (assert (petalisp.type-inference:ntype-subtypep
                     (lazy-array-ntype new-lazy-array)
                     (lazy-array-ntype old-lazy-array)))
            (setf (gethash old-lazy-array *substitutions*)
                  new-lazy-array))
    (mapcar #'substitute-lazy-array roots)))

(defun substitute-lazy-array (lazy-array)
  (declare (lazy-array lazy-array))
  (multiple-value-bind (substitution present-p)
      (gethash lazy-array *substitutions*)
    (if present-p
        substitution
        (setf (gethash lazy-array *substitutions*)
              (make-lazy-array
               :shape (lazy-array-shape lazy-array)
               :ntype (lazy-array-ntype lazy-array)
               :depth (lazy-array-depth lazy-array)
               :refcount (lazy-array-refcount lazy-array)
               :delayed-action
               (substitute-delayed-action (lazy-array-delayed-action lazy-array)))))))

(defgeneric substitute-delayed-action (delayed-action))

(defmethod substitute-delayed-action ((delayed-map delayed-map))
  (make-delayed-map
   :operator (delayed-map-operator delayed-map)
   :inputs (mapcar #'substitute-lazy-array (delayed-map-inputs delayed-map))))

(defmethod substitute-delayed-action ((delayed-multiple-value-map delayed-multiple-value-map))
  (make-delayed-multiple-value-map
   :operator (delayed-multiple-value-map-operator delayed-multiple-value-map)
   :ntypes (delayed-multiple-value-map-ntypes delayed-multiple-value-map)
   :inputs (mapcar #'substitute-lazy-array (delayed-multiple-value-map-inputs delayed-multiple-value-map))))

(defmethod substitute-delayed-action ((delayed-nth-value delayed-nth-value))
  (make-delayed-nth-value
   :number (delayed-nth-value-number delayed-nth-value)
   :input (substitute-lazy-array (delayed-nth-value-input delayed-nth-value))))

(defmethod substitute-delayed-action ((delayed-reshape delayed-reshape))
  (make-delayed-reshape
   :transformation (delayed-reshape-transformation delayed-reshape)
   :input (substitute-lazy-array (delayed-reshape-input delayed-reshape))))

(defmethod substitute-delayed-action ((delayed-fuse delayed-fuse))
  (make-delayed-fuse
   :inputs (mapcar #'substitute-lazy-array (delayed-fuse-inputs delayed-fuse))))

(defmethod substitute-delayed-action ((delayed-action delayed-action))
  ;; Other delayed actions need not be copied because they don't depend on
  ;; other lazy-arrays.
  delayed-action)
