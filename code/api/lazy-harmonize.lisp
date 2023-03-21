;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun harmonized-element-type (&rest arrays)
  (labels ((harmonize (ntype1 ntype2)
             (if (or (typo:ntype-subtypepc2 ntype1 (typo:type-specifier-ntype 'number))
                     (typo:ntype-subtypepc2 ntype2 (typo:type-specifier-ntype 'number)))
                 (typo:ntype-union ntype1 ntype2)
                 (typo:ntype-contagion ntype1 ntype2))))
    (if (null arrays)
        'nil
        (typo:ntype-type-specifier
         (reduce #'harmonize arrays :key (alexandria:compose #'lazy-array-ntype #'lazy-array))))))

(defun lazy-harmonize (&rest arrays)
  (values-list
   (lazy-harmonize-list-of-arrays arrays)))

(defun lazy-harmonize-list-of-arrays (arrays)
  (let* ((lazy-arrays (mapcar #'lazy-array arrays))
         (target-element-type (apply #'harmonized-element-type lazy-arrays)))
    (mapcar (lambda (lazy-array)
              (lazy #'coerce lazy-array target-element-type))
            lazy-arrays)))

(defun lazy-fuse-and-harmonize (array &rest more-arrays)
  (apply #'lazy-fuse
         (lazy-harmonize-list-of-arrays
          (list* array more-arrays))))

(defun lazy-overwrite-and-harmonize (array &rest more-arrays)
  (apply #'lazy-overwrite
         (lazy-harmonize-list-of-arrays
          (list* array more-arrays))))
