;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun harmonized-element-type (arrays)
  (labels ((harmonize-ntypes (ntype1 ntype2)
             (if (and (typo:ntype-subtypep ntype1 (typo:type-specifier-ntype 'number))
                      (typo:ntype-subtypep ntype2 (typo:type-specifier-ntype 'number)))
                 (typo:ntype-contagion ntype1 ntype2)
                 (typo:ntype-union ntype1 ntype2))))
    (if (null arrays)
        'nil
        (typo:ntype-type-specifier
         (reduce #'harmonize-ntypes arrays
                 :key
                 (alexandria:compose #'petalisp.core:lazy-array-ntype #'lazy-array))))))

(defun harmonize (arrays)
  (let* ((lazy-arrays (mapcar #'lazy-array arrays))
         (target-element-type (harmonized-element-type lazy-arrays)))
    (mapcar (lambda (lazy-array)
              (lazy #'coerce lazy-array target-element-type))
            lazy-arrays)))

(defun lazy-fuse-and-harmonize (array &rest more-arrays)
  (apply #'lazy-fuse
         (harmonize
          (list* array more-arrays))))

(defun lazy-overwrite-and-harmonize (array &rest more-arrays)
  (apply #'lazy-overwrite
         (harmonize
          (list* array more-arrays))))
