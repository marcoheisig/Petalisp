(in-package #:petalisp.api)

(defun lazy-overwrite (array &rest more-arrays)
  ;; No need to overwrite when only a single array is supplied.
  (when (null more-arrays)
    (return-from lazy-overwrite (lazy-array array)))
  (let ((arrays (list* array more-arrays)))
    (multiple-value-bind (lazy-arrays result-shape)
        (petalisp.core:broadcast-for-fusion arrays)
      (let ((identity (petalisp.core:identity-transformation (shape-rank result-shape))))
        (apply
         #'lazy-fuse
         (mapcar
          (lambda (fragment)
            (destructuring-bind (shape . bitmask) fragment
              (petalisp.core:lazy-ref
               (nth (1- (integer-length bitmask)) lazy-arrays)
               shape
               identity)))
          (petalisp.core:subdivide-arrays lazy-arrays)))))))
