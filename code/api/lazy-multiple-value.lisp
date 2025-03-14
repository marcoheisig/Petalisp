(in-package #:petalisp.api)

(defun lazy-multiple-value (n-values function &rest arrays)
  (petalisp.core:lazy-map n-values function (broadcast arrays)))
