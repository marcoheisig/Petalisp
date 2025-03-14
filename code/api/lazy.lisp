(in-package #:petalisp.api)

(defun lazy (function &rest arrays)
  (petalisp.core:lazy-map 1 function (broadcast arrays)))
