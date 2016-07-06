;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod generic-size ((object strided-array))
  (apply #'* (mapcar #'range-elements (ranges object))))
