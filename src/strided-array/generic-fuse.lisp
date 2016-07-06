;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass strided-array-fusion (strided-array fusion) ())

(defmethod generic-fuse ((object strided-array) &rest more-objects)
  ;; TODO
  objects)
