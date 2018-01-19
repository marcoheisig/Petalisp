;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/kernel-creation/kernelize
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/all
   :petalisp/core/kernel-creation/map-subtrees
   :petalisp/core/kernel-creation/map-subtree-fragments
   :petalisp/core/kernel-creation/build-kernel)
  (:export
   #:kernelize))

(in-package :petalisp/core/kernel-creation/kernelize)

(defun kernelize (graph-roots)
  "Translate the data flow graph specified by the given GRAPH-ROOTS to a
graph of immediates and kernels. Return the roots of this new graph."
  (map-subtrees #'kernelize-subtree graph-roots))

(defun kernelize-subtree (target root leaf-function)
  (dx-flet ((kernelize-subtree-fragment (index-space dimension)
              (build-kernel target root leaf-function index-space dimension)))
    (setf (kernels target)
          (map-subtree-fragments #'kernelize-subtree-fragment root leaf-function))))
