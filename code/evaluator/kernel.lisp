;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; Each Petalisp kernel consists of several input data structures, a
;;; single output data structure (WIP: support multiple values) and a
;;; recipe.

(define-class kernel (strided-array-immediate)
  ((target :type intermediate-result :accessor target)
   (recipe :type hcons)
   (bindings :type (vector immediate))))

(defmethod graphviz-node-plist append-plist
    ((purpose data-flow-graph) (kernel kernel))
  `(:shape "box"
    :fillcolor "skyblue"))

(defmethod graphviz-successors
    ((purpose data-flow-graph) (kernel kernel))
  (bindings kernel))

(defmethod graphviz-edge-plist append-plist
    ((purpose data-flow-graph) (a kernel) (b immediate))
  `(:style "dashed"))
