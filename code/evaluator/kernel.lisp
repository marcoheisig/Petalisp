;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; Each Petalisp kernel consists of several input data structures, a
;;; single output data structure (WIP: support multiple values) and a
;;; recipe.

(define-class kernel ()
  ((target :type intermediate-result :accessor target)
   (recipe :type t)
   (iteration-space :type index-space)
   (sources :type (vector immediate))))

(defmethod print-object ((object kernel) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (iteration-space object) stream)))

(defmethod graphviz-node-plist append-plist
    ((purpose data-flow-graph) (kernel kernel))
  `(:shape "box"
    :fillcolor "skyblue"))

(defmethod graphviz-successors
    ((purpose data-flow-graph) (kernel kernel))
  (sources kernel))

(defmethod graphviz-edge-plist append-plist
    ((purpose data-flow-graph) (a kernel) (b immediate))
  `(:style "dashed"))
