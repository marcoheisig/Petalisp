;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class kernel-target (strided-array-immediate)
  ((fragments :type (vector kernel-fragment)))
  (:documentation
   "The kernel is a fundamental building block of Petalisp evaluation. Its
   RECIPE is a graph of data structures, whose nodes are the input of at
   most one other data structure."))

(define-class kernel-fragment ()
  ((target :type kernel-target :accessor target)
   (recipe)
   (bindings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Graphviz visualization of graphs containing kernels

(defmethod graphviz-successors ((purpose <data-flow-graph>) (kernel-target kernel-target))
  (fragments kernel-target))

(defmethod graphviz-successors ((purpose <data-flow-graph>) (kernel-fragment kernel-fragment))
  (list (recipe kernel-fragment)))

(defmethod graphviz-successors ((purpose <data-flow-graph>) (list cons))
  (ecase (car list)
    (application (cddr list))
    (reduction (cddr list))
    (reference (cddr list))))

(defmethod graphviz-node-plist append-plist ((purpose <data-flow-graph>) (kernel-target kernel-target))
  `(:label ,(format nil "kernel ~A" (index-space kernel-target))
    :shape "octagon"
    :fillcolor "cornflowerblue"))

(defmethod graphviz-node-plist append-plist ((purpose <data-flow-graph>) (list cons))
  (ecase (car list)
    (application `(:label ,(format nil "(α ~A)" (second list)) :fillcolor "indianred1"))
    (reduction `(:label ,(format nil "(β ~A)" (second list)) :fillcolor "indianred3"))
    (reference `(:label ,(format nil "~A" (second list)) :fillcolor "gray"))))

(defmethod graphviz-edge-plist append-plist
    ((purpose <data-flow-graph>) (list list) (kernel-target kernel-target))
  `(:style "dashed"))

