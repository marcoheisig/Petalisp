;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class kernel (strided-array-immediate)
  ((recipes :type (vector list) :accessor recipes)
   (bindings :type (vector (or kernel nil)))
   (cost :type non-negative-integer :accessor cost)
   (users :type list :initform nil :accessor users)
   (dependencies :type list :initform nil))
  (:documentation
   "The kernel is a fundamental building block of Petalisp evaluation. Its
   RECIPE is a graph of data structures, whose nodes are the input of at
   most one other data structure."))

(defun make-kernel (index-space recipes bindings)
  (make-instance 'kernel
    :index-space index-space
    :recipes recipes
    :bindings bindings
    :cost 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Graphviz visualization of graphs containing kernels

(defmethod graphviz-successors ((purpose <data-flow-graph>) (kernel kernel))
  (recipes kernel))

(defmethod graphviz-successors ((purpose <data-flow-graph>) (list cons))
  (ecase (car list)
    (application (cddr list))
    (reduction (cddr list))
    (reference (cddr list))))

(defmethod graphviz-node-plist append-plist ((purpose <data-flow-graph>) (kernel kernel))
  `(:label ,(format nil "kernel ~A" (index-space kernel))
    :shape "octagon"
    :fontsize 18))

(defmethod graphviz-node-plist append-plist ((purpose <data-flow-graph>) (list cons))
  (ecase (car list)
    (application `(:label ,(format nil "(α ~A)" (second list)) :fillcolor "indianred1"))
    (reduction `(:label ,(format nil "(β ~A)" (second list)) :fillcolor "indianred3"))
    (reference `(:label ,(format nil "~A" (second list)) :fillcolor "gray"))))

(defmethod graphviz-edge-plist append-plist ((purpose <data-flow-graph>) (list list) (kernel kernel))
  `(:style "dashed"))

