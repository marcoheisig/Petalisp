;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class kernel-target (strided-array-immediate)
  ((fragments :type (vector kernel-fragment))
   (unevaluated-fragment-counter :type non-negative-integer)))

(defun kernel-ready? (kernel-target)
  (zerop (unevaluated-fragment-counter kernel-target)))

(defmethod graphviz-successors ((purpose <data-flow-graph>) (kernel-target kernel-target))
  (fragments kernel-target))

(defmethod graphviz-node-plist append-plist ((purpose <data-flow-graph>) (kernel-target kernel-target))
  `(:shape "octagon"
    :fillcolor "cornflowerblue"))

(defmethod graphviz-edge-plist append-plist
    ((purpose <data-flow-graph>) (list list) (kernel-target kernel-target))
  `(:style "dashed"))

(defmethod graphviz-successors ((purpose <data-flow-graph>) (list cons))
  (ecase (car list)
    (application (cddr list))
    (reduction (cddr list))
    (reference (cddr list))))


(defmethod graphviz-node-plist append-plist ((purpose <data-flow-graph>) (list cons))
  (ecase (car list)
    (application `(:label ,(format nil "(α ~A)" (second list)) :fillcolor "indianred1"))
    (reduction `(:label ,(format nil "(β ~A)" (second list)) :fillcolor "indianred3"))
    (reference `(:label ,(format nil "~A" (second list)) :fillcolor "gray"))))

