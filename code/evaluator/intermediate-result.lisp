;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class intermediate-result (strided-array-constant)
  ((storage :type (or array null) :initform nil)
   (kernels :type (vector kernel))
   (users :initform nil
          :type (vector kernel)
          :accessor users)
   (refcount :accessor refcount :initform nil)))

(defmethod graphviz-successors ((purpose data-flow-graph) (intermediate-result intermediate-result))
  (kernels intermediate-result))

(defmethod graphviz-node-plist append-plist ((purpose data-flow-graph) (intermediate-result intermediate-result))
  `(:shape "octagon"
    :fillcolor "cornflowerblue"))

(defmethod graphviz-edge-plist append-plist
    ((purpose data-flow-graph) (a kernel) (b immediate))
  `(:style "dashed"))

