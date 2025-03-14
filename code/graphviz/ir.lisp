(in-package #:petalisp.graphviz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass ir-graph (petalisp-graph) ())

(defclass ir-edge (petalisp-edge) ())

(defclass load-edge (ir-edge) ())

(defclass store-edge (ir-edge) ())

(defclass input-edge (ir-edge) ())

(defclass output-edge (ir-edge) ())

(defmethod graphviz-default-graph ((node petalisp.ir:kernel))
  'ir-graph)

(defmethod graphviz-default-graph ((node petalisp.ir:buffer))
  'ir-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tasks as Subgraphs

(defvar *task-cluster-table*)

(defmethod cl-dot:generate-graph-from-roots :around
    ((graph ir-graph) roots &optional attributes)
  (declare (ignore graph roots attributes))
  (let ((*task-cluster-table* (make-hash-table)))
    (call-next-method)))

(defun task-cluster (task)
  (alexandria:ensure-gethash
   task
   *task-cluster-table*
   (make-instance 'cl-dot:cluster
     :attributes
     (list :label (format nil "task-~D" (petalisp.ir:task-number task))
           :penwidth 2.0
           :color "gray"))))

(defmethod cl-dot:graph-object-cluster ((graph ir-graph) (kernel petalisp.ir:kernel))
  (task-cluster (petalisp.ir:kernel-task kernel)))

(defmethod cl-dot:graph-object-cluster ((graph ir-graph) (buffer petalisp.ir:buffer))
  (task-cluster (petalisp.ir:buffer-task buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Connectivity

(defmethod graphviz-potential-edges append
    ((graph ir-graph) node)
  (list (make-instance 'load-edge)
        (make-instance 'input-edge)))

(defmethod graphviz-incoming-edge-origins
    ((graph ir-graph)
     (edge input-edge)
     (buffer petalisp.ir:buffer))
  (petalisp.utilities:with-collectors ((kernels collect))
    (petalisp.ir:map-buffer-inputs #'collect buffer)
    (kernels)))

(defmethod graphviz-outgoing-edge-targets
    ((graph ir-graph)
     (edge output-edge)
     (buffer petalisp.ir:buffer))
  (petalisp.utilities:with-collectors ((kernels collect))
    (petalisp.ir:map-buffer-outputs #'collect buffer)
    (kernels)))

(defmethod graphviz-incoming-edge-origins
    ((graph ir-graph)
     (edge load-edge)
     (kernel petalisp.ir:kernel))
  (let ((buffers '()))
    (petalisp.ir:map-kernel-inputs
     (lambda (buffer) (push buffer buffers))
     kernel)
    buffers))

(defmethod graphviz-outgoing-edge-targets
    ((graph ir-graph)
     (edge store-edge)
     (kernel petalisp.ir:kernel))
  (let ((buffers '()))
    (petalisp.ir:map-kernel-outputs
     (lambda (buffer) (push buffer buffers))
     kernel)
    buffers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Appearance

(defmethod graphviz-node-attributes
    ((graph ir-graph)
     (node petalisp.ir:kernel))
  `(:fillcolor "#B2DF8A"))

(defmethod graphviz-node-attributes
    ((graph ir-graph)
     (node petalisp.ir:buffer))
  `(:fillcolor "#ABCEE3"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Node Labels

(defmethod graphviz-node-properties append
    ((graph ir-graph)
     (buffer petalisp.ir:buffer))
  `(("shape" . ,(stringify (petalisp.ir:buffer-shape buffer)))
    ("type" . ,(stringify (typo:ntype-type-specifier (petalisp.ir:buffer-ntype buffer))))
    ("depth" . ,(stringify (petalisp.ir:buffer-depth buffer)))
    ("number" . ,(stringify (petalisp.ir:buffer-number buffer)))
    ("reuse-potential" . ,(stringify (petalisp.ir:buffer-reuse-potential buffer)))
    ,@(when (petalisp.ir:buffer-storage buffer)
        `(("storage" . ,(stringify (type-of (petalisp.ir:buffer-storage buffer))))))))

(defun hide-buffers (references)
  (subst-if :buffer #'petalisp.ir:bufferp references))

(defun simplify-input (input)
  (destructuring-bind (value-n . instruction) input
    (cons value-n (petalisp.ir:instruction-number instruction))))

(defmethod graphviz-node-properties append
    ((graph ir-graph)
     (kernel petalisp.ir:kernel))
  `(("iteration-space" . ,(stringify (petalisp.ir:kernel-iteration-space kernel)))
    ("number" . ,(stringify (petalisp.ir:kernel-number kernel)))
    ("reuse-potential" . ,(stringify (petalisp.ir:kernel-reuse-potential kernel)))
    ,@(let ((instructions '()))
        (petalisp.ir:map-kernel-instructions
         (lambda (instruction)
           (push instruction instructions))
         kernel)
        (loop for instruction in (sort instructions #'< :key #'petalisp.ir:instruction-number)
              collect
              (cons
               (format nil "instruction ~2D" (petalisp.ir:instruction-number instruction))
               (etypecase instruction
                 (petalisp.ir:call-instruction
                  (format nil "~S~{ ~S~}~%"
                          (or
                           (typo:fnrecord-name
                            (petalisp.ir:call-instruction-fnrecord instruction))
                           (typo:fnrecord-function
                            (petalisp.ir:call-instruction-fnrecord instruction)))
                          (mapcar #'simplify-input
                                  (petalisp.ir:instruction-inputs instruction))))
                 (petalisp.ir:iref-instruction
                  (format nil "iref ~S~%"
                          (petalisp.ir:instruction-transformation instruction)))
                 (petalisp.ir:load-instruction
                  (format nil "load ~S ~S~%"
                          (typo:ntype-type-specifier
                           (petalisp.ir:buffer-ntype
                            (petalisp.ir:load-instruction-buffer instruction)))
                          (petalisp.ir:instruction-transformation instruction)))
                 (petalisp.ir:store-instruction
                  (format nil "store ~S ~S ~S~%"
                          (typo:ntype-type-specifier
                           (petalisp.ir:buffer-ntype
                            (petalisp.ir:store-instruction-buffer instruction)))
                          (petalisp.ir:instruction-transformation instruction)
                          (simplify-input
                           (first
                            (petalisp.ir:instruction-inputs instruction)))))))))))
