;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.graphviz)

(defparameter *graphviz-default-viewer*
  #+windows
  "chrome"
  #-windows
  (flet ((program-in-path-p (program)
           (multiple-value-bind (out err exit-code)
               (uiop:run-program
                (list "which" program)
                :ignore-error-status t)
             (declare (ignore out err))
             (zerop exit-code))))
    (find-if #'program-in-path-p '("okular" "evince" "xpdf"))))

(defparameter *graphviz-default-format* :pdf)

(defun view (graph-root &key
                          (format *graphviz-default-format*)
                          (viewer *graphviz-default-viewer*)
                          (graph (graphviz-default-graph graph-root)))
  (when (symbolp graph)
    (setf graph (make-instance graph)))
  (uiop:with-temporary-file (:pathname image-file)
    (cl-dot:dot-graph
     (cl-dot:generate-graph-from-roots graph (alexandria:ensure-list graph-root) '(:margin 0.0))
     image-file
     :format format)
    (uiop:run-program
     (list viewer (uiop:native-namestring image-file))))
  graph-root)
