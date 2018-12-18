;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

(defvar *graphviz-default-viewer*
  (flet ((program-in-path-p (program)
           (multiple-value-bind (out err exit-code)
               (uiop:run-program
                (list "which" program)
                :ignore-error-status t)
             (declare (ignore out err))
             (zerop exit-code))))
    (find-if #'program-in-path-p '("evince" "okular" "xpdf"))))

(defvar *graphviz-default-format* :pdf)

(defun view (graph-root &key
                          (format *graphviz-default-format*)
                          (viewer *graphviz-default-viewer*)
                          (graph (graphviz-default-graph graph-root)))
  (when (symbolp graph)
    (setf graph (make-instance graph)))
  (uiop:with-temporary-file (:pathname image-file)
    (cl-dot:dot-graph
     (cl-dot:generate-graph-from-roots graph (alexandria:ensure-list graph-root))
     image-file
     :format format)
    (uiop:run-program
     (list viewer (uiop:native-namestring image-file))))
  graph-root)
