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
    (find-if #'program-in-path-p '("xdg-open" "okular" "evince" "xpdf"))))

(defparameter *petalisp-view-directory*
  (uiop:xdg-cache-home "petalisp" "view"))

(defun file-open-p (file)
  #+windows
  t
  #-windows
  (plusp
   (length
    (uiop:run-program
     (list "lsof" "-t" (uiop:native-namestring file))
     :output :string
     :ignore-error-status t))))

(defun purge-unused-files-in-directory (directory)
  "Delete all files in the supplied directory that are not currently open."
  (dolist (file (uiop:directory-files directory))
    (unless (file-open-p file)
      (delete-file file))))

(defun view
    (graph-root
     &key
       (viewer *graphviz-default-viewer*)
       (graph (graphviz-default-graph graph-root)))
  (when (symbolp graph)
    (setf graph (make-instance graph)))
  (let* ((directory *petalisp-view-directory*)
         (name (format nil "graph~36,10,'0R.pdf" (random (expt 36 10))))
         (file (uiop:merge-pathnames* name directory))
         (graph (cl-dot:generate-graph-from-roots graph (alexandria:ensure-list graph-root) '(:margin 0.0))))
    (purge-unused-files-in-directory directory)
    (cl-dot:dot-graph graph file :format :pdf)
    (uiop:run-program (list viewer (uiop:native-namestring file))))
  graph-root)
