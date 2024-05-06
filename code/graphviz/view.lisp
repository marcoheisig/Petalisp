;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.graphviz)

(defparameter *viewer*
  #+windows
  "chrome"
  #+darwin
  "open"
  #-(or windows darwin)
  (flet ((program-in-path-p (program)
           (multiple-value-bind (out err exit-code)
               (uiop:run-program
                (list "which" program)
                :ignore-error-status t)
             (declare (ignore out err))
             (zerop exit-code))))
    (find-if #'program-in-path-p '("xdg-open" "okular" "evince" "xpdf"))))

(defparameter *petalisp-view-directory*
  (uiop:xdg-cache-home (list "petalisp" "view/")))

(defun file-open-p (file)
  #+windows
  t ; Admittedly conservative.
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

(defun view (graph-roots &key (viewer *viewer*) (verbose nil))
  (let* ((graph-roots (alexandria:ensure-list graph-roots))
         (graph (graphviz-default-graph (first graph-roots)))
         (directory *petalisp-view-directory*)
         (name (format nil "graph~36,10,'0R.pdf" (random (expt 36 10))))
         (file (uiop:merge-pathnames* name directory))
         (graph (cl-dot:generate-graph-from-roots graph graph-roots '(:margin 0.0))))
    (ensure-directories-exist directory)
    (when verbose
      (cl-dot:print-graph graph))
    (purge-unused-files-in-directory directory)
    (cl-dot:dot-graph graph file :format :pdf)
    (uiop:launch-program
     (list viewer (uiop:native-namestring file)))
    (values-list graph-roots)))
