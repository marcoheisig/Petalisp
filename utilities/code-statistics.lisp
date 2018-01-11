;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/utilities/code-statistics
  (:use :closer-common-lisp :alexandria)
  (:export
   #:system-source-file-pathnames
   #:system-git-revision
   #:print-system-statistics
   #:print-package-statistics
   #:print-platform-information))

(in-package :petalisp/utilities/code-statistics)

(defun system-source-file-pathnames (system)
  (mapcar
   #'asdf:component-pathname
   (remove-if-not
    (lambda (x) (typep x 'asdf:source-file))
    (asdf:required-components
     (asdf:find-system system)))))

(defun system-git-revision (system)
  (uiop:with-current-directory
      ((asdf:system-source-directory
        (asdf:find-system system)))
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program
         (list "git" "rev-parse" "HEAD")
         :output '(:string :stripped t)
         :ignore-error-status t)
      (declare (ignore error-output))
      (when (= 0 exit-code) output))))

(defun print-system-statistics (system &optional (stream *standard-output*))
  (loop
    for pathname in (system-source-file-pathnames system)
    summing (count #\newline (read-file-into-string pathname)) into lines-of-code
    counting pathname into files
    finally
       (format stream "The system ~s consists of ~d lines of code in ~d file~:P.~%"
               (asdf:primary-system-name system) lines-of-code files)))

(defun print-package-statistics (package &optional (stream *standard-output*))
  (loop
    for sym being the present-symbols of package
    counting (find-class sym nil) into classes
    counting (macro-function sym) into macros
    counting (fboundp sym) into functions
    finally
       (format stream "The package ~s defines ~d functions, ~d macros and ~d classes.~%"
               (package-name (find-package package)) functions macros classes)))

(defun print-platform-information (&optional (stream *standard-output*))
  (format stream "Implementation: ~:[Something weird~;~:*~a~]"
          (lisp-implementation-type))
  (format stream "~@[ ~a~]~%"
          (lisp-implementation-version))
  (format stream "Machine: ~:[a strange system~;~:*~a~]"
          (machine-type))
  (format stream "~@[ ~a~]~%"
          (machine-version)))

