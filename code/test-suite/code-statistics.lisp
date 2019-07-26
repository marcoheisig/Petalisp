;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.test-suite)

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
      (if (zerop exit-code)
          output
          nil))))

(defun print-system-statistics (system &optional (stream *standard-output*))
  (loop
    for pathname in (system-source-file-pathnames system)
    summing (count #\newline (read-file-into-string pathname)) into lines-of-code
    counting pathname into files
    finally
       (format stream "The system ~s consists of ~d lines of code in ~d file~:P.~%"
               (asdf:primary-system-name system) lines-of-code files)))

(defun class-name-p (symbol)
  (and (find-class symbol nil) t))

(defun macro-name-p (symbol)
  (and (macro-function symbol) t))

(defun defun-name-p (symbol)
  (and (fboundp symbol)
       (not (typep (symbol-function symbol) 'generic-function))))

(defun defgeneric-name-p (symbol)
  (and (fboundp symbol)
       (typep (symbol-function symbol) 'generic-function)))

(defun symbol-methods (symbol)
  (if (not (fboundp symbol))
      '()
      (let ((f (symbol-function symbol)))
        (if (typep f 'generic-function)
            (closer-common-lisp:generic-function-methods f)
            '()))))

(defun print-package-statistics (package &optional (stream *standard-output*))
  (loop for symbol being the present-symbols of package
        counting (class-name-p symbol) into classes
        counting (macro-name-p symbol) into macros
        counting (defun-name-p symbol) into defuns
        counting (defgeneric-name-p symbol) into defgenerics
        summing (length (symbol-methods symbol)) into methods
        finally
           (format
            stream
            "~<The package ~S defines ~D classes, ~D macros and ~D functions.  ~
               ~D of the functions are generic.  ~
               Each generic function has, on average, ~,2F methods.~:@>~%"
            (list
             (package-name (find-package package))
             classes macros (+ defuns defgenerics) defgenerics (float (/ methods defgenerics))))))

(defun print-platform-information (&optional (stream *standard-output*))
  (format stream "Implementation: ~:[Something weird~;~:*~a~]"
          (lisp-implementation-type))
  (format stream "~@[ ~a~]~%"
          (lisp-implementation-version))
  (format stream "Machine: ~:[a strange system~;~:*~a~]"
          (machine-type))
  (format stream "~@[ ~a~]~%"
          (machine-version)))

