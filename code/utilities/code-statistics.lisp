;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defun system-source-file-pathnames (system)
  (mapcar
   #'asdf:component-pathname
   (remove-if-not
    (lambda (x) (typep x 'asdf:source-file))
    (asdf:required-components
     (asdf:find-system system)))))

(defun print-system-statistics (system &optional (stream *standard-output*))
  (loop :for pathname :in (system-source-file-pathnames system)
        :summing (count #\newline (read-file-into-string pathname))
          :into lines-of-code
        :counting pathname :into files
        :finally
           (format
            stream
            "The system ~s consists of ~d lines of code in ~d file~:P.~%"
            (asdf:primary-system-name system) lines-of-code files)))

(defun print-package-statistics (package &optional (stream *standard-output*))
  (loop :for sym :being :the :present-symbols :of package
        :counting (find-class sym nil) :into classes
        :when (macro-function sym)
          :count :it :into macros
        :else
          :count (fboundp sym) :into functions
        :end
        :finally
           (format
            stream
            "The package ~s defines ~d functions, ~d macros and ~d classes.~%"
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
