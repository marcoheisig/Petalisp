;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass strided-array-constant (strided-array lisp-input) ()
  (:documentation
   "A Petalisp object that returns the same value for all indices."))

(defmethod generic-input ((object t) &rest arguments)
  (declare (ignore arguments))
  (make-instance
   'strided-array-constant
   :lisp-object object))

(defun |#i-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  `(make-instance
    'strided-array-index-space
    :ranges
    (list ,@(loop for spec in (read stream t nil t)
                  collect
                  (if (atom spec)
                      `(range ,spec)
                      `(range ,@spec))))))

 (set-dispatch-macro-character #\# #\i #'|#i-reader|)
