;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defparameter *compile-cache* (make-hash-table :test #'equalp))

(defun compile-form (form)
  (with-hash-table-memoization (form :multiple-values nil)
      *compile-cache*
    (print "Cache miss!")
    (print form)
    (compile nil form)))
