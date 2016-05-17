;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Input and output for petalisp datastructures

(defgeneric slot-info (object)
  (:method-combination append :most-specific-last))

(defparameter *io-readtable* (copy-readtable))

(defun read-object (stream char)
  (declare (ignore char))
  (apply #'make-instance (read-delimited-list #\] stream t)))

(set-macro-character #\[ #'read-object nil *io-readtable*)
(set-syntax-from-char #\] #\) *io-readtable*)

(defun print-object-readably (object stream)
  (let ((*package* (find-package '#:keyword)))
    (pprint-logical-block (stream nil :prefix "[" :suffix "]")
      (format stream "~s ~2i" (class-name (class-of object)))
      (loop for (initarg reader) in (slot-info object) do
        (format stream "~_~s ~_~W"
                initarg
                (funcall reader object))))))

(defmacro defio (class &body slot-info)
  `(progn
     (defmethod print-object ((object ,class) stream)
           (print-object-readably object stream))

     (defmethod slot-info append ((object ,class))
       ',slot-info)))
