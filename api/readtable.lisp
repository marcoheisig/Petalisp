;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-api)

(alexandria:define-constant +whitespace+ '(#\space #\tab #\linefeed #\return #\page)
  :test #'equal)

(defun stop-char-p (char)
  (or (member char +whitespace+)
      (char= char #\))))

(defun read-number-of-values (input-stream &optional (radix *read-base*))
  (let ((string
          (with-output-to-string (output-stream)
            (loop for char = (peek-char nil input-stream)
                  while (digit-char-p char radix)
                  do (write-char (read-char input-stream) output-stream)))))
    (if (zerop (length string))
        1
        (parse-integer string :radix radix))))

(defun read-α (stream char)
  (declare (ignore char))
  (if (stop-char-p (peek-char nil stream))
      'α
      `(lambda (&rest args)
         (apply 'α
                ,(read-number-of-values stream)
                #',(read stream) args))))

(defun read-β (stream char)
  (declare (ignore char))
  (if (stop-char-p (peek-char nil stream))
      `β
      `(lambda (&rest args)
         (apply 'β #',(read stream) args))))

(named-readtables:defreadtable petalisp-readtable
  (:merge :common-lisp)
  (:macro-char #\α 'read-α t)
  (:macro-char #\β 'read-β t))

