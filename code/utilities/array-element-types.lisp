;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp-internals)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant upgraded-array-element-types
      (let ((floats '(short-float single-float double-float long-float))
            (miscellaneous '(base-char extended-char boolean t)))
        (remove-duplicates
         (mapcar #'upgraded-array-element-type
                 (append
                  floats
                  (loop for i from 1 to 64
                        collect `(unsigned-byte ,i)
                        collect `(signed-byte ,i))
                  (loop for float in floats collect `(complex ,float))
                  miscellaneous))
         :test #'type=
         :from-end t))
    :test #'equal))

(macrolet
    ((generate ()
       (flet ((make-atomic (type-specifier)
                (if (atom type-specifier)
                    type-specifier
                    (ecase (first type-specifier)
                      (complex
                       (ecase (second type-specifier)
                         (single-float 'c32)
                         (double-float 'c64)))
                      (signed-byte
                       (format-symbol t "I~D" (second type-specifier)))
                      (unsigned-byte
                       (format-symbol t "U~D" (second type-specifier)))))))
         `(progn
            ,@(loop for type-specifier in upgraded-array-element-types
                    unless (atom type-specifier)
                      collect
                    `(deftype ,(make-atomic type-specifier) () ',type-specifier))
            (defun atomic-array-element-type-specifier (array)
              (etypecase array
                ,@(loop for type-specifier in upgraded-array-element-types
                        collect
                        `((array ,type-specifier) ',(make-atomic type-specifier)))))))))
  (generate))
