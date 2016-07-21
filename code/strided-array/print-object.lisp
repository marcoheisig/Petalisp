;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod print-object ((object strided-array-index-space) stream)
  (format stream "#i(~{~a~^ ~})"
          (mapcar
           (lambda (range)
             (list (range-start range)
                   (range-step range)
                   (range-end range)))
           (ranges object))))

(defmethod print-object ((object strided-array) stream)
  (print-unreadable-object (object stream :type t)
    (princ (index-space object) stream)
    (princ #\SPACE)
    (princ (codomain-type object) stream)))

