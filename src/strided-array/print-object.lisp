;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod print-object ((object strided-array-index-space) stream)
  (format stream "#i(~{~a~^ ~})"
          (mapcar
           (lambda (range)
             (list (range-start range)
                   (range-step range)
                   (range-end range)))
           (reverse (reverse-ranges object)))))
