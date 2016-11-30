;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod print-object ((object strided-array-index-space) stream)
  (format stream "~<(σ~@{ ~w~})~:>"
          (map 'list
               (lambda (range)
                 (if (= 1 (range-step range))
                     (list (range-start range)
                           (range-end range))
                     (list (range-start range)
                           (range-step range)
                           (range-end range))))
               (ranges object))))

(defmethod print-object ((object strided-array) stream)
  (print-unreadable-object (object stream :type t)
    (princ (index-space object) stream)))
