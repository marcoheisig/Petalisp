;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod generic-size ((object strided-array))
  (reduce #'* (mapcar #'generic-size (ranges object))))

(defmethod generic-size ((range range))
  (1+ (the integer (/ (- (range-end range)
                         (range-start range))
                      (range-step range)))))
