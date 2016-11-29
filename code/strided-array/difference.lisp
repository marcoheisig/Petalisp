;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod difference ((space-1 strided-array-index-space)
                       (space-2 strided-array-index-space))
  (let ((intersection (intersection space-1 space-2)))
    (unless intersection (return-from difference `(,space-1)))
    (loop for r1 across (ranges space-1)
          and r2 across (ranges space-2)
          and i from 0
          nconcing
          (loop for difference in (difference r1 r2)
                collect (let ((ranges (copy-array (ranges space-1))))
                          (replace ranges (ranges intersection) :end1 i)
                          (setf (aref ranges i) difference)
                          (make-instance
                           'strided-array-index-space
                           :ranges ranges))))))

(defmethod difference ((space-1 range) (space-2 range))
  ;; we only care about the part of space-2 that intersects with space-1
  (let ((space-2 (intersection space-1 space-2)))
    (unless space-2 (return-from difference `(,space-1)))
    (let ((start-1 (range-start space-1))
          (step-1 (range-step space-1))
          (end-1 (range-end space-1))
          (start-2 (range-start space-2))
          (step-2 (range-step space-2))
          (end-2 (range-end space-2)))
      (flet ((range (start step end)
               (when (<= start-1 start end end-1)
                 (range start step end))))
        (loop for x from start-2 below end-2 by step-2
              when (range (+ x step-1) step-1 (- (+ x step-2) step-1))
                collect it into result
              finally
                 (awhen (range start-1 step-1 (- start-2 step-1))
                   (push it result))
                 (awhen (range (+ end-2 step-1) step-1 end-1)
                   (push it result))
                 (return result))))))
