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
  (let ((intersection (intersection space-1 space-2)))
    (unless intersection (return-from difference `(,space-1)))
    (let ((start-1 (range-start space-1))
          (step-1 (range-step space-1))
          (end-1 (range-end space-1))
          (i-start (range-start intersection))
          (i-step (range-step intersection))
          (i-end (range-end intersection)))
      (flet ((range (start step end)
               (when (<= start-1 start end end-1)
                 (range start step end))))
        (loop for x from i-start below i-end by i-step
              when (range (+ x step-1) step-1 (- (+ x i-step) step-1))
                collect it into result
              finally
                 (awhen (range start-1 step-1 (- i-start step-1))
                   (push it result))
                 (awhen (range (+ i-end step-1) step-1 end-1)
                   (push it result))
                 (return result))))))
