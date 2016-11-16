;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod difference ((space-1 strided-array-index-space)
                       (space-2 strided-array-index-space))
  (let ((intersection
          (or (intersection space-1 space-2)
              (return-from difference `(,space-1)))))
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
  (let ((intersection
          (or (intersection space-1 space-2)
              (return-from difference `(,space-1)))))
    (let ((start (range-start space-1))
          (step (range-step space-1))
          (end (range-end space-1))
          (i-start (range-start intersection))
          (i-step (range-step intersection))
          (i-end (range-end intersection)))
      (flet ((valid (range)
               (<= start (range-start range) (range-end range) end)))
        (cond
          ((equalp intersection space-1)
           `())
          ((= 1 i-step)
           (remove-if-not
            #'valid
            `(,(range start step (- i-start step))
              ,(range (+ i-end step) step end))))
          ((and (evenp i-step) (= step (/ i-step 2)))
           (let ((lower (if (= start i-start)
                            (+ i-start step)
                            (- i-start step)))
                 (upper (if (= end i-end)
                            (- i-end step)
                            (+ i-end step))))
             (remove-if-not
              #'valid
              `(,(range start step (- i-start step step))
                ,(range lower i-step upper)
                ,(range (+ i-end step step) step end)))))
          (t
           (nconc
            (remove-if-not
             #'valid
             `(,(range start step (- i-start step))
               ,(range (+ i-end step) step end)))
            (when (> i-step step)
              (loop for x from i-start below i-end by i-step
                    collect
                    (range (+ x step) step (- (+ x i-step) step)))))))))))
