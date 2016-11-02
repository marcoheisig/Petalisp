;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; (difference #i ((1 2 5)) #i((1 1 4)))
;;; (difference #i((1 1 5) (1 1 5)) #i((2 2 4) (2 2 4)))
;;; (difference #i((1 1 5) (1 1 5)) #i((1 2 5) (1 2 5)))
;;; (difference #i((1 1 9) (1 1 9) (1 1 9)) #i((1 8 9) (1 8 9) (1 8 9)))
(defmethod difference ((space-1 strided-array-index-space)
                       (space-2 strided-array-index-space))
  (let* ((ranges-1 (ranges space-1))
         (ranges-2 (ranges space-2))
         (intersection (intersection space-1 space-2))
         (dimension (length ranges-1))
         differences)
    (unless intersection (return-from difference space-1))
    (loop for i below dimension do
      (loop for difference in (difference (aref ranges-1 i)
                                          (aref ranges-2 i))
            do (let ((array (copy-array ranges-1)))
                 (replace array
                  (ranges intersection)
                  :end1 i)
                 (setf (aref array i) difference)
                 (push array differences))))
    (mapcar
     (lambda (ranges)
       (make-instance
        'strided-array-index-space
        :ranges ranges))
     differences)))

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
