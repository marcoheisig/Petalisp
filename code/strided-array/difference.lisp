;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; (difference #i((1 1 5) (1 1 5)) #i((2 2 4) (2 2 4)))
;;; (difference #i((1 1 5) (1 1 5)) #i((1 2 5) (1 2 5)))
(defmethod difference ((space-1 strided-array-index-space)
                       (space-2 strided-array-index-space))
  (labels ((rec (s1 s2)
             (when s1
               (append
                (let ((head (intersection (car s1) (car s2))))
                  (mapcar
                   (lambda (tail) (cons head tail))
                   (rec (cdr s1) (cdr s2))))
                (let ((tail (cdr s1)))
                  (mapcar
                   (lambda (head) (cons head tail))
                   (difference (car s1) (car s2))))))))
    (mapcar
     (lambda (ranges)
       (make-instance
        'strided-array-index-space
        :ranges ranges))
     (rec (ranges space-1) (ranges space-2)))))

(defmethod difference ((space-1 range) (space-2 range))
  (let ((intersection
          (or (intersection space-1 space-2)
              ;; (difference (range 1 3 7) (range 8 1 22))
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
          ;; (difference (range 1 3 7) (range 1 3 10))
          ((equalp intersection space-1)
           `())
          ((= 1 i-step)
           (remove-if-not
            #'valid
            `(,(range start step (- i-start step))
              ,(range (+ i-end step) step end))))
          ;; (difference (range 1 3 13) (range 4 2 10))
          ;; (difference (range 0 1 9) (range 2 2 4))
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
          ;; (difference (range 1 2 23) (range 3 10 23))
          (t
           (nconc
            (remove-if-not
             #'valid
             `(,(range start step (- i-start step))
               ,(range (+ i-end step) step end)))
            (loop for x from i-start below i-end by i-step
                  collect
                  (range (+ x step) step (- (+ x i-step) step))))))))))
