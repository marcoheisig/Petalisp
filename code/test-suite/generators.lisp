;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod generator ((result-type (eql 'integer))
                      &key (minimum -1000) (maximum 1000))
  (lambda ()
    (+ minimum (random (1+ (- maximum minimum))))))

(defmethod generator ((result-type (eql 'array))
                      &key
                        (element-type 'single-float)
                        (dimensions (loop repeat (random 4) collect (random 8)))
                        (element-generator (generator element-type)))
  (lambda ()
    (let ((result (make-array dimensions :element-type element-type)))
      (loop for index below (array-total-size result) do
        (setf (row-major-aref result index) (funcall element-generator)))
      result)))

(macrolet
    ((define-float-generator (type)
       (let ((zero (coerce 0 type))
             (one  (coerce 1 type))
             (two  (coerce 2 type)))
         ;; These generators use Marsaglia's polar method to convert the
         ;; uniform random numbers from RANDOM to a normal distribution.
         `(defmethod generator ((result-type (eql ',type))
                                &key
                                  (mean ,zero)
                                  (standard-deviation ,one))
            "Return a generator for floating point numbers over a uniform
             distribution with given MEAN and STANDARD-DEVIATION."
            (let (cache)
              (lambda ()
                (or (shiftf cache nil)
                    (loop for u ,type = (- (random ,two) ,one)
                          for v ,type = (- (random ,two) ,one)
                          for s ,type = (+ (* u u) (* v v))
                          until (and (<= s ,one)
                                     (/= s ,zero))
                          finally
                             (let ((m (sqrt (* (- ,two) (log s) (/ s)))))
                               (setf cache (+ (* v m standard-deviation) mean))
                               (return     (+ (* u m standard-deviation) mean)))))))))))
  (define-float-generator short-float)
  (define-float-generator single-float)
  (define-float-generator double-float)
  (define-float-generator long-float))
