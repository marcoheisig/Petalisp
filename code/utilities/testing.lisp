;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmacro test (name &body body)
  "A thin wrapper around FIVEAM:TEST that automatically uses the name of
the current package (interned in the current package) as test suite."
  `(progn
     (fiveam:in-suite* ,(intern (package-name *package*)))
     (fiveam:test ,name ,@body)))

(macrolet
    ((float-generator (type)
       `(defun ,(symbolicate type "-GENERATOR")
            (&optional
               (infimum ,(coerce -2 type))
               (supremum ,(coerce 2 type)))
          (assert (< infimum supremum))
          (let ((infimum (coerce infimum ',type))
                (supremum (coerce supremum ',type)))
            (let ((middle (+ (/ supremum 2) (/ infimum 2)))
                  (half   (- (/ supremum 2) (/ infimum 2))))
              (if (zerop half)
                  (constantly middle)
                  (lambda ()
                    (let ((offset (random half)))
                      (if (zerop (random 2))
                          (- middle offset)
                          (+ middle offset))))))))))
  (float-generator short-float)
  (float-generator single-float)
  (float-generator double-float)
  (float-generator long-float))

(defun integer-generator (&optional
                            (minimum #.(floor most-negative-fixnum 4/5))
                            (maximum #.(ceiling most-positive-fixnum 4/5)))
  (check-type minimum integer)
  (check-type maximum integer)
  (assert (<= minimum maximum))
  (lambda ()
    (+ minimum (random (1+ (- maximum minimum))))))

(defun array-generator (&key (dimensions '(5 5)) element-generator)
  (lambda ()
    (let ((result (make-array dimensions)))
      (loop :for index :below (array-total-size result) :do
        (setf (row-major-aref result index) (funcall element-generator)))
      result)))

(defun random-array (&optional (dimension 2) (length 5))
  (funcall (array-generator
            :dimensions (make-list dimension :initial-element length)
            :element-generator (integer-generator 0 9))))
