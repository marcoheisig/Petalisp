;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmacro define-class (name direct-superclasses slots &rest options)
  `(defclass ,name ,direct-superclasses
     ,(loop for slot in slots collect
            `(,slot :initarg ,(make-keyword slot) :reader ,slot))
     ,@options))

(defun kuṭṭaka (d1 d2 c)
  "Returns A, B and GCD(d1,d2) such that A * d1 - B * d2 = c. Returns false
if no solution exists."
  (declare (integer d1 d2 c))
  ;; The first part is just Euclids algorithm to determine the GCD, where
  ;; we additionally keep track of all the quotients
  (let* ((quotients ())
         (gcd
           (loop with u of-type integer = (abs d1)
                 and  v of-type integer = (abs d2) do
                   (when (= v 0) (return u))
                   (multiple-value-bind (quot rem) (floor u v)
                     (push quot quotients)
                     (psetf v rem u v)))))
    ;; If C cannot be divided by GCD, there is no solution
    (let ((c (/ c gcd)))
      (unless (integerp c) (return-from kuṭṭaka nil))
      ;; now comes the algorithm of Aryabhata
      (let* ((a 0)
             (b (if (evenp (length quotients)) c (- c))))
        (mapc
         (lambda (x)
           (psetf a b b (the integer (+ (* x b) a))))
         (cdr quotients))
        (values a b gcd)))))
