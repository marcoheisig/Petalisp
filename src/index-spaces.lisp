;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Working with index spaces
(in-package :petalisp)

(defclass index-space (petalisp-object)
  ((%shape :initarg :shape :reader shape)))

(defun make-index-space (&rest ranges)
  (let ((shape (loop for range in ranges
                     collect
                     (ematch range
                       ((list min step max) `(,min ,step ,max))
                       ((list min max) `(,min 1 ,max))
                       ((list max) `(0 1 ,max))
                       ( max  `(0 1 ,max))))))
    (make-instance 'index-space :shape shape)))

(defmacro x (&rest ranges)
  `(make-index-space
    ,@(loop for range in ranges collect `',range)))

(defmethod normalize ((instance index-space))
  (loop for triple in (shape instance) do
    (symbol-macrolet ((start (first triple))
                      (step (second triple))
                      (end (third triple)))
      ;; ensure that STEP is positive
      (when (minusp step) (setf step (- step)))
      (when (zerop step) (setf step 1))
      ;; ensure START and END are congruent relative to STEP
      (setf end (+ start (* step (truncate (- end start) step))))
      ;; ensure START is bigger than END
      (when (> start end) (rotatef start end)))))

(defun kuṭṭaka (d1 d2 c)
  "Find and return A, B and GCD(d1,d2) such that A * d1 - B * d2 = c. If no
solution could be found, return NIL."
  (declare (integer d1 d2 c))
  ;; The first part is just Euclids algorithm, where we additionally keep
  ;; track of all the quotients
  (let* ((quotients ())
         (gcd
           (loop with u of-type integer = (abs d1)
                 and  v of-type integer = (abs d2) do
             (when (= v 0) (return u))
             (multiple-value-bind (quot rem) (floor u v)
               (push quot quotients)
               (psetf v rem u v)))))
    ;; If C cannot be divided by GCD, ther is no solution
    (let ((c (/ c gcd)))
      (unless (integerp c) (return-from kuṭṭaka nil))
      ;; now comes the algorithm of Aryabhata
      (let* ((a 0)
             (b (if (evenp (length quotients)) c (- c))))
        (mapc
         (lambda (x)
           (psetf a b b (+ (* x b) a)))
         (cdr quotients))
        (values a b gcd)))))

;;; Examples:
;;; (index-space-intersection (x (0 2 20)) (x (1 5 17))) = (x (6 10 16))
;;; (index-space-intersection (x (5 9 18)) (x 100 100)) = (x (5 9 14))
(defun index-space-intersection (index-space-1 index-space-2)
  (make-instance
   'index-space
   :shape
   (loop for (start-1 step-1 end-1) in (shape index-space-1)
         and (start-2 step-2 end-2) in (shape index-space-2)
         collect
         (multiple-value-bind (a b gcd)
             (kuṭṭaka step-1 step-2 (- start-2 start-1))
           (declare (ignore b))
           (unless a (return-from index-space-intersection nil))
           (let ((lb (max start-1 start-2))
                 (ub (min end-1 end-2))
                 (lcm (/ (* step-1 step-2) gcd))
                 (x (+ (* a step-1) start-1)))
             (let ((smallest (+ x (* lcm (ceiling (- lb x) lcm))))
                   (biggest (+ x (* lcm (floor (- ub x) lcm)))))
               (break)
               (unless (<= lb smallest biggest ub)
                 (return-from index-space-intersection nil))
               `(,smallest ,lcm ,biggest)))))))
