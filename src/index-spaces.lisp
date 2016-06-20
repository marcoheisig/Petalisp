;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Working with index spaces

(in-package :petalisp)

;;; Mathematically, a Petalisp index space is the Cartesian product of sets
;;; of the form { X | START <= X <= END | ∃ n ∈ N, X = START + n * STEP },
;;; where START, STEP and END are integers. For implementation purposes, an
;;; index space is a list of instances of the structure RANGE.

(defstruct (range (:constructor %make-range (start step end)))
  (start 0 :type fixnum :read-only t)
  (step 1 :type fixnum :read-only t)
  (end 0 :type fixnum :read-only t))

(defun range (&rest spec)
  (multiple-value-bind (start step end)
      (ematch spec
        ((list start step end) (values start step end))
        ((list start end) (values start 1 end))
        ((list end) (values 0 1 end)))
    ;; TODO error handling:
    ;; - STEP not towards END
    ;; - empty range
    ;; - start /= end but STEP is zero

    ;; ensure that STEP is positive
    (when (minusp step) (setf step (- step)))
    (when (zerop step) (setf step 1))
    ;; ensure START and END are congruent relative to STEP
    (setf end (+ start (* step (truncate (- end start) step))))
    ;; ensure START is bigger than END
    (when (> start end) (rotatef start end))
    (%make-range start step end)))

(defmacro x (&rest specs)
  `(list ,@(loop for spec in specs
                 collect (if (atom spec) `(range ,spec) `(range ,@spec)))))

(defun kuṭṭaka (d1 d2 c)
  "Returns A, B and GCD(d1,d2) such that A * d1 - B * d2 = c. Returns NIL
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

(defun index-space-intersection (index-space-1 index-space-2)
  "Return the new index space containing the common elements the two given
index spaces, or NIL if there are no common elements.

Examples:
 (index-space-intersection (x 9) (x (10 15))) == NIL
 (index-space-intersection (x (0 2 20)) (x (1 5 17))) == (x (6 10 16))
 (index-space-intersection (x (5 9 18)) (x 100 100)) == (x (5 9 14))

Note: The intersection of two valid Petalisp index spaces is either empty
or again a valid Petalisp index space, so this function never signals an
error."
  (catch 'no-intersection
    (mapcar #'index-space-intersection-1D index-space-1 index-space-2)))

(defun index-space-intersection-1D (range-1 range-2)
  (let ((start-1 (range-start range-1))
        (start-2 (range-start range-2))
        (step-1 (range-step range-1))
        (step-2 (range-step range-2))
        (end-1 (range-end range-1))
        (end-2 (range-end range-2)))
    (multiple-value-bind (a b gcd)
        (kuṭṭaka step-1 step-2 (- start-2 start-1))
      (declare (ignore b))
      (unless a (throw 'no-intersection nil))
      (let ((lb (max start-1 start-2))
            (ub (min end-1 end-2))
            (lcm (/ (* step-1 step-2) gcd))
            (x (+ (* a step-1) start-1)))
        (let ((smallest (+ x (* lcm (ceiling (- lb x) lcm))))
              (biggest  (+ x (* lcm (floor (- ub x) lcm)))))
          (unless (<= lb smallest biggest ub)
            (throw 'no-intersection nil))
          (range smallest lcm biggest))))))

(defun index-space-union (&rest index-spaces)
  "If the union of the given index spaces is again a valid Petalisp index
space, returns the resulting space. Otherwise signals an error.

Examples:
 (index-space-union (x (2 2 8)) (x (1 2 9))) == (x (1 1 9))"
  ;; check that all index-spaces are disjoint
  nil)

#+nil
(defun %index-space-union-1D (&rest ranges)
  (loop for (start step end) in ranges do
    )
  (let* ((ins (sort (copy-list ranges) #'< :key #'first))
         (del (sort (copy-list ranges) #'< :key #'third)))
    (flet ((pop-ins ()
             (prog1 (pop ins)
               (when (= next-start (setf next-start (first (car ins))))
                 (throw 'not-unifiable))))
           (pop-del ()
             (prog1 (pop del)
               (when (= next-end (setf next-end (third (car del))))
                 (throw 'not-unifiable)))))
      (loop until (null ends)
            with current = next-start
            with active = (list (pop-ins))
            do
        (if (and next-start (< next-start next-end))
            ;; process [current next-start)
            ;; insert element
            ;; advance current
            (let ((elt (pop starts)))
              (when (find elt active :test #'equal)
                (throw 'not-unifiable))
              (push elt active)
              (setf next-start (first (car starts))))
            ;; remove element
            )))))
