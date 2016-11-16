;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;; a test suite that can be invoked via (asdf:test-system :petalisp)

(in-package :petalisp)

(defun run-test-suite ()
  (format t "== Testing Petalisp ==~%")
  (print-platform-information)
  (print-system-statistics :petalisp)
  (print-package-statistics :petalisp)
  (fiveam:run! 'petalisp-test-suite))

(def-suite petalisp-test-suite
  :description "All Petalisp related tests.")

(in-suite petalisp-test-suite)

;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;;  utilities
;;; _________________________________________________________________

(test (array-map)
  (is (identical (iota 5) :test #'eql :key #'numberp))
  (is
   (equalp
    (array-map
     #'+
     #2a((1 1 1 1) (1 1 1 1) (1 1 1 1) (1 1 1 1))
     #2a((0 1 2 3) (0 1 2 3) (0 1 2 3) (0 1 2 3)))
    #2a ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)))))


;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;;  transformations
;;; _________________________________________________________________

(test (identity-transformation)
  (flet ((? (x &optional y)
           (is (identity-transformation? x))
           (is (equal? x x))
           (is (equal? x (invert (invert x))))
           (is (equal? (identity-transformation (input-dimension x))
                       (compose x (invert x))))
           (when y
             (is (equal? x y)))))
    (? (identity-transformation 1))
    (? (τ (a b) (a b)) (identity-transformation 2))
    (? (identity-transformation 3))
    (? (identity-transformation 4) (τ (a b c d) (a b c d)))
    (is (equal? (transform (σ (0 100)) (identity-transformation 1))
                (σ (0 100))))))

(test (affine-transformation)
  (flet ((? (x)
           (is (affine-transformation? x))
           (is (equal? x (invert (invert x))))
           (unless (or (input-constraints x)
                       (input-constraints (invert x)))
             (is (equal? (compose (invert x) x)
                         (identity-transformation
                          (input-dimension x)))))))
    (? (τ (m n) (n m)))
    (? (τ (m) ((* 2 m))))
    (? (τ (m) ((/ (+ (* 90 (+ 2 m)) 15) 2))))
    (? (τ (m) (m 1 2 3)))
    (? (τ (m) (5 9 m 2)))
    (? (τ (0 n 0) (n)))
    (? (τ (i j 5) (i j)))))

;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;;  strided arrays
;;; _________________________________________________________________

(test (range)
  (is (range? (range 0 1 10)))
  (is (equal? (range 0 -3 3) (range 0 3 3)))
  (is (equal? (range 0 5 0) (range 0 0 0)))
  (is (equal? (range 5 3 12) (range 5 3 11)))
  (is (equal? (range -2 2 2) (range 2 2 -2)))
  (is (= (size (range 0 1 100))
         (size (range 5 5 505))))
  (is (unary-range? (range 99 -5 103))))

(test (strided-array-index-space)
  (is (strided-array-index-space? (σ)))
  (is (= 2 (dimension (σ (1 2 3) (4 5 6)))))
  (is (equal? (σ (0 10 20) (5 1 5)) (σ (0 10 25) (5 5 5))))
  (is (= 1000 (size (σ (0 1 9) (0 1 9) (0 1 9))))))

(test (strided-array-broadcast)
  (flet ((? (a b result)
           (is (equal? (broadcast a b) result))))
    (? (σ (0 9)) (σ (9 9)) (σ (0 9)))
    (? (σ (9 9)) (σ (0 9)) (σ (0 9)))
    (? (σ (-5 5)) (σ (-5 5)) (σ (-5 5)))
    (? (σ) (σ (0 100) (0 100)) (σ (0 100) (0 100)))
    (? (σ (0 100) (0 100)) (σ) (σ (0 100) (0 100)))
    (signals error (broadcast (σ (2 4)) (σ (1 3))))))

(test (strided-array-intersection)
  (flet ((? (a b result)
           (is (equal? (intersection a b) result))))
    (is (intersection (range 0 0 0) (range 0 0 0)))
    (is (null (intersection (range 1 1 5) (range 6 1 10))))
    (? (range -5 1 5) (range 0 1 10) (range 0 1 5))
    (? (range 0 3 12) (range 0 2 12) (range 0 6 12))
    (? (range 1 2 23) (range 3 10 23) (range 3 10 23))
    (? (range 0 55 1000) (range 0 143 1000) (range 0 715 1000))
    (? (range -10 40902 50000000) (range 24 24140 50000000)
       (range 13783964 (lcm 40902 24140) 42824384))
    (? (range 0 (expt 6 40) (expt 2 200)) (range 0 (expt 9 40) (expt 2 200))
       (range 0 (expt 18 40) (expt 2 200)))
    (?  (σ) (σ) (σ))
    (? (σ (0 9) (0 9)) (σ (2 10) (2 10)) (σ (2 9) (2 9)))))

(test (strided-array-transform)
  (flet ((? (object transformation result)
           (is (equal? (transform object transformation)
                       result))
           (is (equal? (transform result (invert transformation))
                       object))))
    (? (σ (1 1 1)) (τ (m) ((1+ m)))
       (σ (2 1 2)))
    (? (σ (0 9) (0 5)) (τ (m n) (n m))
       (σ (0 5) (0 9)))
    (? (σ (1 1) (2 2) (3 3)) (τ (1 2 3) (4))
       (σ (4 4)))
    (? (σ (2 2)) (τ (m) (1 m 3))
       (σ (1 1) (2 2) (3 3)))
    (? (σ (0 5 10) (0 7 21))
       (τ (m n)
          ((+ (* 2 n) 5)
           (+ (* 3 m) 99)))
       (σ (5 14 47) (99 15 129)))
    (? (σ (0 0) (0 0) (0 0) (0 0) (0 0))
       (τ (a 0 c 0 e) (0 a 0 c 0 e 0))
       (σ (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)))
    (signals error
      (transform (σ (0 0) (0 0)) (τ (1 m) (m 1))))))

(test (strided-array-fusion)
  (flet ((? (&rest args)
           (let ((fusion (apply #'fusion args)))
             (is (every (lambda (x) (subspace? x fusion)) args))
             (is (= (size fusion)
                    (reduce #'+ (mapcar #'size args)))))))
    (? (σ (2 2 4) (2 2 4))
       (σ (2 2 4) (1 2 5))
       (σ (1 2 5) (1 1 5))))) ; TODO more test cases

(test (strided-array-difference)
  (flet ((? (a b)
           (if (intersection a b)
               (is (equal? a (apply #'fusion
                                    (intersection a b)
                                    (difference a b))))
               (is (equal? a (first (difference a b)))))))
    (? (range 1 3 7) (range 8 1 22))
    (? (range 1 3 7) (range 1 3 10))
    (? (range 1 3 13) (range 4 2 10))
    (? (range 0 1 9) (range 2 2 4))
    (? (range 1 2 23) (range 3 10 23))
    (? (σ (1 1 4)) (σ (1 2 5)))
    (? (σ (1 2 5)) (σ (1 1 4)))
    (? (σ (1 2 5)) (σ (2 2 4)))
    (? (σ (1 1 5) (1 1 5))
       (σ (2 2 4) (2 2 4)))
    (? (σ (1 1 5) (1 1 5))
       (σ (1 2 5) (1 2 5)))
    (? (σ (1 1 9) (1 1 9) (1 1 9))
       (σ (1 8 9) (1 8 9) (1 8 9)))))

(test (strided-array-subdivision)
  (flet ((? (&rest args)
           (let ((result (apply #'subdivision args)))
             ;; check for disjointness
             (let (intersections)
               (map-combinations
                (lambda (x)
                  (push (apply #'intersection x) intersections))
                result :length 2)
               (is (every #'null intersections)))
             ;; check for coverage
             (is (every #'subspace?
                        args
                        (forever (apply #'fusion result)))))))
    (? (σ (1 1 4)) (σ (1 2 5)))
    (? (σ (1 1 10) (1 1 10))
       (σ (5 1 10) (5 1 10)))))

(test (strided-array-subspace?)
  (is (subspace? (range 1 1 2) (range 0 1 3)))
  (is (subspace? (range 0 4 8) (range 0 2 10)))
  (is (subspace? (σ (0 6 120) (1 1 100))
                 (σ (0 2 130) (0 1 101)))))

;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;;  type inference
;;; _________________________________________________________________

(test (type-inference)
  (flet ((? (result fun &rest types)
           (is (equal (apply #'result-type fun types)
                      result))))
    (? 'double-float #'+ 'double-float 'double-float)
    (? 't #'- 'double-float 'character)
    (? '(complex double-float) #'* 'double-float '(complex single-float))
    (? '(complex double-float) #'* '(complex double-float) 'single-float)
    (? 'double-float #'+ 'single-float 'double-float 'single-float)))
