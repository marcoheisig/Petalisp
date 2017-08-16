;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Petalisp test suite

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

(defun ? (expression result)
  (is (equal? (compute expression) (compute result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  utilities

(test (utilities)
  (is (identical (iota 5) :test #'eql :key #'numberp))
  (is
   (equalp
    #2a((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))
    (array-map
     #'+
     (make-array '(4 4))
     #2a((1 1 1 1) (1 1 1 1) (1 1 1 1) (1 1 1 1))
     #2a((0 1 2 3) (0 1 2 3) (0 1 2 3) (0 1 2 3))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  transformations

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
    (? (τ (a b) a b) (identity-transformation 2))
    (? (identity-transformation 3))
    (? (identity-transformation 4) (τ (a b c d) a b c d))
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
    (? (τ (m n) n m))
    (? (τ (m) (* 2 m)))
    (? (τ (m) (/ (+ (* 90 (+ 2 m)) 15) 2)))
    (? (τ (m) m 1 2 3))
    (? (τ (m) 5 9 m 2))
    (? (τ (0 n 0) n))
    (? (τ (i j 5) i j))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  strided arrays

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
           (is (equal? result (broadcast a b)))))
    (? (σ (0 9)) (σ (9 9)) (σ (0 9)))
    (? (σ (9 9)) (σ (0 9)) (σ (0 9)))
    (? (σ (-5 5)) (σ (-5 5)) (σ (-5 5)))
    (? (σ) (σ (0 100) (0 100)) (σ (0 100) (0 100)))
    (? (σ (0 100) (0 100)) (σ) (σ (0 100) (0 100)))
    (signals error (broadcast (σ (2 4)) (σ (1 3))))))

(test (strided-array-intersection)
  (flet ((? (a b result)
           (is (equal? result (intersection a b)))))
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
    (? (σ (0 9) (0 9)) (σ (2 10) (2 10)) (σ (2 9) (2 9)))
    (? (σ (1 2 3) (0 3 6)) (σ (1 1 3) (0 2 6)) (σ (1 2 3) (0 6 6)))))

(test (strided-array-transform)
  (flet ((? (object transformation result)
           (is (equal? result (transform object transformation)))
           (is (equal? (transform result (invert transformation))
                       object))))
    (? (σ (1 1 1)) (τ (m) (1+ m))
       (σ (2 1 2)))
    (? (σ (0 9) (0 5)) (τ (m n) n m)
       (σ (0 5) (0 9)))
    (? (σ (1 1) (2 2) (3 3)) (τ (1 2 3) 4)
       (σ (4 4)))
    (? (σ (2 2)) (τ (m) 1 m 3)
       (σ (1 1) (2 2) (3 3)))
    (? (σ (0 5 10) (0 7 21))
       (τ (m n)
          (+ (* 2 n) 5)
          (+ (* 3 m) 99))
       (σ (5 14 47) (99 15 129)))
    (? (σ (0 0) (0 0) (0 0) (0 0) (0 0))
       (τ (a 0 c 0 e) 0 a 0 c 0 e 0)
       (σ (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)))
    (signals error
      (transform (σ (0 0) (0 0)) (τ (1 m) m 1)))))

(test (strided-array-index-space-fusion)
  (flet ((? (&rest args)
           (let ((fusion (apply #'fusion args)))
             (is (every (lambda (x) (subspace? x fusion)) args))
             (is (= (size fusion)
                    (reduce #'+ (mapcar #'size args)))))))
    (? (σ (1 1)) (σ (2 2)) (σ (3 3)) (σ (0 0)))
    (? (σ (1 2 (expt 10 15))) (σ (2 2 (expt 10 15))))
    (? (σ (1 5) (1 5))
       (σ (6 10) (1 10))
       (σ (1 5) (6 10)))
    (? (σ (2 2 4) (2 2 4))
       (σ (2 2 4) (1 2 5))
       (σ (1 2 5) (1 1 5)))
    (? (σ (1 2 3) (1 2 3))
       (σ (2 2 4) (1 2 3))
       (σ (1 2 3) (2 2 4))
       (σ (2 2 4) (2 2 4)))
    (?
     (σ (2 2 6) (2 2 6))
     (σ (8 8) (0 2 8))
     (σ (0 0) (0 2 8))
     (σ (2 2 6) (0 0))
     (σ (2 2 6) (8 8)))))

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

(test (strided-array-reference)
  (? (-> #(1 2 3 4) (τ (i) (* -1 i)))
     #(4 3 2 1))
  (? (-> #2a((1 2) (3 4)) (τ (m n) n m))
     #2a((1 3) (2 4)))
  (? (-> #(42) (τ (0) 0 0 0 0))
     #4a((((42)))))
  (? (fuse (-> #(1 2 3 4) (τ (i) (+ (* 2 i) 1)))
           (-> #(0 0 0 0) (τ (i) (* 2 i))))
     #(0 1 0 2 0 3 0 4))
  (? (-> #2a((42)) (τ (0 0)))
     #0a42)
  (? (-> #(1 2 3 4) (σ (1 2)) (τ (x) x 0))
     #2a((2) (3))))

(test (strided-array-application)
  (? (α #'+ #(1 1 1 1) #(2 2 2 2))
     #(3 3 3 3))
  (? (α #'* #2a((2 2) (2 2)) #2a((1 2) (3 4)))
     #2a((2 4) (6 8))))

(test (strided-array-repetition)
  (? (repetition (lisp->petalisp #(7)) (σ (0 9)))
     #(7 7 7 7 7 7 7 7 7 7))
  (? (repetition (lisp->petalisp #(1)) (σ (0 2) (0 2)))
     #2a((1 1 1) (1 1 1) (1 1 1)))
  (? (α #'* #(1 2 3) #2a((1 2 3)))
     #2a((1 2 3) (2 4 6) (3 6 9))))

(test (strided-array-fusion)
  (? (fuse #(1 2 3) (-> #(4 5 6) (τ (x) (+ x 3))))
     #(1 2 3 4 5 6))
  (?
   (apply #'fusion
          (loop for (dx dy) in '((0 0) (0 2) (2 0) (2 2))
                collect (-> (α #'* (+ (* 2 dx) dy)
                               #2a((1 1) (1 1)))
                            (τ (x y) (+ x dx) (+ y dy)))))
   #2a((0 0 2 2) (0 0 2 2) (4 4 6 6) (4 4 6 6)))
  (? (fuse (-> #(2 3) (τ (x) (+ x 1)))
           (-> #(1 4) (τ (x) (* 3 x))))
     #(1 2 3 4))
  )

(test (strided-array-reduction)
  (? (β #'+ #(1 2 3 4 5 6 7 8 9 10)) #0a55)
  (let* ((1x10 #2a((0 0 0 0 0 0 0 0 0 0)))
         (10x1 (-> 1x10 (τ (m n) n m))))
    (? (β #'+ (β #'+ (α #'+ 1 10x1 1x10))) #0a100))
  (? (β #'+ #2a((1 2) (3 4))) #(3 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  type inference

(test (type-inference)
  (flet ((? (result fun &rest types)
           (is (equal result (apply #'result-type fun types)))))
    (? 'double-float #'+ 'double-float 'double-float)
    (? 't #'- 'double-float 'character)
    (? '(complex double-float) #'* 'double-float '(complex single-float))
    (? '(complex double-float) #'* '(complex double-float) 'single-float)
    (? 'double-float #'+ 'single-float 'double-float 'single-float)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  full programs

(test (matrix-multiplication)
  (flet ((matmul (a b)
           (β #'+
              (α #'*
                 (-> A (τ (m n) m 1 n))
                 (-> B (τ (n k) 1 k n))))))
    (let ((nullmatrix (-> 0 (σ (0 9) (0 9)))))
      (? (matmul nullmatrix nullmatrix) (compute nullmatrix)))
    (let ((I-2x2 #2a((1.0 0.0) (0.0 1.0)))
          (A-2x2 #2a((1.0 2.0) (3.0 4.0))))
      (? (matmul I-2x2 I-2x2) I-2x2)
      (? (matmul I-2x2 A-2x2) A-2x2)
      (? (matmul A-2x2 A-2x2) #2a((7.0 10.0) (15.0 22.0))))))

(test (iterative-solvers)
  (flet ((jacobi-2d (grid iterations)
           (let ((interior (σ* grid
                               ((+ start 1) step (- end 1))
                               ((+ start 1) step (- end 1)))))
             (loop repeat iterations do
               (setf grid (fuse*
                           grid
                           (α #'* 0.25
                              (α #'+
                                 (-> grid (τ (i j) (1+ i) j) interior)
                                 (-> grid (τ (i j) (1- i) j) interior)
                                 (-> grid (τ (i j) i (1+ j)) interior)
                                 (-> grid (τ (i j) i (1- j)) interior)))))
                   finally (return grid))))
         (rbgs-2d (u iterations)
           (let ((r1 (σ* u ((+ start 2) 2 (1- end)) ((+ start 2) 2 (1- end))))
                 (r2 (σ* u ((+ start 1) 2 (1- end)) ((+ start 1) 2 (1- end))))
                 (b1 (σ* u ((+ start 2) 2 (1- end)) ((+ start 1) 2 (1- end))))
                 (b2 (σ* u ((+ start 1) 2 (1- end)) ((+ start 2) 2 (1- end)))))
             (labels ((update (u what)
                        (α #'* 0.25
                           (α #'+
                              (-> u (τ (i j) (1+ i) j) what)
                              (-> u (τ (i j) (1- i) j) what)
                              (-> u (τ (i j) i (1+ j)) what)
                              (-> u (τ (i j) i (1- j)) what)))))
               (loop repeat iterations do
                 (setf u (fuse* u (update u r1) (update u r2)))
                 (setf u (fuse* u (update u b1) (update u b2))))
               u)))
         (inf-error-2d (x exact-solution)
           (β #'max (β #'max (α #'abs (α #'- x exact-solution))))))
    ;; zero start values should remain so
    (? (jacobi-2d (-> 0.0 (σ (0 10) (0 10))) 10)
       (-> 0.0 (σ (0 10) (0 10))))
    (? (rbgs-2d (-> 0.0 (σ (42 54) (11 27))) 10)
       (-> 0.0 (σ (42 54) (11 27))))
    ;; both methods should converge to some exact solution
    (let* ((grid (fuse* (-> 1.0 (σ (10 20) (10 20)))
                        (-> 0.0 (σ (11 19) (11 19)))))
           (exact-solution (-> 1.0 (σ (10 20) (10 20)))))
      (is (< (compute (inf-error-2d (jacobi-2d grid 6) exact-solution))
             (compute (inf-error-2d (jacobi-2d grid 5) exact-solution))))
      (is (< (compute (inf-error-2d (rbgs-2d grid 6) exact-solution))
             (compute (inf-error-2d (rbgs-2d grid 5) exact-solution)))))))
