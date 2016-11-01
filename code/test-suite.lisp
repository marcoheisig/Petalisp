;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defun print-test-suite-banner (destination)
  (format destination "== Testing Petalisp ==~%")
  (format destination "Implementation: ~:[Something weird~;~:*~a~]"
          (lisp-implementation-type))
  (format destination "~@[ ~a~]~%"
          (lisp-implementation-version))
  (format destination "Machine: ~:[a strange system~;~:*~a~]"
          (machine-type))
  (format destination "~@[ ~a~]~%"
          (machine-version)))

(defun run-test-suite ()
  (print-test-suite-banner *test-dribble*)
  (fiveam:run! 'petalisp-test-suite))

(def-suite petalisp-test-suite
  :description "All Petalisp related tests.")

(in-suite petalisp-test-suite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; tests on strided arrays

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
  (is (strided-array-index-space? #i()))
  (is (= 2 (dimension #i((1 2 3) (4 5 6)))))
  (is (equal? #i((0 10 20) (5 1 5)) #i((0 10 25) (5 5 5))))
  (is (= 1000 (size #i((0 1 9) (0 1 9) (0 1 9))))))

(test (strided-array-broadcast)
  (is (not nil)))

(test (strided-array-intersection)
  (is (intersection (range 0 0 0) (range 0 0 0)))
  (is (null (intersection (range 1 1 5) (range 6 1 10))))
  (is (equal? (range 0 1 5)
              (intersection (range -5 1 5) (range 0 1 10))))
  (is (equal? (intersection (range 0 3 12) (range 0 2 12))
              (range 0 6 12)))
  (is (equal? (intersection (range 0 55 1000) (range 0 143 1000))
              (range 0 715 1000)))
  (is (equal? (intersection (range -10 40902 30000000) (range 24 24140 30000000))
              (range 327 (lcm 40902 24140) 29040747)))
  (is (equal? (intersection (range 0 (expt 6 40) (expt 2 200))
                            (range 0 (expt 9 40) (expt 2 200)))
              (range 0 (expt 18 40) (expt 2 200))))
  (is (equal? (intersection #i() #i()) #i()))
  (is (equal? (intersection #i((0 9) (0 9)) #i((2 10) (2 10)))
              #i((2 9) (2 9)))))

(test (strided-array-difference)
  (is (not nil)))

(test (strided-array-subspace?)
  (is (not nil)))
