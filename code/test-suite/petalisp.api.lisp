(in-package #:petalisp.test-suite)

(check-package '#:petalisp :skip '(petalisp:to))

(define-test lazy-rearrange-test
  (flet ((test (initial-shape &rest other-shapes)
           (alexandria:map-permutations
            (lambda (shapes)
              (let ((lazy-array (lazy-index-components initial-shape)))
                (dolist (shape shapes)
                  (let ((rank (lazy-array-rank lazy-array)))
                    (setf lazy-array
                          (lazy-rearrange lazy-array rank shape))))
                (compute lazy-array)))
            other-shapes)))
    (test (~ 1 101)
          (~ 0 5 ~ 0 5 ~ 0 4)
          (~ 0 2 ~ 0 5 ~ 0 1 ~ 0 2 ~ 0 5)
          (~ 1 3 ~ 1 6 ~ 1 3 ~ 1 6)
          (~ 1 4 2 ~ 1 10 2 ~ 1 4 2 ~ 1 10 2)
          (~ 100))
    (test (~ 1 201)
          (~ 0 2 ~ 0 5 ~ 0 5 ~ 0 4)
          (~ 0 2 ~ 0 2 ~ 0 5 ~ 0 1 ~ 0 2 ~ 0 5)
          (~ 0 2 ~ 0 100))
    (test (~ 1 201)
          (~ 0 5 ~ 0 5 ~ 0 4 ~ 0 2)
          (~ 0 2 ~ 0 5 ~ 0 1 ~ 0 2 ~ 0 5 ~ 0 2)
          (~ 0 100 ~ 0 2))))

(define-test lazy-sort-test
  (loop for n to 8 do
    (let ((vector (coerce (alexandria:iota n) 'vector)))
      (loop repeat n do
        (let ((permutation (alexandria:shuffle (alexandria:copy-array vector))))
          (is (equalp vector (compute (lazy-sort permutation #'<))))))))
  (let ((strings
          '("The quick brown fox jumps over the lazy dog."
            "Sphinx of black quartz, judge my vow."))
        (list-of-args
          '((char-lessp)
            (char-greaterp)
            (char<)
            (char>)
            (< :key char-code)
            (> :key char-code))))
    (dolist (string strings)
      (dolist (args list-of-args)
        (is (string= (apply #'sort (copy-seq string) args)
                     (compute (apply #'lazy-sort string args))))))))
