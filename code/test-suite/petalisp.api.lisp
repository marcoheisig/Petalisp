;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.test-suite)

(check-package '#:petalisp.api :skip '(petalisp:to))

(define-test lazy-sort-test
  (loop for n to 8 do
    (let ((vector (coerce (alexandria:iota n) 'vector)))
      (loop repeat n do
        (let ((permutation (alexandria:shuffle (alexandria:copy-array vector))))
          (is (equalp vector (compute (lazy-sort permutation #'<)))))))))

