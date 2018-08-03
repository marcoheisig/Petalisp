;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-test-suite)

(in-suite* petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Whole Program Tests

(defun compute! (&rest arguments)
  (is-true (apply #'compute arguments)))

(defun ndarray (n &optional (length 10))
  "Create a LENGTH^N array of double floats."
  (generate-instance 'array
                     :element-type 'double-float
                     :dimensions (make-list n :initial-element length)))

(defmacro with-testing-backend (&body body)
  `(call-with-testing-backend
    (lambda () ,@body)))

(defun call-with-testing-backend (thunk)
  (let ((*backend* (make-instance 'reference-backend)))
    (funcall thunk)))

(test petalisp-api
  (with-testing-backend
    (compute! (α #'+ 2 3))
    (compute! (α #'+ #(2 3 4) #(5 4 3)))
    (compute! (transform #(1 2 3) (τ (i) ((- i)))))
    (compute! (fuse* (reshape 0.0 '((2 4) (2 4)))
                     (reshape 1.0 '((3 3) (3 3)))))))

(test jacobi
  (with-testing-backend
    (compute! (jacobi (ndarray 1) :iterations 2))
    (compute! (jacobi (ndarray 2) :iterations 2))
    (compute! (jacobi (ndarray 3) :iterations 2))
    (compute! (jacobi (ndarray 3) :iterations 5))))

(test red-black-gauss-seidel
  (with-testing-backend
    (compute! (red-black-gauss-seidel (ndarray 1) :iterations 2))
    (compute! (red-black-gauss-seidel (ndarray 2) :iterations 2))
    (compute! (red-black-gauss-seidel (ndarray 3) :iterations 2))
    (compute! (red-black-gauss-seidel (ndarray 3) :iterations 5))))

(test linear-algebra
  (with-testing-backend
    (loop for dimension upto 2 do
      (compute! (transpose (ndarray dimension))))
    (let ((a (ndarray 2))
          (b (ndarray 2)))
      (compute! (matmul a b)))
    (compute! (dot #(1 2 3) #(4 5 6)))
    (compute! (norm #(1 2 3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test Suite Interface

(defun run-test-suite (&optional debug)
  (format t "~&== Testing Petalisp ==~%")
  (print-platform-information)
  (print-system-statistics :petalisp)
  (print-package-statistics :petalisp)
  (format t "~&Git revision: ~a" (system-git-revision :petalisp))
  (let ((*on-error*   (if debug :debug *on-error*))
        (*on-failure* (if debug :debug *on-failure*)))
    (fiveam:run! 'petalisp)))
