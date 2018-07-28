(in-package :common-lisp-user)

(defpackage :petalisp/examples/red-black-gauss-seidel
  (:shadowing-import-from :petalisp :set-difference)
  (:use :cl :petalisp)
  (:export #:red-black-gauss-seidel))

(in-package :petalisp/examples/red-black-gauss-seidel)

(defun red-black-coloring (index-space)
  (with-index-space-accessors (rank start step-size end) index-space
    (labels ((prepend-1 (list)
               (cons 1 list))
             (prepend-2 (list)
               (cons 2 list))
             (offsets (red black depth)
               (if (= depth rank)
                   (values red black)
                   (offsets
                    (append (mapcar #'prepend-1 red)
                            (mapcar #'prepend-2 black))
                    (append (mapcar #'prepend-1 black)
                            (mapcar #'prepend-2 red))
                    (1+ depth))))
             (offset-space (offsets)
               (canonicalize-index-space
                (loop for offset in offsets
                      for i from 0
                      collect (let ((step (step-size i)))
                                (list (+ (start i) (* step offset))
                                      (* 2 step)
                                      (- (end i) step)))))))
      (multiple-value-bind (red-offsets black-offsets)
          (offsets '((2)) '((1)) 1)
        (values
         (mapcar #'offset-space red-offsets)
         (mapcar #'offset-space black-offsets))))))

(defun red-black-gauss-seidel (u &key (iterations 1)
                                   (h (/ (1- (expt (size u) (/ (dimension u))))))
                                   (f 0))
  "Iteratively solve the Poisson equation -Δu = f for a given uniform grid
  with spacing h, using the Red-Black Gauss-Seidel scheme."
  (let ((stencil (ecase (dimension u)
                   (1 (lambda (space)
                        (α #'* (float 1/2)
                           (α #'+
                              (reshape (transform u (τ (i) ((1+ i)))) space)
                              (reshape (transform u (τ (i) ((1- i)))) space)
                              (reshape (α #'* (* h h) f) space)))))
                   (2 (lambda (space)
                        (α #'* (float 1/4)
                           (α #'+
                              (reshape (transform u (τ (i j) ((1+ i) j))) space)
                              (reshape (transform u (τ (i j) ((1- i) j))) space)
                              (reshape (transform u (τ (i j) (i (1+ j)))) space)
                              (reshape (transform u (τ (i j) (i (1- j)))) space)
                              (reshape (α #'* (* h h) f) space)))))
                   (3 (lambda (space)
                        (α #'* (float 1/6)
                           (α #'+
                              (reshape (transform u (τ (i j k) ((1+ i) j k))) space)
                              (reshape (transform u (τ (i j k) ((1- i) j k))) space)
                              (reshape (transform u (τ (i j k) (i (1+ j) k))) space)
                              (reshape (transform u (τ (i j k) (i (1- j) k))) space)
                              (reshape (transform u (τ (i j k) (i j (1+ k)))) space)
                              (reshape (transform u (τ (i j k) (i j (1- k)))) space)
                              (reshape (α #'* (* h h) f) space))))))))
    (multiple-value-bind (red-spaces black-spaces)
        (red-black-coloring u)
      (flet ((update (spaces)
               (setf u (apply #'fuse* u (mapcar stencil spaces)))))
        (loop repeat iterations do
          (update red-spaces)
          (update black-spaces))
        u))))
