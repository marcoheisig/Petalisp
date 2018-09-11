(in-package :common-lisp-user)

(defpackage :petalisp-iterative-methods
  (:shadowing-import-from :petalisp :set-difference)
  (:use :cl :petalisp)
  (:export #:jacobi #:rbgs))

(in-package :petalisp-iterative-methods)

(defun interior (shape)
  (with-shape-accessors (rank start step-size end) shape
    (loop for i below rank
          collect
          (let ((step (step-size i)))
            (list (+ (start i) step)
                  step
                  (- (end i) step))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Jacobi method

(defun jacobi (u &key (iterations 1) (h 1.0) (f 0))
  "Iteratively solve the Poisson equation -Δu = f for a given uniform grid
  with spacing h, using the Jacobi scheme."
  (let ((interior (interior u)))
    (ecase (dimension u)
      (1
       (loop repeat iterations do
         (setf u (fuse* u (α #'* (float 1/2)
                             (α #'+
                                (reshape (reshape u (τ (i) ((1+ i)))) interior)
                                (reshape (reshape u (τ (i) ((1- i)))) interior)
                                (reshape (α #'* (* h h) f) interior))))))
       u)
      (2
       (loop repeat iterations do
         (setf u (fuse* u (α #'* (float 1/4)
                             (α #'+
                                (reshape (reshape u (τ (i j) ((1+ i) j))) interior)
                                (reshape (reshape u (τ (i j) ((1- i) j))) interior)
                                (reshape (reshape u (τ (i j) (i (1+ j)))) interior)
                                (reshape (reshape u (τ (i j) (i (1- j)))) interior)
                                (reshape (α #'* (* h h) f) interior))))))
       u)
      (3
       (loop repeat iterations do
         (setf u (fuse* u (α #'* (float 1/6)
                             (α #'+
                                (reshape (reshape u (τ (i j k) ((1+ i) j k))) interior)
                                (reshape (reshape u (τ (i j k) ((1- i) j k))) interior)
                                (reshape (reshape u (τ (i j k) (i (1+ j) k))) interior)
                                (reshape (reshape u (τ (i j k) (i (1- j) k))) interior)
                                (reshape (reshape u (τ (i j k) (i j (1+ k)))) interior)
                                (reshape (reshape u (τ (i j k) (i j (1- k)))) interior)
                                (reshape (α #'* (* h h) f) interior))))))
       u))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Gauss-Seidel Method

(defun red-black-coloring (index-space)
  (with-shape-accessors (rank start step-size end) index-space
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
               (make-shape
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

(defun rbgs (u &key (iterations 1) (h 1.0) (f 0))
  "Iteratively solve the Poisson equation -Δu = f for a given uniform grid
  with spacing h, using the Red-Black Gauss-Seidel scheme."
  (let ((stencil (ecase (dimension u)
                   (1 (lambda (space)
                        (α #'* (float 1/2)
                           (α #'+
                              (reshape (reshape u (τ (i) ((1+ i)))) space)
                              (reshape (reshape u (τ (i) ((1- i)))) space)
                              (reshape (α #'* (* h h) f) space)))))
                   (2 (lambda (space)
                        (α #'* (float 1/4)
                           (α #'+
                              (reshape (reshape u (τ (i j) ((1+ i) j))) space)
                              (reshape (reshape u (τ (i j) ((1- i) j))) space)
                              (reshape (reshape u (τ (i j) (i (1+ j)))) space)
                              (reshape (reshape u (τ (i j) (i (1- j)))) space)
                              (reshape (α #'* (* h h) f) space)))))
                   (3 (lambda (space)
                        (α #'* (float 1/6)
                           (α #'+
                              (reshape (reshape u (τ (i j k) ((1+ i) j k))) space)
                              (reshape (reshape u (τ (i j k) ((1- i) j k))) space)
                              (reshape (reshape u (τ (i j k) (i (1+ j) k))) space)
                              (reshape (reshape u (τ (i j k) (i (1- j) k))) space)
                              (reshape (reshape u (τ (i j k) (i j (1+ k)))) space)
                              (reshape (reshape u (τ (i j k) (i j (1- k)))) space)
                              (reshape (α #'* (* h h) f) space))))))))
    (multiple-value-bind (red-spaces black-spaces)
        (red-black-coloring u)
      (flet ((update (spaces)
               (setf u (apply #'fuse* u (mapcar stencil spaces)))))
        (loop repeat iterations do
          (update red-spaces)
          (update black-spaces))
        u))))
