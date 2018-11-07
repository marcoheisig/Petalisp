(in-package :common-lisp-user)

(defpackage :petalisp/examples/iterative-methods
  (:shadowing-import-from :petalisp :set-difference)
  (:use :cl :petalisp)
  (:export #:jacobi #:rbgs))

(in-package :petalisp/examples/iterative-methods)

(defun interior (array)
  (loop for range in (ranges (array-shape array))
        collect (multiple-value-bind (start step end)
                    (range-start-step-end range)
                  (list (+ start step) step (- end step)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Jacobi method

(defun jacobi (u &key (iterations 1) (h 1.0) (f 0))
  "Iteratively solve the Poisson equation -Δu = f for a given uniform grid
  with spacing h, using the Jacobi scheme."
  (let ((interior (interior u)))
    (ecase (rank u)
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

(defun red-black-coloring (array)
  (let* ((strided-array (coerce-to-strided-array array))
         (ranges (ranges (array-shape strided-array))))
    (labels ((prepend-1 (list)
               (cons 1 list))
             (prepend-2 (list)
               (cons 2 list))
             (offsets (red black depth)
               (if (= depth (rank strided-array))
                   (values red black)
                   (offsets
                    (append (mapcar #'prepend-1 red)
                            (mapcar #'prepend-2 black))
                    (append (mapcar #'prepend-1 black)
                            (mapcar #'prepend-2 red))
                    (1+ depth)))))
      (multiple-value-bind (red-offsets black-offsets) (offsets '((2)) '((1)) 1)
        (flet ((offset-space (offsets)
                 (apply #'shape
                  (loop for offset in offsets
                        for range in ranges
                        collect (multiple-value-bind (start step end)
                                    (range-start-step-end range)
                                  (range (+ start (* step offset))
                                         (* 2 step)
                                         (- end step)))))))
          (values
           (mapcar #'offset-space red-offsets)
           (mapcar #'offset-space black-offsets)))))))

(defun rbgs (u &key (iterations 1) (h 1.0) (f 0))
  "Iteratively solve the Poisson equation -Δu = f for a given uniform grid
  with spacing h, using the Red-Black Gauss-Seidel scheme."
  (let ((stencil (ecase (rank u)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Multigrid Method

(defun prolongate (u)
  ;; TODO
  #+nil
  (let* ((u* (reshape u (τ (i j) ((* i 2) (* j 2)))))
         (space-1
           (σ* u* ((1+ start) step (- (1+ end) step))
                  (start step end)))
         (space-2
           (σ* u* (start step end)
                  ((1+ start) step (- (1+ end) step))))
         (space-3
           (σ* u* ((1+ start) step (- (1+ end) step))
                  ((1+ start) step (- (1+ end) step)))))
    (fuse u*
          (α #'* 0.5
             (α #'+
                (reshape u* (τ (i j) ((1+ i) j)) space-1)
                (reshape u* (τ (i j) ((1- i) j)) space-1)))
          (α #'* 0.5
             (α #'+
                (reshape u* (τ (i j) (i (1+ j))) space-2)
                (reshape u* (τ (i j) (i (1- j))) space-2)))
          (α #'* 0.25
             (α #'+
                (reshape u* (τ (i j) ((1+ i) (1+ j))) space-3)
                (reshape u* (τ (i j) ((1+ i) (1- j))) space-3)
                (reshape u* (τ (i j) ((1- i) (1+ j))) space-3)
                (reshape u* (τ (i j) ((1- i) (1- j))) space-3))))))

(defun restrict (u)
  ;; TODO
  #+nil
  (let ((selection (σ* u (start 2 end) (start 2 end)))
        (interior (interior u)))
    (reshape
     (fuse*
      (reshape u selection)
      (α #'+
         (α #'* 0.25   (reshape u selection interior))
         (α #'* 0.125  (reshape u (τ (i j) ((1+ i) j)) selection interior))
         (α #'* 0.125  (reshape u (τ (i j) ((1- i) j)) selection interior))
         (α #'* 0.125  (reshape u (τ (i j) (i (1+ j))) selection interior))
         (α #'* 0.125  (reshape u (τ (i j) (i (1- j))) selection interior))
         (α #'* 0.0625 (reshape u (τ (i j) ((1+ i) (1+ j))) selection interior))
         (α #'* 0.0625 (reshape u (τ (i j) ((1- i) (1+ j))) selection interior))
         (α #'* 0.0625 (reshape u (τ (i j) ((1+ i) (1- j))) selection interior))
         (α #'* 0.0625 (reshape u (τ (i j) ((1- i) (1- j))) selection interior))))
     (τ (i j) ((/ i 2) (/ j 2))))))

(defun residual (u b)
  (let ((interior (interior u))
        (h (/ (1- (sqrt (total-size u))))))
    (fuse* (reshape 0.0 (array-shape u))
           (α #'- (reshape b interior)
              (α #'* (/ (* h h))
                 (α #'+
                    (reshape (α #'* -4.0 u) interior)
                    (reshape u (τ (i j) ((1+ i) j)) interior)
                    (reshape u (τ (i j) ((1- i) j)) interior)
                    (reshape u (τ (i j) (i (1+ j))) interior)
                    (reshape u (τ (i j) (i (1- j))) interior)))))))

(defun v-cycle (u f v1 v2)
  (if (<= (total-size u) 25)
      (rbgs u f 5) ; solve "exactly"
      (let* ((x (rbgs u f v1))
             (r (restrict (residual x f)))
             (s (array-shape r))
             (c (v-cycle (reshape 0.0 s) r v1 v2)))
        (rbgs (α #'- x (prolongate c)) f v2))))
