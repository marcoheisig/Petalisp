(in-package :common-lisp-user)

(defpackage #:petalisp.examples.iterative-methods
  (:shadowing-import-from #:petalisp #:set-difference)
  (:use #:common-lisp #:petalisp)
  (:export
   #:jacobi
   #:rbgs
   #:prolongate
   #:restrict
   #:v-cycle))

(in-package #:petalisp.examples.iterative-methods)

(defun interior (array)
  (flet ((range-interior (range)
           (multiple-value-bind (start step end)
               (range-start-step-end range)
             (range (+ start step) step (- end step)))))
    (make-shape (mapcar #'range-interior (ranges (shape array))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Jacobi method

(defun jacobi (u f h &optional (iterations 1))
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
  (let* ((lazy-array (coerce-to-lazy-array array))
         (ranges (ranges (shape lazy-array))))
    (labels ((prepend-1 (list)
               (cons 1 list))
             (prepend-2 (list)
               (cons 2 list))
             (offsets (red black depth)
               (if (= depth (rank lazy-array))
                   (values red black)
                   (offsets
                    (append (mapcar #'prepend-1 red)
                            (mapcar #'prepend-2 black))
                    (append (mapcar #'prepend-1 black)
                            (mapcar #'prepend-2 red))
                    (1+ depth)))))
      (multiple-value-bind (red-offsets black-offsets)
          (offsets '((2)) '((1)) 1)
        (flet ((offset-space (offsets)
                 (make-shape
                  (loop for offset in offsets
                        for range in ranges
                        collect
                        (multiple-value-bind (start step end)
                            (range-start-step-end range)
                          (range (+ start (* step offset))
                                 (* 2 step)
                                 (- end step)))))))
          (values
           (mapcar #'offset-space red-offsets)
           (mapcar #'offset-space black-offsets)))))))

(defun rbgs (u f h &optional (iterations 1))
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
  (let ((u* (reshape u (τ (i j) ((* i 2) (* j 2))))))
    (trivia:ematch (shape u*)
      ((~ a b c ~ d e f)
       (let ((space-1 (~ (1+ a) b (- (1+ c) b) ~ d e f))
             (space-2 (~ a b c ~ (1+ d) e (- (1+ f) e)))
             (space-3 (~ (1+ a) b (- (1+ c) b) ~ (1+ d) e (- (1+ f) e))))
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
                     (reshape u* (τ (i j) ((1- i) (1- j))) space-3)))))))))

(defun restrict (u)
  (trivia:ematch (shape u)
    ((~ start-1 1 end-1 ~ start-2 1 end-2)
     (let* ((selection (~ start-1 2 end-1 ~ start-2 2 end-2))
            (interior (interior selection)))
       (reshape
        (fuse*
         (reshape u selection)
         (α #'+
            (α #'* 0.25d0   (reshape u interior))
            (α #'* 0.125d0  (reshape u (τ (i j) ((1+ i) j)) interior))
            (α #'* 0.125d0  (reshape u (τ (i j) ((1- i) j)) interior))
            (α #'* 0.125d0  (reshape u (τ (i j) (i (1+ j))) interior))
            (α #'* 0.125d0  (reshape u (τ (i j) (i (1- j))) interior))
            (α #'* 0.0625d0 (reshape u (τ (i j) ((1+ i) (1+ j))) interior))
            (α #'* 0.0625d0 (reshape u (τ (i j) ((1- i) (1+ j))) interior))
            (α #'* 0.0625d0 (reshape u (τ (i j) ((1+ i) (1- j))) interior))
            (α #'* 0.0625d0 (reshape u (τ (i j) ((1- i) (1- j))) interior))))
        (τ (i j) ((/ i 2) (/ j 2))))))))

(defun residual (u b h)
  (let ((interior (interior u)))
    (fuse* (reshape 0d0 (shape u))
           (α #'- (reshape b interior)
              (α #'* (/ 1 (* h h))
                 (α #'-
                    (reshape (α #'* 4.0d0 u) interior)
                    (reshape u (τ (i j) ((1+ i) j)) interior)
                    (reshape u (τ (i j) ((1- i) j)) interior)
                    (reshape u (τ (i j) (i (1+ j))) interior)
                    (reshape u (τ (i j) (i (1- j))) interior)))))))

(defun v-cycle (u f h v1 v2)
  (if (<= (total-size u) 25)
      (rbgs u f h 3)                    ; solve "exactly"
      (let* ((x (rbgs u f h v1))
             (r (restrict (residual x f h)))
             (c (v-cycle (reshape 0d0 (shape r)) r (* 2 h) v1 v2)))
        (rbgs (α #'+ x (prolongate c)) f h v2))))


