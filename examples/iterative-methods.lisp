(in-package :common-lisp-user)

(defpackage #:petalisp.examples.iterative-methods
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
    (make-shape (mapcar #'range-interior (shape-ranges (shape array))))))

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

(defun red-black-coloring (array &key (boundary 0))
  (let* ((lazy-array (coerce-to-lazy-array array)))
    (labels ((red-black-shapes (red black ranges)
               (if (null ranges)
                   (values
                    (mapcar #'make-shape red)
                    (mapcar #'make-shape black))
                   (with-accessors ((start range-start)
                                    (step range-step)
                                    (end range-end)
                                    (size range-size))
                       (first ranges)
                     (cond ((<= size (* 2 boundary))
                            (return-from red-black-coloring (values '() '())))
                           ((= size (1+ (* 2 boundary)))
                            (let ((range (range (+ start (* boundary step)))))
                              (red-black-shapes
                               (mapcar (alexandria:curry #'cons range) red)
                               (mapcar (alexandria:curry #'cons range) black)
                               (rest ranges))))
                           (t
                            (let* ((new-start (+ start (* boundary step)))
                                   (new-step (* step 2))
                                   (new-end (- end (* boundary step)))
                                   (range-1 (range new-start new-step new-end))
                                   (range-2 (range (+ new-start step) new-step new-end)))
                              (red-black-shapes
                               (nconc
                                (mapcar (alexandria:curry #'cons range-1) red)
                                (mapcar (alexandria:curry #'cons range-2) black))
                               (nconc
                                (mapcar (alexandria:curry #'cons range-1) black)
                                (mapcar (alexandria:curry #'cons range-2) red))
                               (rest ranges)))))))))
      (red-black-shapes '(()) '() (reverse (shape-ranges (shape lazy-array)))))))

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
        (red-black-coloring u :boundary 1)
      (flet ((update (spaces)
               (setf u (apply #'fuse* u (mapcar stencil spaces)))))
        (loop repeat iterations do
          (update red-spaces)
          (update black-spaces))
        u))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Multigrid Method

(defun scale-array (array factor)
  (let* ((lazy-array (coerce-to-lazy-array array)))
    (reshape
     array
     (make-transformation
            :scalings (make-array (rank lazy-array) :initial-element factor)))))

(defun prolongate (u)
  (let ((u* (scale-array u 2)))
    (trivia:ematch (shape u*)
      ((~ start-1 2 end-1)
       (let ((space-1 (~ (1+ start-1) 2 end-1)))
         (fuse u*
               (α #'* 1/2
                  (α #'+
                     (reshape u* (τ (i) ((1+ i))) space-1)
                     (reshape u* (τ (i) ((1- i))) space-1))))))
      ((~ start-1 2 end-1 ~ start-2 2 end-2)
       (let ((space-1 (~ (1+ start-1) 2 end-1  ~ start-2 2 end-2))
             (space-2 (~ start-1 2 end-1 ~ (1+ start-2) 2 end-2))
             (space-3 (~ (1+ start-1) 2 end-1 ~ (1+ start-2) 2 end-2)))
         (fuse u*
               (α #'* 1/2
                  (α #'+
                     (reshape u* (τ (i j) ((1+ i) j)) space-1)
                     (reshape u* (τ (i j) ((1- i) j)) space-1)))
               (α #'* 1/2
                  (α #'+
                     (reshape u* (τ (i j) (i (1+ j))) space-2)
                     (reshape u* (τ (i j) (i (1- j))) space-2)))
               (α #'* 1/4
                  (α #'+
                     (reshape u* (τ (i j) ((1+ i) (1+ j))) space-3)
                     (reshape u* (τ (i j) ((1+ i) (1- j))) space-3)
                     (reshape u* (τ (i j) ((1- i) (1+ j))) space-3)
                     (reshape u* (τ (i j) ((1- i) (1- j))) space-3))))))
      ((~ start-1 2 end-1 ~ start-2 2 end-2 ~ start-3 2 end-3)
       (let ((space-1 (~ (1+ start-1) 2 end-1 ~ start-2 2 end-2 ~ start-3 2 end-3))
             (space-2 (~ start-1 2 end-1 ~ (1+ start-2) 2 end-2 ~ start-3 2 end-3))
             (space-3 (~ start-1 2 end-1 ~ start-2 2 end-2 ~ (1+ start-3) 2 end-3))
             (space-4 (~ start-1 2 end-1 ~ (1+ start-2) 2 end-2 ~ (1+ start-3) 2 end-3))
             (space-5 (~ (1+ start-1) 2 end-1 ~ start-2 2 end-2 ~ (1+ start-3) 2 end-3))
             (space-6 (~ (1+ start-1) 2 end-1 ~ (1+ start-2) 2 end-2 ~ start-3 2 end-3))
             (space-7 (~ (1+ start-1) 2 end-1 ~ (1+ start-2) 2 end-2 ~ (1+ start-3) 2 end-3)))
         (fuse u*
               (α #'* 1/2
                  (α #'+
                     (reshape u* (τ (i j k) ((1+ i) j k)) space-1)
                     (reshape u* (τ (i j k) ((1- i) j k)) space-1)))
               (α #'* 1/2
                  (α #'+
                     (reshape u* (τ (i j k) (i (1+ j) k)) space-2)
                     (reshape u* (τ (i j k) (i (1- j) k)) space-2)))
               (α #'* 1/2
                  (α #'+
                     (reshape u* (τ (i j k) (i j (1+ k))) space-3)
                     (reshape u* (τ (i j k) (i j (1- k))) space-3)))
               (α #'* 1/4
                  (α #'+
                     (reshape u* (τ (i j k) (i (1+ j) (1+ k))) space-4)
                     (reshape u* (τ (i j k) (i (1+ j) (1- k))) space-4)
                     (reshape u* (τ (i j k) (i (1- j) (1+ k))) space-4)
                     (reshape u* (τ (i j k) (i (1- j) (1- k))) space-4)))
               (α #'* 1/4
                  (α #'+
                     (reshape u* (τ (i j k) ((1+ i) j (1+ k))) space-5)
                     (reshape u* (τ (i j k) ((1+ i) j (1- k))) space-5)
                     (reshape u* (τ (i j k) ((1- i) j (1+ k))) space-5)
                     (reshape u* (τ (i j k) ((1- i) j (1- k))) space-5)))
               (α #'* 1/4
                  (α #'+
                     (reshape u* (τ (i j k) ((1+ i) (1+ j) k)) space-6)
                     (reshape u* (τ (i j k) ((1+ i) (1- j) k)) space-6)
                     (reshape u* (τ (i j k) ((1- i) (1+ j) k)) space-6)
                     (reshape u* (τ (i j k) ((1- i) (1- j) k)) space-6)))
               (α #'* 1/8
                  (α #'+
                     (reshape u* (τ (i j k) ((1+ i) (1+ j) (1+ k))) space-7)
                     (reshape u* (τ (i j k) ((1+ i) (1+ j) (1- k))) space-7)
                     (reshape u* (τ (i j k) ((1+ i) (1- j) (1+ k))) space-7)
                     (reshape u* (τ (i j k) ((1+ i) (1- j) (1- k))) space-7)
                     (reshape u* (τ (i j k) ((1- i) (1+ j) (1+ k))) space-7)
                     (reshape u* (τ (i j k) ((1- i) (1+ j) (1- k))) space-7)
                     (reshape u* (τ (i j k) ((1- i) (1- j) (1+ k))) space-7)
                     (reshape u* (τ (i j k) ((1- i) (1- j) (1- k))) space-7)))))))))

(defun restrict (u)
  (trivia:ematch (shape u)
    ((~ start-1 1 end-1)
     (let* ((selection (~ start-1 2 end-1))
            (interior (interior selection)))
       (reshape
        (fuse*
         (reshape u selection)
         (α #'+
            (α #'* 1/2 (reshape u interior))
            (α #'* 1/4 (reshape u (τ (i) ((1+ i))) interior))
            (α #'* 1/4 (reshape u (τ (i) ((1- i))) interior))))
        (τ (i) ((+ start-1 (/ (- i start-1) 2)))))))
    ((~ start-1 1 end-1 ~ start-2 1 end-2)
     (let* ((selection (~ start-1 2 end-1 ~ start-2 2 end-2))
            (interior (interior selection)))
       (reshape
        (fuse*
         (reshape u selection)
         (α #'+
            (α #'* 1/4
               (reshape u interior))
            (α #'* 1/8
               (α #'+
                  (reshape u (τ (i j) ((1+ i) j)) interior)
                  (reshape u (τ (i j) ((1- i) j)) interior)
                  (reshape u (τ (i j) (i (1+ j))) interior)
                  (reshape u (τ (i j) (i (1- j))) interior)))
            (α #'* 1/16
               (reshape u (τ (i j) ((1+ i) (1+ j))) interior)
               (reshape u (τ (i j) ((1- i) (1+ j))) interior)
               (reshape u (τ (i j) ((1+ i) (1- j))) interior)
               (reshape u (τ (i j) ((1- i) (1- j))) interior))))
        (τ (i j) ((+ start-1 (/ (- i start-1) 2))
                  (+ start-2 (/ (- j start-2) 2)))))))
    ((~ start-1 1 end-1 ~ start-2 1 end-2 ~ start-3 1 end-3)
     (let* ((selection (~ start-1 2 end-1 ~ start-2 2 end-2 ~ start-3 2 end-3))
            (interior (interior selection)))
       (reshape
        (fuse*
         (reshape u selection)
         (α #'+
            (α #'* 1/8
               (reshape u interior))
            (α #'* 1/16
               (reshape u (τ (i j k) ((1+ i) j k)) interior)
               (reshape u (τ (i j k) (i (1+ j) k)) interior)
               (reshape u (τ (i j k) (i j (1+ k))) interior)
               (reshape u (τ (i j k) ((1- i) j k)) interior)
               (reshape u (τ (i j k) (i (1- j) k)) interior)
               (reshape u (τ (i j k) (i j (1- k))) interior))
            (α #'* 1/32
               (reshape u (τ (i j k) (i (1+ j) (1+ k))) interior)
               (reshape u (τ (i j k) ((1+ i) j (1+ k))) interior)
               (reshape u (τ (i j k) ((1+ i) (1+ j) k)) interior)
               (reshape u (τ (i j k) (i (1- j) (1+ k))) interior)
               (reshape u (τ (i j k) ((1- i) j (1+ k))) interior)
               (reshape u (τ (i j k) ((1- i) (1+ j) k)) interior)
               (reshape u (τ (i j k) (i (1+ j) (1- k))) interior)
               (reshape u (τ (i j k) ((1+ i) j (1- k))) interior)
               (reshape u (τ (i j k) ((1+ i) (1- j) k)) interior)
               (reshape u (τ (i j k) (i (1- j) (1- k))) interior)
               (reshape u (τ (i j k) ((1- i) j (1- k))) interior)
               (reshape u (τ (i j k) ((1- i) (1- j) k)) interior))
            (α #'* 1/64
               (reshape u (τ (i j k) ((1+ i) (1+ j) (1+ k))) interior)
               (reshape u (τ (i j k) ((1+ i) (1+ j) (1- k))) interior)
               (reshape u (τ (i j k) ((1+ i) (1- j) (1+ k))) interior)
               (reshape u (τ (i j k) ((1+ i) (1- j) (1- k))) interior)
               (reshape u (τ (i j k) ((1- i) (1+ j) (1+ k))) interior)
               (reshape u (τ (i j k) ((1- i) (1+ j) (1- k))) interior)
               (reshape u (τ (i j k) ((1- i) (1- j) (1+ k))) interior)
               (reshape u (τ (i j k) ((1- i) (1- j) (1- k))) interior))))
        (τ (i j k) ((+ start-1 (/ (- i start-1) 2))
                    (+ start-2 (/ (- j start-2) 2))
                    (+ start-3 (/ (- k start-3) 2)))))))))

(defun residual (u b h)
  (let ((interior (interior u)))
    (ecase (rank u)
      (1
       (fuse* (reshape 0d0 (shape u))
              (α #'- (reshape b interior)
                 (α #'* (/ 1 (* h h))
                    (α #'-
                       (reshape (α #'* 2 u) interior)
                       (reshape u (τ (i) ((1+ i))) interior)
                       (reshape u (τ (i) ((1- i))) interior))))))
      (2
       (fuse* (reshape 0d0 (shape u))
              (α #'- (reshape b interior)
                 (α #'* (/ 1 (* h h))
                    (α #'-
                       (reshape (α #'* 4 u) interior)
                       (reshape u (τ (i j) ((1+ i) j)) interior)
                       (reshape u (τ (i j) ((1- i) j)) interior)
                       (reshape u (τ (i j) (i (1+ j))) interior)
                       (reshape u (τ (i j) (i (1- j))) interior))))))
      (3
       (fuse* (reshape 0d0 (shape u))
              (α #'- (reshape b interior)
                 (α #'* (/ 1 (* h h))
                    (α #'-
                       (reshape (α #'* 6 u) interior)
                       (reshape u (τ (i j k) ((1+ i) j k)) interior)
                       (reshape u (τ (i j k) ((1- i) j k)) interior)
                       (reshape u (τ (i j k) (i (1+ j) k)) interior)
                       (reshape u (τ (i j k) (i (1- j) k)) interior)
                       (reshape u (τ (i j k) (i j (1+ k))) interior)
                       (reshape u (τ (i j k) (i j (1- k))) interior)))))))))

(defun v-cycle (u f h v1 v2)
  (if (<= (range-size (first (shape-ranges (shape u)))) 3)
      (rbgs u f h 3) ; solve "exactly"
      (let* ((x (rbgs u f h v1))
             (r (restrict (residual x f h)))
             (c (v-cycle (reshape 0d0 (shape r)) r (* 2 h) v1 v2)))
        (rbgs (α #'+ x (prolongate c)) f h v2))))


