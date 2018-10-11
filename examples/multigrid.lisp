(asdf:test-system :petalisp)

(defpackage :petalisp-multigrid-demo
  (:shadowing-import-from :petalisp :set-difference)
  (:use :common-lisp :petalisp))

(in-package :petalisp-multigrid-demo)

(defun present (expression)
  (petalisp-dev:view expression)
  (format t "~%=> ~A~%~%" (compute expression)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Let's implement a Multigrid solverpresent

(defun rbgs (u rhs &optional (iterations 1))
  (let ((r1 (σ* u ((+ start 2) 2 (1- end))
                  ((+ start 2) 2 (1- end))))
        (r2 (σ* u ((+ start 1) 2 (1- end))
                  ((+ start 1) 2 (1- end))))
        (b1 (σ* u ((+ start 2) 2 (1- end))
                  ((+ start 1) 2 (1- end))))
        (b2 (σ* u ((+ start 1) 2 (1- end))
                  ((+ start 2) 2 (1- end))))
        (h (/ (1- (sqrt (size u))))))
    (labels ((update (u what)
               (α #'* 0.25
                  (α #'+
                     (reshape u (τ (i j) ((1+ i) j)) what)
                     (reshape u (τ (i j) ((1- i) j)) what)
                     (reshape u (τ (i j) (i (1+ j))) what)
                     (reshape u (τ (i j) (i (1- j))) what)
                     (reshape (α #'* (* h h) rhs) what)))))
      (loop repeat iterations do
        (setf u (fuse* u (update u r1) (update u r2)))
        (setf u (fuse* u (update u b1) (update u b2))))
      u)))

(defun prolongate (u)
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
  (let ((selection (σ* u (start 2 end) (start 2 end)))
        (inner (σ* u ((1+ start) 1 (1- end))
                     ((1+ start) 1 (1- end)))))
    (reshape
     (fuse*
      (reshape u selection)
      (α #'+
         (α #'* 0.25   (reshape u selection inner))
         (α #'* 0.125  (reshape u (τ (i j) ((1+ i) j)) selection inner))
         (α #'* 0.125  (reshape u (τ (i j) ((1- i) j)) selection inner))
         (α #'* 0.125  (reshape u (τ (i j) (i (1+ j))) selection inner))
         (α #'* 0.125  (reshape u (τ (i j) (i (1- j))) selection inner))
         (α #'* 0.0625 (reshape u (τ (i j) ((1+ i) (1+ j))) selection inner))
         (α #'* 0.0625 (reshape u (τ (i j) ((1- i) (1+ j))) selection inner))
         (α #'* 0.0625 (reshape u (τ (i j) ((1+ i) (1- j))) selection inner))
         (α #'* 0.0625 (reshape u (τ (i j) ((1- i) (1- j))) selection inner))))
     (τ (i j) ((/ i 2) (/ j 2))))))

(defun residual (u b)
  (let ((inner (σ* u
                   ((1+ start) 1 (1- end))
                   ((1+ start) 1 (1- end))))
        (h (/ (1- (sqrt (size u))))))
    (fuse* (reshape 0.0 (index-space u))
           (α #'- (reshape b inner)
              (α #'* (/ (* h h))
                 (α #'+
                    (reshape (α #'* -4.0 u) inner)
                    (reshape u (τ (i j) ((1+ i) j)) inner)
                    (reshape u (τ (i j) ((1- i) j)) inner)
                    (reshape u (τ (i j) (i (1+ j))) inner)
                    (reshape u (τ (i j) (i (1- j))) inner)))))))

(defun v-cycle (u f v1 v2)
  (if (<= (size u) 25)
      (rbgs u f 5) ; solve "exactly"
      (let ((x (rbgs u f v1)))
        (rbgs
         (α #'- x
            (prolongate
             (v-cycle
              (reshape 0.0 (σ* u (0 1 (/ end 2))
                          (0 1 (/ end 2))))
              (restrict
               (residual x f))
              v1 v2)))
         f v2))))

(defun mg (u f n)
  (loop repeat n do
    (setf u (v-cycle u f 1 1)))
  u)

(present (rbgs (reshape 0.0 (σ (0 9) (0 9)))
         (reshape 0.0 (σ (0 9) (0 9))) 1))

(present (rbgs (fuse* (reshape 0.0 (σ (0 9) (0 9)))
                (reshape 1.0 (σ (0 9) (0 0))))
         (reshape 0.0 (σ (0 9) (0 9)))
         1))

(present (fuse* (reshape 0.0 (σ (2 4) (2 4)))
          (reshape 1.0 (σ (3 3) (3 3)))))

(present (prolongate
    (prolongate (fuse* (reshape 0.0 (σ (2 4) (2 4)))
                       (reshape 1.0 (σ (3 3) (3 3)))))))

(present (restrict (reshape 0.0 (σ (0 8) (0 8)))))

(present (fuse* (reshape 0.0 (σ (0 8) (0 8)))
          (reshape 1.0 (σ (2 6) (2 6)))))

(present (fuse* (reshape 0.0 (σ (0 4) (0 4)))
            (reshape 1.0 (σ (2 2) (2 2)))))

(let ((rhs (reshape 0.0 (σ (0 8) (0 8))))
      (u0  (fuse* (reshape 0.0 (σ (0 8) (0 8)))
                  (reshape 1.0 (σ (2 6) (2 6))))))
  (present (residual (rbgs u0 rhs 0) rhs)))

(defun inf-norm (u u-exact)
  (β #'max #'identity
     (β #'max #'identity
        (α #'abs
           (α #'- u u-exact)))))

(let* ((f (reshape 0.0 (σ (0 8) (0 8))))
       (u0  (fuse* f
                   (reshape 1.0
                       (σ* f
                           ((1+ start) step (1- end))
                           ((1+ start) step (1- end)))))))
  (present (mg u0 f 1)))
