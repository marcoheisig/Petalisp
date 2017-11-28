
(asdf:test-system :petalisp)

(in-package :petalisp)

(defun ! (expression)
  (print (compute expression)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Introduction

(! (-> 5 (σ (0 9) (0 9))))

(! (fuse* (-> 0 (σ (0 9) (0 9)))
          (-> 1 (σ (2 7) (2 7)))))

(defun chessboard (h w)
  (fuse (-> 0 (σ (0 2 h) (0 2 w)))
        (-> 0 (σ (1 2 h) (1 2 w)))
        (-> 1 (σ (0 2 h) (1 2 w)))
        (-> 1 (σ (1 2 h) (0 2 w)))))

(! (chessboard 8 5))

(! (α #'* 2 3))

(! (α #'* (-> 2 (σ (0 9))) 3))

(! (β #'+ (α #'* (-> 2 (σ (0 9))) 3)))

(! (-> #(1 2 3 4) (τ (i) (- i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Matrix multiplication

(defun matmul (A B)
  (β #'+
     (α #'*
        (-> A (τ (m n) m 1 n))
        (-> B (τ (n k) 1 k n)))))

(defparameter I #2a((1.0 0.0)
                    (0.0 1.0)))

(defparameter A #2a((2.0 3.0)
                    (4.0 5.0)))

(! (matmul I I))

(! (matmul I A))

(! (matmul A A))

(! (matmul (-> 3.0 (σ (1 10) (1 10)))
           (-> 2.0 (σ (1 10) (1 10)))))

(defparameter M (-> #(1 2 3 4 5 6) (σ (1 6) (1 6))))

(! (matmul M (-> M (τ (m n) n m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Jacobi Method

(defun jacobi-2d (grid &optional (iterations 10))
  (let ((interior
          (σ* grid ((+ start 1) step (- end 1))
                   ((+ start 1) step (- end 1)))))
    (loop repeat iterations do
      (setf grid (fuse*
                  grid
                  (α #'* 0.25
                     (α #'+
                        (-> grid (τ (i j) (1+ i) j) interior)
                        (-> grid (τ (i j) (1- i) j) interior)
                        (-> grid (τ (i j) i (1+ j)) interior)
                        (-> grid (τ (i j) i (1- j)) interior))))))
    grid))

(! (jacobi-2d (-> 0.0 (σ (0 99) (0 99))) 2))


(! (jacobi-2d
    (fuse* (-> 0.0 (σ (0 4) (0 4)))
           (-> 1.0 (σ (1 3) (0 4 4))))
    2))

(time
 (compute
  (jacobi-2d (fuse* (-> 0.0 (σ (0 100) (0 100)))
                    (-> 1.0 (σ (1 99) (0 0)))) 5)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Let's implement a Multigrid solver!

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
                     (-> u (τ (i j) ((1+ i) j)) what)
                     (-> u (τ (i j) ((1- i) j)) what)
                     (-> u (τ (i j) (i (1+ j))) what)
                     (-> u (τ (i j) (i (1- j))) what)
                     (-> (α #'* (* h h) rhs) what)))))
      (loop repeat iterations do
        (setf u (fuse* u (update u r1) (update u r2)))
        (setf u (fuse* u (update u b1) (update u b2))))
      u)))

(! (rbgs (-> 0.0 (σ (0 9) (0 9)))
         (-> 0.0 (σ (0 9) (0 9))) 1))

(! (rbgs (fuse* (-> 0.0 (σ (0 9) (0 9)))
                (-> 1.0 (σ (0 9) (0 0))))
         (-> 0.0 (σ (0 9) (0 9)))
         1))

(defun prolongate (u)
  (let* ((u* (-> u (τ (i j) ((* i 2) (* j 2)))))
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
                (-> u* (τ (i j) ((1+ i) j)) space-1)
                (-> u* (τ (i j) ((1- i) j)) space-1)))
          (α #'* 0.5
             (α #'+
                (-> u* (τ (i j) (i (1+ j))) space-2)
                (-> u* (τ (i j) (i (1- j))) space-2)))
          (α #'* 0.25
             (α #'+
                (-> u* (τ (i j) ((1+ i) (1+ j))) space-3)
                (-> u* (τ (i j) ((1+ i) (1- j))) space-3)
                (-> u* (τ (i j) ((1- i) (1+ j))) space-3)
                (-> u* (τ (i j) ((1- i) (1- j))) space-3))))))

(! (fuse* (-> 0.0 (σ (2 4) (2 4)))
          (-> 1.0 (σ (3 3) (3 3)))))

(! (prolongate (fuse* (-> 0.0 (σ (2 4) (2 4)))
                      (-> 1.0 (σ (3 3) (3 3))))))

(defun restrict (u)
  (let ((selection (σ* u (start 2 end) (start 2 end)))
        (inner (σ* u ((1+ start) 1 (1- end))
                     ((1+ start) 1 (1- end)))))
    (->
     (fuse*
      (-> u selection)
      (α #'+
         (α #'* 0.25   (-> u selection inner))
         (α #'* 0.125  (-> u (τ (i j) (1+ i) j) selection inner))
         (α #'* 0.125  (-> u (τ (i j) (1- i) j) selection inner))
         (α #'* 0.125  (-> u (τ (i j) i (1+ j)) selection inner))
         (α #'* 0.125  (-> u (τ (i j) i (1- j)) selection inner))
         (α #'* 0.0625 (-> u (τ (i j) (1+ i) (1+ j)) selection inner))
         (α #'* 0.0625 (-> u (τ (i j) (1- i) (1+ j)) selection inner))
         (α #'* 0.0625 (-> u (τ (i j) (1+ i) (1- j)) selection inner))
         (α #'* 0.0625 (-> u (τ (i j) (1- i) (1- j)) selection inner))))
     (τ (i j) (/ i 2) (/ j 2)))))

(! (restrict (-> 0.0 (σ (0 8) (0 8)))))

(! (fuse* (-> 0.0 (σ (0 8) (0 8)))
          (-> 1.0 (σ (2 6) (2 6)))))

(! (fuse* (-> 0.0 (σ (0 4) (0 4)))
            (-> 1.0 (σ (2 2) (2 2)))))

(defun residual (u b)
  (let ((inner (σ* u
                   ((1+ start) 1 (1- end))
                   ((1+ start) 1 (1- end))))
        (h (/ (1- (sqrt (size u))))))
    (fuse* (-> 0.0 (index-space u))
           (α #'- (-> b inner)
              (α #'* (/ (* h h))
                 (α #'+
                    (-> (α #'* -4.0 u) inner)
                    (-> u (τ (i j) (1+ i) j) inner)
                    (-> u (τ (i j) (1- i) j) inner)
                    (-> u (τ (i j) i (1+ j)) inner)
                    (-> u (τ (i j) i (1- j)) inner)))))))

(let ((rhs (-> 0.0 (σ (0 8) (0 8))))
      (u0  (fuse* (-> 0.0 (σ (0 8) (0 8)))
                  (-> 1.0 (σ (2 6) (2 6))))))
  (! (residual (rbgs u0 rhs 0) rhs)))

(defun v-cycle (u f v1 v2)
  (if (<= (size u) 25)
      (rbgs u f 5) ; solve "exactly"
      (let ((x (rbgs u f v1)))
        (rbgs
         (α #'- x
            (prolongate
             (v-cycle
              (-> 0.0 (σ* u (0 1 (/ end 2))
                          (0 1 (/ end 2))))
              (restrict
               (residual x f))
              v1 v2)))
         f v2))))

(defun mg (u f n)
  (loop repeat n do
    (setf u (v-cycle u f 1 1)))
  u)

(defun inf-norm (u u-exact)
  (β #'max
     (β #'max
        (α #'abs
           (α #'- u u-exact)))))

(let* ((f (-> 0.0 (σ (0 8) (0 8))))
       (u0  (fuse* f
                   (-> 1.0
                       (σ* f
                           ((1+ start) step (1- end))
                           ((1+ start) step (1- end)))))))
  (view (mg u0 f 1)))
