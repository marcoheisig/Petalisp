;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defun jacobi (u &key (iterations 1)
                   (h (/ (1- (expt (size u) (/ (dimension u))))))
                   (f 0))
  "Iteratively solve the Poisson equation -Δu = f for a given uniform grid
  with spacing h, using the Jacobi scheme."
  (ecase (dimension u)
    (1 (let ((interior (σ* u ((+ start 1) step (- end 1)))))
         (loop repeat iterations do
           (setf u (fuse* u (α #'* (float 1/2)
                               (α #'+
                                  (-> u (τ (i) (1+ i)) interior)
                                  (-> u (τ (i) (1- i)) interior)
                                  (-> (α #'* (* h h) f) interior))))))
         u))
    (2 (let ((interior (σ* u ((+ start 1) step (- end 1))
                           ((+ start 1) step (- end 1)))))
         (loop repeat iterations do
           (setf u (fuse* u (α #'* (float 1/4)
                               (α #'+
                                  (-> u (τ (i j) (1+ i) j) interior)
                                  (-> u (τ (i j) (1- i) j) interior)
                                  (-> u (τ (i j) i (1+ j)) interior)
                                  (-> u (τ (i j) i (1- j)) interior)
                                  (-> (α #'* (* h h) f) interior))))))
         u))
    (3 (let ((interior (σ* u ((+ start 1) step (- end 1))
                           ((+ start 1) step (- end 1))
                           ((+ start 1) step (- end 1)))))
         (loop repeat iterations do
           (setf u (fuse* u (α #'* (float 1/6)
                               (α #'+
                                  (-> u (τ (i j k) (1+ i) j k) interior)
                                  (-> u (τ (i j k) (1- i) j k) interior)
                                  (-> u (τ (i j k) i (1+ j) k) interior)
                                  (-> u (τ (i j k) i (1- j) k) interior)
                                  (-> u (τ (i j k) i j (1+ k)) interior)
                                  (-> u (τ (i j k) i j (1- k)) interior)
                                  (-> (α #'* (* h h) f) interior))))))
         u))))

(defun jacobi-test ()
  (let ((array #2A((1 1 1 1 1 1)
                   (2 2 2 2 2 2)
                   (1 1 1 1 1 1)
                   (1 1 1 1 1 1)
                   (1 1 1 1 1 1)
                   (1 1 1 1 1 1))))
    (values
     (let ((*virtual-machine* (make-instance 'common-lisp-virtual-machine)))
       (compute (jacobi array)))
     (let ((*virtual-machine* (make-instance 'reference-virtual-machine)))
       (compute (jacobi array))))))
