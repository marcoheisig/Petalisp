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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Jacobi method

(defun jacobi (u f h &optional (iterations 1))
  "Iteratively solve the Poisson equation -Δu = f for a given uniform grid
  with spacing h, using the Jacobi scheme."
  (let* ((u (lazy-array u))
         (f (lazy-array f)))
    (ecase (lazy-array-rank u)
      (1
       (let ((interior (lazy-reshape u (peeler 1))))
         (loop repeat iterations do
           (setf u (lazy-overwrite
                    u
                    (lazy #'* (float 1/2)
                     (lazy #'+
                      (lazy-reshape u (transform i to (1+ i)) interior)
                      (lazy-reshape u (transform i to (1- i)) interior)
                      (lazy-reshape (lazy #'* (* h h) f) interior)))))))
       u)
      (2
       (let ((interior (lazy-reshape u (peeler 1 1))))
         (loop repeat iterations do
           (setf u (lazy-overwrite
                    u
                    (lazy #'* (float 1/4)
                     (lazy #'+
                      (lazy-reshape  u (transform i j to (1+ i) j) interior)
                      (lazy-reshape  u (transform i j to (1- i) j) interior)
                      (lazy-reshape  u (transform i j to i (1+ j)) interior)
                      (lazy-reshape  u (transform i j to i (1- j)) interior)
                      (lazy-reshape (lazy #'* (* h h) f) interior)))))))
       u)
      (3
       (let ((interior (lazy-reshape u (peeler 1 1 1))))
         (loop repeat iterations do
           (setf u (lazy-overwrite
                    u
                    (lazy #'* (float 1/6)
                     (lazy #'+
                      (lazy-reshape  u (transform i j k to (1+ i) j k) interior)
                      (lazy-reshape  u (transform i j k to (1- i) j k) interior)
                      (lazy-reshape  u (transform i j k to i (1+ j) k) interior)
                      (lazy-reshape  u (transform i j k to i (1- j) k) interior)
                      (lazy-reshape  u (transform i j k to i j (1+ k)) interior)
                      (lazy-reshape  u (transform i j k to i j (1- k)) interior)
                      (lazy-reshape (lazy #'* (* h h) f) interior)))))))
       u))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Gauss-Seidel Method

(defun red-black-coloring (array &key (boundary 0))
  (let* ((lazy-array (lazy-array array)))
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
                            (let* ((index (+ start (* boundary step)))
                                   (range (range index (1+ index))))
                              (red-black-shapes
                               (mapcar (alexandria:curry #'cons range) red)
                               (mapcar (alexandria:curry #'cons range) black)
                               (rest ranges))))
                           (t
                            (let* ((new-start (+ start (* boundary step)))
                                   (new-end (- end (* boundary step)))
                                   (new-step (* step 2))
                                   (range-1 (range new-start new-end new-step))
                                   (range-2 (range (+ new-start step) new-end new-step)))
                              (red-black-shapes
                               (nconc
                                (mapcar (alexandria:curry #'cons range-1) red)
                                (mapcar (alexandria:curry #'cons range-2) black))
                               (nconc
                                (mapcar (alexandria:curry #'cons range-1) black)
                                (mapcar (alexandria:curry #'cons range-2) red))
                               (rest ranges)))))))))
      (red-black-shapes '(()) '() (reverse (shape-ranges (lazy-array-shape lazy-array)))))))

(defun rbgs (u f h &optional (iterations 1))
  "Iteratively solve the Poisson equation -Δu = f for a given uniform grid
  with spacing h, using the Red-Black Gauss-Seidel scheme."
  (let* ((u (lazy-array u))
         (f (lazy-array f))
         (stencil
           (ecase (lazy-array-rank u)
             (1 (lambda (space)
                  (lazy #'* (float 1/2)
                   (lazy #'+
                    (lazy-reshape u (transform i to (1+ i)) space)
                    (lazy-reshape u (transform i to (1- i)) space)
                    (lazy-reshape (lazy #'* (* h h) f) space)))))
             (2 (lambda (space)
                  (lazy #'* (float 1/4)
                   (lazy #'+
                    (lazy-reshape u (transform i j to (1+ i) j) space)
                    (lazy-reshape u (transform i j to (1- i) j) space)
                    (lazy-reshape u (transform i j to i (1+ j)) space)
                    (lazy-reshape u (transform i j to i (1- j)) space)
                    (lazy-reshape (lazy #'* (* h h) f) space)))))
             (3 (lambda (space)
                  (lazy #'* (float 1/6)
                   (lazy #'+
                    (lazy-reshape u (transform i j k to (1+ i) j k) space)
                    (lazy-reshape u (transform i j k to (1- i) j k) space)
                    (lazy-reshape u (transform i j k to i (1+ j) k) space)
                    (lazy-reshape u (transform i j k to i (1- j) k) space)
                    (lazy-reshape u (transform i j k to i j (1+ k)) space)
                    (lazy-reshape u (transform i j k to i j (1- k)) space)
                    (lazy-reshape (lazy #'* (* h h) f) space))))))))
    (multiple-value-bind (red-spaces black-spaces)
        (red-black-coloring u :boundary 1)
      (flet ((update (spaces)
               (setf u (apply #'lazy-overwrite u (mapcar stencil spaces)))))
        (loop repeat iterations do
          (update red-spaces)
          (update black-spaces))
        u))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Multigrid Method

(defun scale-array (array factor)
  (let* ((lazy-array (lazy-array array)))
    (lazy-reshape
     array
     (make-transformation
      :scalings (make-array (lazy-array-rank lazy-array) :initial-element factor)))))

(defun prolongate (u)
  (let ((u* (scale-array u 2)))
    (trivia:ematch (lazy-array-shape u*)
      ((~ start-1 end-1 2 ~ start-2 end-2 2 ~ start-3 end-3 2)
       (let ((space-1 (~ (1+ start-1) end-1 2 ~ start-2 end-2 2 ~ start-3 end-3 2))
             (space-2 (~ start-1 end-1 2 ~ (1+ start-2) end-2 2 ~ start-3 end-3 2))
             (space-3 (~ start-1 end-1 2 ~ start-2 end-2 2 ~ (1+ start-3) end-3 2))
             (space-4 (~ start-1 end-1 2 ~ (1+ start-2) end-2 2 ~ (1+ start-3) end-3 2))
             (space-5 (~ (1+ start-1) end-1 2 ~ start-2 end-2 2 ~ (1+ start-3) end-3 2))
             (space-6 (~ (1+ start-1) end-1 2 ~ (1+ start-2) end-2 2 ~ start-3 end-3 2))
             (space-7 (~ (1+ start-1) end-1 2 ~ (1+ start-2) end-2 2 ~ (1+ start-3) end-3 2)))
         (lazy-fuse
          u*
          (lazy #'* 1/2
           (lazy #'+
            (lazy-reshape u* (transform i j k to (1+ i) j k) space-1)
            (lazy-reshape u* (transform i j k to (1- i) j k) space-1)))
          (lazy #'* 1/2
           (lazy #'+
            (lazy-reshape u* (transform i j k to i (1+ j) k) space-2)
            (lazy-reshape u* (transform i j k to i (1- j) k) space-2)))
          (lazy #'* 1/2
           (lazy #'+
            (lazy-reshape u* (transform i j k to i j (1+ k)) space-3)
            (lazy-reshape u* (transform i j k to i j (1- k)) space-3)))
          (lazy #'* 1/4
           (lazy #'+
            (lazy-reshape u* (transform i j k to i (1+ j) (1+ k)) space-4)
            (lazy-reshape u* (transform i j k to i (1+ j) (1- k)) space-4)
            (lazy-reshape u* (transform i j k to i (1- j) (1+ k)) space-4)
            (lazy-reshape u* (transform i j k to i (1- j) (1- k)) space-4)))
          (lazy #'* 1/4
           (lazy #'+
            (lazy-reshape u* (transform i j k to (1+ i) j (1+ k)) space-5)
            (lazy-reshape u* (transform i j k to (1+ i) j (1- k)) space-5)
            (lazy-reshape u* (transform i j k to (1- i) j (1+ k)) space-5)
            (lazy-reshape u* (transform i j k to (1- i) j (1- k)) space-5)))
          (lazy #'* 1/4
           (lazy #'+
            (lazy-reshape u* (transform i j k to (1+ i) (1+ j) k) space-6)
            (lazy-reshape u* (transform i j k to (1+ i) (1- j) k) space-6)
            (lazy-reshape u* (transform i j k to (1- i) (1+ j) k) space-6)
            (lazy-reshape u* (transform i j k to (1- i) (1- j) k) space-6)))
          (lazy #'* 1/8
           (lazy #'+
            (lazy-reshape u* (transform i j k to (1+ i) (1+ j) (1+ k)) space-7)
            (lazy-reshape u* (transform i j k to (1+ i) (1+ j) (1- k)) space-7)
            (lazy-reshape u* (transform i j k to (1+ i) (1- j) (1+ k)) space-7)
            (lazy-reshape u* (transform i j k to (1+ i) (1- j) (1- k)) space-7)
            (lazy-reshape u* (transform i j k to (1- i) (1+ j) (1+ k)) space-7)
            (lazy-reshape u* (transform i j k to (1- i) (1+ j) (1- k)) space-7)
            (lazy-reshape u* (transform i j k to (1- i) (1- j) (1+ k)) space-7)
            (lazy-reshape u* (transform i j k to (1- i) (1- j) (1- k)) space-7))))))
      ((~ start-1 end-1 2 ~ start-2 end-2 2)
       (let ((space-1 (~ (1+ start-1) end-1 2  ~ start-2 end-2 2))
             (space-2 (~ start-1 end-1 2 ~ (1+ start-2) end-2 2))
             (space-3 (~ (1+ start-1) end-1 2 ~ (1+ start-2) end-2 2)))
         (lazy-fuse
          u*
          (lazy #'* 1/2
           (lazy #'+
            (lazy-reshape u* (transform i j to (1+ i) j) space-1)
            (lazy-reshape u* (transform i j to (1- i) j) space-1)))
          (lazy #'* 1/2
           (lazy #'+
            (lazy-reshape u* (transform i j to i (1+ j)) space-2)
            (lazy-reshape u* (transform i j to i (1- j)) space-2)))
          (lazy #'* 1/4
           (lazy #'+
            (lazy-reshape u* (transform i j to (1+ i) (1+ j)) space-3)
            (lazy-reshape u* (transform i j to (1+ i) (1- j)) space-3)
            (lazy-reshape u* (transform i j to (1- i) (1+ j)) space-3)
            (lazy-reshape u* (transform i j to (1- i) (1- j)) space-3))))))
      ((~ start-1 end-1 2)
       (let ((space-1 (~ (1+ start-1) end-1 2)))
         (lazy-fuse
          u*
          (lazy #'* 1/2
           (lazy #'+
            (lazy-reshape u* (transform i to (1+ i)) space-1)
            (lazy-reshape u* (transform i to (1- i)) space-1)))))))))

(defun restrict (u)
  (let ((u (lazy-array u)))
    (ecase (lazy-array-rank u)
      (1 (let* ((selection (lazy-reshape u 1 (peeler '(0 0 2))))
                (interior (lazy-reshape selection 1 (peeler 1))))
           (lazy-reshape
            (lazy-overwrite
             (lazy-reshape u selection)
             (lazy #'+
              (lazy #'* 1/2 (lazy-reshape u interior))
              (lazy #'* 1/4 (lazy-reshape u (transform i to (1+ i)) interior))
              (lazy #'* 1/4 (lazy-reshape u (transform i to (1- i)) interior))))
            (deflater 1))))
      (2 (let* ((selection (lazy-reshape u 2 (peeler '(0 0 2) '(0 0 2))))
                (interior (lazy-reshape selection 2 (peeler 1 1))))
           (lazy-reshape
            (lazy-overwrite
             (lazy-reshape u selection)
             (lazy #'+
              (lazy #'* 1/4
               (lazy-reshape u interior))
              (lazy #'* 1/8
               (lazy #'+
                (lazy-reshape u (transform i j to (1+ i) j) interior)
                (lazy-reshape u (transform i j to (1- i) j) interior)
                (lazy-reshape u (transform i j to i (1+ j)) interior)
                (lazy-reshape u (transform i j to i (1- j)) interior)))
              (lazy #'* 1/16
               (lazy-reshape u (transform i j to (1+ i) (1+ j)) interior)
               (lazy-reshape u (transform i j to (1- i) (1+ j)) interior)
               (lazy-reshape u (transform i j to (1+ i) (1- j)) interior)
               (lazy-reshape u (transform i j to (1- i) (1- j)) interior))))
            (deflater 2))))
      (3 (let* ((selection (lazy-reshape u 3 (peeler '(0 0 2) '(0 0 2) '(0 0 2))))
                (interior (lazy-reshape selection 3 (peeler 1 1 1))))
           (lazy-reshape
            (lazy-overwrite
             selection
             (lazy #'+
              (lazy #'* 1/8
               (lazy-reshape u interior))
              (lazy #'* 1/16
               (lazy-reshape u (transform i j k to (1+ i) j k) interior)
               (lazy-reshape u (transform i j k to i (1+ j) k) interior)
               (lazy-reshape u (transform i j k to i j (1+ k)) interior)
               (lazy-reshape u (transform i j k to (1- i) j k) interior)
               (lazy-reshape u (transform i j k to i (1- j) k) interior)
               (lazy-reshape u (transform i j k to i j (1- k)) interior))
              (lazy #'* 1/32
               (lazy-reshape u (transform i j k to i (1+ j) (1+ k)) interior)
               (lazy-reshape u (transform i j k to (1+ i) j (1+ k)) interior)
               (lazy-reshape u (transform i j k to (1+ i) (1+ j) k) interior)
               (lazy-reshape u (transform i j k to i (1- j) (1+ k)) interior)
               (lazy-reshape u (transform i j k to (1- i) j (1+ k)) interior)
               (lazy-reshape u (transform i j k to (1- i) (1+ j) k) interior)
               (lazy-reshape u (transform i j k to i (1+ j) (1- k)) interior)
               (lazy-reshape u (transform i j k to (1+ i) j (1- k)) interior)
               (lazy-reshape u (transform i j k to (1+ i) (1- j) k) interior)
               (lazy-reshape u (transform i j k to i (1- j) (1- k)) interior)
               (lazy-reshape u (transform i j k to (1- i) j (1- k)) interior)
               (lazy-reshape u (transform i j k to (1- i) (1- j) k) interior))
              (lazy #'* 1/64
               (lazy-reshape u (transform i j k to (1+ i) (1+ j) (1+ k)) interior)
               (lazy-reshape u (transform i j k to (1+ i) (1+ j) (1- k)) interior)
               (lazy-reshape u (transform i j k to (1+ i) (1- j) (1+ k)) interior)
               (lazy-reshape u (transform i j k to (1+ i) (1- j) (1- k)) interior)
               (lazy-reshape u (transform i j k to (1- i) (1+ j) (1+ k)) interior)
               (lazy-reshape u (transform i j k to (1- i) (1+ j) (1- k)) interior)
               (lazy-reshape u (transform i j k to (1- i) (1- j) (1+ k)) interior)
               (lazy-reshape u (transform i j k to (1- i) (1- j) (1- k)) interior))))
            (deflater 3)))))))

(defun residual (u b h)
  (let* ((u (lazy-array u))
         (b (lazy-array b)))
    (ecase (lazy-array-rank u)
      (1
       (let ((interior (lazy-reshape u (peeler 1))))
         (lazy-overwrite
          (lazy-reshape 0d0 (lazy-array-shape u))
          (lazy #'- (lazy-reshape b interior)
           (lazy #'* (/ 1 (* h h))
            (lazy #'-
             (lazy-reshape (lazy #'* 2 u) interior)
             (lazy-reshape u (transform i to (1+ i)) interior)
             (lazy-reshape u (transform i to (1- i)) interior)))))))
      (2
       (let ((interior (lazy-reshape u (peeler 1 1))))
         (lazy-overwrite
          (lazy-reshape 0d0 (lazy-array-shape u))
          (lazy #'- (lazy-reshape b interior)
           (lazy #'* (/ 1 (* h h))
            (lazy #'-
             (lazy-reshape (lazy #'* 4 u) interior)
             (lazy-reshape u (transform i j to (1+ i) j) interior)
             (lazy-reshape u (transform i j to (1- i) j) interior)
             (lazy-reshape u (transform i j to i (1+ j)) interior)
             (lazy-reshape u (transform i j to i (1- j)) interior)))))))
      (3
       (let ((interior (lazy-reshape u (peeler 1 1 1))))
         (lazy-overwrite
          (lazy-reshape 0d0 (lazy-array-shape u))
          (lazy #'- (lazy-reshape b interior)
           (lazy #'* (/ 1 (* h h))
            (lazy #'-
             (lazy-reshape (lazy #'* 6 u) interior)
             (lazy-reshape u (transform i j k to (1+ i) j k) interior)
             (lazy-reshape u (transform i j k to (1- i) j k) interior)
             (lazy-reshape u (transform i j k to i (1+ j) k) interior)
             (lazy-reshape u (transform i j k to i (1- j) k) interior)
             (lazy-reshape u (transform i j k to i j (1+ k)) interior)
             (lazy-reshape u (transform i j k to i j (1- k)) interior))))))))))

(defun v-cycle (u f h v1 v2)
  (let ((u (lazy-array u))
        (f (lazy-array f)))
    (if (<= (range-size (first (shape-ranges (lazy-array-shape u)))) 3)
        (rbgs u f h 3) ; solve "exactly"
        (let* ((x (rbgs u f h v1))
               (r (restrict (residual x f h)))
               (c (v-cycle (lazy-reshape 0d0 (lazy-array-shape r)) r (* 2 h) v1 v2)))
          (rbgs (lazy #'+ x (prolongate c)) f h v2)))))


