;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-index-space (strided-array index-space)
  ((element-type :initform t :allocation :class)
   (predecessors :initform () :allocation :class)
   (ranges :type (simple-vector range (*)))))

(defmethod initialize-instance :after ((object strided-array)
                                       &key &allow-other-keys)
  (if (typep object 'strided-array-index-space)
      (setf (slot-value object 'index-space) object)
      (setf (slot-value object 'index-space)
            (make-instance
             'strided-array-index-space
             :ranges (ranges object)))))

(defgeneric make-strided-array-index-space (specification)
  (:method ((space strided-array-index-space)) space)
  (:method ((vector simple-vector))
    (if (every #'range? vector)
        (make-instance
         'strided-array-index-space
         :ranges vector)
        (call-next-method)))
  (:method ((array array))
    (make-instance
     'strided-array-index-space
     :ranges (map 'vector (λ end (range 0 1 (1- end)))
                  (array-dimensions array))))
  (:method ((range-specifications list))
    (make-instance
     'strided-array-index-space
     :ranges (map 'vector (λ spec (apply #'range spec))
                  range-specifications))))

(defmacro σ (&rest range-specifications)
  `(make-strided-array-index-space
    (vector ,@(iterate (for spec in range-specifications)
                       (collect `(range ,@spec))))))

(test strided-array-index-space
  (is (strided-array-index-space? (σ)))
  (is (= 2 (dimension (σ (1 2 3) (4 5 6)))))
  (is (equal? (σ (0 10 20) (5 1 5)) (σ (0 10 25) (5 5 5))))
  (is (= 1000 (size (σ (0 1 9) (0 1 9) (0 1 9))))))

(defmacro σ* (space &rest dimensions)
  (with-gensyms (dim)
    (once-only (space)
      `(symbol-macrolet
           ((,(intern "START") (range-start (aref (ranges ,space) ,dim)))
            (,(intern "STEP") (range-step (aref (ranges ,space) ,dim)))
            (,(intern "END") (range-end (aref (ranges ,space) ,dim))))
         (make-strided-array-index-space
          (vector
           ,@(iterate (for form in dimensions)
                      (for d from 0)
                      (collect `(let ((,dim ,d)) (range ,@form))))))))))

(defmethod difference ((space-1 strided-array-index-space)
                       (space-2 strided-array-index-space))
  (if-let ((intersection (intersection space-1 space-2)))
    (iterate outer
             (for r1 in-vector (ranges space-1))
             (for r2 in-vector (ranges space-2))
             (for i from 0)
             (iterate (for difference in (difference r1 r2))
                      (let ((ranges (copy-array (ranges space-1))))
                        (replace ranges (ranges intersection) :end1 i)
                        (setf (aref ranges i) difference)
                        (in outer
                            (collect
                                (make-instance 'strided-array-index-space
                                               :ranges ranges))))))
    (list space-1)))

(test |(difference strided-array-index-space)|
  (flet ((? (a b)
           (if (intersection a b)
               (is (equal? a (apply #'fusion
                                    (intersection a b)
                                    (difference a b))))
               (is (equal? a (first (difference a b)))))))
    (? (σ (1 1 4)) (σ (1 2 5)))
    (? (σ (1 2 5)) (σ (1 1 4)))
    (? (σ (1 2 5)) (σ (2 2 4)))
    (? (σ (1 1 5) (1 1 5))
       (σ (2 2 4) (2 2 4)))
    (? (σ (1 1 5) (1 1 5))
       (σ (1 2 5) (1 2 5)))
    (? (σ (1 1 9) (1 1 9) (1 1 9))
       (σ (1 8 9) (1 8 9) (1 8 9)))))

(defmethod dimension ((object strided-array-index-space))
  (length (ranges object)))

(defmethod equal? ((object-1 strided-array-index-space)
                   (object-2 strided-array-index-space))
  (and (= (dimension object-1) (dimension object-2))
       (every #'equalp
              (ranges object-1)
              (ranges object-2))))

(defmethod fusion ((object strided-array-index-space) &rest more-objects)
  (let ((objects (cons object more-objects)))
    (make-strided-array-index-space
     (apply #'vector (fuse-recursively objects)))))

(test |(fusion strided-array-index-space)|
  (flet ((? (&rest args)
           (let ((fusion (apply #'fusion args)))
             (is (every (lambda (x) (subspace? x fusion)) args))
             (is (= (size fusion)
                    (reduce #'+ (mapcar #'size args)))))))
    (? (σ (1 1)) (σ (2 2)) (σ (3 3)) (σ (0 0)))
    (? (σ (1 2 (expt 10 15))) (σ (2 2 (expt 10 15))))
    (? (σ (1 5) (1 5))
       (σ (6 10) (1 10))
       (σ (1 5) (6 10)))
    (? (σ (2 2 4) (2 2 4))
       (σ (2 2 4) (1 2 5))
       (σ (1 2 5) (1 1 5)))
    (? (σ (1 2 3) (1 2 3))
       (σ (2 2 4) (1 2 3))
       (σ (1 2 3) (2 2 4))
       (σ (2 2 4) (2 2 4)))
    (?
     (σ (2 2 6) (2 2 6))
     (σ (8 8) (0 2 8))
     (σ (0 0) (0 2 8))
     (σ (2 2 6) (0 0))
     (σ (2 2 6) (8 8)))))

(defmethod intersection ((space-1 strided-array-index-space)
                         (space-2 strided-array-index-space))
  (make-instance
   'strided-array-index-space
   :ranges (map 'vector
                (lambda (a b)
                  (or (intersection a b)
                      (return-from intersection nil)))
                (ranges space-1)
                (ranges space-2))))

(test |(intersection strided-array-index-space)|
  (flet ((? (a b result)
           (is (equal? result (intersection a b)))))
    (?  (σ) (σ) (σ))
    (? (σ (0 9) (0 9)) (σ (2 10) (2 10)) (σ (2 9) (2 9)))
    (? (σ (1 2 3) (0 3 6)) (σ (1 1 3) (0 2 6)) (σ (1 2 3) (0 6 6)))))

(defmethod print-object ((object strided-array-index-space) stream)
  (format stream "~<(σ~@{ ~w~})~:>"
          (map 'list
               (lambda (range)
                 (if (= 1 (range-step range))
                     (list (range-start range)
                           (range-end range))
                     (list (range-start range)
                           (range-step range)
                           (range-end range))))
               (ranges object))))

(defmethod generic-unary-funcall ((transformation affine-transformation)
                                  (object strided-array-index-space))
  (let ((result (make-array (output-dimension transformation)))
        (ranges (ranges object)))
    (iterate (for input-constraint in-vector (input-constraints transformation))
             (for range in-vector ranges)
             (when input-constraint
               (assert (= (range-start range) input-constraint (range-end range)))))
    (let ((A (linear-operator transformation))
          (b (translation-vector transformation)))
      (declare (type scaled-permutation-matrix A)
               (type (simple-array number (*)) b))
      (with-unsafe-optimizations
        (iterate (for row from 0)
                 (for column in-vector (spm-column-indices A))
                 (for scale in-vector (spm-values A))
                 (for offset in-vector b)
                 (let ((range (aref ranges column)))
                   (setf (aref result row)
                         (range
                          (+ offset (* scale (range-start range)))
                          (* scale (range-step range))
                          (+ offset (* scale (range-end range)))))))))
    (make-instance 'strided-array-index-space :ranges result)))

(test |(generic-unery-funcall affine-transformation strided-array-index-space)|
  (flet ((? (object transformation result)
           (is (equal? result (funcall transformation object)))
           (is (equal? object (funcall (inverse transformation) result)))))
    (? (σ (1 1 1)) (τ (m) (1+ m))
       (σ (2 1 2)))
    (? (σ (0 9) (0 5)) (τ (m n) n m)
       (σ (0 5) (0 9)))
    (? (σ (1 1) (2 2) (3 3)) (τ (1 2 3) 4)
       (σ (4 4)))
    (? (σ (2 2)) (τ (m) 1 m 3)
       (σ (1 1) (2 2) (3 3)))
    (? (σ (0 5 10) (0 7 21))
       (τ (m n)
          (+ (* 2 n) 5)
          (+ (* 3 m) 99))
       (σ (5 14 47) (99 15 129)))
    (? (σ (0 0) (0 0) (0 0) (0 0) (0 0))
       (τ (a 0 c 0 e) 0 a 0 c 0 e 0)
       (σ (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)))
    (signals error
      (funcall (τ (1 m) m 1) (σ (0 0) (0 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  fusion islands - specially annotated index spaces

(define-class fusion-island (strided-array-index-space)
  (spaces-to-fuse))

(defun fuse-recursively (spaces)
  (unless (every (composition #'zerop #'dimension) spaces)
    (let ((islands
            (apply
             #'subdivision
             (mapcar
              (lambda (space)
                (let ((ranges (ranges space)))
                  (make-instance
                   'fusion-island
                   :ranges (subseq ranges 0 1)
                   :spaces-to-fuse
                   (list
                    (make-strided-array-index-space
                     (subseq ranges 1))))))
              spaces))))
      (let ((results (mapcar ; recurse
                      (composition #'fuse-recursively #'spaces-to-fuse)
                      islands)))
        (assert (identical results :test #'equal? :key #'first))
        (cons (apply #'fusion
                     (mapcar
                      (lambda (x)
                        (elt (ranges x) 0))
                      islands))
              (first results))))))

(defmethod intersection :around ((space-1 fusion-island)
                                 (space-2 fusion-island))
  (let ((result (call-next-method)))
    (when result
      (change-class result 'fusion-island
                    :spaces-to-fuse (union (spaces-to-fuse space-1)
                                           (spaces-to-fuse space-2))))))

(defmethod difference :around ((space-1 fusion-island)
                               (space-2 fusion-island))
  (let ((result (call-next-method)))
    (mapcar
     (lambda (x)
       (change-class x 'fusion-island
                     :spaces-to-fuse (spaces-to-fuse space-1)))
     result)))

(defmethod size ((object strided-array-index-space))
  (reduce #'* (ranges object) :key #'size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; miscellaneous strided array index space tests

(test |(subdivide strided-array-index-space)|
  (flet ((? (&rest args)
           (let ((result (apply #'subdivision args)))
             ;; check for disjointness
             (let (intersections)
               (map-combinations
                (lambda (x)
                  (push (apply #'intersection x) intersections))
                result :length 2)
               (is (every #'null intersections)))
             ;; check for coverage
             (is (every #'subspace?
                        args
                        (forever (apply #'fusion result)))))))
    (? (σ (1 1 4)) (σ (1 2 5)))
    (? (σ (1 1 10) (1 1 10))
       (σ (5 1 10) (5 1 10)))))

(test |(subspace? strided-array-index-space)|
  (is (subspace? (range 1 1 2) (range 0 1 3)))
  (is (subspace? (range 0 4 8) (range 0 2 10)))
  (is (subspace? (σ (0 6 120) (1 1 100))
                 (σ (0 2 130) (0 1 101)))))
