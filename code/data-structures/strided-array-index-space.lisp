;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-index-space (index-space)
  ((ranges :type simple-vector)))

(defgeneric make-strided-array-index-space (specification)
  (:method ((space strided-array-index-space)) space)
  (:method ((vector simple-vector))
    (if (every #'range? vector)
        (make-instance 'strided-array-index-space
          :ranges vector)
        (call-next-method)))
  (:method ((array array))
    (make-instance 'strided-array-index-space
      :ranges (map 'vector (λ end (range 0 1 (1- end)))
                   (array-dimensions array))))
  (:method ((range-specifications list))
    (make-instance 'strided-array-index-space
      :ranges (map 'vector (λ spec (apply #'range spec))
                   range-specifications))))

(defun strided-array-index-space-generator
    (&key (dimension 3) (max-size 30) (max-extent 100) intersecting)
  (assert (or (not intersecting) (= dimension (dimension intersecting))))
  (let ((range-generators
          (if intersecting
              (map 'list (λ range (range-generator :max-size max-size
                                                   :max-extent max-extent
                                                   :intersecting range))
                   (ranges intersecting))
              (make-list dimension :initial-element
                         (range-generator :max-size max-size
                                          :max-extent max-extent)))))
    (lambda ()
      (make-strided-array-index-space
       (map 'vector #'funcall range-generators)))))

(defmacro σ (&rest range-specifications)
  `(make-strided-array-index-space
    (vector ,@(iterate (for spec in range-specifications)
                       (collect `(range ,@spec))))))

(defmacro σ* (space-form &rest dimensions)
  (with-gensyms (dim space)
    `(let ((,space (index-space (petalispify ,space-form))))
       (symbol-macrolet
           ((,(intern "START") (range-start (aref (ranges ,space) ,dim)))
            (,(intern "STEP") (range-step (aref (ranges ,space) ,dim)))
            (,(intern "END") (range-end (aref (ranges ,space) ,dim))))
         (make-strided-array-index-space
          (vector
           ,@(iterate (for form in dimensions)
                      (for d from 0)
                      (collect `(let ((,dim ,d)) (range ,@form))))))))))

(defmethod common-broadcast-space ((space strided-array-index-space) &rest more-spaces)
  (let ((list-of-ranges (list* (ranges space) (mapcar #'ranges more-spaces))))
    (let ((result-ranges (copy-array (iterate (for ranges in list-of-ranges)
                                              (finding ranges maximizing (length ranges))))))
      (iterate (for ranges in list-of-ranges)
               (for argument from 0)
               (iterate (for range in-vector ranges)
                        (for broadcast-range in-vector result-ranges)
                        (for dimension from 0)
                        (cond
                          ((equal? range broadcast-range)) ; NOP
                          ((unary-range? range)) ; NOP
                          ((unary-range? broadcast-range)
                           (setf (aref result-ranges dimension) range))
                          (t
                           (error "Illegal broadcasting in dimension ~D of argument ~D."
                                  dimension argument)))))
      (make-strided-array-index-space result-ranges))))

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
  (let ((fiveam::*num-trials* (ceiling (sqrt fiveam::*num-trials*))))
    (for-all ((a (strided-array-index-space-generator :dimension 3
                                                      :max-extent 40)))
      (for-all ((b (strided-array-index-space-generator :dimension 3
                                                        :intersecting a
                                                        :max-extent 40)))
        (is (equal? a (apply #'fusion
                             (intersection a b)
                             (difference a b))))))))

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

(defmethod intersection ((space-1 strided-array-index-space)
                         (space-2 strided-array-index-space))
  (make-instance 'strided-array-index-space
    :ranges (map 'vector (λ a b (or (intersection a b)
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
  (flet ((range-list (range)
           (list (range-start range)
                 (range-step range)
                 (range-end range))))
    (prin1 `(σ ,@(map 'list #'range-list (ranges object)))
           stream)))

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
                  (make-instance 'fusion-island
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
             (let ((fusion (apply #'fusion result)))
               (is (every (λ x (subspace? x fusion)) args))))))
    (? (σ (1 1 4)) (σ (1 2 5)))
    (? (σ (1 1 10) (1 1 10))
       (σ (5 1 10) (5 1 10)))))

(test |(subspace? strided-array-index-space)|
  (is (subspace? (range 1 1 2) (range 0 1 3)))
  (is (subspace? (range 0 4 8) (range 0 2 10)))
  (is (subspace? (σ (0 6 120) (1 1 100))
                 (σ (0 2 130) (0 1 101)))))
