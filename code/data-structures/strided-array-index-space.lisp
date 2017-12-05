;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-index-space (index-space)
  ((ranges :type vector)))

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
      (index-space
       (map 'vector #'funcall range-generators)))))

(defmacro σ (&rest range-specifications)
  `(index-space
    (vector ,@(iterate (for spec in range-specifications)
                       (collect `(range ,@spec))))))

(defmacro σ* (space-form &rest dimensions)
  (with-gensyms (dim space)
    `(let ((,space (index-space (immediate ,space-form))))
       (symbol-macrolet
           ((,(intern "START") (range-start (aref (ranges ,space) ,dim)))
            (,(intern "STEP") (range-step (aref (ranges ,space) ,dim)))
            (,(intern "END") (range-end (aref (ranges ,space) ,dim))))
         (index-space
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
      (index-space result-ranges))))

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

(defmethod dimension ((object strided-array-index-space))
  (length (ranges object)))

(defmethod enlarge-index-space
    ((from strided-array-index-space)
     (to strided-array-index-space))
  (let ((new-ranges (copy-array (ranges (index-space to)))))
    (replace new-ranges (ranges from))
    (index-space new-ranges)))

(defmethod equal? ((object-1 strided-array-index-space)
                   (object-2 strided-array-index-space))
  (and (= (dimension object-1) (dimension object-2))
       (every #'equalp
              (ranges object-1)
              (ranges object-2))))

(defmethod fusion ((object strided-array-index-space) &rest more-objects)
  (let ((objects (cons object more-objects)))
    (with-memoization ((mapcar #'ranges objects) :test #'equalp)
      (index-space
       (apply #'vector (fuse-recursively objects))))))

(defmethod index-space ((array array))
  (make-instance 'strided-array-index-space
    :ranges (map 'vector (λ end (range 0 1 (1- end)))
                 (array-dimensions array))))

(defmethod index-space ((range-specifications list))
  (flet ((rangeify (spec)
           (etypecase spec
             (integer (range 0 spec))
             (list (apply #'range spec))
             (range spec))))
    (make-instance 'strided-array-index-space
      :ranges (map 'vector #'rangeify range-specifications))))

(defmethod index-space ((vector vector))
    (if (every #'range? vector)
        (make-instance 'strided-array-index-space :ranges vector)
        (call-next-method)))

(defmethod intersection ((space-1 strided-array-index-space)
                         (space-2 strided-array-index-space))
  (make-instance 'strided-array-index-space
    :ranges (map 'vector (λ a b (or (intersection a b)
                                    (return-from intersection nil)))
                 (ranges space-1)
                 (ranges space-2))))

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

(defmethod size ((object strided-array-index-space))
  (reduce #'* (ranges object) :key #'size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  fusion islands - specially annotated index spaces

(define-class fusion-island (strided-array-index-space)
  (spaces-to-fuse))

(defun fuse-recursively (spaces)
  (unless (every (composition #'zerop #'dimension) spaces)
    (let ((islands
            (subdivision
             (mapcar
              (lambda (space)
                (let ((ranges (ranges space)))
                  (make-instance 'fusion-island
                    :ranges (subseq ranges 0 1)
                    :spaces-to-fuse
                    (list
                     (index-space
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
