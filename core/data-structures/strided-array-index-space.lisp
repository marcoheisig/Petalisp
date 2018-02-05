;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/data-structures/strided-array-index-space
  (:use :closer-common-lisp :alexandria :iterate)
  (:use
   :petalisp/utilities/all
   :petalisp/core/error-handling
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/data-structure
   :petalisp/core/data-structures/immediate
   :petalisp/core/data-structures/index-space)
  (:export
   #:strided-array-index-space
   #:ranges
   #:σ
   #:σ*))

(in-package :petalisp/core/data-structures/strided-array-index-space)

(define-class strided-array-index-space (index-space)
  ((ranges :type vector)))

(defmethod generator ((result-type (eql 'strided-array-index-space))
                      &key (dimension 3) (max-size 30) (max-extent 100) intersecting)
  (assert (or (not intersecting)
              (= dimension (dimension intersecting))))
  (let ((range-generators
          (if intersecting
              (map 'list (lambda (range)
                           (generator 'range :max-size max-size
                                             :max-extent max-extent
                                             :intersecting range))
                   (ranges intersecting))
              (make-list dimension :initial-element
                         (generator 'range :max-size max-size
                                           :max-extent max-extent)))))
    (lambda ()
      (index-space
       (map 'vector #'funcall range-generators)))))

(defmacro σ (&rest range-specifications)
  `(index-space
    (vector ,@(iterate (for spec in range-specifications)
                       (collect `(range ,@spec))))))

(defmacro σ* (space-form &body dimensions)
  (with-gensyms (dim space)
    `(let ((,space (index-space (make-immediate ,space-form))))
       (symbol-macrolet
           ((,(intern "START") (range-start (aref (ranges ,space) ,dim)))
            (,(intern "STEP") (range-step (aref (ranges ,space) ,dim)))
            (,(intern "END") (range-end (aref (ranges ,space) ,dim))))
         (index-space
          (vector
           ,@(iterate (for form in dimensions)
                      (for d from 0)
                      (collect `(let ((,dim ,d)) (range ,@form))))))))))

(define-condition no-common-broadcast-space
  (petalisp-user-error)
  ((%index-spaces :initarg :data-structures :reader index-spaces)))

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
                          ((equalp range broadcast-range)) ; NOP
                          ((size-one-range? range)) ; NOP
                          ((size-one-range? broadcast-range)
                           (setf (aref result-ranges dimension) range))
                          (t
                           (error 'no-common-broadcast-space
                                  :data-structures (cons space more-spaces))))))
      (index-space result-ranges))))

(defmethod index-space-difference ((space-1 strided-array-index-space)
                                   (space-2 strided-array-index-space))
  (if-let ((intersection (index-space-intersection space-1 space-2)))
    (iterate outer
             (for r1 in-vector (ranges space-1))
             (for r2 in-vector (ranges space-2))
             (for i from 0)
             (iterate (for difference in (range-difference r1 r2))
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

(defmethod index-space-equality ((object-1 strided-array-index-space)
                                 (object-2 strided-array-index-space))
  (and (= (dimension object-1) (dimension object-2))
       (every #'equalp
              (ranges object-1)
              (ranges object-2))))

(defmethod index-space ((array array))
  (make-instance 'strided-array-index-space
    :ranges (map 'vector (lambda (end) (range 0 1 (1- end)))
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

(defmethod index-space-intersection ((space-1 strided-array-index-space)
                                     (space-2 strided-array-index-space))
  (make-instance 'strided-array-index-space
    :ranges
    (map 'vector
         (lambda (a b)
           (or (range-intersection a b)
               (return-from index-space-intersection nil)))
         (ranges space-1)
         (ranges space-2))))

(defmethod index-space-intersection?
    ((space-1 strided-array-index-space)
     (space-2 strided-array-index-space))
  (loop for range-1 across (ranges space-1)
        for range-2 across (ranges space-2)
        always (range-intersection? range-1 range-2)))

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
          (b (translation transformation)))
      (declare (type scaled-permutation-matrix A)
               (type (simple-array number (*)) b))
      (iterate (for row from 0)
               (for column in-vector (spm-column-indices A))
               (for scale in-vector (spm-values A))
               (for offset in-vector b)
               (if (zerop scale)
                   (setf (aref result row)
                         (range offset 1 offset))
                   (let ((range (aref ranges column)))
                     (setf (aref result row)
                           (range
                            (+ offset (* scale (range-start range)))
                            (* scale (range-step range))
                            (+ offset (* scale (range-end range)))))))))
    (make-instance 'strided-array-index-space :ranges result)))

(defmethod size ((object strided-array-index-space))
  (reduce #'* (ranges object) :key #'range-size))

(defun index-space-union-range-oracle (&rest ranges)
  (declare (dynamic-extent ranges))
  ;; determine the bounding box
  (loop for range in ranges
        minimize (range-start range) into global-start
        maximize (range-end range) into global-end
        finally
           (return
             (if (= global-start global-end)
                 (first ranges)
                 ;; now determine the step size
                 (let ((step-size (- global-end global-start)))
                   (dolist (range ranges)
                     (flet ((check (n)
                              (setf step-size
                                    (min step-size
                                         (- n global-start)))))
                       (if (> (range-start range) global-start)
                           (check (range-start range))
                           (unless (size-one-range? range)
                             (check (+ (range-start range)
                                       (range-step range)))))))
                   (range global-start step-size global-end))))))

(defmethod index-space-union
    ((space-1 strided-array-index-space) &rest more-spaces)
  (index-space
   (apply #'map 'vector
          #'index-space-union-range-oracle
          (ranges space-1) (mapcar #'ranges more-spaces))))

(defmethod index-space-union :around
    ((space-1 strided-array-index-space) &rest more-spaces)
  (let ((union (call-next-method)))
    (flet ((proper-subspace-p (space)
             (subspace? space union)))
      (assert (proper-subspace-p space-1))
      (assert (every #'proper-subspace-p more-spaces))
      union)))
