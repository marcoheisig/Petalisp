;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-index-space (strided-array index-space)
  ((element-type :initform t :allocation :class)
   (predecessors :initform () :allocation :class)))

(defmethod initialize-instance :after ((object strided-array)
                                       &key &allow-other-keys)
  (if (typep object 'strided-array-index-space)
      (setf (slot-value object 'index-space) object)
      (setf (slot-value object 'index-space)
            (make-instance
             'strided-array-index-space
             :ranges (ranges object)))))

(defun make-index-space (&rest ranges)
  (make-instance
   'strided-array-index-space
   :ranges (make-array (length ranges) :initial-contents ranges)))

(defmacro σ (&rest ranges)
  `(make-index-space
    ,@(loop for range in ranges
            collect `(range ,@range))))

(defmacro σ* (space &rest dimensions)
  (with-gensyms (dim)
    (once-only (space)
      `(symbol-macrolet
           ((,(intern "START") (range-start (aref (ranges ,space) ,dim)))
            (,(intern "STEP") (range-step (aref (ranges ,space) ,dim)))
            (,(intern "END") (range-end (aref (ranges ,space) ,dim))))
         (make-index-space
          ,@(loop for form in dimensions and d from 0
                  collect `(let ((,dim ,d)) (range ,@form))))))))

(defmethod difference ((space-1 strided-array-index-space)
                       (space-2 strided-array-index-space))
  (let ((intersection (intersection space-1 space-2)))
    (unless intersection (return-from difference `(,space-1)))
    (loop for r1 across (ranges space-1)
          and r2 across (ranges space-2)
          and i from 0
          nconcing
          (loop for difference in (difference r1 r2)
                collect (let ((ranges (copy-array (ranges space-1))))
                          (replace ranges (ranges intersection) :end1 i)
                          (setf (aref ranges i) difference)
                          (make-instance
                           'strided-array-index-space
                           :ranges ranges))))))

(defmethod equal? ((object-1 strided-array-index-space)
                   (object-2 strided-array-index-space))
  (and (= (dimension object-1) (dimension object-2))
       (every #'equalp
              (ranges object-1)
              (ranges object-2))))

(defmethod fusion ((object strided-array-index-space) &rest more-objects)
  (let ((objects (cons object more-objects)))
    (apply #'make-index-space
           (fuse-recursively objects)
           #+nil
           (loop for i below (dimension object)
                 collect (apply #'fuse-dimension i objects)))))

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
    (loop for input-constraint across (input-constraints transformation)
          and range across ranges do
          (when input-constraint
            (assert (and (unary-range? range)
                         (= (range-start range)
                            input-constraint)))))
    (loop for p across (permutation transformation)
          and i from 0 do
            (setf (aref result i)
                  (let ((a (aref (affine-coefficients transformation) i 0))
                        (b (aref (affine-coefficients transformation) i 1))
                        (x (or (and p (aref ranges p))
                               (range 0 1 0))))
                    (range (+ (* a (range-start x)) b)
                           (* (range-step x) a)
                           (+ (* (range-end x) a) b)))))
    (make-instance
     'strided-array-index-space
     :ranges result)))

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
                   :spaces-to-fuse (list
                                    (apply #'make-index-space
                                           (cdr (vector->list ranges)))))))
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
