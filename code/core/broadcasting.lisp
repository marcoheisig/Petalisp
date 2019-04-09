;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun broadcast-ranges (range-1 range-2)
  (if (size-one-range-p range-1)
      range-2
      (if (size-one-range-p range-2)
          range-1
          (if (range-equal range-1 range-2)
              range-2
              (error "~@<Cannot broadcast the ranges ~S and ~S.~:@>"
                     range-1 range-2)))))

(define-modify-macro broadcast-ranges-f (range-2)
  broadcast-ranges)

(defun broadcast-arrays (&rest arrays)
  (let ((n (length arrays)))
    (case n
      (0 (values))
      (1 (values (coerce-to-lazy-array (first arrays))))
      (otherwise
       (values-list
        (broadcast-list-of-arrays arrays))))))

#+(or)
(defmacro define-array-broadcast-function (name arity)
  (labels ((prefixer (symbol)
             (lambda (index)
               (intern (concatenate 'string (string symbol) "-" (format nil "~d" index))
                       #.*package*)))
           (mksym (symbol index)
             (funcall (prefixer symbol) index)))
    (let* ((arrays (mapcar (prefixer 'array) (iota arity)))
           (lazy-array-bindings
             (loop for array in arrays
                   and index from 1 collect
                   `(,(mksym 'lazy-array index) (coerce-to-lazy-array ,array))))
           (shape-bindings
             (loop for (lazy-array nil) in lazy-array-bindings
                   and index from 1 collect
                   `(,(mksym 'shape index) (shape ,lazy-array))))
           (rank-bindings
             (loop for (shape nil) in shape-bindings
                   and index from 1 collect
                   `(,(mksym 'rank index) (rank ,shape)))))
      `(defun ,name (,@arrays)
         (let ,lazy-array-bindings
           (let ,shape-bindings
             (let ,rank-bindings
               (let ((rank (max ,@(mapcar #'first rank-bindings)))
                     (broadcast-ranges '()))
                 (loop for axis from (1- rank) downto 0 do
                   (let ((broadcast-range
                           (nth-broadcast-range ,(caar shape-bindings) rank axis)))
                     ,@(loop for (shape nil) in (rest shape-bindings)
                             collect
                             `(broadcast-ranges-f
                               broadcast-range
                               (nth-broadcast-range ,shape rank axis)))
                     (push broadcast-range broadcast-ranges)))
                 (let ((broadcast-shape nil))
                   (values
                    ,@(loop for (lazy-array nil) in lazy-array-bindings
                            for (shape nil) in shape-bindings
                            collect
                            `(if (set-equal ,shape broadcast-shape)
                                 ,lazy-array
                                 (reshape ,lazy-array broadcast-shape)))))))))))))

#+(or)
(define-array-broadcast-function broadcast-two-arrays 2)

(defun broadcast-list-of-arrays (list-of-arrays)
  (let* ((lazy-arrays (mapcar #'coerce-to-lazy-array list-of-arrays))
         (shapes (mapcar #'shape lazy-arrays))
         (rank (loop for shape in shapes maximize (rank shape)))
         (broadcast-ranges '()))
    (loop for axis from (1- rank) downto 0 do
      (let ((broadcast-range (range 0)))
        (loop for shape in shapes do
          (let ((other-range (nth-broadcast-range shape rank axis)))
            (broadcast-ranges-f broadcast-range other-range)))
        (push broadcast-range broadcast-ranges)))
    (let ((broadcast-shape (make-shape broadcast-ranges)))
      (loop for lazy-array in lazy-arrays
            for shape in shapes
            collect
            (if (set-equal shape broadcast-shape)
                lazy-array
                (reshape lazy-array broadcast-shape))))))

;;; Pad SHAPE with leading one element ranges until it reaches
;;; BROADCAST-RANK.  Then, access the range corresponding to AXIS of the
;;; resulting padded shape.
(defun nth-broadcast-range (shape broadcast-rank axis)
  (with-accessors ((rank rank) (ranges ranges)) shape
    (let* ((padding (- broadcast-rank rank))
           (n (- axis padding)))
      (if (minusp n)
          (range 0)
          (nth n ranges)))))

(defun broadcasting-transformation (input-shape output-shape)
  (let* ((input-ranges (ranges input-shape))
         (output-ranges (ranges output-shape))
         (input-rank (length input-ranges))
         (output-rank (length output-ranges))
         (offsets (make-array output-rank :initial-element 0))
         (scalings (make-array output-rank :initial-element 1))
         (input-mask
           (map 'simple-vector
                (lambda (range)
                  (when (size-one-range-p range)
                    (range-start range)))
                input-ranges)))
    (loop for index below (min input-rank output-rank)
          for input-range in input-ranges
          for output-range in output-ranges do
            (let ((output-size (range-size output-range))
                  (input-size (range-size input-range)))
              (cond ( ;; Select
                     (> output-size input-size)
                     (setf (svref offsets index) 0)
                     (setf (svref scalings index) 1))
                    ( ;; Move
                     (= output-size input-size)
                     (let ((scale (/ (range-step output-range)
                                     (range-step input-range))))
                       (setf (svref scalings index) scale)
                       (setf (svref offsets index)
                             (- (range-start output-range)
                                (* scale (range-start input-range))))))
                    ( ;; Broadcast
                     (= 1 output-size)
                     (setf (svref offsets index) (range-start output-range))
                     (setf (svref scalings index) 0))
                    (t (error "Cannot broadcast the range ~S to the range ~S."
                              input-range output-range)))))
    (make-transformation
     :input-rank input-rank
     :output-rank output-rank
     :offsets offsets
     :scalings scalings
     :input-mask input-mask)))
