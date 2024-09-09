;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun deflater (rank-or-bit-vector)
  (let ((bit-vector
          (typecase rank-or-bit-vector
            (unsigned-byte
             (make-array rank-or-bit-vector :element-type 'bit :initial-element 1))
            (bit-vector
             rank-or-bit-vector)
            (otherwise
             (error "~@<Not a rank or bit vector: ~A~@:>" rank-or-bit-vector)))))
    (lambda (shape)
      (let* ((rank (length bit-vector))
             (scalings (make-array rank :initial-element 1))
             (offsets (make-array rank :initial-element 0)))
        (loop for index below rank for range in (shape-ranges shape) do
          (unless (zerop (bit bit-vector index))
            (let ((scaling (/ (range-step range))))
              (setf (svref scalings index)
                    scaling)
              (setf (svref offsets index)
                    (- (* scaling (range-start range)))))))
        (make-transformation
         :scalings scalings
         :offsets offsets)))))

(defun peeler (&rest amount-specifiers)
  (let ((min-rank (length amount-specifiers))
        (range-peelers (mapcar #'range-peeler amount-specifiers)))
    (lambda (shape)
      (when (< (shape-rank shape) min-rank)
        (error "~@<Cannot apply a rank ~R peeler to the shape ~S ~
                   that has rank ~R.~:@>"
               min-rank shape (shape-rank shape)))
      (make-shape
       (mapcar #'funcall range-peelers (shape-ranges shape))))))

(defun range-peeler (amount-specifier)
  (multiple-value-bind (lo hi stride)
      (trivia:match amount-specifier
        ((list (and lo (type unsigned-byte))
               (and hi (type unsigned-byte))
               (and stride (type unsigned-byte)))
         (values lo hi stride))
        ((list (and lo (type unsigned-byte))
               (and hi (type unsigned-byte)))
         (values lo hi 1))
        ((or (list (and amount (type unsigned-byte)))
             (and amount (type unsigned-byte)))
         (values amount amount 1))
        ((list)
         (values 0 0 1))
        (_
         (error "~@<Malformed peel amount specifier ~S. ~
                    Must be either an unsigned integer, ~
                    or a list of up to three unsigned integers.~:@>"
                amount-specifier)))
    (lambda (range)
      (declare (range range))
      (if (range-emptyp range)
          range
          (let* ((start (range-start range))
                 (end (range-end range))
                 (step (range-step range)))
            (range (+ start (* lo step))
                   (- end (* hi step))
                   (* step stride)))))))

(defun slicer (&rest slice-specifiers)
  (let ((min-rank (length slice-specifiers))
        (range-slicers (mapcar #'range-slicer slice-specifiers))
        (drop-axis-p (mapcar #'integerp slice-specifiers)))
    (lambda (shape)
      (when (< (shape-rank shape) min-rank)
        (error "~@<Cannot apply a rank ~R slicer to the shape ~S ~
                   that has rank ~R.~:@>"
               min-rank shape (shape-rank shape)))
      (let* ((selection
               (make-shape
                (mapcar #'funcall range-slicers (shape-ranges shape))))
             (transformation
               (make-transformation
                :input-mask
                (loop for drop-p in drop-axis-p
                      for range in (shape-ranges selection)
                      collect
                      (if drop-p
                          (range-start range)
                          nil))
                :output-mask
                (let ((axis 0))
                  (loop for drop-p in drop-axis-p unless drop-p
                        collect axis
                        do (incf axis))))))
        (values selection transformation)))))

(defun range-slicer (slice-specifier)
  (multiple-value-bind (start1 end1 step1)
      (trivia:match slice-specifier
        ((list (and start (type unsigned-byte))
               (and end (type unsigned-byte))
               (and step (type unsigned-byte)))
         (values start end step))
        ((list (and start (type unsigned-byte))
               (and end (type unsigned-byte)))
         (values start end 1))
        ((or (list (and start (type unsigned-byte)))
             (and start (type unsigned-byte)))
         (values start (1+ start) 1))
        ((list)
         (return-from range-slicer #'identity))
        (_
         (error "~@<Malformed slice specifier ~S.  ~
                    Must be either an unsigned integer, ~
                    or a list of up to three unsigned integers.~:@>"
                slice-specifier)))
    (unless (<= start1 end1)
      (error "~@<Malformed slice specifier ~S.  ~
                 The end of the slice specifier must be larger than ~
                 or equal to its start.~@:>"
             slice-specifier))
    (lambda (range)
      (declare (range range))
      (let* ((size (range-size range)))
        (unless (< -1 start1 size)
          (error "~@<The slice start ~S is out of bounds ~
                   for the range ~S with size ~D.~:@>"
                 start1 range size))
        (unless (<= end1 size)
          (error "~@<The slice end ~S is out of bounds ~
                     for the range ~S with size ~D.~:@>"
                 end1 range size))
        (if (range-emptyp range)
            range
            (let* ((start2 (range-start range))
                   (end2 (range-size range))
                   (step2 (range-step range))
                   (new-range
                     (range (+ start2 (* start1 step1 step2))
                            (if (not end1)
                                end2
                                (min end2 (+ start2 (* end1 step1 step2))))
                            (* step1 step2))))
              new-range))))))
