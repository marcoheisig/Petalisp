(in-package :common-lisp-user)

(defpackage #:petalisp.examples.machine-learning
  (:use
   #:common-lisp
   #:petalisp)
  (:export
   #:trainable-parameter
   #:softmax
   #:relu
   #:make-fully-connected-layer
   #:make-convolutional-layer
   #:make-maxpool-layer))

(in-package #:petalisp.examples.machine-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Trainable Parameters

(defclass trainable-parameter (parameter)
  ((%value :initarg :value :accessor trainable-parameter-value)))

(defun make-trainable-parameter (initial-value)
  (let* ((value (lazy-array initial-value))
         (element-type (lazy-array-element-type value)))
    (make-instance 'trainable-parameter
      :shape (lazy-array-shape value)
      :element-type (upgraded-array-element-type element-type)
      :value value)))

(declaim (inline trainable-parameter-p))
(defun trainable-parameter-p (object)
  (typep object 'trainable-parameter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Machine Learning Building Blocks

(defun average (array)
  (let ((lazy-array (lazy-array array)))
    (lazy #'/
          (lazy-allreduce #'+ lazy-array)
          (lazy-array-size lazy-array))))

(defun make-random-array (dimensions &key (element-type 't))
  (let ((array (make-array dimensions :element-type element-type)))
    (loop for index below (array-total-size array) do
      (setf (row-major-aref array index)
            (1- (random (coerce 2 (array-element-type array))))))
    array))

(defun softmax (array)
  (let ((totals (lazy #'exp array)))
    (lazy #'/ totals (lazy-allreduce #'+ totals))))

(defun relu (array)
  (let ((lazy-array (lazy-array array)))
    (lazy #'max (coerce 0 (lazy-array-element-type lazy-array))
          lazy-array)))

(defun make-fully-connected-layer (array output-shape)
  (let* ((lazy-array (lazy-array array))
         (element-type (lazy-array-element-type lazy-array))
         (m (shape-size output-shape))
         (n (lazy-array-size lazy-array))
         (weights
           (make-trainable-parameter
            (lazy #'/
                  (make-random-array (list m n) :element-type element-type)
                  (* m n))))
         (biases
           (make-trainable-parameter
            (lazy #'/
                  (make-random-array m :element-type element-type)
                  m))))
    (lazy-reshape
     (lazy #'+
           (lazy-reduce
            #'+
            (lazy #'*
                  (lazy-reshape weights (transform m n to n m))
                  (lazy-reshape (lazy-flatten array) (transform n to n 0))))
           biases)
     output-shape)))

(define-modify-macro minf (&rest numbers) min)
(define-modify-macro maxf (&rest numbers) max)

(defun make-convolutional-layer (array &key (stencil '()) (n-filters 1))
  (let* ((lazy-array (lazy-array array))
         (rank (lazy-array-rank array))
         (n-weights (length stencil))
         (lower-bounds (make-array rank :initial-element 0))
         (upper-bounds (make-array rank :initial-element 0))
         (d nil))
    ;; Determine the dimension of the stencil.
    (loop for offsets in stencil do
      (if (null d)
          (setf d (length offsets))
          (assert (= (length offsets) d))))
    ;; Determine the bounding box of the stencil.
    (loop for offsets in stencil do
      (loop for offset in offsets
            for index from (- rank d) do
              (minf (aref lower-bounds index) offset)
              (maxf (aref upper-bounds index) offset)))
    ;; Use the bounding box to compute the shape of the result.
    (let ((result-shape
            (~ 1 ~l
               (loop for lb across lower-bounds
                     for ub across upper-bounds
                     for range in (shape-ranges (lazy-array-shape lazy-array))
                     collect
                     (if (and (integerp lb)
                              (integerp ub))
                         (let ((lo (- (range-start range) lb))
                               (hi (- (range-end range) ub)))
                           (assert (< lo hi))
                           (range lo hi))
                         range))))
          (filters
            (make-trainable-parameter
             (make-random-array
              (list* n-weights
                     n-filters
                     (make-list rank :initial-element 1))
              :element-type (lazy-array-element-type lazy-array)))))
      ;; Compute the result.
      (lazy-collapse
       (apply #'lazy #'+
              (loop for offsets in stencil
                    for index from 0
                    collect
                    (lazy #'*
                       (lazy-slice filters index)
                       (lazy-reshape
                        lazy-array
                        (make-transformation
                         :offsets
                         (append
                          (make-list (- rank d) :initial-element 0)
                          (mapcar #'- offsets)))
                        result-shape))))))))

(defun make-maxpool-layer (array pooling-factors)
  (let* ((pooling-factors (coerce pooling-factors 'vector))
         (lazy-array (lazy-array array))
         (pooling-ranges
           (loop for pooling-factor across pooling-factors
                 for range in (shape-ranges (lazy-array-shape lazy-array))
                 for index from 1 do
                   (check-type pooling-factor (integer 1 *) "a positive integer")
                   (unless (zerop (mod (range-size range) pooling-factor))
                     (error "~@<The ~:R range of the shape ~S is not ~
                         divisible by the corresponding pooling factor ~S.~:@>"
                            index (lazy-array-shape lazy-array) pooling-factor))
                 collect
                 (loop for offset below pooling-factor
                       collect
                       (range (+ (range-start range) offset)
                              (range-end range)
                              (* (range-step range) pooling-factor))))))
    (lazy-collapse
     (apply #'lazy #'max
            (apply #'alexandria:map-product
                   (lambda (&rest ranges)
                     (lazy-reshape lazy-array (~l ranges)))
                   pooling-ranges)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Training

(defun train (network output-training-data
              &rest training-data-plist
              &key learning-rate &allow-other-keys)
  (let* ((trainable-parameters
           (remove-if-not #'trainable-parameter-p (network-parameters network)))
         (output-parameters
           (loop for output in (network-outputs network)
                 collect (make-instance 'parameter
                           :element-type (lazy-array-element-type output)
                           :shape (lazy-array-shape output))))
         (gradient
           (differentiator
            (network-outputs network)
            (loop for output in (network-outputs network)
                  for output-parameter in output-parameters
                  collect
                  (lazy #'- output output-parameter))))
         (training-network
           (apply #'make-network
                  (loop for trainable-parameter in trainable-parameters
                        collect
                        (lazy #'- trainable-parameter
                           (lazy #'* learning-rate
                              (funcall gradient trainable-parameter))))))
         (n nil))
    ;; Determine the training data size.
    (dolist (data output-training-data)
      (if (null n)
          (setf n (range-size (first (shape-ranges (lazy-array-shape data)))))
          (assert (= n (range-size (first (shape-ranges (lazy-array-shape data))))))))
    (alexandria:doplist (parameter data training-data-plist) ()
      (unless (symbolp parameter)
        (assert (= n (range-size (first (shape-ranges (lazy-array-shape data))))))))
    ;; Iterate over the training data.
    (loop for index below n do
      (when (and (plusp index)
                 (zerop (mod index 100)))
        (format t "Processed ~D slices of training data~%" index))
      ;; Assemble the arguments.
      (let ((args '()))
        ;; Inputs.
        (alexandria:doplist (parameter data training-data-plist)
            (unless (symbolp parameter)
              (push parameter args)
              (push (lazy-slice data index) args)))
        ;; Outputs.
        (loop for data in output-training-data
              for output-parameter in output-parameters do
                (push output-parameter args)
                (push (lazy-slice data index) args))
        ;; Trainable parameters.
        (dolist (trainable-parameter trainable-parameters)
          (push trainable-parameter args)
          (push (trainable-parameter-value trainable-parameter) args))
        ;; Update all trainable parameters.
        (let ((new-values
                (multiple-value-list
                 (apply #'call-network training-network (reverse args)))))
          (loop for trainable-parameter in trainable-parameters
                for new-value in new-values
                do (setf (trainable-parameter-value trainable-parameter)
                         new-value)))))
    ;; Return the trained network.
    network))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classification of MNIST Data

;;; Note: We only ship a very small sample of the MNIST data with Petalisp,
;;; so learning will not be too good.  If you want to run serious tests,
;;; you should replace these paths with paths to the full MNIST data set.

(defparameter *mnist*
  (asdf:find-component
   (asdf:find-system "petalisp.examples")
   "mnist-data"))

(defun load-array (&rest path)
  (numpy-file-format:load-array
   (asdf:component-pathname
    (asdf:find-component *mnist* path))))

(defparameter *train-images* (load-array "train-images.npy"))
(defparameter *train-labels* (load-array "train-labels.npy"))
(defparameter *test-images* (load-array "test-images.npy"))
(defparameter *test-labels* (load-array "test-labels.npy"))

(defun make-mnist-classification-network ()
  (let* ((input (make-instance 'parameter
                  :shape (~ 0 27 ~ 0 27)
                  :element-type 'single-float)))
    (values
     (make-network
      (relu
       (make-fully-connected-layer
        (relu
         (make-maxpool-layer
          (make-convolutional-layer
           (relu
            (make-maxpool-layer
             (make-convolutional-layer
              input
              :stencil '((0 0) (1 0) (0 1) (-1 0) (0 -1))
              :n-filters 12)
             '(1 2 2)))
           :stencil '((0 0) (2 0) (0 2) (-2 0) (0 -2))
           :n-filters 2)
          '(1 1 3 3)))
        (~ 0 9))))
     input)))

(defun main (&key (n 100) (batch-size 100))
  (multiple-value-bind (network input)
      (make-mnist-classification-network)
    (loop for offset below n by batch-size do
      (let* ((batch-range (range offset (+ offset 99)))
             (batch-data (lazy-slices *train-images* batch-range))
             (batch-labels (lazy-slices *train-labels* batch-range))
             (input-data
               (compute
                (lazy-collapse
                 (lazy #'/ batch-data 255.0))))
             (output-data
               (compute
                (lazy-collapse
                 (lazy 'coerce
                    (lazy (lambda (n i) (if (= n i) 1.0 0.0))
                       (lazy-reshape batch-labels (transform i to i 0))
                       #(0 1 2 3 4 5 6 7 8 9))
                    'single-float)))))
        (format t "Training batch ~S.~%" batch-range)
        (train network (list output-data)
               :learning-rate 0.02
               input input-data)))
    network))

(defun check-test-data (network index)
  (format t "Label: ~S Prediction: ~S~%"
          (compute (lazy-slice *test-labels* index))
          (apply #'call-network
                 network
                 (loop for parameter in (network-parameters network)
                       collect parameter
                       collect (if (trainable-parameter-p parameter)
                                   (trainable-parameter-value parameter)
                                   (lazy-slice *train-images* index))))))
