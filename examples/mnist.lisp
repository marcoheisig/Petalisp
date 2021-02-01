(in-package :common-lisp-user)

(defpackage #:petalisp.examples.mnist
  (:use
   #:common-lisp
   #:petalisp
   #:petalisp.examples.linear-algebra)
  (:export
   #:argmax
   #:softmax
   #:relu))

(in-package #:petalisp.examples.mnist)

(defun random-array (shape)
  (let* ((array (make-array (shape-dimensions shape) :element-type 'single-float)))
    (loop for index below (array-total-size array) do
      (setf (row-major-aref array index)
            (random 0.05)))
    array))

(defun floatify (x)
  (α #'coerce x 'single-float))

(defun relu (x)
  (floatify
   (α (lambda (v)
        (declare (single-float v))
        (if (plusp v) v 0.0))
      x)))

(defun relu-prime (x)
  (floatify
   (α (lambda (v)
        (declare (single-float v))
        (if (plusp v) 1.0 0.0))
      x)))

(defun softmax (x)
  (let* ((max (β* #'max 0.0 x 1))
         (totals (α #'exp (α #'- x (reshape max (τ (i) (i 0)))))))
    (α #'/ totals
       (reshape (β* #'+ 0.0 totals 1) (τ (i) (i 0))))))

(defun sigmoid (x)
  (α #'/ (α #'1+ (α #'exp (α #'- x)))))

(defun sigmoid-d (x)
  (α #'* x (α #'1- x)))

;;; We only ship a very tiny subset of the MNIST data with Petalisp.  This
;;; also means that learning will probably not work as expected.  You can
;;; download the full data set at http://yann.lecun.com/exdb/mnist/.
(defparameter *mnist*
  (asdf:find-component
   (asdf:find-system "petalisp.examples")
   "mnist-data"))

(defun load-array (&rest path)
  (numpy-file-format:load-array
   (asdf:component-pathname
    (asdf:find-component *mnist* path))))

(defparameter *train-images*
  (compute (α #'/ (load-array "train-images.npy") 255.0)))

(defparameter *train-labels* (load-array "train-labels.npy"))

(defparameter *test-images*
  (compute (α #'/ (load-array "test-images.npy") 255.0)))

(defparameter *test-labels* (load-array "test-labels.npy"))

(defun main (&key (batch-size 64) (learning-rate (/ 0.01 batch-size)))
  (destructuring-bind (n w h) (shape-dimensions (array-shape *train-images*))
    (let ((W1 (random-array (~ (* w h) ~ 128)))
          (W2 (random-array (~ 128 ~ 64)))
          (W3 (random-array (~ 64 ~ 10))))
      (format t "~&Training the model ...~%")
      (loop for i below 10000 do
        ;; Pick a random batch of images and labels.
        (let* ((batch-start (random (- n batch-size)))
               (batch-end (+ batch-start batch-size))
               (batch (range batch-start batch-end))
               (train-images
                 (reshape (slices *train-images* batch) (~ batch-size ~ (* w h))))
               (train-labels
                 (compute
                  (floatify
                   (α (lambda (i j) (if (= i j) 1.0 0.0))
                      (reshape (slices *train-labels* batch) (τ (i) (i 0)))
                      (shape-indices (~ 10))))))
               ;; Feed forward
               (a1 (relu (matmul train-images W1)))
               (a2 (relu (matmul a1 W2)))
               (yhat (softmax (matmul a2 W3)))
               ;; Back propagation
               (dyhat (α #'- yhat (coerce-to-matrix train-labels)))
               ;; dW3 = a2.T * dyhat
               (dW3 (matmul (transpose a2) dyhat))
               ;; dz2 = dyhat * W3.T * relu'(a2)
               (dz2 (α #'* (matmul dyhat (transpose W3)) (relu-prime a2)))
               ;; dW2 = a1.T * dz2
               (dW2 (matmul (transpose a1) dz2))
               ;; dz1 = dz2 * W2.T * relu'(a1)
               (dz1 (α #'* (matmul dz2 (transpose w2)) (relu-prime a1)))
               ;; dW1 = X.T * dz1
               (dW1 (matmul (transpose train-images) dz1)))
          #+(or)
          (petalisp.graphviz:view
           (petalisp.ir:ir-from-lazy-arrays
            (list a1 a2 yhat)))
          ;; Update the parameters.
          (setf (values W1 W2 W3)
                (compute
                 (α #'- W1 (α #'* learning-rate dW1))
                 (α #'- W2 (α #'* learning-rate dW2))
                 (α #'- W3 (α #'* learning-rate dW3))
                 ;; Also compute some intermediate arrays to break up the
                 ;; data flow graph.
                 a1 a2 yhat dyhat))
          (when (zerop (mod i 10))
            (format t "~&------------- Epoch ~4D --------------~%" i)
            (format t "W3max ~S~%" (compute (β* #'max 0.0 W3)))
            #+(or)
            (format t "Predictions:~%~S~%" (compute yhat))
            #+(or)
            (format t "Ground truth:~%~S~%" (compute train-labels))
            (trivial-garbage:gc :full t)
            (format t "Loss: ~S~%" (compute (β* #'+ 0.0 (α #'* dyhat dyhat))))
            (format t "~&---------- End of Epoch ~4D ----------~%" i)))))))
