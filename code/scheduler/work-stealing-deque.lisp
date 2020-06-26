(in-package #:petalisp.scheduler)

;;; This file contains an implementation of a work-stealing deque.  It is
;;; closely modeled after the paper "Dynamic Circular Work-Stealing Deque"
;;; by David Chase and Yossi Lev.
;;;
;;; The work-stealing deque allows its owner to push and pop elements at
;;; the bottom, and it allows other threads to steal elements from the top.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Circular Array

(defstruct (circular-array
            (:copier nil)
            (:predicate circular-array-p)
            (:constructor make-circular-array
                (log-size
                 &aux (segment (make-array (ash 1 log-size))))))
  (log-size nil :type (integer 0 (#.(integer-length most-positive-fixnum))))
  (segment nil :type simple-vector))

(declaim (inline circular-array-size))
(defun circular-array-size (circular-array)
  (declare (circular-array circular-array))
  (ash 1 (circular-array-log-size circular-array)))

(declaim (inline circular-array-elt))
(defun circular-array-elt (circular-array index)
  (declare (circular-array circular-array))
  (declare (fixnum index))
  (svref (circular-array-segment circular-array)
         (logand index (1- (ash 1 (circular-array-log-size circular-array))))))

(declaim (inline (setf circular-array-elt)))
(defun (setf circular-array-elt) (value circular-array index)
  (declare (circular-array circular-array))
  (declare (fixnum index))
  (setf (svref (circular-array-segment circular-array)
               (logand index (1- (ash 1 (circular-array-log-size circular-array)))))
        value))

(declaim (ftype (function (circular-array fixnum fixnum) (values circular-array &optional))
                circular-array-grow))
(defun circular-array-grow (circular-array bottom top)
  (declare (circular-array circular-array))
  (declare (fixnum bottom))
  (declare (fixnum top))
  (let ((new (make-circular-array (1+ (circular-array-log-size circular-array)))))
    (loop for index fixnum from top below bottom do
      (setf (circular-array-elt new index)
            (circular-array-elt circular-array index)))
    new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Work Stealing Deque

(defstruct (work-stealing-deque
            (:copier nil)
            (:constructor make-work-stealing-deque ()))
  (bottom 0 :type fixnum)
  (top 0 :type fixnum)
  (circular-array (make-circular-array 0) :type circular-array))

(defun work-stealing-deque-push (work-stealing-deque value)
  (declare (work-stealing-deque work-stealing-deque))
  (let* ((bottom (work-stealing-deque-bottom work-stealing-deque))
         (top (work-stealing-deque-top work-stealing-deque))
         (ca (work-stealing-deque-circular-array work-stealing-deque))
         (size (- bottom top)))
    (when (>= size (1- (circular-array-size ca)))
      (let ((new (circular-array-grow ca bottom top)))
        (setf (work-stealing-deque-circular-array work-stealing-deque) new)
        (setf ca new)))
    (setf (circular-array-elt ca bottom) value)
    (incf (work-stealing-deque-bottom work-stealing-deque))
    value))

(defun work-stealing-deque-pop (work-stealing-deque)
  (declare (work-stealing-deque work-stealing-deque))
  (let* ((ca (work-stealing-deque-circular-array work-stealing-deque))
         (bottom (decf (work-stealing-deque-bottom work-stealing-deque)))
         (top (work-stealing-deque-top work-stealing-deque))
         (size (- bottom top)))
    (if (< size 0)
        ;; If the deque is already empty, we set BOTTOM to TOP to restore
        ;; its canonical state.
        (progn
          (setf (work-stealing-deque-bottom work-stealing-deque) top)
          (values nil nil))
        (let ((object (circular-array-elt ca bottom)))
          ;; If the deque contains more than one element, we can simply
          ;; remove that element.
          (if (> size 0)
              (values object t)
              ;; If the deque contains exactly one element, we might
              ;; compete with another thread.  A CAS shows whether we get
              ;; the last element, or not.  Either way, BOTTOM is set to
              ;; TOP+1.
              (if (atomics:cas (work-stealing-deque-top work-stealing-deque) top (1+ top))
                  (progn
                    (setf (work-stealing-deque-bottom work-stealing-deque) (1+ top))
                    (values object t))
                  (progn
                    (setf (work-stealing-deque-bottom work-stealing-deque) (1+ top))
                    (values nil nil))))))))

(defun work-stealing-deque-steal (work-stealing-deque)
  (declare (work-stealing-deque work-stealing-deque))
  (let* ((top (work-stealing-deque-top work-stealing-deque))
         (bottom (work-stealing-deque-bottom work-stealing-deque))
         (ca (work-stealing-deque-circular-array work-stealing-deque))
         (size (- bottom top)))
    (if (<= size 0)
        (values nil nil)
        (let ((object (circular-array-elt ca top)))
          (if (atomics:cas (work-stealing-deque-top work-stealing-deque) top (1+ top))
              (values object t)
              (work-stealing-deque-steal work-stealing-deque))))))

