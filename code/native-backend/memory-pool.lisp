;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

;;; This file contains an implementation of a memory pool that manages
;;; specialized vectors whose size is a power of two.  Freed vectors are
;;; tracked with weak references only, so the memory pool doesn't interfere
;;; with regular garbage collection.  The main purpose of the memory pool
;;; is to improve memory locality.
;;;
;;; Always rounding to the next power of two simplifies the implementation,
;;; but will waste 50% of memory in the worst case, and 25% of memory in
;;; the average case.  But given that memory being allocated but not
;;; touched only wastes virtual address space and doesn't put additional
;;; strain on the bandwidth, we consider this an acceptable trade-off.

(defconstant +memory-pool-ntypes+ (length petalisp.type-inference:*ntypes*))

(defconstant +memory-pool-bits+ 53)

(defconstant +memory-pool-buckets+ (* +memory-pool-ntypes+ +memory-pool-bits+))

(deftype memory-pool-bucket-vector ()
  `(simple-vector ,+memory-pool-buckets+))

(defstruct (memory-pool
            (:predicate memory-pool-p)
            (:constructor make-memory-pool ()))
  (bucket-vector
   (make-array +memory-pool-buckets+ :initial-element '())
   :type memory-pool-bucket-vector
   :read-only t))

(defun memory-pool-allocate (memory-pool ntype size)
  "Returns a vector whose elements are of the supplied NTYPE, and whose size
is at least SIZE."
  (declare (memory-pool memory-pool) (unsigned-byte size))
  (let* ((a (memory-pool-bucket-vector memory-pool))
         (i (+ (* (petalisp.type-inference:ntype-id ntype) +memory-pool-bits+)
               (integer-length size))))
    (loop for x = (atomics:atomic-pop (svref a i)) until (not x)
          for value = (trivial-garbage:weak-pointer-value x)
          when value return value
            finally
               (return
                 (make-array
                  (clp2 size)
                  :element-type (petalisp.type-inference:type-specifier ntype))))))

(defun memory-pool-free (memory-pool allocation)
  (declare (memory-pool memory-pool)
           (vector allocation))
  (let* ((a (memory-pool-bucket-vector memory-pool))
         (n (length allocation))
         (i (+ (* (petalisp.type-inference:ntype-id
                   (petalisp.type-inference:array-element-ntype allocation))
                  +memory-pool-bits+)
               (integer-length n))))
    (unless (= n (clp2 n))
      (error "Can only manage allocations whose size is a power of two."))
    (atomics:atomic-push
     (trivial-garbage:make-weak-pointer allocation)
     (svref a i))
    (values)))

(defun memory-pool-benchmark (&optional (rep 100000))
  (let ((pool (make-memory-pool))
        (ntype (petalisp.type-inference:ntype 't)))
    (time
     (loop repeat rep do
       (let* ((a (memory-pool-allocate pool ntype 100))
              (b (memory-pool-allocate pool ntype 100))
              (c (memory-pool-allocate pool ntype 100))
              (d (memory-pool-allocate pool ntype 0)))
         (memory-pool-free pool a)
         (memory-pool-free pool b)
         (memory-pool-free pool c)
         (memory-pool-free pool d))))))
