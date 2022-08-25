;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

;;; Colors
;;;
;;; Each color is represented as a positive integer.  The color N is the
;;; integer whose binary representation is all zeros except for the Nth
;;; bit.  So first color is #b1, the second color is #b10, and so forth.
;;; This representation has the advantage that operations on sets of colors
;;; can be implemented efficiently as logical operations on integers.

(deftype color () '(and unsigned-byte fixnum))

(deftype colorset () 'unsigned-byte)

(defconstant +empty-colorset+ 0)

(defun colorset-union (colorset1 colorset2)
  (declare (colorset colorset1 colorset2))
  (logior colorset1 colorset2))

(defun colorset-contains (colorset color)
  (declare (colorset colorset) (color color))
  (logbitp color colorset))

(defun colorset-add (colorset color)
  (logior colorset (ash 1 color)))

(defun colorset-next-free-color (colorset)
  (declare (colorset colorset))
  ;; We are looking for the lowest bit in the color set that is not set,
  ;; which is equivalent to counting the number of trailing ones.  Hacker's
  ;; Delight chapter 5 section 4 has us covered.
  (if (< colorset most-positive-fixnum)
      (integer-length (logand colorset (1- (lognot colorset))))
      (integer-length (logand colorset (1- (lognot colorset))))))


;;; Conflict Graphs

(defstruct cnode
  (neighbors '()
   :type list)
  (object (alexandria:required-argument :object)
   :read-only t)
  (color nil
   :type (or color null))
  ;; All colors that are already assigned to a neighbor of this node.
  (colorset +empty-colorset+
   :type colorset)
  ;; The number of different colors that have already been assigned to
  ;; neighbors of this node.
  (saturation 0
   :type (and unsigned-byte fixnum))
  ;; The number of neighbors of this node that don't yet have an assigned
  ;; color.
  (degree 0
   :type (and unsigned-byte fixnum))
  ;; The priority queue node of this node.
  (queue-node nil))

(defun cnode> (cnode1 cnode2)
  (with-accessors ((saturation1 cnode-saturation)
                   (degree1 cnode-degree)) cnode1
    (with-accessors ((saturation2 cnode-saturation)
                     (degree2 cnode-degree)) cnode2
      (or (> saturation1 saturation2)
          (> degree1 degree2)))))

(defstruct cgraph
  ;; A hash table, mapping objects to their corresponding cnodes.
  (table (make-hash-table)))

(defun cgraph-ensure-cnode (cgraph object)
  (let ((table (cgraph-table cgraph)))
    (or (gethash object table)
        (setf (gethash object table)
              (make-cnode :object object)))))

(defun cgraph-add-conflict (cgraph a b)
  (declare (cgraph cgraph) (cnode a b))
  (declare (ignore cgraph))
  ;; Self-edges really don't make sense during graph coloring, because they
  ;; immediately render the coloring problem unsolvable.
  (assert (not (eq a b)))
  (unless (member a (cnode-neighbors b))
    (incf (cnode-degree a))
    (incf (cnode-degree b))
    (push a (cnode-neighbors b))
    (push b (cnode-neighbors a)))
  (values))

(defun cgraph-coloring (cgraph)
  "For a supplied conflict graph, returns a vector whose elements are lists
of objects with no conflicts."
  ;; Finding the minimal set of colors for a conflict graph is an NP-hard
  ;; problem, so we are justified in using a reasonable heuristic from the
  ;; literature.  In this case, the heuristic is the DSatur algorithm by
  ;; Daniel Bréliaz (https://doi.org/10.1145/359094.359101)
  (let* ((queue (queues:make-queue :priority-queue :compare #'cnode>))
         (maxcolor -1))
    (flet ((qpush (cnode)
             (setf (cnode-queue-node cnode)
                   (nth-value 1 (queues:qpush queue cnode))))
           (qremove (cnode)
             (queues:queue-delete queue (cnode-queue-node cnode))))
      (loop for cnode being the hash-values of (cgraph-table cgraph) do
        (qpush cnode))
      (loop for cnode = (queues:qpop queue) while cnode do
        (with-accessors ((neighbors cnode-neighbors)
                         (neighbor-colors cnode-neighbor-colors)
                         (degree cnode-degree)
                         (colorset cnode-colorset)
                         (color cnode-color)) cnode
          (assert (null color))
          (setf color (colorset-next-free-color colorset)
                maxcolor (max color maxcolor))
          (dolist (neighbor neighbors)
            (with-accessors ((neighbor-color cnode-color)
                             (neighbor-colorset cnode-colorset)
                             (neighbor-saturation cnode-saturation)
                             (neighbor-degree cnode-degree)) neighbor
              (when (null neighbor-color)
                (qremove neighbor)
                (decf neighbor-degree)
                (unless (colorset-contains neighbor-colorset color)
                  (setf neighbor-colorset (colorset-add neighbor-colorset color))
                  (incf neighbor-saturation))
                (if (zerop neighbor-degree)
                    (setf neighbor-color (colorset-next-free-color neighbor-colorset)
                          maxcolor (max neighbor-color maxcolor))
                    (qpush neighbor)))))))
      (let ((result (make-array (1+ maxcolor) :initial-element '())))
        (loop for cnode being the hash-values of (cgraph-table cgraph) do
          (push (cnode-object cnode)
                (svref result (cnode-color cnode))))
        result))))
