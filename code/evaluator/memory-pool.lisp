;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defvar *memory-pool* (make-hash-table :test #'equalp))

(defun bind-memory (intermediate-result)
  (let ((array-dimensions
          (map 'list #'size (ranges (index-space intermediate-result))))
        (element-type (element-type intermediate-result)))
    (setf (storage intermediate-result)
          (or
           (pop (gethash (cons element-type array-dimensions) *memory-pool*))
           (make-array array-dimensions :element-type element-type)))))

(defun free-memory (intermediate-result)
  (let ((array-dimensions
          (map 'list #'size (ranges (index-space intermediate-result))))
        (element-type (element-type intermediate-result)))
    (push (storage intermediate-result)
          (gethash (cons element-type array-dimensions) *memory-pool*))))
