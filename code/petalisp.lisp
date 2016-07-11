;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod generic-source ((object source) &rest arguments)
  (declare (ignore arguments))
  object)

(defun index-space (object)
  (generic-index-space object))

(defun α (operator object &rest more-objects)
  (let* ((objects
           (mapcar #'generic-source (list* object more-objects)))
         (index-space
           (reduce #'generic-broadcast objects))
         (objects
           (mapcar
            (lambda (object)
              (generic-repeat object index-space))
            objects))
         (operator (find-operator operator)))
    (apply #'generic-apply operator objects)))

(defun β (operator object)
  (generic-reduce operator object))

(defun repeat (object space)
  (generic-repeat object space))

(defun select (object space)
  (generic-select object space))

(defun %+ (transformation &rest numbers))

;;; Example:
;;;    (reshape A (m n) (n (* (+ m b) 8)))
;;; -> (let ((t (make-instance 'affine-index-space-transformation
;;;               :scaling '(1 1) :translation '(0 0) :permutation '(1 0))))
;;;       (%* 1 (%+ 1 t b) 8))

(defmacro reshape (object indices transformation)
  (assert (every #'symbolp indices))
  ;; determine permutation
  ;; determine affine transformation
  `(generic-transform ,object ,transformation))

(defun transform (object transformation)
  (generic-transform object transformation))

(defun fuse (object &rest more-objects)
  (apply #'generic-fuse object more-objects))

(defun size (object)
  (generic-size object))

(defun dimension (object)
  (generic-dimension object))
