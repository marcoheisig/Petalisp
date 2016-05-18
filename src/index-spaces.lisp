;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Working with index spaces

(defclass index-space (petalisp-object)
  ((%shape :initarg :shape :reader shape)))

(defmethod normalize ((instance index-space))
  (with-accessors ((shape shape)) instance
    (loop for i below (array-dimension shape 0) do
      (symbol-macrolet ((start (aref shape i 0))
                        (step (aref shape i 1))
                        (end (aref shape i 2)))
        ;; ensure (plusp step)
        (when (cl:minusp step) (setf step (cl:- step)))
        ;; ensure (zerop (rem (- END START) STEP))
        (setf end
              (if (zerop step start)
                  (cl:+ start (cl:* step (cl:truncate (cl:- end start) step)))))
        ;; ensure (<= start end)
        (when (cl:> start end) (rotatef start end))
        ;; ensure (= start end) -> (= step 0)
        (when (cl:= start end) (setf step 0))))))

(defun index-space-disjointp (space-1 space-2)
  ())

(defun index-space-intersection (&rest index-spaces)
  (let* ((shapes (mapcar #'shape index-spaces))
         (dimensions (mapcar (lambda (x) (array-dimension x 0)) shapes)))
    (assert (apply #'cl:= dimensions))
    ;; check that all index spaces are disjoint
    (let ((result (make-array `(,(first dimensions) 3))))
      (loop for dim below (first dimensions) do
        ;; compute the new index space
           (break))
      (make-instance 'index-space :shape result))))

(defun index-space-reduction (dimension index-space)
  ())

