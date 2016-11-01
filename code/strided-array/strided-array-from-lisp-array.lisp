;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-from-lisp-array (strided-array)
  (lisp-array))

(defmethod stream-draw-graph ((node strided-array-from-lisp-array) stream)
  (format stream "    ~a [fillcolor = cyan, label = \"~a\"];~%"
          (id node) (lisp-array node)))

(defmethod lisp ((object t))
  (strided-array-from-lisp-object object))

(define-memo-function (strided-array-from-lisp-object
                       :table (make-hash-table :test #'equal :weakness :value))
    (lisp-object)
  (let ((array (or
                (and (typep lisp-object 'simple-array) lisp-object)
                (make-array () :initial-element lisp-object
                               :element-type (type-of lisp-object)))))
    (make-instance
     'strided-array-from-lisp-array
     :lisp-array array
     :ranges (array-ranges array))))

(defun array-ranges (array)
  (map 'vector
       (lambda (end)
         (range 0 1 (1- end)))
       (array-dimensions array)))

;;; constant folding
(defmethod application ((operator operator) (object strided-array-from-lisp-array)
                        &rest more-objects)
  (unless (and (< (size object) 42) ; constant fold only small arrays
               (every #'strided-array-from-lisp-array? more-objects))
    (return-from application (call-next-method)))
  (let ((objects (list* object more-objects)))
    (assert (identical objects :test #'equal? :key #'index-space))
    (make-instance
     'strided-array-from-lisp-array
     :lisp-array
     (array-map (lisp-function operator)
                (mapcar #'lisp-array objects))
     :ranges (ranges object))))

