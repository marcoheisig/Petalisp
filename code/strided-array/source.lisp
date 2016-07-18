;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-source (strided-array source) ())

(define-class strided-array-from-lisp-array (strided-array-source) (lisp-array))

(define-class strided-array-from-lisp-scalar (strided-array-source) (object))

(defmethod source ((object t) &rest arguments)
  (let ((ranges (when arguments (ranges (first arguments)))))
    (make-instance
     'strided-array-from-lisp-scalar
     :object object
     :ranges ranges)))

(defmethod source ((object array) &rest arguments)
  (assert (null arguments))
  (make-instance
   'strided-array-from-lisp-array
   :object object
   :ranges ())) ;; TODO
