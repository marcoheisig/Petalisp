;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defun α (operator object &rest more-objects)
  (let* ((objects
           (mapcar #'make-source (list* object more-objects)))
         (index-space
           (reduce #'broadcast objects))
         (objects
           (mapcar
            (lambda (object)
              (make-repetition object index-space))
            objects))
         (operator (find-operator operator)))
    (apply #'make-application operator objects)))

(defun β (operator object)
  (make-reduction operator object))

(defun select (object space)
  (make-selection object space))

(defmacro reshape (object from to)
  (assert (every #'symbolp symbols))
  (let ((dimension (length symbols))))
  ;; determine permutation
  ;; determine affine transformation
  (once-only (object)
    `(with-space ,object
       (make-transformation
        ,object (expand-transformation ,from ,to)))))

(defun fuse (object &rest more-objects)
  (apply #'make-fusion object more-objects))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Octave style index expression keywords

(defvar *magic-symbols* nil
  "A list of symbols that have special meaning in the body of petalisp macros.")

(defvar dimsym (gensym "DIMENSION")
  "A symbol that is lexically bound to the current dimension.")

(defvar spacesym (gensym "INDEX-SPACE")
  "A symbol that is lexically bound to the current index space.")

;;; if you think the following macros are ugly, remember they just try to
;;; formalize the semantic of Octave-style implicit keywords like `end'

(defmacro with-space (object &body body)
  `(let ((,spacesym ,object))
     (declare (ignorable ,spacesym))
     ,@body))

(defmacro with-dimension (dim &body body)
  `(let ((,dimsym ,dim))
     (declare (ignorable ,dimsym))
     ,@body))

(defmacro with-dimensions (index-space &rest forms)
  (let ((macrobindings
          (mapcar
           (lambda (symbol)
             `(,(intern (symbol-name symbol))
               (magic-symbol-value ',symbol ,spacesym ,dimsym)))
           *magic-symbols*)))
    `(symbol-macrolet ,macrobindings
      ,@(loop for form in forms
              and i from 0 collect
              `(with-dimension ,i ,form)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Quick notation of affine index space transformations
;;;
;;; Example:
;;;    (expand-transformation (m n) (n (* (+ m b) 8)))
;;; -> (let ((#:t (identity-transformation 2)))
;;;       (with-dimensions
;;;          (%* (%+ t b) 8)
;;;          ())

(defun %+ (dimension transformation &rest numbers))

(defun identity-transformation (dimension)
  (let ((coefficients (make-array `(dimension 2)
                                  :element-type 'integer)))
    (make-instance
     'affine-index-space-transformation
     :coefficients coefficients)))

(defmacro expand-transformation (from to)
  (with-gensyms (transformation)
    `(let ((,transformation (identity-transformation)))
       (with-dimensions
         ,body)
       ,transformation)))

