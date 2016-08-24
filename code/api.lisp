;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defun α (operator object &rest more-objects)
  (let* ((objects
           (mapcar #'source (list* object more-objects)))
         (index-space
           (reduce #'broadcast objects))
         (objects
           (mapcar
            (lambda (object)
              (repetition object index-space))
            objects))
         (operator (find-operator operator)))
    (apply #'application operator objects)))

(defun β (operator object)
  (reduction operator object))

;;; fusion

;;; repetition

;;; source operations

(defmacro <- (object &rest subspaces-and-transformations)
  (apply #'reference object subspaces-and-transformations))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Quick notation of index space transformations
;;;
;;; Example: #t((m n) ((+ 2 n) (* 9 m)))

(defmacro expand-transformation (symbols mappings)
  (let* ((permuted-symbols
           (loop for mapping in mappings
                 collect
                 (tree-find-if
                  (lambda (x)
                    (member x symbols :test #'eq))
                  mapping)))
         (permutation
           (mapcar
            (lambda (symbol)
              (position symbol permuted-symbols :test #'eq))
            symbols))
         (affine-mappings
           `(list
             ,@(loop for p in permutation
                     and dimension from 0
                     collect
                     `(lambda (,(nth p permuted-symbols))
                        ,(nth p mappings))))))
    `(make-instance
      'transformation
      :affine-mappings ,affine-mappings
      :permutation ',permutation)))

(defun |#t-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  `(expand-transformation ,@(read stream t nil t)))

(set-dispatch-macro-character #\# #\t #'|#t-reader|)
