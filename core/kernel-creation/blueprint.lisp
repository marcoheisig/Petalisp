;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/kernel-creation/blueprint
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/all)
  (:export
   #:blueprint/reference
   #:blueprint/call
   #:blueprint/reduce
   #:blueprint/store
   #:blueprint/with-metadata))

(in-package :petalisp/core/kernel-creation/blueprint)

;;; The blueprint grammar:
;;;
;;; BLUEPRINT          := [:blueprint BOUNDS-INFORMATION ARRAY-INFORMATION BODY]
;;; BOUNDS-INFORMATION := array-index | [array-index array-index]
;;; ARRAY-INFORMATION  := atomic-type-specifier
;;; BODY               := [:store REFERENCE EXPRESSION]
;;; REFERENCE          := [:reference array-id INDEX-COMPUTATION]
;;; INDEX-COMPUTATION  := [scale offset index]
;;; EXPRESSION         := REFERENCE | CALL | REDUCTION
;;; CALL               := [function-symbol EXPRESSION*] | [:call function-id EXPRESSION*]
;;; REDUCTION          := [:reduce function function EXPRESSION]

(defun blueprint/reference (id transformation)
  (let (ulists)
    (dx-flet ((store-triple (output-index input-index scale offset)
                (declare (ignore output-index))
                ;; TODO WTF?! scale should never be a non-integer, yet
                ;; somehow this can happen. The below fix is certainly not
                ;; the solution. Need to fix this ASAP
                (let ((scale (if (integerp scale) scale 1)))
                  (push (ulist input-index scale offset) ulists))))
      (do-outputs transformation #'store-triple))
    (ulist* :reference id
            (reduce (lambda (a b) (ucons b a))
                    ulists :initial-value nil))))

(defun blueprint/call (symbol-or-id input-blueprints)
  (ulist* :call symbol-or-id input-blueprints))

(defun blueprint/reduce (binary-operator unary-operator input)
  (ulist :reduce binary-operator unary-operator input))

(defun blueprint/store (place expression)
  (ulist :store place expression))

(defun blueprint/with-metadata (dimensions references body)
  (flet ((dimension-metadata (dimension)
           (if (<= dimension 8)
               dimension
               (let ((lb (floor (log dimension 2))))
                 (ulist (expt lb 2) (expt (1+ lb) 2)))))
         (reference-metadata (reference)
           (atomic-type (element-type reference))))
    (ulist :blueprint
           (map-ulist #'dimension-metadata dimensions)
           (map-ulist #'reference-metadata references)
           body)))


