;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

;;; A blueprint is an s-expression made of ucons cells.  It contains all
;;; the information necessary to compute an efficient evaluation function
;;; for this kernel.  The idea is that blueprints can be used to cache
;;; compiled evaluation functions.

(defvar *kernel*)

(defvar *normalization*)

(defun compute-blueprint (kernel)
  (let ((*kernel* kernel)
        (*normalization*
          (invert-transformation
           (transformation (first (petalisp-ir:outputs kernel))))))
    (flet ((approximate-size (range)
             ;; Return an ulist, consisting of an inclusive lower bound on
             ;; the size of the range and an exclusive upper bound.
             (let ((size (set-size range)))
               (if (< size 9)
                   (ucons:ulist size (1+ size))
                   (let ((floor (floor (log (set-size range) 2))))
                     (ucons:ulist
                      (expt 2 floor)
                      (expt 2 (1+ floor))))))))
      (ucons:ulist
       (ucons:umapcar #'approximate-size (ranges (shape kernel)))
       (blueprint-from-kernel-body (petalisp-ir:body kernel))))))

(defgeneric blueprint-from-reference (buffer transformation))

(defun blueprint-from-transformation (transformation)
  (let ((result '()))
    (map-transformation-outputs
     transformation
     (lambda (output-index input-index scaling offset)
       (declare (ignore output-index))
       (setf result (ucons:ucons (ucons:ulist input-index scaling offset) result)))
     :from-end t)))

(defun blueprint-from-kernel-body (body)
  (trivia:ematch body
    ;; Translate memory loads.
    ((list 'pref input transformation)
     (blueprint-from-reference input transformation))
    ;; Translate reductions.
    ((list* 'preduce size value-n operator forms)
     (ucons:ulist*
      'preduce
      size
      value-n
      operator
      (let ((*normalization* (enlarge-transformation *normalization* 1 0)))
        (ucons:umapcar #'blueprint-from-kernel-body forms))))
    ;; Translate function calls.
    ((list* 'pcall value-n operator forms)
     (ucons:ulist*
      'pcall
      value-n
      operator
      (ucons:umapcar #'blueprint-from-kernel-body forms)))))

(defmethod blueprint-from-reference
    ((native-backend-array-immediate native-backend-array-immediate)
     (transformation transformation))
  (ucons:ulist 'pref (position native-backend-array-immediate (inputs *kernel*))
               (blueprint-from-transformation
                (compose-transformations transformation *normalization*))))

(defmethod blueprint-from-reference
    ((native-backend-scalar-immediate native-backend-scalar-immediate)
     (transformation transformation))
  (ucons:ulist 'scalar-ref (position native-backend-scalar-immediate (inputs *kernel*))))

(defmethod blueprint-from-reference
    ((native-backend-range-immediate native-backend-range-immediate)
     (transformation transformation))
  (ucons:ulist 'range-ref (axis native-backend-range-immediate)))

(defmethod blueprint-from-reference
    ((native-backend-buffer native-backend-buffer)
     (transformation transformation))
  (ucons:ulist 'pref (position native-backend-buffer (inputs *kernel*))
               (blueprint-from-transformation
                (compose-transformations
                 (compose-transformations
                  (transformation native-backend-buffer)
                  transformation)
                 *normalization*))))
