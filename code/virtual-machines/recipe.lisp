;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; The recipe of a kernel is used both as a blueprint of some
;;; performance-critical function and as a key to search whether such a
;;; function has already been generated and compiled. The latter case is
;;; expected to be far more frequent, so the primary purpose of a recipe is
;;; to select an existing function as fast as possible and without consing.
;;;
;;; To achieve this, each recipe is built from uconses. Furthermore, the
;;; recipe grammar has been chosen to maximize structural sharing and to
;;; avoid unnecessary uconses.

(define-ustruct %recipe
  (range-info ulist)
  (storage-info ulist)
  (expression ulist))

(define-ustruct %reference
  (storage non-negative-fixnum)
  &rest indices)

(define-ustruct %store
  (reference ulist)
  (expression ulist))

(define-ustruct %call
  operator
  &rest expressions)

(define-ustruct %reduce
  (range non-negative-fixnum)
  operator
  (expression ulist))

(define-ustruct %accumulate
  (range non-negative-fixnum)
  operator
  initial-value
  (expression ulist))

(define-ustruct %for
  (range non-negative-fixnum)
  (expression ulist))

(defgeneric %indices (transformation)
  (:method ((transformation identity-transformation))
    (let ((dimension (input-dimension transformation)))
      (with-vector-memoization (dimension)
        (let (result)
          (iterate
            (for index from (1- dimension) downto 0)
            (setf result (ulist* (ulist index 1 0) result)))
          result))))
  (:method ((transformation affine-transformation))
    (let (result)
      (iterate
        (for column in-vector (spm-column-indices (linear-operator transformation)) downto 0)
        (for value in-vector (spm-values (linear-operator transformation)) downto 0)
        (for offset in-vector (translation-vector transformation) downto 0)
        (setf result (ulist* (ulist column value offset) result)))
      result)))

(defun recipe-range-information-ulist (ranges)
  (let (result)
    (iterate
      (for range in-vector ranges downto 0)
      (let ((min-size (size range))
            (max-size (size range))
            (step (range-step range)))
        (setf result (ucons
                      (ulist min-size max-size step)
                      result))))
    result))

(defun recipe-sources-ulist (immediates)
  (let (result)
    (iterate
      (for immediate in-vector immediates downto 0)
      (setf result (ucons (element-type immediate) result)))
    result))
