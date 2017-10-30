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
  (data-info ulist)
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
            (setf result (ulist index 0 0)))
          result))))
  (:method ((transformation affine-transformation))
    (let (result)
      (iterate
        (for column in-vector (spm-column-indices (linear-operator transformation)) downto 0)
        (for value in-vector (spm-values (linear-operator transformation)) downto 0)
        (for offset in-vector (translation-vector transformation) downto 0)
        (setf result (ulist* (ulist column value offset) result))))))

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

(defun map-recipes (function data-structure &key leaf-function)
  "Invoke FUNCTION for every recipe that computes values of DATA-STRUCTURE
   and references the data structures returned by LEAF-FUNCTION.

   For every recipe, FUNCTION receives the following arguments:
   1. the recipe
   2. the index space computed by the recipe
   3. a vector of all referenced ranges
   4. a vector of all referenced data-structures"
  (let ((recipe-iterator
          (recipe-iterator
           leaf-function
           data-structure
           (index-space data-structure)
           (make-identity-transformation (dimension data-structure)))))
    (handler-case
        (loop (multiple-value-call function (funcall recipe-iterator)))
      (iterator-exhausted () (values)))))

;; the iterator should be factored out as a separate utility...
(define-condition iterator-exhausted () ())

(defvar *recipe-iteration-space* nil)
(defvar *recipe-ranges* nil)
(defvar *recipe-sources* nil)

(defun recipe-iterator (leaf-function node relevant-space transformation)
  (let ((body-iterator (recipe-body-iterator leaf-function node relevant-space transformation)))
    (λ (let ((*recipe-iteration-space* (index-space node))
             (*recipe-ranges*  (copy-array (ranges relevant-space) :fill-pointer t))
             (*recipe-sources* (make-array 6 :fill-pointer 0)))
         (let ((body (funcall body-iterator)))
           (iterate
             (for index from (1- (length *recipe-ranges*)) downto 0)
             (setf body (ulist '%for index body)))
           (values
            (ulist '%recipe
                   (recipe-range-information-ulist *recipe-ranges*)
                   (ucons (element-type node)
                          (recipe-sources-ulist *recipe-sources*))
                   body)
            *recipe-iteration-space*
            *recipe-ranges*
            *recipe-sources*))))))

(defun recipe-body-iterator (leaf-function node relevant-space transformation)
  (if-let ((leaf (funcall leaf-function node)))
    ;; convert leaf nodes to an iterator with a single value
    (let ((first-visit? t))
      (λ (if first-visit?
             (let ((source
                     ;; reserve ID 0 for the target
                     (1+ (or (position leaf *recipe-sources*)
                             (vector-push-extend leaf *recipe-sources*)))))
               (setf first-visit? nil)
               (%reference source (%indices transformation)))
             (signal 'iterator-exhausted))))
    (etypecase node
      ;; application nodes simply call the iterator of each input
      (application
       (let ((input-iterators
               (map 'vector
                    (λ input (recipe-body-iterator leaf-function input relevant-space transformation))
                    (inputs node))))
         (λ (ulist* '%call (operator node)
                    (let (args)
                      (iterate (for input-iterator in-vector input-iterators downto 0)
                               (setf args (ulist* (funcall input-iterator) args)))
                      args)))))
      ;; reference nodes are eliminated entirely
      (reference
       (let* ((new-transformation (composition (transformation node) transformation))
              (new-relevant-space (intersection relevant-space (index-space node))))
         (recipe-body-iterator leaf-function (input node) new-relevant-space new-transformation)))
      ;; reduction nodes introduce a new iteration range
      (reduction
       (error "TODO"))
      ;; fusion nodes are eliminated by path replication. This replication
      ;; process is the only reason why we use tree iterators. A fusion node
      ;; with N inputs returns an iterator returning N recipes.
      (fusion
       (let* ((max-size (length (inputs node)))
              (iteration-spaces (make-array max-size :fill-pointer 0))
              (input-iterators (make-array max-size :fill-pointer 0)))
         (iterate
           (for input in-sequence (inputs node))
           (for subspace = (index-space input))
           (when-let ((relevant-subspace (intersection relevant-space subspace)))
             (vector-push (funcall (inverse transformation) relevant-subspace)
                          iteration-spaces)
             (vector-push (recipe-body-iterator leaf-function input relevant-subspace transformation)
                          input-iterators)))
         (λ (loop
              (if (= 0 (fill-pointer input-iterators))
                  (signal 'iterator-exhausted)
                  (handler-case
                      (progn
                        (setf *recipe-iteration-space* (vector-pop iteration-spaces))
                        (return (funcall (vector-pop input-iterators))))
                    (iterator-exhausted ()))))))))))

