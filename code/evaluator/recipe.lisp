;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; The recipe of a kernel is used both as a blueprint of some
;;; performance-critical function and as a key to search whether such a
;;; function has already been generated and compiled. The latter case is
;;; expected to be far more frequent, so the primary purpose of a recipe is
;;; to select an existing function as fast as possible and without
;;; consing.

(defmacro define-recipe-grammar (&body definitions)
  `(progn
     ,@(iterate
         (for definition in definitions)
         (destructuring-bind (name &key ((:= lambda-list)) &allow-other-keys) definition
           (when lambda-list
             (let* ((variadic? (eq (last-elt lambda-list) '*))
                    (lambda-list (if variadic? (butlast lambda-list) lambda-list)))
               (collect
                   `(defun ,name ,lambda-list
                      ,(if variadic?
                           `(list* ',name ,@lambda-list)
                           `(list ',name ,@lambda-list))))
               (collect
                   `(defun ,(symbolicate name "?") (x)
                      (and (consp x) (eq (car x) ',name))))))))))

;;; The grammar of a recipe is:

(define-recipe-grammar
  (%recipe     := (range-info* target-info* source-info* expression))
  (%reference  := (%source-or-%target %index *))
  (%store      := (%reference expression))
  (%call       := (operator expression *))
  (%reduce     := (%range operator expression))
  (%accumulate := (%range operator initial-value expression))
  (%for        := (%range expression))
  (%index      :? (cons symbol (cons rational (cons rational null))))
  (%source     :? symbol)
  (%target     :? symbol)
  (%range      :? symbol)
  (expression  :? (or %reference %store %call %reduce %accumulate %for))
  (range-info  :? (cons (integer 0 *) (cons (integer 0 *) (cons (integer 1 *) null))))
  (target-info :? petalisp-type-specifier)
  (source-info :? petalisp-type-specifier))

(define-symbol-pool %source "S")
(define-symbol-pool %target "T")
(define-symbol-pool %index  "I")
(define-symbol-pool %range  "R")

(defgeneric %indices (transformation)
  (:method ((transformation identity-transformation))
    (let ((dimension (input-dimension transformation))
          (zeros '(0 0)))
      (with-vector-memoization (dimension)
        (iterate
          (for index from (1- dimension) downto 0)
          (collect (cons (%index index) zeros) at beginning)))))
  (:method ((transformation affine-transformation))
    (iterate
      (for column in-vector (spm-column-indices (linear-operator transformation)) downto 0)
      (for value in-vector (spm-values (linear-operator transformation)) downto 0)
      (for offset in-vector (translation-vector transformation) downto 0)
      (collect (list (%index column) value offset) at beginning))))

;; ugly
(defun zero-based-transformation (index-space)
  (let* ((ranges (ranges index-space))
         (dimension (length ranges)))
    (make-affine-transformation
     (make-array dimension :initial-element nil)
     (scaled-permutation-matrix
      dimension dimension
      (apply #'vector (iota dimension))
      (map 'vector #'range-step ranges))
     (map 'vector #'range-start ranges))))

;; the iterator should be factored out as a separate utility...
(define-condition iterator-exhausted () ())

(defvar *recipe-sources* nil)

(defvar *recipe-space* nil)

(defun map-recipes (function data-structure &key (leaf-test #'immediate?))
  "Invoke FUNCTION for every recipe that computes values of DATA-STRUCTURE
   and references data-structures that satisfy LEAF-TEST.

   For every recipe, FUNCTION receives the following arguments:
   1. the recipe
   2. the index space computed by the recipe
   3. a vector of all referenced data-structures"
  (labels
      ((mkiter (node space transformation depth)
         ;; convert leaf nodes to an iterator with a single value
         (if (funcall leaf-test node)
             (let ((first-visit? t))
               (λ (if first-visit?
                      (let ((index
                              (or (position node *recipe-sources*)
                                  (vector-push-extend node *recipe-sources*)))
                            (transformation
                              (composition
                               (inverse (zero-based-transformation (index-space node)))
                               transformation)))
                        (setf first-visit? nil)
                        (%reference (%source index) (%indices transformation)))
                      (signal 'iterator-exhausted))))
             (etypecase node
               ;; unconditionally eliminate fusion nodes by path
               ;; replication. This replication process is the only
               ;; reason why we use tree iterators. A fusion node with
               ;; N inputs returns an iterator returning N recipes.
               (fusion
                (let* ((max-size (length (inputs node)))
                       (input-iterators (make-array max-size :fill-pointer 0))
                       (iteration-spaces (make-array max-size :fill-pointer 0)))
                  (iterate
                    (for input in-sequence (inputs node))
                    (for subspace = (index-space input))
                    (when-let ((relevant-space (intersection space subspace)))
                      (vector-push (funcall (inverse transformation) relevant-space)
                                   iteration-spaces)
                      (vector-push (mkiter input relevant-space transformation depth)
                                   input-iterators)))
                  (λ (loop
                       (if (= 0 (fill-pointer input-iterators))
                           (signal 'iterator-exhausted)
                           (handler-case
                               (progn
                                 (setf *recipe-space* (vector-pop iteration-spaces))
                                 (return (funcall (vector-pop input-iterators))))
                             (iterator-exhausted ())))))))
               ;; application nodes simply call the iterator of each input
               (application
                (let ((input-iterators
                        (map 'vector
                             (λ x (mkiter x space transformation depth))
                             (inputs node))))
                  (λ
                   (let (args)
                     (iterate (for input-iterator in-vector input-iterators downto 0)
                              (setf args (cons (funcall input-iterator) args)))
                     (%call (operator node) args)))))
               ;; eliminate reference nodes entirely
               (reference
                (let ((new-transformation (composition (transformation node) transformation))
                      (space (intersection
                              space
                              (funcall (inverse transformation)
                                       (index-space node)))))
                  (mkiter (input node) space new-transformation depth)))
               ;; reduction nodes
               (reduction
                (let ((input-iterator (mkiter (input node) space transformation (1+ depth))))
                  (λ (%reduce (%range depth) (operator node) (funcall input-iterator)))))))))
    (let ((recipe-iterator
            (mkiter data-structure
                    (index-space data-structure)
                    (zero-based-transformation (index-space data-structure))
                    (dimension data-structure))))
      (handler-case
          (loop
            (let ((*recipe-sources* (make-array 6 :fill-pointer 0))
                  (*recipe-space* (index-space data-structure)))
              (funcall function
                       (funcall recipe-iterator)
                       *recipe-space*
                       *recipe-sources*)))
        (iterator-exhausted () (values))))))
