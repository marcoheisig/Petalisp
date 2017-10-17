;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; The recipe of a kernel is used both as a blueprint of some
;;; performance-critical function and as a key to search whether such a
;;; function has already been generated and compiled. The latter case is
;;; expected to be far more frequent, so the primary purpose of a recipe is
;;; to select an existing function as fast as possible and without
;;; consing. To achieve this, the recipe is stored as a tree of hash
;;; conses.

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
                           `(hlist* ',name ,@lambda-list)
                           `(hlist ',name ,@lambda-list))))
               (collect
                   `(defun ,(symbolicate name "?") (x)
                      (and (hconsp x) (eq (hcons-car x) ',name))))))))))

;;; The grammar of a recipe is:

(define-recipe-grammar
  (%recipe     := (range-info* target-info* source-info* expression))
  (%reference  := (%source-or-%target %index *))
  (%store      := (%reference expression))
  (%call       := (operator expression *))
  (%reduce     := (%range operator expression))
  (%accumulate := (%range operator initial-value expression))
  (%for        := (%range expression))
  (%index      :? (symbol rational rational))
  (%source     :? (symbol))
  (%target     :? (symbol))
  (%range      :? (symbol))
  (expression  :? (or %reference %store %call %reduce %accumulate %for))
  (range-info  :? (hlist min-size max-size step))
  (target-info :? petalisp-type-specifier)
  (source-info :? petalisp-type-specifier))

(define-symbol-pool %source "S")
(define-symbol-pool %target "T")
(define-symbol-pool %index  "I")
(define-symbol-pool %range  "R")

(defgeneric %indices (transformation)
  (:method ((transformation identity-transformation))
    (let ((dimension (input-dimension transformation))
          (zeros (load-time-value (hlist 0 0))))
      (with-vector-memoization (dimension)
        (let (result)
          (iterate
            (for index from (1- dimension) downto 0)
            (setf result (hcons (hcons (%index index) zeros) result)))
          result))))
  (:method ((transformation affine-transformation))
    (let (result)
      (iterate
        (for column in-vector (spm-column-indices (linear-operator transformation)) downto 0)
        (for value in-vector (spm-values (linear-operator transformation)) downto 0)
        (for offset in-vector (translation-vector transformation) downto 0)
        (setf result (hcons (hlist (%index column) value offset) result)))
      result)))
