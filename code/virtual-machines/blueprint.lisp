;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; The blueprint of a kernel is used to construct some
;;; performance-critical function and as a key to search whether such a
;;; function has already been generated and compiled. The latter case is
;;; expected to be far more frequent, so the primary purpose of a blueprint
;;; is to select an existing function as fast as possible and without
;;; consing.
;;;
;;; To achieve this, each blueprint is built from uconses. Furthermore, the
;;; blueprint grammar has been chosen to maximize structural sharing and to
;;; avoid unnecessary uconses.

(define-ustruct %blueprint
  (iteration-space-info ulist)
  ;; ITERATION-SPACE-INFO has one entry per iteration space dimension,
  ;; stored as an ulist of the form [step min-size max-size]
  (memory-references ulist)
  ;; MEMORY-REFERENCES describes how a particular piece of memory is
  ;; accessed relative to the iteration space. Its entries are of the form
  ;; [source/target-id [range-id-1 scale-1 offset-1] ...]
  (target-info ulist)
  (source-info ulist)
  ;; SOURCE-INFO and TARGET-INFO store the element type of each source and
  ;; target, respectively
  (expression ulist))

(define-ustruct %call
  operator
  &rest expressions)

(define-ustruct %reduce
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
