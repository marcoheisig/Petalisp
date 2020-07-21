;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;; The lazy array that is the root of the graph currently being processed.
(defvar *root*)

;; An alist of alists, whose primary keys are layouts, whose secondary keys
;; are buffer, and whose values are load instructions from a buffer of that
;; layout.  This special variable is bound during kernel creation - all
;; users can rely on the fact that it only holds values relevant to the
;; kernel that is currently being created.
(defvar *layout-buffer-loads*)

;;; A hash table that maps certain lazy arrays of a data flow graph to
;;; their corresponding layouts.
(defvar *layout-table*)
