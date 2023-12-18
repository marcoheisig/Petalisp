;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.graphviz
  (:use #:common-lisp #:petalisp)

  (:export
   #:*graphviz-default-viewer*
   #:*graphviz-default-format*
   #:view))

