;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.graphviz
  (:use #:common-lisp)
  (:export
   #:*graphviz-default-viewer*
   #:*graphviz-default-format*
   #:view))

