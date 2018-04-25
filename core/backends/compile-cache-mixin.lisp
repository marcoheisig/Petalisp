;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(uiop:define-package :petalisp/core/backends/compile-cache-mixin
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/backends/default-scheduler-mixin)
  (:export
   #:compile-cache-mixin))

(in-package :petalisp/core/backends/compile-cache-mixin)

(defclass compile-cache-mixin ()
  ((%compile-cache :reader compile-cache
                   :initform (make-hash-table :test #'eq)
                   :type hash-table)))

(defmethod vm/compile :around
    ((backend compile-cache-mixin) blueprint)
  (with-hash-table-memoization (blueprint)
      (compile-cache backend)
    (call-next-method)))
