;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/virtual-machines/compile-cache-mixin
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/virtual-machines/default-scheduler-mixin)
  (:export
   #:compile-cache-mixin))

(in-package :petalisp/core/virtual-machines/compile-cache-mixin)

(defclass compile-cache-mixin ()
  ((%compile-cache :reader compile-cache
                   :initform (make-hash-table :test #'eq)
                   :type hash-table)))

(defmethod vm/compile :around
    ((virtual-machine compile-cache-mixin) blueprint)
  (with-hash-table-memoization (blueprint)
      (compile-cache virtual-machine)
    (call-next-method)))
