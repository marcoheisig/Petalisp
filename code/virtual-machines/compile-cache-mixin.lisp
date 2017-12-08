;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp-internals)

(define-class compile-cache-mixin ()
  ((compile-cache :type hash-table :initform (make-hash-table :test #'eq))))

(defmethod vm/compile :around
    ((virtual-machine compile-cache-mixin)
     (blueprint ucons))
  (with-hash-table-memoization (blueprint :multiple-values nil)
      (compile-cache virtual-machine)
    (call-next-method)))
