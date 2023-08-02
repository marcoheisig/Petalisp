;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defgeneric backend-worker-pool (backend))

(defclass backend
    (petalisp.core:backend compile-cache-mixin lisp-plus-cpp-compiler-mixin)
  ((%worker-pool
    :initform (alexandria:required-argument :worker-pool)
    :initarg :worker-pool
    :type worker-pool
    :reader backend-worker-pool)))

(defmethod initialize-instance :after
    ((backend backend) &key &allow-other-keys)
  (trivial-garbage:finalize
   backend
   (let ((worker-pool (backend-worker-pool backend)))
     (lambda ()
       (worker-pool-join worker-pool)))))

(defun make-native-backend (&key (threads (petalisp.utilities:number-of-cpus)) (debug nil))
  (check-type threads (integer 1))
  (make-instance 'backend
    :worker-pool (make-worker-pool threads)
    :debug debug))

(defmethod delete-backend
    ((backend backend))
  (worker-pool-join
   (backend-worker-pool backend)))
