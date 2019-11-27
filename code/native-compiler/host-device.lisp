;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.host-device)

(defclass host-device (device)
  (;; A hash table, mapping from blueprints to compiled kernels.
   (%compile-cache :initform (make-hash-table) :reader device-compile-cache :type hash-table))
  (:default-initargs
   :name (concatenate 'string (lisp-implementation-type) " " (lisp-implementation-version))
   :workers (number-of-processors)
   :memory (host-memory-size)))

(defmethod compile-blueprint (blueprint (host-device host-device))
  (with-accessors ((compile-cache device-compile-cache)) device
    (multiple-value-bind (value present-p)
        (gethash blueprint compile-cache)
      (if present-p
          value
          (setf (gethash blueprint compile-cache)
                (call-next-method))))))

(defun number-of-processors ()
  (handler-case
      (values
       (parse-integer
        (with-output-to-string (stream)
          (uiop:run-program
           (list "getconf" "_NPROCESSORS_ONLN")
           :output stream))))
    (uiop:subprocess-error () 1)
    (parse-error () 1)))

;; TODO Make this function more accurate and portable.
(defun host-memory-size ()
  #+sbcl (sb-ext:dynamic-space-size)
  #-sbcl (* 8 1000 1000 1000))
