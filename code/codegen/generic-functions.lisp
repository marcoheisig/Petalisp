(in-package #:petalisp.codegen)

(defgeneric compile-kernel (client kernel)
  (:documentation
   "Returns a function that returns zero values and that takes five arguments ---
the kernel, the iteration space, an opaque object representing all targets, an
opaque object representing all sources, and an opaque object representing the
evaluation environment.")
  (:method (client (kernel kernel))
    (compile-blueprint client (kernel-blueprint kernel))))

(defgeneric compile-blueprint (client blueprint)
  (:documentation
   "Returns a function that is a suitable return value for compiling a kernel that has
the supplied blueprint."))

(defgeneric target-function (client)
  (:documentation
   "Returns the name of a function that will be invoked with the values of the
.targets. kernel parameter and an index, and that returns the target with that
index.

As a second value, returns the values type specifier that describes the results
of invoking that function.")
  (:method :around (client)
    (let ((values (multiple-value-list (call-next-method))))
      (ecase (length values)
        (1 (values
            (first values)
            `(values t &optional)))
        (2 (values-list values))))))

(defgeneric source-function (client)
  (:documentation
   "Returns the name of a function that will be invoked with the
.sources. kernel parameter and an index, and that returns the source with that
index.

As a second value, returns the values type specifier that describes the results
of invoking that function.")
  (:method :around (client)
    (let ((values (multiple-value-list (call-next-method))))
      (ecase (length values)
        (1 (values
            (first values)
            `(values t &optional)))
        (2 (values-list values))))))

(defgeneric unpack-function (client ntype rank)
  (:documentation
   "Returns the name of a function for unpacking a storage object and environment
into a base, an offset, and as many strides as the supplied RANK.

As a second value, returns the values type specifier that describes the results
of invoking that function.")
  (:method :around (client ntype rank)
    (let ((values (multiple-value-list (call-next-method))))
      (ecase (length values)
        (1 (values
            (first values)
            `(values t fixnum ,@(loop repeat rank collect '(and unsigned-byte fixnum)))))
        (2 (values-list values))))))

(defgeneric store-function (client ntype)
  (:documentation
   "Returns the name of a function for storing an object of the supplied ntype at
some base and index."))

(defgeneric load-function (client ntype)
  (:documentation
   "Returns the name of a function for loading an object of the supplied ntype
from some base and index."))
