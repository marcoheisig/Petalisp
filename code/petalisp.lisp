;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Petalisp Vocabulary - Classes

(define-class index-space () ()
  (:documentation
   "An index space of dimension D is a set of D-tuples i1,...,iD."))

(define-class transformation (unary-funcallable-object) ()
  (:metaclass funcallable-standard-class)
  (:documentation
   "A transformation is an analytically tractable function from indices to
   indices."))

(define-class data-structure ()
  ((element-type :type type-specifier      :initform t)
   (index-space  :type index-space)
   (inputs       :type list                :initform nil)
   (refcount     :type non-negative-fixnum :initform 0 :accessor refcount))
  (:documentation
   "A data structure of dimension D is a mapping from elements of
   INDEX-SPACE to values of type ELEMENT-TYPE.

   INPUTS is a list of data structures on which the definition of this data
   structure depends on.

   REFCOUNT is an implementation detail. For ordinary data structures it
   tracks how many times the data structure appears as an input of another
   data structure. For immediate data structures, it tracks how many times
   the data structure appears as the source of a kernel."))

(defmethod initialize-instance :after ; reference counting
    ((instance data-structure) &key &allow-other-keys)
  (mapc (λ input (incf (refcount input))) (inputs instance)))

(define-class immediate (data-structure)
  ((inputs       :type null)
   (storage      :type t :initform nil :accessor storage)
   (to-storage   :type transformation)
   (from-storage :type transformation)
   (kernels      :type list :accessor kernels))
  (:documentation
   "An immediate is a data structure whose elements can be referenced in
    constant time. It has a STORAGE slot that contains its elements in some
    unspecified format. The transformation TO-STORAGE maps indices
    referencing the immediate to indices referencing STORAGE. The
    transformation FROM-STORAGE is the inverse of TO-STORAGE.

    If KERNELS is a non-empty sequence, it denotes the set of kernels that
    must be executed before the immediate is fully initialized."))

(defmethod shared-initialize :before
    ((instance immediate) slot-names &key &allow-other-keys)
  (setf (slot-value instance 'inputs) nil))

(define-class application (data-structure)
  ((operator :type function))
  (:documentation
   "Let F be a referentially transparent Common Lisp function that accepts
   n arguments, and let A1...AN be data structures with index space Ω. The
   the application of f to A1...AN is a data structure that maps each index
   k ∈ Ω to (F (A1 k) ... (AN k))."))

(define-class reduction (data-structure)
  ((operator :type function))
  (:documentation
   "Let F be a referentially transparent Common Lisp function that accepts
   two arguments, and let A be a data structure of dimension n, i.e. a
   mapping from each element of the cartesian product of the spaces S1,
   ..., Sn to some values. Then the reduction of A by F is a data structure
   of dimension n-1 that maps each element k of S1 ⨯ ... ⨯ Sn-1 to the
   pairwise combination of the elements {a(i) | i ∈ k ⨯ Sn} by F in some
   arbitrary order."))

(define-class fusion (data-structure) ()
  (:documentation
   "Let A1...AN be strided arrays with equal dimension, each mapping from
   an index space Ωk to a set of values.  Furthermore, let the sets Ω1...ΩN
   be pairwise disjoint, and let Ωf = ∪ Ω1...Ωk be again a valid index
   space. Then the fusion of A1...AN is a data structure that maps each
   index i ∈ Ωf to the value of i of the unique strided array Ak whose
   index space contains i."))

(define-class reference (data-structure)
  ((transformation :type transformation))
  (:documentation
   "Let A be a strided array with domain ΩA, let ΩB be a strided array
   index space and let T be a transformation from ΩB to ΩA. Then the
   reference of A by ΩB and T is a strided array that maps each index tuple
   k \in ΩB to A(T(k))."))

(define-class kernel ()
  ((target :type immediate)
   (recipe :type ulist)
   (ranges :type list)
   (sources :type list))
  (:documentation
   "A kernel is the fundamental unit of work in Petalisp. It's RECIPE
   describes how elements of the storage of TARGET can be computed by using
   elements of the storage of SOURCES. ITERATION-SPACE is a subspace of the
   index space of the storage of TARGET."))

(defmethod initialize-instance :after ; reference counting
    ((kernel kernel) &key &allow-other-keys)
  (incf (refcount (target kernel))))

(define-class virtual-machine () ()
  (:documentation
   "A virtual machine is an abstraction over a set of hardware
   resources. All handling of kernels --- such as performance analysis,
   compilation and execution --- is done in the context of a particular
   virtual machine."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Petalisp Vocabulary - Generic Functions

(defgeneric application (f a1 &rest a2...aN)
  (:documentation
   "Return a (potentially optimized and simplified) data structure
   equivalent to an instance of class APPLICATION.")
  (:method :around ((f function) (a1 data-structure) &rest a2...aN)
    (let/de ((a1...aN (list* a1 a2...aN)))
      (check-arity f (length a1...aN))
      (assert (identical a1...aN :test #'equal? :key #'index-space)))
    (or (apply #'optimize-application f a1 a2...aN)
        (call-next-method))))

(defgeneric broadcast (object space)
  (:documentation
   "Return a broadcasting reference to the elements of OBJECT with the
   shape of SPACE.")
  (:method :before ((object data-structure) (space index-space))
    (assert (<= (dimension object) (dimension space)))))

(defgeneric common-broadcast-space (space &rest more-spaces)
  (:documentation
   "Return a space such that all objects whose index space is SPACE or in
   MORE-SPACES can be broadcast to this space. Signal an error if there is
   no such space."))

(defgeneric composition (g f)
  (:documentation
   "Return g ∘ f, i.e. return a function whose application to some
   arguments is equivalent to the application of g to the result of the
   application of f to these arguments.")
  (:method ((g function) (f function))
    (alexandria:compose g f))
  (:method :before ((g transformation) (f transformation))
    (assert (= (input-dimension g) (output-dimension f)))))

(defgeneric corresponding-immediate (data-structure)
  (:documentation
   "Return an immediate with the same shape and element type as
   DATA-STRUCTURE.")
  (:method ((immediate immediate)) immediate))

(defgeneric make-immediate! (data-structure)
  (:documentation
   "Change the class of DATA-STRUCTURE to a subclass of immediate.")
  (:method ((immediate immediate))
    immediate)
  (:method ((data-structure data-structure))
    (change-class data-structure 'immediate)))

(defgeneric depetalispify (object)
  (:documentation
   "If OBJECT is a Petalisp data structure, return an array with the
   dimension, element type and contents of OBJECT. Otherwise return
   OBJECT.")
  (:method ((object t)) object))

(defgeneric difference (space-1 space-2)
  (:documentation
   "Return a list of index spaces that denote exactly those indices of
   SPACE-1 that are not indices of SPACE-2.")
  (:method :before ((space-1 index-space) (space-2 index-space))
    (assert (= (dimension space-1) (dimension space-2)))))

(defgeneric dimension (object)
  (:documentation
   "Return the number of dimensions of OBJECT.")
  (:method ((object t)) 0)
  (:method ((list list)) (length list))
  (:method ((array array)) (array-rank array))
  (:method ((data-structure data-structure))
    (dimension (index-space data-structure))))

(defgeneric equal? (a b)
  (:documentation
   "Two objects are EQUAL? if their use in Petalisp will always result in
  identical behavior.")
  (:method ((a t) (b t)) (eql a b))
  (:method ((a structure-object) (b structure-object)) (equalp a b)))

(defgeneric fusion (a1 &rest a2...aN)
  (:documentation
   "Return a (potentially optimized and simplified) data structure
   equivalent to an instance of class FUSION.")
  (:method :around ((a1 data-structure) &rest a2...aN)
    (let/de ((a1...aN (list* a1 a2...aN)))
      (assert (identical a1...aN :test #'= :key #'dimension))
      (or (apply #'optimize-fusion a1 a2...aN)
          (call-next-method)))))

(defmethod generic-unary-funcall :before ((transformation transformation)
                                          (object index-space))
  (assert (= (input-dimension transformation) (dimension object))))

(defgeneric index-space (object)
  (:documentation
   "Return the INDEX-SPACE of OBJECT, i.e. a data structure whose elements
are the indices of OBJECT.")
  (:method ((object index-space))
    object))

(defgeneric input-dimension (transformation)
  (:documentation
   "Return the number of dimensions that a data structure must have to be a
   valid argument for TRANSFORMATION.")
  (:method ((A matrix)) (matrix-n A)))

(defgeneric intersection (space-1 space-2)
  (:documentation
   "Return an index space containing all indices that occur both in SPACE-1
   and SPACE-2.")
  (:method :before ((space-1 index-space) (space-2 index-space))
    (assert (= (dimension space-1) (dimension space-2)))))

(defgeneric inverse (transformation)
  (:documentation
   "Return a transformation whose composition with the argument of this
function is the identity transformation."))

(defgeneric optimize-application (f a1 &rest a2...aN)
  (:documentation
   "Return an optimized data-structure, or NIL.")
  (:method-combination or)
  (:method or ((f function) (a1 data-structure) &rest a2...aN)
    (when (and (eq f #'identity) (null a2...aN)) a1)))

(defgeneric optimize-fusion (a1 &rest a2...aN)
  (:documentation
   "Return an optimized data-structure, or NIL.")
  (:method-combination or)
  (:method or ((a1 data-structure) &rest a2...aN)
    "One-argument fusions are equivalent to that argument."
    (unless a2...aN a1)))

(defgeneric optimize-reduction (f a)
  (:documentation
   "Return an optimized data-structure, or NIL.")
  (:method-combination or)
  (:method or ((f function) (a data-structure))
    (declare (ignore f a))
    nil))

(defgeneric optimize-reference (object space transformation)
  (:documentation
   "Return an optimized data-structure, or NIL.")
  (:method-combination or)
  (:method or ((object data-structure) (space index-space) (transformation transformation)) nil)
  (:method or ((object reference) (space index-space) (transformation transformation))
    "Fold consecutive references. This method is crucial for Petalisp, as
    it ensures there will never be two consecutive references."
    (reference (input object)
               space
               (composition (transformation object) transformation))))

(defgeneric output-dimension (transformation)
  (:documentation
   "Return the number of dimensions of data structures generated by
   TRANSFORMATION.")
  (:method ((A matrix)) (matrix-m A)))

(defgeneric petalispify (object)
  (:documentation
   "If OBJECT is a Lisp array, convert it to a Petalisp data structure with
   the same dimension, element type and contents. If OBJECT is already a
   Petalisp data structure, return OBJECT. Otherwise return a
   zero-dimensional Petalisp data structure with OBJECT as its sole
   element.")
  (:method ((object data-structure)) object)
  (:method ((object t))
    (petalispify
     (make-array '() :initial-element object
                     :element-type (type-of object)))))

(defmethod print-object ((object data-structure) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (index-space object) stream)))

(defmethod print-object ((object kernel) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (ranges object) stream)))

(defgeneric reduction (f a)
  (:documentation
   "Return a (potentially optimized and simplified) data structure
   equivalent to an instance of class REDUCTION.")
  (:method :around ((f function) (a data-structure))
    (assert (< 0 (dimension a)))
    (check-arity f 2)
    (or (optimize-reduction f a)
        (call-next-method))))

(defgeneric reference (object space transformation)
  (:documentation
   "Return a (potentially optimized and simplified) data structure
   equivalent to an instance of class REFERENCE.")
  (:method :around ((object data-structure)
                    (space index-space)
                    (transformation transformation))
    (assert (= (dimension space) (input-dimension transformation)))
    (or (optimize-reference object space transformation)
        (call-next-method))))

(defgeneric result-type (function &rest type-specifiers)
  (:documentation
   "Return a type specifier that is a conservative estimate of the return
   type of FUNCTION, when applied to arguments that are representatives of
   the given TYPE-SPECIFIERS. A return type of NIL signifies that FUNCTION
   will never return for the given argument types.")
  (:method ((function function) &rest type-specifiers)
    (declare (ignore type-specifiers))
    t))

(defgeneric shallow-copy (instance)
  "Make a copy of INSTANCE that is EQUAL? but not EQ to it."
  (:method ((immediate immediate))
    immediate) ;; TODO violates the documentation
  (:method ((application application))
    (apply #'application (operator instance) (inputs instance)))
  (:method ((reduction reduction))
    (reduction (operator instance) (input instance)))
  (:method ((fusion fusion))
    (apply #'fusion (inputs instance)))
  (:method ((reference reference))
    (reference (input instance) (index-space instance) (transformation instance))))

(defgeneric size (object)
  (:documentation
   "The size of a compound object, such as an array or hash-table, is
   the number of its elements. All other objects have a size of 1.")
  (:method ((object t)) 1)
  (:method ((object array)) (array-total-size object))
  (:method ((object hash-table)) (hash-table-count object))
  (:method ((object data-structure)) (size (index-space object))))

(defgeneric subspace? (space-1 space-2)
  (:documentation
   "Return true if every index in SPACE-1 also occurs in SPACE-2.")
  (:method ((space-1 t) (space-2 t))
    (equal? space-1 (intersection space-1 space-2))))

(defgeneric vm/bind-memory (virtual-machine immediate)
  (:documentation
   "Instruct VIRTUAL-MACHINE to suitably set the STORAGE slot of
   IMMEDIATE."))

(defgeneric vm/compile (virtual-machine recipe)
  (:documentation
   "Instruct VIRTUAL-MACHINE to prepare the given RECIPE for execution."))

(defgeneric vm/compute (virtual-machine graph-roots)
  (:documentation
   "Instruct VIRTUAL-MACHINE to compute the sequence of data structures
   GRAPH-ROOTS. Return the computed values of all GRAPH-ROOTS."))

(defgeneric vm/execute (virtual-machine kernel)
  (:documentation
   "Instruct VIRTUAL-MACHINE to execute the given KERNEL, assuming that all
   its sources and targets have already been allocated and computed."))

(defgeneric vm/free-memory (virtual-machine immediate)
  (:documentation
   "Instruct VIRTUAL-MACHINE to reclaim the STORAGE of IMMEDIATE and set
   the STORAGE slot of IMMEDIATE to NIL."))

(defgeneric vm/schedule (virtual-machine targets recipes &optional request)
  (:documentation
   "Instruct VIRTUAL-MACHINE to compute all given GRAPH-ROOTS
   asynchronously. Return an object of type REQUEST that can be used to
   block until the task is complete.")
  (:method :before ((virtual-machine virtual-machine) (targets sequence) (recipes sequence))
    (assert (every #'immediate? targets))
    (assert (every #'data-structure? recipes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Petalisp Vocabulary - Non-generic Functions

(defun input (object)
  "Return the unique input of OBJECT."
  (destructuring-bind (input) (inputs object) input))

(defun subdivision (objects)
  "Return a list of disjoint objects. Each resulting object is a proper
subspace of one or more of the arguments and their fusion covers all
arguments."
  (flet ((shatter (dust object) ; dust is a list of disjoint objects
           (let* ((object-w/o-dust (list object))
                  (new-dust
                    (loop for particle in dust do
                      (setf object-w/o-dust
                            (loop for x in object-w/o-dust
                                  append (difference x particle)))
                          append (difference particle object)
                          when (intersection particle object) collect it)))
             (append object-w/o-dust new-dust))))
    (cond ((emptyp objects) nil)
          ((= 1 (length objects)) (list (elt objects 0)))
          (t (reduce #'shatter objects :initial-value nil)))))

(defun run-test-suite ()
  (format t "== Testing Petalisp ==~%")
  (print-platform-information)
  (print-system-statistics :petalisp)
  (print-package-statistics :petalisp)
  (fiveam:run! 'petalisp))
