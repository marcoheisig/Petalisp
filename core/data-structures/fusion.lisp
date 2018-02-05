;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/data-structures/fusion
  (:use :closer-common-lisp :alexandria :trivia)
  (:use
   :petalisp/utilities/all
   :petalisp/core/error-handling
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/index-space
   :petalisp/core/data-structures/data-structure
   :petalisp/core/data-structures/immediate)
  (:export
   #:fusion
   #:make-fusion))

(in-package :petalisp/core/data-structures/fusion)

(defclass fusion (non-immediate) ()
  (:documentation
   "Let A1...AN be strided arrays with equal dimension, each mapping from
an index space Ωk to a set of values.  Furthermore, let the sets Ω1...ΩN be
pairwise disjoint, and let Ωf = ∪ Ω1...Ωk be again a valid index
space. Then the fusion of A1...AN is a data structure that maps each index
i ∈ Ωf to the value of i of the unique strided array Ak whose index space
contains i."))

(defgeneric make-fusion (first-input all-inputs)
  (:documentation
   "Create an instance of a suitable subclass of fusion."))

(define-condition fusion-error (petalisp-user-error)
  ((%index-spaces :initarg :index-spaces :reader index-spaces)))

(define-condition fusion-of-index-spaces-of-different-dimension
  (fusion-error)
  ())

(define-condition fusion-of-intersecting-index-spaces
  (fusion-error)
  ((%intersecting-spaces :initarg :intersecting-spaces :reader intersecting-spaces)))

(defgeneric fusion (first-index-space index-spaces)
  (:documentation
   "Return the fusion of the sequence INDEX-SPACES, i.e. a suitable object
that contains the entries of each index-space from INDEX-SPACES. The index-spaces
of INDEX-SPACES must not intersect.

FIRST-INDEX-SPACE must be EQ to the first index-space of INDEX-SPACES. Its sole
purpose is to dispatch on it.")
  (:method-combination or)
  ;; by default, just call MAKE-FUSION
  (:method or ((first-index-space data-structure) (index-spaces list))
    (make-fusion first-index-space index-spaces))
  ;; default error handling and optimizations
  (:method :around (first-index-space index-spaces)
    (assert (eq first-index-space (elt index-spaces 0)))
    (assert (identical index-spaces :test #'= :key #'dimension) (index-spaces)
            'fusion-of-index-spaces-of-different-dimension
            :index-spaces index-spaces)
    (map-combinations
     (lambda-match
      ((list a b)
       (assert (not (index-space-intersection?
                     (index-space a)
                     (index-space b)))
               (a b)
               'fusion-of-intersecting-index-spaces
               :index-spaces index-spaces
               :intersecting-spaces (list a b))))
     index-spaces
     :length 2
     :copy nil)
    ;; ignore one-index-space fusions
    (if (= 1 (length index-spaces))
        first-index-space
      (call-next-method))))
