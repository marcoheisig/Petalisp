;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class kernel ()
  ((recipe :type data-structure)
   (cost :type non-negative-integer)
   (number-of-dependencies :type non-negative-integer)
   (users :type list))
  (:documentation
   "The kernel is a fundamental building block of Petalisp evaluation. Its
   RECIPE is a graph of data structures, whose nodes are the input of at
   most one other data structure."))
