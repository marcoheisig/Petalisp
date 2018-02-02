;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/data-structures/all
  (:use-reexport
   :petalisp/core/data-structures/index-space
   :petalisp/core/data-structures/data-structure
   :petalisp/core/data-structures/immediate
   :petalisp/core/data-structures/application
   :petalisp/core/data-structures/reduction
   :petalisp/core/data-structures/fusion
   :petalisp/core/data-structures/reference
   :petalisp/core/data-structures/shallow-copy
   :petalisp/core/data-structures/strided-array
   :petalisp/core/data-structures/strided-array-index-space
   :petalisp/core/data-structures/strided-array-immediate))
