;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

(document-function ir-from-lazy-arrays
  "Returns a list of buffers that form the root of an IR data flow graph
that is equivalent to the data flow graph specified by the supplied
LAZY-ARRAYS.

An IR graph is consists of alternating buffers and kernels.  Each kernel
reads and writes from zero or more buffers and writes to zero and more
buffers.  The behavior or each kernel is described by a directed acyclic
graph of instructions.
")
