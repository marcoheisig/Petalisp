;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

(in-suite petalisp)

(test jacobi
  (compute (jacobi (ndarray 1) :iterations 2))
  (compute (jacobi (ndarray 2) :iterations 2))
  (compute (jacobi (ndarray 3) :iterations 2))
  (compute (jacobi (ndarray 3) :iterations 5)))

(test rbgs
  (compute (rbgs (ndarray 1) :iterations 2))
  (compute (rbgs (ndarray 2) :iterations 2))
  (compute (rbgs (ndarray 3) :iterations 2))
  (compute (rbgs (ndarray 3) :iterations 5)))
