;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

(test jacobi-test
  (compute (jacobi (ndarray 1) :iterations 2))
  (compute (jacobi (ndarray 2) :iterations 2))
  (compute (jacobi (ndarray 3) :iterations 2))
  (compute (jacobi (ndarray 3) :iterations 5)))

(test rbgs-test
  (compute (rbgs (ndarray 1) :iterations 2))
  (compute (rbgs (ndarray 2) :iterations 2))
  (compute (rbgs (ndarray 3) :iterations 2))
  (compute (rbgs (ndarray 3) :iterations 5)))

#+nil
(test iterate-randomly
  (flet ((act-randomly (array)
           (funcall
            (random-elt
             (list #'reshape-randomly
                   #'rbgs
                   #'jacobi))
                    array)))
    (loop repeat 10 do
      (let ((array (ndarray 3)))
        (loop repeat (random 8) do
          (setf array (act-randomly array)))
        (compute array)))))
