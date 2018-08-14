;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-test-suite)

(in-suite petalisp)

(test petalisp-api
  (check (α #'+ 2 3) 5)
  (check (α #'+ #(2 3 4) #(5 4 3)) #(7 7 7))
  (check (transform #(1 2 3) (τ (i) ((- i)))) #(3 2 1))
  (check (fuse* (reshape 0.0 '((2 4) (2 4)))
                (reshape 1.0 '((3 3) (3 3))))
         #2A((0.0 0.0 0.0)
             (0.0 1.0 0.0)
             (0.0 0.0 0.0))))
