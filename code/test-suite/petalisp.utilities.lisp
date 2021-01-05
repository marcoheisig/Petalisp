;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.test-suite)

(check-package '#:petalisp.utilities)

(define-test wsdeque-test
  (let* ((n-threads (petalisp.utilities:number-of-cpus))
         (iterations 500000)
         (wsdeque (petalisp.utilities:make-wsdeque))
         (threads
           (loop for index below n-threads
                 collect
                 (bordeaux-threads:make-thread
                  (lambda ()
                    (loop repeat iterations
                          with item = nil
                          when (zerop (random n-threads))
                            do (setf item (petalisp.utilities:wsdeque-steal wsdeque))
                            and when item collect it)))))
         (values
           (append
            (loop for value below iterations
                  with item = nil
                  do (petalisp.utilities:wsdeque-push wsdeque value)
                  when (zerop (mod value n-threads))
                    do (setf item (petalisp.utilities:wsdeque-pop wsdeque))
                       and when item collect it)
            (loop for item = (petalisp.utilities:wsdeque-pop wsdeque)
                  while item collect item)
            (mapcan #'bordeaux-threads:join-thread threads))))
    (is (= (length values) iterations))
    (loop for expected below iterations
          for value in (sort values #'<)
          do (is (= value expected)))))
