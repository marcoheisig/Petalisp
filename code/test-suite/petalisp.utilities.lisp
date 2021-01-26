;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.test-suite)

(check-package '#:petalisp.utilities)

(define-test wsdeque-test
  (loop repeat 100 do
    (let* ((n-threads (petalisp.utilities:number-of-cpus))
           (iterations (random 50000))
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
           (v1
             (loop for value below iterations
                   with item = nil
                   do (petalisp.utilities:wsdeque-push wsdeque value)
                   when (zerop (mod value n-threads))
                     do (setf item (petalisp.utilities:wsdeque-pop wsdeque))
                     and when item collect it))
           (v3
             (loop for item = (petalisp.utilities:wsdeque-pop wsdeque)
                   while item collect item))
           (v2
             (mapcan #'bordeaux-threads:join-thread threads)))
      (is (= (+ (length v1) (length v2) (length v3)) iterations))
      (loop for expected below iterations
            for value in (sort (append v1 v2 v3) #'<)
            do (is (= value expected))))))
