;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

(defgenerator range (&key (max-extent (floor most-positive-fixnum 4/5))
                          (max-size (floor (sqrt max-extent)))
                          intersecting)
  "Return a random range with at most MAX-SIZE elements, whose (absolute)
start and end are bounded by MAX-EXTENT. If another range INTERSECTING is
given, the result will intersect this range (potentially violating MAX-EXTENT)."
  (assert (and (plusp max-extent)
               (plusp max-size)
               (<= max-size max-extent)))
  (let ((max-step (floor max-extent (1- max-size))))
    (lambda ()
      (let ((step (1+ (random max-step)))
            (size (1+ (random max-size)))
            (sign (- (* (random 2) 2) 1)))
        (let* ((extent (floor (* size step) 2))
               (offset (* sign (random (max (- max-extent extent) 1))))
               (start (- offset extent))
               (range (make-range start step (+ start (* (1- size) step)))))
          (if (not intersecting)
              range
              (flet ((random-element (range)
                       (+ (range-start range)
                          (* (range-step range)
                             (random (set-size range))))))
                (let ((offset (- (random-element intersecting)
                                 (random-element range))))
                  (make-range
                   (+ (range-start range) offset)
                   (range-step range)
                   (+ (range-end range) offset))))))))))
