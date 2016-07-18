;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass strided-array-fusion (strided-array fusion) ())

(defmethod fusion ((object strided-array) &rest more-objects)
  (let ((objects (list* object more-objects))
        (dimension (dimension object)))
    ;; two objects qualify for fusion in one dimension if they have shared
    ;; elements in all but this dimension
    (map-combinations #'compute-coincidences objects :length 2 :copy nil)
    ;; compute overlaps
    ;; compute coinciding ranges per dimension
    (loop for d below dimension do
      (apply #'fusion (aref coinciding-ranges d)))
    (make-instance
     'strided-array-fusion
     :objects objects
     :ranges ranges)))

(defmethod fusion ((range range) &rest more-ranges)
  ;; note that this method assumes that the ranges are disjoint
  (let* ((ranges (list* range more-ranges))
         (fusion
           (loop for range in (list* range more-ranges)
                 sum (size range) into number-of-elements
                 maximize (range-end range) into end
                 minimize (range-start range) into start
                 finally
                    (let ((step (ceiling (1+ (- end start))
                                         number-of-elements)))
                      (return (range start step end))))))
    (assert (every
             (lambda (range)
               (equalp range
                       (intersection-space range fusion)))
             ranges))
    fusion))
