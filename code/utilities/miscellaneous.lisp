;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defun identical (sequence &key (test #'eql) (key #'identity))
  "Check whether the KEYs of SEQUENCE are identical with respect to TEST."
  (etypecase sequence
    (list
     (or (null sequence)
         (loop :with reference-element := (funcall key (car sequence))
               :for element :in (cdr sequence)
               :always (funcall test
                                reference-element
                                (funcall key element)))))
    (simple-vector #1=
     (or (= 0 (length sequence))
         (loop :with reference-element := (funcall key (elt sequence 0))
               :for i :from 1 :below (length sequence)
               :always (funcall test
                                reference-element
                                (funcall key (elt sequence i))))))
    (sequence #1#)))

(test |(identical)|
  (is-true (identical '(1 1 1)))
  (is-true (identical #(1 1.0 1/1) :test #'=))
  (is-true (identical '(1 2.0 6/2 4d0) :key #'numberp :test #'eq))
  (is-true (identical "aaaAaa" :test #'char= :key #'char-upcase)))

(defun array-map (function result &rest arrays)
  (check-type function function)
  (assert (identical (cons result arrays) :key #'array-dimensions :test #'equal)
          (arrays)
          "Arguments have different ARRAY-DIMENSIONS:~%~{  ~S~%~}" arrays)
  (dotimes (i (array-total-size result))
    (setf (row-major-aref result i)
          (apply function (mapcar
                           #'(lambda (x) (row-major-aref x i))
                           arrays))))
  result)

(test |(array-map)|
  (is (equalp
       #2a((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))
       (array-map
        #'+
        (make-array '(4 4))
        #2a((1 1 1 1) (1 1 1 1) (1 1 1 1) (1 1 1 1))
        #2a((0 1 2 3) (0 1 2 3) (0 1 2 3) (0 1 2 3))))))

(defun free-variables (form &optional environment)
  (let (result)
    (walk-form
     form environment
     :on-every-atom
     (lambda (form env)
       (prog1 form
         (when (and (symbolp form)
                    (not (find form (metaenv-variable-like-entries env) :key #'first)))
           (pushnew form result)))))
    result))

(defun inverse-table (graph-roots children)
  "Given a list of GRAPH-ROOTS and a function to determine the list of
  children of each graph node, return a hash-table mapping each child to a
  list of its parents."
  (let ((table (make-hash-table :test #'eq)))
    (flet ((push-entry (key value)
             (if (consp (gethash key table))
                 (push value (gethash key table))
                 (setf (gethash key table) (list value)))))
      (map nil (named-lambda populate-inverse-table (node)
                 (if-let ((children (funcall children node)))
                   ;; check whether node is visited for the first time
                   (unless (member node (gethash (elt children 0) table))
                     (dolist (child children)
                       (push-entry child node)
                       (populate-inverse-table child)))))
           graph-roots)
      table)))

(declaim (inline ensure-sequence))
(defun ensure-sequence (object)
  (typecase object
    (sequence object)
    (t (list object))))

(defmacro benchmark (form)
  `(the-cost-of-nothing::as-time
    (the-cost-of-nothing:benchmark ,form)))
