(in-package #:petalisp.test-suite)

(defclass testing-backend (backend)
  ((%reference-backend
    :reader reference-backend
    :initform (make-reference-backend))
   (%ir-backend-interpreted
    :reader ir-backend-interpreted
    :initform (make-ir-backend :mode :interpreted))
   (%ir-backend-compiled
    :reader ir-backend-compiled
    :initform (make-ir-backend :mode :compiled))
   (%native-backend
    :reader native-backend
    :initform
    (petalisp.native-backend:make-native-backend :debug t))))

(defun make-testing-backend ()
  (make-instance 'testing-backend))

(defmethod petalisp.core:backend-compute
    ((testing-backend testing-backend)
     (data-structures list))
  (with-accessors ((reference-backend reference-backend)
                   (ir-backend-interpreted ir-backend-interpreted)
                   (ir-backend-compiled ir-backend-compiled)
                   (native-backend native-backend)) testing-backend
    (let ((reference-solutions
            (petalisp.core:backend-compute reference-backend data-structures))
          (ir-backend-interpreted-solutions
            (petalisp.core:backend-compute ir-backend-interpreted data-structures))
          (ir-backend-compiled-solutions
            (petalisp.core:backend-compute ir-backend-compiled data-structures))
          (native-backend-solutions
            (petalisp.core:backend-compute native-backend data-structures)))
      (compare-solutions reference-solutions ir-backend-interpreted-solutions)
      (compare-solutions reference-solutions ir-backend-compiled-solutions)
      (compare-solutions reference-solutions native-backend-solutions)
      reference-solutions)))

(defun compare-solutions (solutions1 solutions2)
  (loop for solution1 in solutions1
        for solution2 in solutions2 do
          (is (approximately-equal solution1 solution2))))

(defmethod petalisp.core:delete-backend ((testing-backend testing-backend))
  (delete-backend (reference-backend testing-backend))
  (delete-backend (ir-backend-interpreted testing-backend))
  (delete-backend (ir-backend-compiled testing-backend))
  (delete-backend (native-backend testing-backend))
  (call-next-method))

(defun call-with-testing-backend (thunk)
  (let ((*backend* (make-testing-backend)))
    (unwind-protect (funcall thunk)
      (delete-backend *backend*))))

(defmacro with-testing-backend (&body body)
  `(call-with-testing-backend (lambda () ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Equality

(defgeneric approximately-equal (a b))

(defmethod approximately-equal
    ((a petalisp.core:delayed-array)
     (b petalisp.core:delayed-array))
  (approximately-equal
   (petalisp.core:delayed-array-storage a)
   (petalisp.core:delayed-array-storage b)))

(defmethod approximately-equal ((a t) (b t))
  (eql a b))

(defmethod approximately-equal ((array-1 array) (array-2 array))
  (and (equal (array-dimensions array-1)
              (array-dimensions array-2))
       (loop for index below (array-total-size array-1)
             always (approximately-equal
                     (row-major-aref array-1 index)
                     (row-major-aref array-2 index)))))

(defmethod approximately-equal ((a single-float) (b single-float))
  (< (abs (- a b)) (* 64 single-float-epsilon)))

(defmethod approximately-equal ((a double-float) (b double-float))
  (< (abs (- a b)) (* 64 double-float-epsilon)))
