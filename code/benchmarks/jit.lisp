(in-package #:petalisp.benchmarks)

(defun make-dummy-array (lazy-array)
  (let ((shape (lazy-array-shape lazy-array))
        (etype (lazy-array-element-type lazy-array)))
    (make-array
     (shape-dimensions shape)
     :initial-element (coerce 0 etype)
     :element-type etype)))

(defun benchmark-compute (unknowns lazy-arrays)
  (let* ((args (mapcar (alexandria:compose #'lazy-array #'make-dummy-array)
                       unknowns))
         (thunk (lambda ()
                  (apply #'compute
                         (petalisp.core:substitute-lazy-arrays lazy-arrays args unknowns)))))
    ;; (optional) view the generated IR
    #+(or)
    (petalisp.graphviz:view
     (petalisp.ir:ir-from-lazy-arrays
      (petalisp.core:substitute-lazy-arrays lazy-arrays args unknowns)))
    ;; Populate the compile cache
    (funcall thunk)
    ;; Run the benchmark
    (benchmark-thunk thunk)))

(defun run-jacobi-jit-benchmark (&optional (n 5))
  (let ((x (make-unknown :shape (~ n ~ n) :element-type 'single-float))
        (nmin 1)
        (nmax 20))
    (format t "Time per Jacobi iteration on ~Dx~D grid: ~9,2E~%"
            n n
            (/
             (-
              (benchmark-compute
               (list x)
               (list (petalisp.examples.iterative-methods:jacobi x 0 1 nmax)))
              (benchmark-compute
               (list x)
               (list (petalisp.examples.iterative-methods:jacobi x 0 1 nmin))))
             (- nmax nmin)))))

(defun run-rbgs-jit-benchmark (&optional (n 5))
  (let ((x (make-unknown :shape (~ n ~ n) :element-type 'single-float))
        (nmin 1)
        (nmax 20))
    (format t "Time per RBGS iteration on ~Dx~D grid: ~9,2E~%"
            n n
            (/
             (-
              (benchmark-compute
               (list x)
               (list (petalisp.examples.iterative-methods:rbgs x 0 1 nmax)))
              (benchmark-compute
               (list x)
               (list (petalisp.examples.iterative-methods:rbgs x 0 1 nmin))))
             (- nmax nmin)))))

(defun run-multigrid-jit-benchmark (&optional (n 5))
  (let ((x (make-unknown :shape (~ n ~ n) :element-type 'single-float))
        (nmin 1)
        (nmax 20))
    (format t "Time per Multigrid V-cycle on ~Dx~D grid: ~9,2E~%"
            n n
            (/
             (-
              (benchmark-compute
               (list x)
               (list (petalisp.examples.iterative-methods:v-cycle x 0 1 1 1)))
              (benchmark-compute
               (list x)
               (list
                (let ((u x))
                  (dotimes (_ nmax u)
                    (setf u (petalisp.examples.iterative-methods:v-cycle u 0 1 1 1)))))))
             (- nmax nmin)))))

(defun run-reduce-jit-benchmark (&optional (n 5))
  (let ((x (make-unknown :shape (~ n) :element-type 'single-float)))
    (format t "Time per reduction on vector of length ~D: ~9,2E~%"
            n
            (benchmark-compute
             (list x)
             (list (lazy-reduce #'+ x))))))

(defun run-all-jit-benchmarks ()
  (run-jacobi-jit-benchmark)
  (run-rbgs-jit-benchmark)
  (run-multigrid-jit-benchmark))
