(in-package #:petalisp.codegen)

(defgeneric bpvalue-form (client bpvalue)
  (:method (client (bpconstant bpconstant))
    `',(bpconstant-value bpconstant))
  (:method (client (bpvariable bpvariable))
    (bpvariable-name bpvariable)))

(defun make-call-form (function &rest arguments)
  (trivia:ematch function
    ((list 'quote (list 'setf function-name))
     `(setf (,function-name ,@(rest arguments)) ,(first arguments)))
    ((list 'quote (and function-name (type typo:function-name)))
     `(,function-name ,@arguments))
    (_
     `(funcall ,function ,@arguments))))

(defgeneric bpinstruction-form (client bpinstruction)
  (:method (client (bpcall bpcall))
    `(the ,(typo:values-ntype-type-specifier (bpinstruction-values-ntype bpcall))
          ,(apply #'make-call-form
                  (bpvalue-form client (bpcall-function bpcall))
                  (map 'list (alexandria:curry #'bpvalue-form client)
                       (bpcall-arguments bpcall)))))
  (:method (client (bpload bpload))
    `(the ,(typo:values-ntype-type-specifier (bpinstruction-values-ntype bpload))
          ,(make-call-form
            `',(load-function client (bpmemref-ntype (bpload-source bpload)))
            (bpvalue-form client (bpmemref-base (bpload-source bpload)))
            (bpvalue-form client (bpload-index bpload)))))
  (:method (client (bpstore bpstore))
    (make-call-form
     `',(store-function client (bpmemref-ntype (bpstore-target bpstore)))
     (bpvalue-form client (bpstore-value bpstore))
     (bpvalue-form client (bpmemref-base (bpstore-target bpstore)))
     (bpvalue-form client (bpstore-index bpstore))))
  (:method (client (bpiref bpiref))
    `(the index ,(bpvalue-form client (bpinstruction-value bpiref)))))

(defun mksym (fmt &rest args)
  (intern (apply #'format nil fmt args) #.*package*))

(defun lisp-translate-blueprint (client blueprint)
  (let ((bpinfo (blueprint-bpinfo blueprint))
        (body '(values)))
    (loop for level from (bpinfo-rank bpinfo) downto 0 do
      (let* ((bpinstructions (aref (bpinfo-levels bpinfo) level))
             (bindings
               (loop for bpinstruction in bpinstructions
                     collect
                     `(,(map 'list #'bpvariable-name (bpinstruction-values bpinstruction))
                       ,(bpinstruction-form client bpinstruction)))))
        (setf body `(bind ,@bindings (() ,body)))
        (unless (zerop level)
          (let* ((axis (1- level))
                 (bpindex (aref (bpinfo-indices bpinfo) axis))
                 (bprange (aref (bpinfo-ranges bpinfo) axis))
                 (bpstart (bprange-start bprange))
                 (bpend (bprange-end bprange))
                 (bpstep (bprange-step bprange)))
            (setf body `(loop for ,(bpvariable-name bpindex)
                                of-type ,(typo:ntype-type-specifier
                                          (bpvariable-ntype bpindex))
                              from ,(bpvalue-form client bpstart)
                              by ,(bpvalue-form client bpstep)
                                below ,(bpvalue-form client bpend)
                              do ,body))))))
    (make-kernel-lambda client bpinfo body)))

(defun lisp-compile-blueprint (client blueprint)
  (compile nil (lisp-translate-blueprint client blueprint)))

(defmacro without-compiler-notes (&body body)
  "Suppress all compiler notes arising during the compilation of BODY."
  `(locally
       #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       ,@body))

(defmacro with-unsafe-optimization (&body body)
  "Optimize the heck out of BODY. Use with caution!

To preserve sanity, compiler efficiency hints are disabled by default. Use
WITH-UNSAFE-OPTIMIZATIONS* to see these hints."
  `(without-compiler-notes
    (with-unsafe-optimization* ,@body)))

(defmacro with-unsafe-optimization* (&body body)
  "Optimize the heck out of BODY. Use with caution!"
  (let ((settings '((speed 3) (space 0) (debug 0) (safety 0) (compilation-speed 0))))
    `(locally (declare (optimize ,@settings))
       ,@body)))

(defmacro with-debug-optimization (&body body)
  "Execute BODY safely and with highest debug settings."
  (let ((settings '((debug 3) (safety 3))))
    `(locally (declare (optimize ,@settings))
       ,@body)))

(defun unpack-array (array environment)
  (declare (array array))
  (declare (ignore environment))
  (let ((rank (array-rank array)))
    (macrolet ((adim (axis) `(array-dimension array ,axis)))
      (case rank
        (0 (values array 0))
        (1 (values array 0 1))
        (2 (values array 0 (adim 1) 1))
        (3 (values array 0 (* (adim 1) (adim 2)) (adim 2) 1))
        (4 (values array 0 (* (adim 1) (adim 2) (adim 3)) (* (adim 2) (adim 3)) (adim 3) 1))
        (otherwise
         (apply
          #'values
          array
          0
          (let ((strides '()))
            (loop for axis from rank downto 1
                  for stride = 1 then (* stride (array-dimension array axis))
                  do (push stride strides))
            strides)))))))

(defmacro bind (&body bindings)
  (labels ((expand (bindings)
             (trivia:ematch bindings
               ((list) `(values))
               ((list* (list variables form) rest)
                (trivia:match variables
                  ((list)
                   `(progn ,form ,(expand rest)))
                  ((list variable)
                   `(let ((,variable ,form))
                      (declare (ignorable ,variable))
                      ,(expand rest)))
                  ((list* variables)
                   `(multiple-value-bind ,variables ,form
                      (declare (ignorable ,@variables))
                      ,(expand rest))))))))
    (expand bindings)))

(defun make-kernel-lambda (client bpinfo body)
  (declare (bpinfo bpinfo))
  (flet ((stencil-vars (bpmemrefs prefix)
           (loop for bpmemref across bpmemrefs
                 for number from 0
                 collect
                 (list*
                  (mksym "~A~DBUFFER" prefix number)
                  (loop for stencil across (bpmemref-stencils bpmemref)
                        for index from 0
                        collect
                        (mksym "~A~DSTENCIL~D" prefix number index)))))
         (stencil-bindings (bpmemrefs stencil-vars)
           (loop for bpmemref across bpmemrefs
                 for vars in stencil-vars
                 append
                 (loop for stencil-var in (rest vars)
                       for bpstencil across (bpmemref-stencils bpmemref)
                       for offsets-var = (mksym "~AOFFSETS" (symbol-name stencil-var))
                       for scalings-var = (mksym "~ASCALINGS" (symbol-name stencil-var))
                       collect `(,offsets-var (stencil-center ,stencil-var))
                       collect `(,scalings-var (stencil-scalings ,stencil-var))
                       append
                       (loop for bpoffset across (bpstencil-offsets bpstencil)
                             for bpscaling across (bpstencil-scalings bpstencil)
                             for axis from 0
                             collect `(,(bpvariable-name bpoffset)
                                       (svref ,offsets-var ,axis))
                             when (bpvariablep bpscaling)
                               collect `(,(bpvariable-name bpscaling)
                                         (svref ,scalings-var ,axis))))))
         (unpack-bindings (bpmemrefs ref data)
           (loop for number from 0
                 for bpmemref across bpmemrefs
                 collect
                 (multiple-value-bind (fn type) (funcall ref client)
                   (multiple-value-bind (unpack-function unpack-type)
                       (unpack-function
                        client
                        (bpmemref-ntype bpmemref)
                        (bpmemref-rank bpmemref))
                     `((,(bpvariable-name (bpmemref-base bpmemref))
                        ,(bpvariable-name (bpmemref-start bpmemref))
                        ,@(mapcar #'bpvariable-name
                                    (butlast
                                     (coerce (bpmemref-strides bpmemref) 'list))))
                       (the ,unpack-type
                            (,unpack-function
                             (the ,type (,fn ,data ,number))
                             .environment.))))))))
    (let* ((rank (bpinfo-rank bpinfo))
           (range-vars (loop for axis below rank collect (mksym "RANGE~D" axis)))
           (target-stencil-vars (stencil-vars (bpinfo-targets bpinfo) "DST"))
           (source-stencil-vars (stencil-vars (bpinfo-sources bpinfo) "SRC"))
           (bindings
             (append
              (list `(.instructions. (kernel-instruction-vector .kernel.)))
              (loop for bprange across (bpinfo-ranges bpinfo)
                    for range-var in range-vars
                    collect `(,(bpvariable-name (bprange-start bprange))
                              (the index (range-start ,range-var)))
                    collect `(,(bpvariable-name (bprange-end bprange))
                              (the index (range-end ,range-var)))
                    when (bpvariablep (bprange-step bprange))
                      collect `(,(bpvariable-name (bprange-step bprange))
                                (the index (range-step ,range-var))))
              (stencil-bindings (bpinfo-targets bpinfo) target-stencil-vars)
              (stencil-bindings (bpinfo-sources bpinfo) source-stencil-vars)
              (loop for bpinstruction across (bpinfo-instructions bpinfo)
                    for instruction-number from 0
                    append
                    (etypecase bpinstruction
                      (bpcall
                       (when (bpvariablep (bpcall-function bpinstruction))
                         (let ((inst-var (mksym "INST~D" instruction-number)))
                           `((,inst-var (svref .instructions. ,instruction-number))
                             (,(bpvariable-name (bpcall-function bpinstruction))
                              (call-instruction-function ,inst-var))))))
                      (bpload '())
                      (bpstore '())
                      (bpiref
                       (let ((inst-var (mksym "INST~D" instruction-number))
                             (tr-var (mksym "INST~DTR" instruction-number)))
                         `((,inst-var (svref .instructions. ,instruction-number))
                           (,tr-var (iref-instruction-transformation ,inst-var))
                           (,(bpvariable-name (bpiref-offset bpinstruction))
                            (svref (transformation-offsets ,tr-var) 0))
                           ,@(when (bpvariablep (bpiref-scaling bpinstruction))
                               `((,(bpvariable-name (bpiref-scaling bpinstruction))
                                  (svref (transformation-scalings ,tr-var) 0))))))))))))
      `(lambda (.kernel. .iteration-space. .targets. .sources. .environment.)
         (declare (kernel .kernel.) (shape .iteration-space.))
         (declare (ignorable .kernel. .iteration-space. .targets. .sources. .environment.))
         (with-unsafe-optimization
           (destructuring-bind ,range-vars (shape-ranges .iteration-space.)
             (declare (ignorable ,@range-vars))
             (destructuring-bind ,target-stencil-vars (kernel-targets .kernel.)
               (declare (ignorable ,@(flatten target-stencil-vars)))
               (destructuring-bind ,source-stencil-vars (kernel-sources .kernel.)
                 (declare (ignorable ,@(flatten source-stencil-vars)))
                 (let* ,bindings
                   (declare (ignorable ,@(mapcar #'first bindings)))
                   (bind
                     ,@(unpack-bindings (bpinfo-targets bpinfo) #'target-function '.targets.)
                     ,@(unpack-bindings (bpinfo-sources bpinfo) #'source-function '.sources.)
                     (() ,body)))))))))))

(defun flatten (thing)
  (if (not (listp thing))
      (list thing)
      (mapcan #'flatten thing)))

(defun store-instruction-offsets (store-instruction)
  (declare (store-instruction store-instruction))
  (transformation-offsets
   (store-instruction-transformation store-instruction)))

(defun store-instruction-scalings (store-instruction)
  (declare (store-instruction store-instruction))
  (transformation-scalings
   (store-instruction-transformation store-instruction)))
