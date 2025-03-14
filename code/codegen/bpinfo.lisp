(in-package #:petalisp.codegen)

;;; Extract all information from a blueprint

(defstruct (bpvalue
            (:predicate bpvaluep))
  ;; The level of the expression that produces this value, or zero if the value
  ;; is a constant or parameter that is supplied from the outside.
  (level 0 :type unsigned-byte :read-only t)
  ;; An ntype describing the nature of the value.
  (ntype (typo:universal-ntype) :type typo:ntype :read-only t))

(defgeneric bpvalue= (v1 v2)
  (:method ((v1 bpvalue) (v2 bpvalue)) nil))

(defstruct (bpconstant
            (:predicate bpconstantp)
            (:include bpvalue)
            (:constructor make-bpconstant
                (value &aux (level 0) (ntype (typo:ntype-of value)))))
  (value nil :read-only t))

(defmethod bpvalue= ((v1 bpconstant) (v2 bpconstant))
  (eql (bpconstant-value v1)
       (bpconstant-value v2)))

(defstruct (bpvariable
            (:predicate bpvariablep)
            (:include bpvalue))
  (name nil :type typo:variable-name :read-only t))

(defmethod bpvalue= ((v1 bpvariable) (v2 bpvariable))
  (eq (bpvariable-name v1)
      (bpvariable-name v2)))

(defstruct (bpinstinfo
            (:predicate bpinstinfo)
            (:include bpvariable)
            (:constructor nil))
  ;; The instruction number of the instruction that this value is derived from.
  (instruction-number nil :type unsigned-byte :read-only t))

(defstruct (bpfunction
            (:predicate bpfunctionp)
            (:include bpinstinfo)
            (:constructor make-bpfunction
                (instruction-number
                 &aux
                   (name (bpname "FN~D" instruction-number))
                   (ntype (typo:type-specifier-ntype 'function))))))

(defstruct (bpioffset
            (:predicate bpioffsetp)
            (:include bpinstinfo)
            (:constructor make-bpioffset
                (instruction-number
                 &aux
                   (name (bpname "IOFFSET~D" instruction-number))
                   (ntype *index-ntype*)))))

(defstruct (bpiscaling
            (:predicate bpiscalingp)
            (:include bpinstinfo)
            (:constructor make-bpiscaling
                (instruction-number
                 &aux
                   (name (bpname "ISCALING~D" instruction-number))
                   (ntype *index-ntype*)))))

(defstruct (bpstencil
            (:predicate bpstencilp))
  ;; The index of the stencil's center.
  (center nil :type bpvalue)
  ;; The offsets of the stencil's transformation.
  (offsets nil :type (simple-array bpvariable (*)) :read-only t)
  ;; The scalings of the stencil's transformation.
  (scalings nil :type (simple-array bpvalue (*)) :read-only t))

(defstruct (bpmemref
            (:predicate bpmemrefp))
  ;; The ntype of each element being referenced.
  (ntype nil :type typo:ntype)
  ;; The rank of the storage being referenced.
  (rank nil :type rank)
  ;; The base of this memory region.
  (base nil :type bpvariable)
  ;; The distance of the base to the first element of this memory region.
  (start nil :type bpvariable)
  ;; A vector of one stride for each axis of the memory region.
  (strides nil :type (simple-array bpvalue (*)))
  ;; A vector of one index for each transformation into the memory region.
  (stencils nil :type (simple-array bpstencil (*))))

(defun bpmemref-stride (bpmemref axis)
  (declare (bpmemref bpmemref) (unsigned-byte axis))
  (aref (bpmemref-strides bpmemref) axis))

(defun bpmemref-stencil (bpmemref index)
  (declare (bpmemref bpmemref) (unsigned-byte index))
  (aref (bpmemref-stencils bpmemref) index))

(defstruct (bpinstruction
            (:predicate bpinstructionp))
  ;; The index of the innermost loop that the instruction depends on.  The
  ;; level of an instruction that doesn't depend on any loop has a level of
  ;; zero.
  (level nil :type unsigned-byte :read-only t)
  ;; One bpvalue for each result produced by the instruction.
  (values #() :type (simple-array bpvariable (*)) :read-only t))

(defun bpinstruction-value (bpinstruction &optional (value-n 0))
  (declare (bpinstruction bpinstruction) (unsigned-byte value-n))
  (aref (bpinstruction-values bpinstruction) value-n))

(defun bpinstruction-values-ntype (bpinstruction)
  (declare (bpinstruction bpinstruction))
  (typo:make-values-ntype
   (map 'list #'bpvalue-ntype (bpinstruction-values bpinstruction))
   '()
    (typo:universal-ntype)))

(defstruct (bpcall
            (:predicate bpcallp)
            (:include bpinstruction))
  ;; The function being called.
  (function nil :type (or bpfunction bpconstant) :read-only t)
  ;; One bpvalue for each argument referenced by the instruction.
  (arguments #() :type (simple-array bpvalue (*)) :read-only t))

(defun bpcall-argument (bpcall &optional (position 0))
  (declare (bpcall bpcall))
  (svref (bpcall-arguments bpcall) position))

(defun bpcall-value (bpcall &optional (value-n 0))
  (declare (bpcall bpcall))
  (svref (bpcall-values bpcall) value-n))

(defstruct (bpload
            (:predicate bploadp)
            (:include bpinstruction))
  (source nil :type bpmemref :read-only t)
  (index nil :type bpvalue :read-only t))

(defstruct (bpstore
            (:predicate bpstorep)
            (:include bpinstruction))
  (target nil :type bpmemref :read-only t)
  (index nil :type bpvalue :read-only t)
  (value nil :type bpvalue :read-only t))

(defstruct (bpiref
            (:predicate bpirefp)
            (:include bpinstruction))
  (offset nil :type bpvariable :read-only t)
  (scaling nil :type bpvalue :read-only t))

(defstruct (bprange
            (:predicate bprangep))
  (start nil :type bpvariable :read-only t)
  (end nil :type bpvariable :read-only t)
  (step nil :type bpvalue :read-only t))

(defstruct (bpinfo
            (:predicate bpinfop))
  ;; A description of each range of the blueprint's iteration space.
  (ranges nil :type (simple-array bprange (*)) :read-only t)
  ;; A vector holding the value of each loop index.
  (indices nil :type (simple-array bpvalue (*)) :read-only t)
  ;; A description of each target being written to.
  (targets nil :type (simple-array bpmemref (*)) :read-only t)
  ;; A description of each source being read from.
  (sources nil :type (simple-array bpmemref (*)) :read-only t)
  ;; A description of all the referenced functions.
  (functions nil :type list :read-only t)
  ;; A description of all the referenced iref instruction offsets.
  (ioffsets nil :type list :read-only t)
  ;; A description of all the referenced iref instruction scalings.
  (iscalings nil :type list :read-only t)
  ;; A description of all the referenced iref instruction scalings.
  (instructions nil :type (simple-array bpinstruction (*)) :read-only t)
  ;; A vector of lists of instructions - one for each level.  The toplevel
  ;; environment has level zero, the calls right after the first loop have
  ;; level one, and so forth.
  (levels nil :type (simple-array list (*))))

(defun bpinfo-rank (bpinfo)
  (declare (bpinfo bpinfo))
  (length (bpinfo-ranges bpinfo)))

(defvar *bpnames*)

(defun bpname (fmt &rest args)
  (let ((name (intern (apply #'format nil fmt args)
                      #.*package*)))
    (if (gethash name *bpnames*)
        (error "Duplicate use of the name ~A." name)
        (setf (gethash name *bpnames*) t))
    name))

(defvar *bpvar-counter*)

(defun next-bpvar (&key (level 0) (ntype (typo:universal-ntype)))
  (make-bpvariable
   :level level
   :ntype ntype
   :name (bpname "V~D" (incf *bpvar-counter*))))

(defvar *bpinfo-reversed-levels*)

(defvar *bpinfo-reversed-functions*)

(defvar *bpinfo-reversed-ioffsets*)

(defvar *bpinfo-reversed-iscalings*)

(defvar *bpinfo-ranges*)

(defvar *bpinfo-indices*)

(defvar *bpinfo-targets*)

(defvar *bpinfo-sources*)

(defvar *bpinfo-instructions*)

(defun blueprint-bpinfo (blueprint)
  (trivia:ematch (ucons:tree-from-utree blueprint)
    ((list* iteration-space targets sources instructions)
     (let* ((*bpnames* (make-hash-table))
            (*bpvar-counter* 0)
            (rank (length iteration-space))
            (*bpinfo-reversed-levels* (make-array (1+ rank) :initial-element '()))
            (*bpinfo-reversed-functions* '())
            (*bpinfo-reversed-ioffsets* '())
            (*bpinfo-reversed-iscalings* '())
            (*bpinfo-ranges* (compute-bpinfo-ranges iteration-space))
            (*bpinfo-indices* (compute-bpinfo-indices))
            (*bpinfo-targets* (compute-bpmemrefs targets "DST"))
            (*bpinfo-sources* (compute-bpmemrefs sources "SRC"))
            (*bpinfo-instructions* (make-array (length instructions) :initial-element nil)))
       (loop for instruction-number from 0
             for instruction in instructions
             do (setf (aref *bpinfo-instructions* instruction-number)
                      (bpeval instruction instruction-number)))
       (make-bpinfo
        :ranges *bpinfo-ranges*
        :indices *bpinfo-indices*
        :targets *bpinfo-targets*
        :sources *bpinfo-sources*
        :functions (reverse *bpinfo-reversed-functions*)
        :ioffsets (reverse *bpinfo-reversed-ioffsets*)
        :iscalings (reverse *bpinfo-reversed-iscalings*)
        :instructions *bpinfo-instructions*
        :levels (map 'vector #'reverse *bpinfo-reversed-levels*))))))

(defun compute-bpinfo-ranges (iteration-space)
  (map 'vector #'compute-bprange
        iteration-space
        (alexandria:iota (length iteration-space))))

(defun compute-bprange (range-spec axis)
  (make-bprange
   :start (make-bpvariable
           :name (bpname "START~D" axis)
           :ntype *index-ntype*)
   :end (make-bpvariable
         :name (bpname "END~D" axis)
         :ntype *index-ntype*)
   :step (ecase range-spec
           (:contiguous (make-bpconstant 1))
           (:strided (make-bpvariable
                      :name (bpname "STEP~D" axis)
                      :ntype *index-ntype*)))))

(defun compute-bpinfo-indices ()
  (map 'vector #'compute-bpinfo-index
        *bpinfo-ranges*
        (alexandria:iota (length *bpinfo-ranges*) :start 1)))

(defun compute-bpinfo-index (bprange level)
  (declare (ignore bprange))
  (make-bpvariable
   :level level
   :ntype *index-ntype*
   :name (bpname "INDEX~D" (1- level))))

(defun compute-bpmemrefs (specs prefix)
  (map 'vector #'compute-bpmemref
        specs
        (make-list (length specs) :initial-element prefix)
        (alexandria:iota (length specs))))

(defun compute-bpmemref (spec prefix number)
  (destructuring-bind (ntype . transformations) spec
    (let* ((rank (length (first transformations)))
           (base (make-bpvariable :name (bpname "~A~DBASE" prefix number)))
           (start (make-bpvariable
                   :name (bpname "~A~DSTART" prefix number)
                   :ntype *index-ntype*))
           (strides
             (map 'vector
                   (lambda (axis)
                     (if (= axis (1- rank))
                         (make-bpconstant 1)
                         (make-bpvariable
                          :name (bpname "~A~DS~D" prefix number axis)
                          :ntype *index-ntype*)))
                   (alexandria:iota rank)))
           (stencils
             (map 'vector
                   (lambda (irefs index)
                     (compute-bpstencil irefs start strides prefix number index))
                   transformations
                   (alexandria:iota (length transformations)))))
      (make-bpmemref
       :ntype ntype
       :rank rank
       :base base
       :start start
       :strides strides
       :stencils stencils))))

(defun compute-bpstencil (irefs start strides prefix number index)
  (let* ((rank (length irefs))
         (offsets
           (map 'vector
                 (lambda (pos)
                   (make-bpvariable
                    :name (bpname "~A~DR~DO~D" prefix number index pos)
                    :ntype *index-ntype*))
                 (alexandria:iota rank)))
         (scalings
           (map 'vector
                 (lambda (iref pos)
                   (destructuring-bind (permutation scaling) iref
                     (declare (ignore permutation))
                     (if (eq scaling :any)
                         (make-bpvariable
                          :name (bpname "~A~DR~DS~D" prefix number index pos)
                          :ntype *index-ntype*)
                         (make-bpconstant scaling))))
                 irefs
                 (alexandria:iota rank))))
    (make-bpstencil
     :offsets offsets
     :scalings scalings
     :center
     (apply
      #'bpindex+
      start
      (loop for (permutation) in irefs
            for index below rank
            for stride across strides
            for offset across offsets
            for scaling across scalings
            collect (bpindex* stride offset)
            collect (if (not permutation)
                        (make-bpconstant 0)
                        (bpindex* stride scaling (aref *bpinfo-indices* permutation))))))))

(defun bpindex+ (&rest bpvalues)
  (let ((fn (make-bpconstant 'index+))
        (zero (make-bpconstant 0)))
    (trivia:ematch (remove zero bpvalues :test #'bpvalue=)
      ((list) zero)
      ((list bpvalue) bpvalue)
      ((list* bpvalues)
       (reduce
        (lambda (a b)
          (bpinstruction-value
           (bpcall 1 fn a b)))
        (stable-sort (copy-list bpvalues) #'< :key #'bpvalue-level))))))

(defun bpindex* (&rest bpvalues)
  (let ((fn (make-bpconstant 'index*))
        (zero (make-bpconstant 0))
        (one (make-bpconstant 1)))
    (if (find zero bpvalues :test #'bpvalue=)
        zero
        (trivia:ematch (remove one bpvalues :test #'bpvalue=)
          ((list) one)
          ((list bpvalue) bpvalue)
          ((list* bpvalues)
           (reduce
            (lambda (a b)
              (bpinstruction-value
               (bpcall 1 fn a b)))
            (stable-sort (copy-list bpvalues) #'< :key #'bpvalue-level)))))))

(defun bpcall (number-of-values fn &rest args)
  (let ((level (reduce #'max (list* fn args) :key #'bpvalue-level)))
    ;; Attempt to reuse an existing, equivalent call.
    (loop for instruction in (aref *bpinfo-reversed-levels* level) do
      (when (and (bpcallp instruction)
                 (bpvalue= fn (bpcall-function instruction))
                 (<= number-of-values (length (bpcall-values instruction)))
                 (= (length args) (length (bpcall-arguments instruction)))
                 (every #'bpvalue= (bpcall-arguments instruction) args))
        (return-from bpcall instruction)))
    (let ((call (make-bpcall
                 :level level
                 :function fn
                 :arguments (coerce args 'vector)
                 :values
                 (map-into (make-array number-of-values)
                           (lambda () (next-bpvar :level level))))))
      (push call (aref *bpinfo-reversed-levels* level))
      call)))

(defun bpeval (instruction instruction-number)
  (trivia:ematch instruction
    ((list* :call number-of-values operator arguments)
     (apply
      #'bpcall
      number-of-values
      (if (eq operator :any)
          (make-bpfunction instruction-number)
          (make-bpconstant operator))
      (mapcar #'bpref arguments)))
    ((list* :load source-number stencil-index offsets)
     (let* ((bpmemref (aref *bpinfo-sources* source-number))
            (bpstencil (bpmemref-stencil bpmemref stencil-index))
            (center (bpstencil-center bpstencil))
            (level (bpvalue-level center))
            (bpload
              (make-bpload
               :level level
               :values (vector (next-bpvar :level level :ntype (bpmemref-ntype bpmemref)))
               :source bpmemref
               :index
               (apply #'bpindex+ center
                        (loop for offset in offsets
                              for stride across (bpmemref-strides bpmemref)
                              collect (bpindex* stride (make-bpconstant offset)))))))
       (push bpload (aref *bpinfo-reversed-levels* level))
       bpload))
    ((list* :store ref target-number stencil-index offsets)
     (let* ((value (bpref ref))
            (bpmemref (aref *bpinfo-targets* target-number))
            (bpstencil (bpmemref-stencil bpmemref stencil-index))
            (center (bpstencil-center bpstencil))
            (level (max (bpvalue-level value)
                        (bpvalue-level center)))
            (bpstore
              (make-bpstore
               :level level
               :value value
               :target bpmemref
               :index
               (apply #'bpindex+ center
                        (loop for offset in offsets
                              for stride across (bpmemref-strides bpmemref)
                              collect (bpindex* stride (make-bpconstant offset)))))))
       (push bpstore (aref *bpinfo-reversed-levels* level))
       bpstore))
    ((list :iref (list permutation scaling))
     (let* ((index
              (if (not permutation)
                  (make-bpconstant 0)
                  (aref *bpinfo-indices* permutation)))
            (iscaling
              (if (eq scaling :any)
                  (make-bpiscaling instruction-number)
                  (make-bpconstant scaling)))
            (ioffset (make-bpioffset instruction-number))
            (result (bpindex+ (bpindex* index iscaling) ioffset)))
       (make-bpiref
        :level (bpvalue-level result)
        :values (vector result)
        :offset ioffset
        :scaling iscaling)))))

(defun bpref (spec)
  (destructuring-bind (value-n instruction-number) spec
    (bpinstruction-value
     (aref *bpinfo-instructions* instruction-number)
     value-n)))
