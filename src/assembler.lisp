;;;; assembler.lisp

(in-package #:specops)

;; (defmacro mqbase (name opcsym mnesym args string &body body)
;;   ;; this is a currying macro for masque, allowing the creation
;;   ;; of a specialized masque with static contents for some fields
;;   (destructuring-bind (static-specs clauses &optional disassembler) body
;;     (let ((dsarg (gensym)) (flargs (gensym))
;;           (static-form (loop :for ss :in static-specs
;;                              :collect (if (not (eq :static (first ss)))
;;                                           (list 'quote ss)
;;                                           `(list :static ,@(loop :for spec :in (rest ss)
;;                                                                  :collect `(list ',(first spec)
;;                                                                                  ,(second spec))))))))
;;       `(defmacro ,name (,opcsym ,mnesym)
;;          (list (list 'lambda ',(cons 'program-api args) ;; TODO - gensym for args
;;                      (list 'flet '((of-program (&rest ,flargs) (apply program-api ,flargs)))
;;                            '(declare (ignorable (function of-program)))
;;                            (append (list 'masque ,string)
;;                                    ;; generate a template incorporating the :static values
;;                                    ;; into a (masque) expansion within the defined macro
;;                                    (list (list ,@static-form))
;;                                    ',clauses)))
;;                ,@(if (not disassembler)
;;                      nil `((list 'lambda (list ',dsarg 'program-api)
;;                                  ;; '(print (list :ee program-api ,dsarg))
;;                                  (list 'flet '((of-program (&rest ,flargs) (apply program-api ,flargs)))
;;                                        '(declare (ignorable (function of-program)))
;;                                        (list 'unmasque ,string ',dsarg (list ,@static-form)
;;                                              ',(mapcar #'first clauses) ;; ',disassembler
;;                                              (list ,@(loop :for item :in disassembler
;;                                                            :collect (if (and (symbolp item)
;;                                                                              (eql item mnesym))
;;                                                                         `(intern (string ,item) "KEYWORD")
;;                                                                         `(quote ,item))))))))))))))

(defmacro mqbase (name opcsym mnesym args string &body body)
  "This is a currying macro for dual functions based on masque and unmasque, allowing the creation of specialized functions to compose and decompose binary values in a consistent way across many permutations. It was originally created to compose IBM System Z functions, with its design facilitated by System Z's consistent instruction formats."
  (destructuring-bind (params clauses &optional disassembler) body
    (let ((dsarg (gensym)) (flargs (gensym))
          (static-form (loop :for ss :in params
                             :collect (if (not (eq :static (first ss)))
                                          (list 'quote ss)
                                          `(list :static ,@(loop :for spec :in (rest ss)
                                                                 :collect `(list ',(first spec)
                                                                                 ,(second spec)))))))
          (determine-format (if (not (assoc :determine-by params))
                                #'list (destructuring-bind (macro-sym schemes bindings)
                                           (rest (assoc :determine-by params))
                                         #'(lambda (body)
                                           `((list ',macro-sym ,mnesym ',schemes ',bindings ,body)))))))
      `(defmacro ,name (,opcsym ,mnesym)
         (list (list 'lambda ',(cons 'program-api args) ;; TODO - gensym for args
                     ,(append `(list 'flet '((of-program (&rest ,flargs) (apply program-api ,flargs)))
                                     '(declare (ignorable (function of-program))))
                              (funcall determine-format
                                       `(append (list 'masque ,string)
                                                ;; generate a template incorporating the :static values
                                                ;; into a (masque) expansion within the defined macro
                                                (list (list ,@static-form))
                                                ',clauses))))
               ,@(if (not disassembler)
                     nil `((list 'lambda (list ',dsarg 'program-api)
                                 ;; '(print (list :ee program-api ,dsarg))
                                 (list 'flet '((of-program (&rest ,flargs) (apply program-api ,flargs)))
                                       '(declare (ignorable (function of-program)))
                                       (list 'unmasque ,string ',dsarg (list ,@static-form)
                                             ',(mapcar #'first clauses) ;; ',disassembler
                                             (list ,@(loop :for item :in disassembler
                                                           :collect (if (and (symbolp item)
                                                                             (eql item mnesym))
                                                                        `(intern (string ,item) "KEYWORD")
                                                                        `(quote ,item))))))))))))))

(defclass width-spec ()
  ((%name :accessor wspec-name
          :initform nil
          :initarg  :name
          :documentation "The spec's name.")
   (%type :accessor wspec-bits
          :initform nil
          :initarg  :type
          :documentation "The spec's width in bits."))
  (:documentation "Generic class for width specs."))

(defmethod wspec-name ((name t))
  "For width specs modeled using symbols, (reg-name) simply returns the symbol."
  nil)

(defmethod wspec-name ((name symbol))
  "For width specs modeled using symbols, (reg-name) simply returns the symbol."
  name)

(defclass register ()
  ((%name  :accessor reg-name
           :initform nil
           :initarg  :name
           :documentation "The register's name.")
   (%type  :accessor reg-type
           :initform nil
           :initarg  :type
           :documentation "The register's type.")
   (%index :accessor reg-index
           :initform nil
           :initarg  :index
           :documentation "The register's index."))
  (:documentation "Generic class for registers."))

(defmethod reg-name ((register t))
  "Objects not recognized as registers return nil for a register name check."
  (declare (ignore register))
  nil)

(defmethod reg-name ((register symbol))
  "For registers modeled using symbols, (reg-name) simply returns the symbol."
  register)
  
(defgeneric of-register-type-index (register &optional type)
  (:documentation "Get a register's index."))

(defmethod of-register-type-index ((register register) &optional type)
  (if (or (not type) (eq type (reg-type register)))
      (values (reg-index register) (or type (reg-type register)))))

(defclass immediate ()
  ((%value :accessor imm-value
           :initform nil
           :initarg  :name
           :documentation "The actual value.")
   (%type  :accessor imm-type
           :initform nil
           :initarg  :type
           :documentation "The value's type.")
   (%width :accessor imm-width
           :initform nil
           :initarg  :width
           :documentation "The value's width in bits."))
  (:documentation "Base class for immediate-values."))

(defmethod imm-value ((value t))
  "Immediates not recognised as a number return nil when their value is checked."
  (declare (ignore value))
  nil)

(defmethod imm-value ((value number))
  "For immediates modeled as plain numbers, (imm-value) simply returns the number."
  value)

(defgeneric make-immediate (value &optional type width)
  (:documentation "Fucntion to create an immediate value."))

;; (defgeneric imm (value &rest type)
;;   (:documentation "Get a register's index."))

(defmethod make-immediate (value &optional type width)
  (when width
    (typecase value
      (integer (assert (and (not (minusp value))
                            (zerop (ash value (- width))))
                       (value width) "Immediate integer value ~a overflows its width of ~a."))))
  (make-instance 'immediate :width width :value (if (integerp value) value)
                            :type (or type (typecase value
                                             (integer 'integer)))))

;; (defmethod imm ((value integer) &rest type)
;;   (if (not type) value))

;; (defmethod imm ((value immediate) &rest type)
;;   (declare (ignore type))
;;   (imm-value value))

(defclass memory-access-scheme ()
  () (:documentation "Base class for memory access schemes."))

(defclass mas-based (memory-access-scheme)
  ((%base :accessor mas-base
          :initform nil
          :initarg  :base))
  (:documentation "A class encompassing memory access schemes with a base register."))

(defclass mas-indexed (memory-access-scheme)
  ((%index :accessor mas-index
           :initform nil
           :initarg  :index))
  (:documentation "A class encompassing memory access schemes with an index register."))

(defclass mas-displaced (memory-access-scheme)
  ((%displ :accessor mas-displ
           :initform nil
           :initarg  :displ))
  (:documentation "A class encompassing memory access schemes with a displacement value."))

(defclass mas-scaling-displaced (mas-displaced)
  ((%scale :accessor mas-sdisp-scale
           :initform nil
           :initarg  :scale))
  (:documentation "A class encompassing memory access schemes with a displacement value that may be scaled; i.e. multiplied by a power of two."))

(defclass mas-absolute (memory-access-scheme)
  ((%addr :accessor mas-addr
          :initform nil
          :initarg  :addr))
  (:documentation "A class encompassing memory access schemes referencing an absolute address."))

(defclass register-file ()
  ((%name   :accessor rfile-name
            :initform nil
            :initarg  :name)
   (%base   :accessor rfile-base
            :initform nil
            :initarg  :source)
   (%tags   :accessor rfile-tags
            :initform nil
            :initarg  :tags)
   (%width  :accessor rfile-width
            :initform nil
            :initarg  :width)
   (%widths :accessor rfile-widths
            :initform nil
            :initarg  :widths)))

(defclass rfile-reduced (register-file)
  ((%map  :accessor rf-reduced-map
          :initform nil
          :initarg  :map)))

(defclass rfile-displaced (register-file)
  ((%bit-width  :accessor rf-displaced-bit-width
                :initform nil
                :initarg  :bit-width)
   (%bit-offset :accessor rf-displaced-bit-offset
                :initform nil
                :initarg  :bit-offset)))

(defclass rfile-blocked (register-file)
  ((%map  :accessor rf-reduced-map
          :initform nil
          :initarg  :map)))

(defmacro as-register-file (name props &body forms)
  (destructuring-bind (tags &rest derivations) forms
    (let ((base (gensym)) (syms))
      `(let ((,base (make-instance 'register-file ,@(append props (list :tags tags))))
             ,@(loop :for d :in derivations
                     :collect (let ((assign (gensym)))
                                (push assign syms)
                                (setf (fourth d) (append (list :base base) (fourth d)))
                                (list assign (macroexpand (append d))))))
         (list ,base ,@(reverse syms))))))

(defmacro deriving-file (type name props &body forms)
  `(make-instance ',(case type (:reduced 'rfile-reduced) (:displaced 'rfile-displaced))
                  :name ,name ,@props))

(defclass instruction ()
  ((%mnem :accessor inst-mnem
          :initform nil
          :initarg  :mnem
          :documentation "Mnemonic for the instruction.")
   (%oper :accessor inst-oper
          :initform nil
          :initarg  :oper
          :documentation "Operands taken by the instruction.")
   (%span :accessor inst-span
          :initform nil
          :initarg  :span
          :documentation "Width of data addressed by the instruction.")
   (%extn :accessor inst-extn
          :initform nil  
          :initarg  :extn
          :documentation "ISA extension(s) within which the instruction is available."))
  (:documentation "A generic instruction class."))

(defclass assembler ()
  ((%name    :accessor   asm-name
             :initform   nil
             :initarg    :name)
   (%extns   :accessor   asm-extns
             :allocation :class
             :initform   nil
             :initarg    :extns)
   (%storage :accessor   asm-storage
             :initform   nil
             :initarg    :storage)
   (%lexicon :accessor   asm-lexicon
             :initform   nil
             :initarg    :lexicon)
   (%pmodel  :accessor   asm-pmodel
             :initform   nil
             :initarg    :pmodel
             :documentation "The program model: collected register files.")
   (%pool    :accessor   asm-pool
             :documentation "Pool of available registers, to be solved for after initialization.")
   (%reserve :accessor   asm-reserve
             :initform   nil
             :initarg    :reserve)
   (%breadth :accessor   asm-breadth
             :allocation :class
             :initform   16
             :initarg    :breadth)
   ;; (%joiner  :accessor   asm-joiner
   ;;           :allocation :class
   ;;           :initform   #'joinb
   ;;           :initarg    :joiner)
   (%exmodes :accessor   asm-exmodes
             :initform   nil
             :initarg    :exmodes)
   (%pro-api :accessor   asm-program-api
             :allocation :class
             :initform   'program-api
             :initarg    :program-api)
   (%pro-aac :accessor   asm-program-api-access
             :allocation :class
             :initform   'of-program
             :initarg    :program-api-access))
  (:documentation "A generic assembler class."))

(defmethod initialize-instance :after ((asm assembler) &key)
  "After-init method for assemblers; builds register pool exclusing blocked registers."
  (let ((pool (make-hash-table :test #'eq))
        (blocked))
    (dolist (file (asm-pmodel asm))
      (typecase file
        (rfile-blocked (loop :for tag :in (rfile-tags file) :unless (member tag blocked)
                             :do (push tag blocked)))))
    (dolist (file (asm-pmodel asm))
      (unless (typep file 'rfile-blocked)
        (loop :for tag :in (rfile-tags file)
              :for index :from 0
              :unless (member tag blocked)
                :do (setf (gethash tag pool)
                          (cons file index)))))
    (setf (asm-pool asm) pool)))

(defclass assembler-encoding (assembler)
  ((%decoder :accessor asm-enc-decoder
             :initform nil
             :initarg  :decoder))
  (:documentation "An assembler with relatively simple correspondences between instruction and encoding (such as z80 or 6502) such that many or most instructions can be disassembled through simple lookups."))

(defclass assembler-masking (assembler)
  ((%segment :accessor asm-msk-segment
             :initform nil
             :initarg  :segment)
   (%battery :accessor asm-msk-battery
             :initform nil
             :initarg  :battery))
  (:documentation "An assembler whose instructions can be disassembled on the basis of comparing and decomposing bitmasks."))

(defgeneric extns-of (assembler)
  (:documentation "Fetch an assembler's types."))

(defgeneric extn-p (assembler extension)
  (:documentation "Check for an assembler's support of an ISA extension."))

(defgeneric %derive-pmodel (assembler &rest params) ;; TODO: Remove this method
  (:documentation "Determine programming model for a given assembler."))

(defgeneric of-lexicon (assembler key &optional value)
  (:documentation "Fetch lexical get/setter for given assembler."))

(defgeneric of-storage (assembler key)
  (:documentation "Fetch storage object for given assembler."))

(defgeneric storage-type-p (assembler type &rest keys)
  (:documentation "Confirm membership of symbol(s) in a given storage type of an assembler."))

(defgeneric of-pmodel (assembler key)
  (:documentation "Fetch register file from program model by name or key for given assembler."))

(defgeneric specify-ops (assembler asm-symbol op-symbol operands params)
  (:documentation "Specify assembly functions for members of a given assembler's class."))

(defgeneric interpret-ops (assembler asm-symbol op-symbol operands params)
  (:documentation "Specify disassembly functions for members of a given assembler's class.")
  (:method-combination or))

(defgeneric qualify-ops (assembler operands form order)
  (:documentation "Qualify operations for members of a given assembler's class."))

(defgeneric extend-clauses (assembler mnemonic operands body params)
  (:documentation "Extend opcode specification clauses for an assembler."))

(defgeneric clause-processor (assembler action mnemonic operands body params)
  (:documentation "Process opcode specification clauses for an assembler."))

(defgeneric locate (assembler items)
  (:documentation "Locate available storage for use by a program."))

(defgeneric reserve (assembler &rest params)
  (:documentation "Reserve a storage location for use by a program."))

(defgeneric locate-relative  (assembler location-spec program-props)
  (:documentation "Find the position of a branch point relative to a location."))

(defgeneric compose (assembler params expression)
  (:documentation "A function composing an instruction for an assembler, translating the symbols in an instruction into specific values and class instances."))

(defgeneric interpret (assembler params array)
  (:documentation "A function converting operation codes for a given ISA into a human-readable assembly language."))

(defgeneric interpret-element (assembler ipattern reader)
  (:documentation "A function to interpret an individual opcode or other element.")
  (:method-combination or))

(defgeneric of-decoder (assembler-encoding key &optional value)
  (:documentation "Encoding getter/setter for an encoding assembler."))

(defgeneric of-battery (assembler-encoding key &optional value)
  (:documentation "Battery getter/setter for an encoding assembler."))

(defgeneric %assemble (assembler assembler-sym params expressions)
  (:documentation "A function implementing generation of opcodes from assembly code."))

(defmethod of-lexicon ((assembler assembler) key &optional value)
  (if value (setf (gethash key (asm-lexicon assembler)) value)
      (gethash key (asm-lexicon assembler))))

(defmethod of-storage ((assembler assembler) key)
  (let ((return-type) (return-index))
    (loop :for (type-key names) :on (asm-storage assembler) :by #'cddr :while (not return-type)
          :do (let ((pos (position key names)))
                (when pos (setf return-type type-key return-index pos))))
    (values return-index return-type)))

(defmethod of-pmodel ((assembler assembler) key)
  (let ((return-type) (return-index))
    (typecase key
      (integer (nth key (asm-pmodel assembler)))
      (symbol (loop :for rfile :on (asm-pmodel assembler)
                    :when (eq key (rfile-name rfile)) :return rfile)))))

(defmethod storage-type-p ((assembler assembler) type &rest keys)
  (let ((type-domain (getf (asm-storage assembler) type)))
    (loop :for key :in keys :always (position key type-domain))))

(defmethod of-decoder ((assembler assembler-encoding) key &optional value)
  (if value (setf (gethash key (asm-enc-decoder assembler)) value)
      (gethash key (asm-enc-decoder assembler))))

(defmethod of-battery ((assembler assembler-masking) key &optional value)
  (if value (setf (gethash key (asm-msk-battery assembler)) value)
      (gethash key (asm-msk-battery assembler))))

(defmethod extns-of ((item t))
  (declare (ignore item))
  nil)

(defmethod extns-of ((assembler assembler))
  (append (call-next-method)
          (asm-extns assembler)))

(defmethod extn-p ((assembler assembler) extension)
  (member extension (asm-extns assembler)))

(defmethod %derive-pmodel ((assembler assembler) &rest params) ;; TODO: remove
  (let ((derived-ranges))
    (loop :for p :in params
          :do (destructuring-bind (qualifier &rest bindings) p
                (when (or (eq t qualifier)
                          (member qualifier (asm-extns assembler)))
                  (loop :for b :in bindings
                        :do (setf (getf derived-ranges (first b))
                                  (max (second b) (or (getf derived-ranges (first b))
                                                      0)))))))
    ;; (print (list :der derived-ranges))
    (setf (asm-pmodel assembler)
          (loop :for (key length) :on derived-ranges :by #'cddr
                :collect (cons key (loop :for i :below length
                                         :unless (member i (rest (assoc key (asm-reserve assembler))))
                                           :collect i))))))

(defmacro derive-pmodel (assembler &rest params) ;; TODO: remove
  `(%derive-pmodel ,assembler ,@(loop :for p :in params :collect `(quote ,p))))

(defmacro specops (symbol operands assembler &body items)
  (let* ((params (if (not (and (listp (first items)) (listp (caar items))
                               (keywordp (caaar items))))
                     nil (first items)))
         (operations (if (not params) items (rest items))))
    (specify-ops (symbol-value assembler)
                 symbol operands (cons (cons :assembler-sym assembler) params)
                 operations)))

(defmethod clause-processor :around ((assembler assembler) action mnemonic operands params body)
  (declare (ignore assembler))
  ;; (print (list :pa params))
  (let ((args (gensym)) (types-to-match (gensym))
        (wrap-body (or (rest (assoc :wrap-body params)) #'identity))
        (api-access-sym (asm-program-api assembler))
        (api-access (asm-program-api-access assembler))
        (operands (case action (of-lexicon operands) (t)))
        (asm-sym (rest (assoc :assembler-sym params))))
    ;; (print (list :abc action mnemonic operands body))
    (multiple-value-bind (content key is-constant) (call-next-method)
      ;; (print (list :gg content key action params operands))
      (list action asm-sym key
            (if is-constant content
                `#'(lambda ,(cons api-access-sym operands)
                   (flet ((,api-access (&rest ,args) (apply ,api-access-sym ,args))
                          ,@(if (assoc :type-matcher params)
                                ;; generate type-matching function for use within instruction assembler
                                `((,(rest (assoc :type-matcher params))
                                   (&rest ,types-to-match)
                                   (intersection ,types-to-match (funcall ,api-access-sym
                                                                          :assembler-type))))))
                     ;; make API and type-checking function (if present) ignorable
                     (declare (ignorable (function ,api-access)
                                         ,@(if (assoc :type-matcher params)
                                               `((function ,(rest (assoc :type-matcher params)))))))
                     ,@(if (assoc :for-types params)
                           ;; generate top-level type-checking code
                           `((assert (intersection (,api-access :assembler-type)
                                                   ',(rest (assoc :for-types params)))
                                     () ,(format nil "Instruction ~a is not compatible ~a."
                                                 key "with this architecture"))))
                     ,@(funcall wrap-body content))))))))

(defmethod extend-clauses ((assembler assembler) mnemonic operands params body)
  (declare (ignore assembler mnemonic operands params))
  body)

(defmethod clause-processor ((assembler assembler) action mnemonic operands params body)
  (declare (ignore assembler action operands params))
  (values body (intern (string mnemonic) "KEYWORD")))

(defun process-clause-matrix (assembler op-symbol operands params operations)
  (let ((clauses) (prefixes) (opcode-base (if (numberp op-symbol) op-symbol 0)))
    (flet ((encoder-and-maybe-decoder-for (clause)
             (let* ((xtclause (if (not (listp (second clause)))
                                  nil (extend-clauses assembler (first clause)
                                                      operands params (rest clause))))
                    (encoder (clause-processor assembler 'of-lexicon (first clause)
                                               operands params (or xtclause (rest clause)))))
               ;; (print (list :xx xtclause))
               (if (not (assoc :duplex params))
                   encoder `(progn ,encoder ,@(loop :for c :in (or xtclause (rest clause))
                                                    :collect (clause-processor
                                                              assembler (rest (assoc :duplex params))
                                                              (first clause)
                                                              operands params c)))))))

      (loop :for row :in (rest operations) :for row-index :from 0
            :do (loop :for cell :in (rest row) :for cellx :in (cdar operations) :for col-index :from 0
                      :do (let ((opcode (+ opcode-base (first cellx) (caar row))))
                            (if (symbolp cell)
                                (case cell
                                  ;; prefix clauses desigate a particular code as a prefix, which
                                  ;; should be shifted and added to the next word to designate
                                  ;; a prefixed opcode; this is used for opcodes as in the
                                  ;; CB__ and ED__ tables of Z80 instructions
                                  (:prefix (push `(of-decoder ,(rest (assoc :assembler-sym params))
                                                              ,opcode :prefix)
                                                 prefixes)))
                                (destructuring-bind (&optional ins &rest opr) cell
                                  (when ins (if (assoc ins clauses)
                                                (when (not (atom (second (assoc ins clauses))))
                                                  ;; don't push an item if it's another possible
                                                  ;; opcode for an instruction without operands,
                                                  ;; as it's redundant and it interferes with the
                                                  ;; clause organization
                                                  (push (list opr opcode) (rest (assoc ins clauses))))
                                                (push (if opr (list ins (list opr opcode))
                                                          (list ins opcode))
                                                      clauses))))))))
      ;; (print (list :clau clauses params))
      ;; (loop :for c :in clauses :collect (decode (clause-processor assembler (first c) operands params c)))
      ;; (loop :for c :in clauses :collect (encoder-and-maybe-decoder-for c))
      (append (mapcar #'encoder-and-maybe-decoder-for clauses)
              prefixes))))

(defun complete-dforms (processor symbol options form)
  "A function that facilitates processing of forms within an operation spec; it is now used mainly for the composition of (determine-in-context) macros according to the parameters of (specops) forms they appear in."
  (if (not processor)
      form (typecase form
             (atom form)
             (list (or (funcall processor symbol options form)
                        (loop :for item :in form
                             :collect (complete-dforms processor symbol options item)))))))

(defmethod specify-ops ((assembler assembler) op-symbol operands params operations)
  "A simple scheme for implementing operations - the (specop) content is directly placed within functions in the lexicon hash table."
  ;; retrieve the form processor specified in the params, if any - this is often added in the
  ;; course of macro currying performed in specific architecture modules
  (let ((form-processor (and (rest (assoc :process-forms params))
                             (symbol-function (rest (assoc :process-forms params))))))
    (cond ((assoc :combine params)
           ;; combinatoric parameters are used to specify mnemonics and opcodes that can be derived
           ;; by combining lists of base components, such as the Xcc conditional instructions seen
           ;; in many ISAs, where many different conditions share a common numeric and mnemonic base
           (destructuring-bind (co-symbol join-by indexer &rest combinators)
               (rest (assoc :combine params))
             (cons 'progn (loop :for co :in combinators :for i :from 0
                                :collect (let ((comp-sym (case join-by
                                                           (:appending (intern (format nil "~a~a" op-symbol co)
                                                                               "KEYWORD"))))
                                               (index (funcall (case indexer (:by-index #'identity))
                                                               i)))
                                           (clause-processor
                                            assembler 'of-lexicon comp-sym operands
                                            (append (list (cons :wrap-body ;; TODO: IL PROBLEM HERE
                                                                (lambda (body)
                                                                  `((let ((,co-symbol ,index)) ,@body)))))
                                                  params)
                                          (complete-dforms form-processor comp-sym params operations)))))))
        ((assoc :tabular params)
         ;; tabular parameters are used to specify many opcodes for ISAs like Z80 and 6502 along
         ;; with more recent one-byte instruction sets like WebAssembly
         (destructuring-bind (mode &rest properties) (rest (assoc :tabular params))
           (declare (ignore properties))
           (case mode (:cross-adding
                       (cons 'progn (process-clause-matrix
                                     assembler op-symbol
                                     operands params (complete-dforms form-processor op-symbol
                                                                      params operations)))))))
        (t (clause-processor assembler 'of-lexicon op-symbol operands params
                             (complete-dforms form-processor op-symbol params operations))))))

(defmacro readops (symbol operands assembler &body items)
  (let* ((params (if (not (and (listp (first items)) (listp (caar items))
                               (keywordp (caaar items))))
                     nil (first items)))
         (operations (if (not params) items (rest items))))
    (interpret-ops (symbol-value assembler)
                   symbol operands (cons (cons :assembler-sym assembler) params)
                   operations)))

(defmethod interpret-ops or ((assembler assembler-encoding) designator operands params operations)
  "A simple scheme for implementing operations - the (specop) content is directly placed within functions in the lexicon hash table."
  (let ((designator (macroexpand designator)))
    (if (not (numberp designator))
        nil `(of-decoder ,(rest (assoc :assembler-sym params)) ,designator
                         (lambda ,operands (declare (ignorable ,@operands)) ,@operations)))))

(defmethod interpret-ops or ((assembler assembler-masking) designator operands params operations)
  "A simple scheme for implementing operations - the (specop) content is directly placed within functions in the lexicon hash table."
  (if (not (symbolp designator))
      nil `(of-battery ,(rest (assoc :assembler-sym params)) ,(intern (string designator) "KEYWORD")
                       (lambda ,operands (declare (ignorable ,@operands)) ,@operations))))

;; (defmacro readop (symbol args assembler-sym &body body)
;;   (let ((symbol (macroexpand symbol))
;;         (function `(lambda ,args (declare (ignorable ,@args)) ,@body)))
;;     (if (numberp symbol)
;;         `(of-decoder *assembler-prototype-m68k* ,symbol ,function)
;;         `(of-battery *assembler-prototype-m68k* ,(intern (string symbol) "KEYWORD") ,function))))

(defmacro determine-in-context (utils mnemonic specs &optional bindings &rest body)
  (destructuring-bind (&key qualify derive verbalize &allow-other-keys) utils
    (labels ((process-level (body bindings specs)
               (if (not bindings)
                   body (if (and (first bindings) (listp (first bindings)))
                            `((multiple-value-bind ,(first bindings)
                                  ;; ,(funcall derive (caar specs) (cadar specs))
                                  ,(funcall derive (first specs))
                                ,@(process-level body (rest bindings) (rest specs))))
                            (process-level body (rest bindings) (rest specs))))))
      (let* ((mnem-length (length (string mnemonic)))
             (op-strings (loop :for spec :in specs :collect (string (first spec))))
             (op-max-length (reduce #'max (loop :for string :in op-strings :collect (length string)))))
        `(if ,(cons 'and (loop :for spec :in specs
                               :collect (destructuring-bind (operand &rest types) spec
                                          (cons 'or (loop :for type :in types
                                                          :collect (funcall qualify operand type))))))
             ,(if body `(let ,(if bindings (loop :for b :in bindings :for s :in specs
                                                 :when (and b (symbolp b))
                                                   :collect (list b (funcall derive s))))
                         ,@(process-level body bindings specs)))
             (error ,(apply #'concatenate 'string
                            (format nil "Invalid operand(s) for instruction ~a. Format: ~%" mnemonic)
                            (format nil "~a ~v,a - ~a" mnemonic op-max-length (caar specs)
                                    (funcall verbalize (cadar specs)))
                            (append (loop :for sub-spec :in (cddar specs)
                                          :collect (or (format nil "~v,a ~a" (+ mnem-length 3 op-max-length)
                                                               #\  (funcall verbalize sub-spec))
                                                       ""))
                                    (loop :for spec :in (rest specs)
                                          :append (append (list (format nil "~v,a ~a - ~a"
                                                                        mnem-length #\ (first spec)
                                                                        (funcall verbalize (cadr spec))))
                                                          (loop :for sub-spec :in (cddr spec)
                                                                :collect (format
                                                                          nil "~v,a ~a"
                                                                          (+ mnem-length 3 op-max-length)
                                                                          #\ (funcall verbalize sub-spec)))
                                                        ))))))))))

(defparameter *default-segment* 1000)

(defmethod locate-relative ((assembler assembler) location-spec program-props)
  ;; (print (list :aea location-spec program-props))
  (destructuring-bind (label bit-offset field-length index) location-spec
    (declare (ignore bit-offset))
    (let ((offset (- (rest (assoc label (getf program-props :marked-points))) index)))
      (if (not (minusp offset)) ;; two's complement conversion
          offset (+ (ash 1 (1- field-length))
                    (abs offset))))))

(defmethod locate ((assembler assembler) items)
  (let ((reserved) (pmodel (asm-pmodel assembler)))
    (loop :for item :in items
          :collect (destructuring-bind (symbol type &key bind &allow-other-keys) item
                     (let* ((type-spec (getf pmodel type))
                            (rset (assoc type reserved))
                            (out-item (if bind (typecase type-spec
                                                 (list     (and (position bind type-spec :test #'eq)
                                                                bind))
                                                 (function (funcall type-spec bind)))
                                          (let ((options (set-difference type-spec (rest rset))))
                                            ;; (print (list :aa pmodel type-spec (rest rset)))
                                            (nth (random (length options)) options)))))
                       (unless rset
                         (push (setf rset (list type)) reserved))
                       (when (and bind out-item)
                         (when (not (member out-item type-spec))
                           (error "The member ~a of type ~a is not available in this assembler's domain."
                                  bind type))
                         (when (member out-item (rest (assoc type reserved)))
                           (error "The member ~a of type ~a has already been reserved." bind type)))
                       (if out-item (push out-item (rest (assoc type reserved)))
                           (error "Unable to reserve member ~a of type ~a." bind type))
                       (list symbol out-item))))))

(defmethod compose ((assembler assembler) params expression)
  "The top-level method for assembly. Generates a byte vector from a list of instructions formatted as small lists."
  (let* ((unit (asm-breadth assembler))
         (codes (make-array *default-segment* :element-type (list 'unsigned-byte unit)
                                              :initial-element 0 :adjustable t :fill-pointer 0))
         (codes-length) (props (list :marked-points nil :tag-points nil))
         (two-power (floor (log unit 2)))
         (api-access (lambda (mode &rest args)
                       (case mode
                         (:assembler-type (asm-extns assembler))
                         (:exmode (or (rest (assoc :exmode params))
                                      (first (asm-exmodes assembler))))
                         (:label (destructuring-bind (field-length offset-bits symbol) args
                                   (typecase symbol
                                     (integer symbol)
                                     (symbol  (let ((spec (list symbol offset-bits field-length
                                                                (fill-pointer codes))))
                                                (or (and (assoc symbol (getf props :marked-points))
                                                         (locate-relative assembler spec props))
                                                    (and (push spec (getf props :tag-points))
                                                         0)))))))))))
    (flet ((add-value (value)
             (unless (< (fill-pointer codes) (1- (length codes)))
               (adjust-array codes (+ *default-segment* (length codes)) :initial-element 0))
             (vector-push value codes)))
      (loop :for item :in expression
            :do (typecase item
                  (symbol (push (cons item (fill-pointer codes)) (getf props :marked-points)))
                  (list   (destructuring-bind (instruction &rest operands) item
                            (let ((build-instruction (of-lexicon assembler instruction)))
                              (multiple-value-bind (code properties)
                                  (if (not (functionp build-instruction))
                                      build-instruction (apply build-instruction api-access operands))
                                (if (functionp code) ;; TODO: remove this clause
                                    (let ((breadth (or (getf properties :breadth) 0)))
                                      (push (append (list (fill-pointer codes) code)
                                                    properties)
                                            (getf props :tag-points))
                                      (dotimes (i breadth) (add-value 0)))
                                    (if (listp code)
                                        (loop :for item :in code :when item
                                              :do (serialize item unit #'add-value))
                                        (serialize code unit #'add-value)))))))))
      (setf codes-length (fill-pointer codes))
      
      (loop :for tag-spec :in (getf props :tag-points)
            :do (destructuring-bind (symbol bit-offset field-length index) tag-spec
                  (let ((value (locate-relative assembler tag-spec props))
                        (unaligned-bits (logand bit-offset (1- (ash 1 two-power)))))
                    (setf (fill-pointer codes) (+ index (ash bit-offset (- two-power))))
                    (unless (zerop unaligned-bits)
                      (let ((first-original (aref codes (fill-pointer codes))))
                        (serialize (+ first-original (ash value (- (- field-length
                                                                      (- unit unaligned-bits)))))
                                   unit #'add-value)))
                    (serialize (rest (assoc symbol (getf props :marked-points)))
                               unit #'add-value))))
      (let ((output (make-array codes-length :element-type (list 'unsigned-byte unit))))
        (loop :for i :below codes-length :do (setf (aref output i) (aref codes i)))
        output))))

(defmethod %assemble ((assembler assembler) assembler-sym params expressions)
  `(let ,(if (not (rest (assoc :store params)))
             nil (locate assembler (rest (assoc :store params))))
     (compose ,assembler-sym ',params
              (list ,@(loop :for e :in expressions
                            :collect (if (not (and (listp e) (keywordp (first e))))
                                         e (cons 'list e)))))))

(defmacro assemble (assembler &rest expressions)
  (let* ((params (if (not (and (listp (first expressions))
                               (caar expressions) (listp (caar expressions))))
                     nil (first expressions)))
         (expressions (if (not params) expressions (rest expressions))))
    (%assemble (symbol-value assembler) assembler params expressions)))

;; (defmethod interpret ((assembler assembler) params array)
;;   "The top-level method for disassembly. Composes a list of instructions from a byte vector according to the properties of a given ISA."
;;   (let* ((etype (let ((element-type (array-element-type array)))
;;                   (unless (and (listp element-type)
;;                                (eq 'unsigned-byte (first element-type)))
;;                     (error "Invalid array."))
;;                   (second element-type)))
;;          (to-read (/ etype (asm-breadth assembler)))
;;          ;; (intervals (loop :for segment :in (asm-msk-segment assembler)
;;          ;;                  :collect (ash etype (- (+ 2 segment)))))
;;          (intervals (coerce (asm-msk-segment assembler) 'vector))
;;          (deltas (cons 0 (loop :for i :from (1- (length intervals)) :downto 1
;;                                :collect (* etype (- (aref intervals i) (aref intervals (1- i)))))))
;;          (point 0) (disassembled))
;;     (labels ((read-words (from count)
;;                ;; (print (list :et etype from count))
;;                (let ((value 0))
;;                  (loop :for c :below (* count to-read)
;;                        :do (setf value (ash  value etype))
;;                            (incf value (aref array (+ c from))))
;;                  value)))
;;       (loop :while (< point (1- (length array)))
;;             :do (let* ((match) (this-interval) ;; (ipatterns)
;;                        (sub-count 0)
;;                        (reader (lambda (in)
;;                                  (lambda (count)
;;                                    (let ((this-count sub-count))
;;                                      (incf sub-count count)
;;                                      (read-words (+ point this-count in) count)))))
;;                        (access (lambda (in)
;;                                  (lambda (mode &rest args)
;;                                    (case mode
;;                                      (:read (let ((count (first args))
;;                                                   (this-count sub-count))
;;                                               (incf sub-count count)))))))
;;                        ;;                      (read-words (+ point this-count in) count))))))
;;                        (ivindex (min (1- (length intervals)) (- (length array) point 1)))
;;                        (pattern (read-words point (aref intervals ivindex))))
;;                   ;; (print (list :dd deltas))
                  
;;                   (loop :for ix :below ivindex :for delta :in deltas
;;                         :do (setf pattern (ash pattern (- delta)))
;;                             (let ((match? (interpret-element assembler pattern
;;                                                              (funcall reader (aref intervals ix)))))
;;                               (when match? (setf match         match?
;;                                                  this-interval (aref intervals ix)))))
                 
;;                   (if match (progn (push match disassembled)
;;                                    (setf point (+ point sub-count this-interval)))
;;                       ;; (progn (push nil disassembled)
;;                       ;;        (setf point (+ point sub-count this-interval)))
;;                       (error "Undecipherable instruction!")
;;                       )))
;;       (reverse disassembled))))

(defmethod interpret ((assembler assembler) params array)
  "The top-level method for disassembly. Composes a list of instructions from a byte vector according to the properties of a given ISA."
  (let* ((etype (let ((element-type (array-element-type array)))
                  (unless (and (listp element-type)
                               (eq 'unsigned-byte (first element-type)))
                    (error "Invalid array."))
                  (second element-type)))
         (to-read (/ etype (asm-breadth assembler)))
         ;; (intervals (loop :for segment :in (asm-msk-segment assembler)
         ;;                  :collect (ash etype (- (+ 2 segment)))))
         (intervals (coerce (asm-msk-segment assembler) 'vector))
         (deltas (cons 0 (loop :for i :from (1- (length intervals)) :downto 1
                               :collect (* etype (- (aref intervals i) (aref intervals (1- i)))))))
         (point 0) (disassembled))
    (labels ((read-words (from count)
               ;; (print (list :et etype from count))
               (let ((value 0))
                 (loop :for c :below (* count to-read)
                       :do (setf value (ash  value etype))
                           (incf value (aref array (+ c from))))
                 value)))
      (loop :while (< point (1- (length array)))
            :do (let* ((match) (this-interval)
                       (base 0) (sub-count 0)
                       ;; (reader (lambda (in)
                       ;;           (lambda (count)
                       ;;             (let ((this-count sub-count))
                       ;;               (incf sub-count count)
                       ;;               (read-words (+ point this-count in) count)))))
                       (access (lambda (in)
                                 (declare (ignore in))
                                 (lambda (mode &rest args)
                                   (case mode
                                     (:read (let ((count (first args))
                                                  (this-count sub-count))
                                              (incf sub-count count)
                                              (read-words (+ point this-count base) count)))))))
                       (ivindex (min (1- (length intervals)) (- (length array) point 1)))
                       (pattern (read-words point (aref intervals ivindex))))
                  ;; (print (list :dd deltas))
                  
                  (loop :for ix :below ivindex :for delta :in deltas
                        :do (setf pattern (ash pattern (- delta))
                                  base    (aref intervals ix))
                            (let ((match? (interpret-element assembler pattern access)))
                              (when match? (setf match         match?
                                                 this-interval (aref intervals ix)))))
                 
                  (if match (progn (push match disassembled)
                                   (setf point (+ point sub-count this-interval)))
                      ;; (progn (push nil disassembled)
                      ;;        (setf point (+ point sub-count this-interval)))
                      (error "Undecipherable instruction!")
                      )))
      (reverse disassembled))))

(defmethod interpret-element or ((assembler assembler-encoding) ipattern reader)
  (let ((match (gethash ipattern (asm-enc-decoder assembler))))
    ;; (print (list :ma match))
    (if (not (functionp match)) match (identity (funcall match reader)))))

(defmethod interpret-element or ((assembler assembler-masking) ipattern reader)
  (let ((match))
    (maphash (lambda (key unmasker)
               (declare (ignore key))
               (unless match (let ((attempt (funcall unmasker ipattern reader)))
                               (when attempt (setf match attempt)))))
             (asm-msk-battery assembler))
    ;; (loop :until match :for unmasker :being :the :hash-values :of (asm-msk-battery assembler)
    ;;       :do (let ((attempt (funcall unmasker ipattern reader)))
    ;;             (when attempt (setf match attempt))))
   match))

(defmacro match-types (&rest pairs)
  (let ((items) (types) (count (length pairs)))
    (loop :for sym :in pairs :for i :from 0 :do (if (< i (ash count -1))
                                                    (push sym items) (push sym types)))
    (cons 'and (loop :for it :in items :for ty :in types :collect `(typep ,it ',ty)))))

(defmacro to-tag (function &rest properties)
  (list 'values function (cons 'list properties)))

(defmacro define-extension (symbol assembler output-sym input-bindings &body body)
  (let ((input (gensym)) (params (gensym)))
    `(defun ,symbol (,input &optional ,params)
       (funcall #'(lambda ,(cons output-sym input-bindings) ,@body)
                (cons (compose ,assembler ,params ,input) ,input)))))

;; (defmethod clause-processor ((assembler assembler) mnemonic operands body params)
;;   (declare (ignore assembler))
;;   (let ((args (gensym))
;;         (wrap-body (or (rest (assoc :wrap-body params)) #'identity))
;;         (api-access-sym (asm-program-api assembler))
;;         (api-access (asm-program-api-access assembler)))
;;     `(of-lexicon ,(rest (assoc :assembler-sym params))
;;                  ;; ,assembler-symbol
;;                  ,(intern (string mnemonic) "KEYWORD")
;;                  ;; ,(add-alias `
;;                  (lambda ,(cons api-access-sym operands)
;;                    (flet ((,api-access (&rest ,args) (apply ,api-access-sym ,args)))
;;                      ,@(funcall wrap-body body))))))

;; (defun process-clause-matrix (assembler asm-sym operands matrix params)
;;   (declare (ignore params))
;;   (let ((clauses) ;; (varops (remove '&optional operands))
;;         (clause-processor (clause-processor assembler asm-sym)))
;;     (print (list :mm matrix))
;;     (symbol-macrolet ((opcode (+ (first cellx) (caar row))))
;;       (loop :for row :in (rest matrix) :for row-index :from 0
;;             :do (loop :for cell :in (rest row) :for col-index :from 0
;;                       :for cellx :in (cdar matrix)
;;                       :do (destructuring-bind (&optional ins &rest opr) cell
;;                             (when ins (if (assoc ins clauses)
;;                                           (when (not (atom (second (assoc ins clauses))))
;;                                             ;; don't push an item if it's another possible
;;                                             ;; opcode for an instruction without operands,
;;                                             ;; as it's redundant and it interferes with the
;;                                             ;; clause organization
;;                                             (push (list opr opcode) (rest (assoc ins clauses))))
;;                                           (push (if opr (list ins (list opr opcode))
;;                                                     (list ins opcode))
;;                                                 clauses))))))
;;       (print (list :clau clauses))
;;       (loop :for c :in clauses :append (funcall clause-processor c operands)))))

;; (defun process-clause-matrix (assembler asm-sym operands matrix params)

;; (defmethod compose :around ((assembler assembler) params expression)
;;   (destructuring-bind (op &rest props) expression
;;     (typecase expression
;;       (atom expression)
;;       (list (if (atom (first expression)) (call-next-method)
;;                 (loop :for item :in expression :collect (compose assembler params item)))))))


;; (defmethod compose ((assembler assembler) params expression)
;;   (print (list :par params))
;;   (destructuring-bind (instruction &rest operands) expression
;;     (let ((build-instruction (gethash instruction (asm-lexicon assembler))))
;;       (if (not (functionp build-instruction))
;;           build-instruction (apply build-instruction operands)))))

;; (defmethod %assemble ((assembler assembler) assembler-sym params expressions)
;;   (let ((compose-params))
;;     `(let ,(locate assembler assembler-sym (rest (assoc :store (rest params))))
;;        (apply (asm-joiner ,assembler-sym)
;;               (compose ,assembler-sym ,compose-params
;;                               (list ,@(loop :for e :in expressions
;;                                             :collect (if (not (and (listp e) (keywordp (first e))))
;;                                                          e (cons 'list e)))))))))

;; (defun label-delta (label-index inst-params)
;;   (- label-index (nth 2 inst-params)))

;; (loop :for ix :from (min (1- (length intervals)) (- (length array) point 1)) :downto 0
;;       :do (let* ((interval (aref intervals ix))
;;                  (pattern (read-words point interval))
;;                  (match? (interpret-element assembler pattern (funcall reader interval))))
;;             (when match? (setf match         match?
;;                                this-interval interval))))

;; (loop :for ix :from (length intervals) :downto (max 0 (- (length array) point))
;;       :for iv :across intervals
;;       :do (let ((pattern (read-words point iv)))))
;; (loop :for in :in intervals :when (> (length array) (+ 1 point in))
;;       :do (push (read-words point in) ipatterns))
;; (print (list :ip ipatterns))
;; (loop :for in :in (reverse intervals) :for ip :in ipatterns ;; :until match
;;       :do (let ((match? (interpret-element assembler ip (funcall reader in))))
;;             (when match?
;;               (setf match match?
;;                     this-interval in))))
;; (print (list match point (length array)))

;; (defmethod locate ((assembler assembler) location-spec program-props)
;;   ;; (print (list :aea location-spec program-props))
;;   (destructuring-bind (label bit-offset field-length index) location-spec
;;     (asm-pmodel)
;;     (let ((offset (- (rest (assoc label (getf program-props :marked-points))) index)))
;;       (if (not (minusp offset)) ;; two's complement conversion
;;           offset (+ (ash 1 (1- field-length))
;;                     (abs offset))))))

;; (define-extension lock-inst #'joinb bytes (instruction &rest operands)
;;   (declare (ignore operands))
;;   (if (not (position instruction #(:add)))
;;       (error "Ineligible instruction for use with lock prefix.")
;;       )
;;   )
  
;; (defmethod interpret or ((assembler assembler-encoding) params array)
;;   (let* ((etype (let ((element-type (array-element-type array)))
;;                   (unless (eq 'unsigned-byte (first element-type))
;;                     (error "Invalid array."))
;;                   (second element-type)))
;;          (to-read (/ etype (asm-breadth assembler)))
;;          (disassembled) (point 0))
;;     (labels ((read-words (count)
;;                (let ((value 0))
;;                  (loop :for i :below count
;;                        :do (loop :for i :below to-read
;;                                  :do (setf value (ash value etype))
;;                                      (incf value (aref array index))
;;                                      (incf index)))
;;                  value)))
;;       (loop :while (< point (1- (length array)))
;;             :do (let ((match (of-decoder assembler (read-words 1) nil)))
;;                   (push (if (not (functionp match))
;;                             match (funcall match #'read-words))
;;                         disassembled))))))

;; (defmethod interpret or ((assembler assembler-masking) params array)
;;   (let* ((etype (let ((element-type (array-element-type array)))
;;                   (unless (and (listp element-type)
;;                                (eq 'unsigned-byte (first element-type)))
;;                     (error "Invalid array."))
;;                   (second element-type)))
;;          (to-read (/ etype (asm-breadth assembler)))
;;          (intervals (loop :for segment :in (asm-msk-segment assembler)
;;                           :collect (ash etype (- (+ 2 segment)))))
;;          (ipatterns) (disassembled) (point 0))
;;     (labels ((read-words (from count)
;;                (let ((value 0))
;;                  (loop :for c :below (* count to-read)
;;                        :do (setf value (ash  value etype))
;;                            (incf value (aref array (+ c from))))
;;                  value)))
      
;;       (loop :while (< point (1- (length array)))
;;             :do (let ((total 0) (match) (this-interval) (sub-count 0))
;;                   (loop :for i :in intervals :do (push (read-words point i) ipatterns))
;;                   (loop :for in :in intervals :for ip :in ipatterns :while (not match)
;;                         :do (loop :for unmasker :being :the :hash-values
;;                                     :of (asm-msk-battery assembler) :while (not match)
;;                                   :do (let ((attempt (funcall unmasker ip
;;                                                               (lambda (count)
;;                                                                 (let ((this-count sub-count))
;;                                                                   (incf sub-count count)
;;                                                                   (read-words (+ point this-count in)
;;                                                                               count))))))
;;                                         ;; (format t "~v,'0B~%" 16 ip)
;;                                         (when attempt (setf match         attempt
;;                                                              this-interval in)))))
;;                   (if match (progn (push match disassembled)
;;                                    (setf point (+ point sub-count this-interval)))
;;                       (error "Undecipherable instruction!"))))
;;       (reverse disassembled))))

;; (defmethod compose ((assembler assembler) params expression)
;;   (destructuring-bind (ins &rest ops) expression
;;     (let ((width (if (not (keywordp (first ops)))
;;                      nil (position (first ops) #(:b :w :l) :test #'eq)))
;;           (bindings (rest (assoc :store params))))
;;       (print (list :bi bindings width params ins ops))
;;       (apply (gethash ins (asm-lexicon assembler))
;;              (process-operands bindings ops)))))

;; (defun process-operands (bindings items)
;;   (loop :for i :in items :collect (typecase o
;;                                     (keyword o)
;;                                     (symbol (second (assoc o bindings)))
;;                                     (list (destructuring-bind (constructor &rest members) o
;;                                             (funcall (symbol-function constructor)
;;                                                      (process-operands bindings members))))
;;                                     (t o))))

;; (let ((compose-params))
;;   `(let ,(locate (symbol-value assembler)
;;                  (rest (assoc :store (rest params))))
;;      (join (compose ,assembler ,compose-params
;;                     (list ,@(loop :for e :in expressions
;;                                   :collect (if (not (and (listp e) (keywordp (first e))))
;;                                                e (cons 'list e)))))))))

;; (defmacro assemble (assembler params &rest expressions)
;;   `(%assemble ,assembler ',(rest params)
;;               ,(cons 'list (loop :for e :in expressions
;;                                  :collect (if (not (and (listp e) (keywordp (first e))))
;;                                               e (list 'quote e))))))

;; (defun field (&rest values)
;;   (let ((collected) (count 0))
;;     (loop :for value :in values
;;           :do  (if (zerop (ash value -8))
;;                    (progn (incf count) (push 0 collected))
;;                    (loop :until (zerop value) ;; larger values are decomposed and appended
;;                          :do (push (logand value #xFF) collected)
;;                              (setf value (ash value -8)) (incf count))))
;;     (make-array count :element-type '(unsigned-byte 8) :initial-contents collected)))

;; (defun vectorize (bytes value)
;;   (let ((collected))
;;     (loop :until (zerop bytes)
;;           :do (push (logand value #xFF) collected)
;;               (setf value (ash value -8)) (decf bytes))
;;     (make-array (length collected) :element-type '(unsigned-byte 8) :initial-contents collected)))

;; (defun inc-field (field delta)
;;   (let ((index 0))
;;     (loop :until (zerop delta)
;;           :do (incf (aref field index) (logand delta #xFF))
;;               (setf delta (ash delta -8))
;;               ;; (print delta)
;;               (incf index))))

;; (let ((base (gensym)) (bits (gensym)) (mapper (gensym)) (input (gensym)))
;;   `(multiple-value-bind (,base ,bits ,mapper) (bitmapper ,string)
;;      ;; ,@(loop for c :in clauses :collect `(format t "~v,'0B~%" 24 (funcall ,mapper ,@c)))
;;      (funcall (if (>= 8 ,bits) #'identity (lambda (,input) (vectorize (ceiling ,bits 8) ,input)))
;;               (reduce #'+ (list ,base ,@(loop for c :in clauses :collect `(funcall ,mapper ,@c))))))))


;; (defun prefix-vex (&key type length m-value third-op)
;;   ;; length is either 2 or 3 bytes with indicating initial byte
;;   (+ (case type (2 #xc500) (3 #xc40000))
;;      ;; the m-value extends the opcode, and is indicated in specs by the
;;      ;; byte sequences 
;;      (case m-value (1 #x0100) (2 #x0200) (3 #x0300))
;;      ;; the third bit in the last byte is 1 if the vector length is 256 bits, off if not
;;      (if (= 256 length) #b00000100 0) ;; the L bit is turned on if the vector is of length 256
;;      (if third-op (ash (flipbits third-op 4) 3))
;;      ;; if a third register address is to be passed in the last byte, reverse its bits and shift it left 3
;;      ))

;; (defun bitmapper (string)
;;   (let ((segments) (symbols) (base 0) (bits 0))
;;     (loop :for c :across string :for ix :from 0 :when (not (char= c #\.)) ;; period is used as a spacer
;;           :do (if (member c '(#\1 #\0) :test #'char=)
;;                   (when (char= c #\1) (incf base)) ;; set constant 1 bits
;;                   (when (or (not symbols) (not (eq (intern (string-upcase c) "KEYWORD") (first symbols))))
;;                     (push (intern (string-upcase c) "KEYWORD") symbols)
;;                     (push bits segments)))
;;               ;; shift the number to the left unless this is the last digit
;;               (unless (= ix (1- (length string))) (setf base (ash base 1)))
;;               (incf bits))
;;     ;; (print (list :in bits))
;;     (setf segments (cons 0 (loop :for s :in segments :collect (abs (- s bits)))))
;;     ;; (list segments symbols)
;;     (flet ((ones-field (n)
;;              (let ((output 0))
;;                (loop :for i :below n
;;                      :do (incf output 1) (when (< i (1- n)) (setf output (ash output 1))))
;;                output)))
;;       ;; (format t "~v,'0B~%" 24 base)
;;       (values base bits
;;               (lambda (seg-sym value)
;;                 ;; (print (list :ss seg-sym value))
;;                 (let* ((insym (intern (string-upcase seg-sym) "KEYWORD"))
;;                        (index (loop :for s :in symbols :for ix :from 0 :when (eq s insym) :return ix))
;;                        (length (- (nth (1+ index) segments) (nth index segments))))
;;                   (ash (logand value (ones-field length))
;;                        (nth index segments))))))))
