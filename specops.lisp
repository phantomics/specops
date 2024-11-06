;;;; specops.lisp

(in-package #:specops)

(defun find-width (number unit)
  (let ((width 0) (shift (- unit)))
    (loop :until (zerop number) :do (setf number (ash number shift))
                                    (incf width))
    width))

(defun serialize (item unit collector)
  "Serialize a series of integers, integer vectors and/or serial integer specifications into a vector of integers of a given width. A serial integer specification takes the form of a pair of a value and the number of elements it is intended to serialize to."
  (let ((mask (1- (ash 1 unit)))) ;; TODO: find a way to create the mask just once?
    (flet ((decompose (number starting-width)
             (let ((shift (- (* unit (1- starting-width)))))
               (loop :for i :below starting-width
                     :do (funcall collector (logand mask (ash number shift)))
                         (incf shift unit)))))
      (if (integerp item) ;; values that fit within a byte are just pushed on
          (if (zerop (ash item (- unit)))
              (funcall collector item)
              (decompose item (find-width item unit)))
          (if (and (consp item) (not (listp (rest item))))
              ;; handle cons cells encoding width and value, like (3 . 5) → #x000005
              (destructuring-bind (width &rest value) item
                (decompose value width))
              (if (vectorp item)
                  (loop :for i :across item :do (funcall collector i))
                  (error "Attempted to serialize incompatible value - must be an integer, a vector or a pair indicating integer value and encoding width.")))))))

;; (defun serialize (item unit collector)
;;   (let ((mask (1- (ash 1 unit)))) ;; TODO: find a way to create the mask just once?
;;     (flet ((decompose (number starting-width)
;;              (print (list :sw starting-width unit))
;;              (let ((shift (- (* unit starting-width))))
;;                (loop :for i :below starting-width
;;                      :do (funcall collector (logand mask (ash number shift)))
;;                          (incf shift unit)))))
;;       (print (list :it item))
;;       (if (integerp item) ;; values that fit within a byte are just pushed on
;;           (if (zerop (ash item (- unit)))
;;               (funcall collector item)
;;               (decompose item (1- (find-width item unit))))
;;           (if (and (consp item) (not (listp (rest item))))
;;               ;; handle cons cells encoding width and value, like (3 . 5) → #x000005
;;               (destructuring-bind (width &rest value) item
;;                 (decompose value (1- width)))
;;               (if (vectorp item)
;;                   (loop :for i :across item :do (funcall collector i))
;;                   (error "Attempted to serialize incompatible value - must be an integer, a vector or a pair indicating integer value and encoding width.")))))))

(defun join-spec (unit items)
  (let ((shift (- unit)) (mask (1- (ash 1 unit))))
      (let ((collected))
        (loop :for item :in items :when item ;; ignore null items
              :do (if (numberp item) ;; values that fit within a byte are just pushed on
                      (if (zerop (ash item shift))
                          (push item collected)
                          (let ((sub-collected))
                            (loop :until (zerop item)
                                  :do (push (logand item mask) sub-collected)
                                      (setf item (ash item shift)))
                            (setf collected (append (reverse sub-collected) collected))))
                      (if (and (consp item) (not (listp (rest item))))
                          ;; handle cons cells encoding width and value, like (3 . 5) → #x000005
                          (destructuring-bind (width &rest value) item
                            (let ((sub-collected))
                              (loop :for i :below width :do (push (logand value mask) sub-collected)
                                                            (setf value (ash value shift)))
                              (setf collected (append (reverse sub-collected) collected))))
                          (when (vectorp item)
                            (loop :for i :across item :do (push i collected))))))
        (make-array (length collected) :element-type (list 'unsigned-byte unit)
                                       :initial-contents (reverse collected)))))

(defun joinb (&rest items)
  (join-spec  8 items))

(defun joinw (&rest items)
  (join-spec 16 items)) 

(defun flipbits (b &optional (n 32))
  (let ((r 0))
    (dotimes (x n r)
      (setq r (logior (ash r 1) (logand b 1))
            b (ash b -1)))))

(defun quantify-mask-string (string params)
  (let ((segments) (symbols) (base 0) (bits 0))
    (loop :for c :across string :for ix :from 0 :when (not (char= c #\.)) ;; period is used as a spacer
          :do (if (position c "01" :test #'char=)
                  (progn (when (and symbols (not (null (first symbols))))
                           (push nil symbols)
                           (push bits segments))
                         (when (char= c #\1) (incf base))) ;; set constant 1 bits
                  (when (or (not symbols) (not (eq (intern (string-upcase c) "KEYWORD") (first symbols))))
                    (push (intern (string-upcase c) "KEYWORD") symbols)
                    ;; (print (list :bit bits c))
                    (push bits segments)))
              ;; shift the number to the left unless this is the last digit
              (unless (= ix (1- (length string))) (setf base (ash base 1)))
              (incf bits))
    (let ((segments (cons 0 (loop :for s :in segments :collect (abs (- s bits))))))
      ;; (print (list :seg segments))
      (values (loop :for pair :in (rest (assoc :static params)) ;; values
                    :do (destructuring-bind (sym value) pair
                          (let* ((insym (intern (string-upcase sym) "KEYWORD"))
                                 (index (loop :for s :in symbols :for ix :from 0
                                              :when (eq s insym) :return ix)))
                            ;; (when reverse-match (push insym static-segments))
                            (if index (incf base (ash value (nth index segments)))
                                (error "Invalid key for static base value increment."))))
                    :finally (return base))
              ;; (cons 0 (loop :for s :in segments :collect (abs (- s bits))))
              segments symbols bits))))

(defmacro masque (string &rest assignments)
  (let* ((base-sym (gensym))
         (params (if (listp (caar assignments)) (first assignments) nil))
         (assignments (if (not params) assignments (rest assignments))))
    (multiple-value-bind (base segments symbols) (quantify-mask-string string params)
      ;; (print (list :ss segments symbols clauses params))
      ;; (format t "~v,'0B~%" 16 (ash (1- (ash 1 length)) (nth index segments)))

      (let ((steps (loop :for assignment :in assignments
                         :collect (destructuring-bind (key value) assignment
                                    (let ((index (position (intern (string-upcase key) "KEYWORD")
                                                           symbols)))
                                      (when (not index)
                                        (error "Symbol ~a not found in mask ~a." key string))
                                      (let ((length (- (nth (1+ index) segments) (nth index segments))))
                                        `(incf ,base-sym (ash (logand ,value ,(1- (ash 1 length)))
                                                              ,(nth index segments)))))))))
        (if (not steps)
            base `(let ((,base-sym ,base)) ,@steps ,base-sym))))))

(defmacro unmasque (string test-value params-or-keys &body body)
  (let* ((params (if (listp (first params-or-keys)) params-or-keys nil))
         (keys   (if (not params) params-or-keys (first body)))
         (body   (if (not params) body (rest body))))
    (multiple-value-bind (base segments symbols bits) (quantify-mask-string string params)
      (let* ((variant-mask (1- (ash 1 bits)))
             (pairs (loop :for key :in keys
                          :collect (let* ((index (position (intern (string-upcase key) "KEYWORD")
                                                           symbols))
                                          (length (- (nth (1+ index) segments) (nth index segments))))
                                     (decf variant-mask (ash (1- (ash 1 length)) (nth index segments)))
                                     `(,key (logand ,(1- (ash 1 length))
                                                    (ash ,test-value ,(- (nth index segments)))))))))
        ;; (print (list :vm (format nil "~v,'0B" 16 variant-mask)
        ;;              (format nil "~v,'0B" 16 base)))
        `(if (/= ,base (logand ,variant-mask ,test-value))
             nil (let ,pairs ,@body))))))

(defmacro mqbase (name opcsym mnesym args string &body body)
  ;; this is a currying macro for masque, allowing the creation
  ;; of a specialized masque with static contents for some fields
  (destructuring-bind (static-specs clauses &optional disassembler) body
    (let ((dsarg (gensym)) (flargs (gensym))
          (static-form (loop :for ss :in static-specs
                             :collect (if (not (eq :static (first ss)))
                                          (list 'quote ss)
                                          `(list :static ,@(loop :for spec :in (rest ss)
                                                                 :collect `(list ',(first spec)
                                                                                 ,(second spec))))))))
      `(defmacro ,name (,opcsym ,mnesym)
         (list (list 'lambda ',(cons 'program-api args) ;; TODO - gensym for args
                     (list 'flet '((of-program (&rest ,flargs) (apply program-api ,flargs)))
                           '(declare (ignorable (function of-program)))
                           (append (list 'masque ,string)
                                   ;; generate a template incorporating the :static values
                                   ;; into a (masque) expansion within the defined macro
                                   (list (list ,@static-form))
                                   ',clauses)))
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

(defgeneric imm (value &rest type)
  (:documentation "Get a register's index."))

(defmethod make-immediate (value &optional type width)
  (when width
    (typecase value
      (integer (assert (and (not (minusp value))
                            (zerop (ash value (- width))))
                       (value width) "Immediate integer value ~a overflows its width of ~a."))))
  (make-instance 'immediate :width width :value (if (integerp value) value)
                            :type (typecase value
                                    (integer 'integer))))

(defmethod imm ((value integer) &rest type)
  (if (not type) value))

(defmethod imm ((value immediate) &rest type)
  (imm-value value))

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

(defclass assembler ()
  ((%name    :accessor   asm-name
             :initform   nil
             :initarg    :name)
   (%type    :accessor   asm-type
             :allocation :class
             :initform   nil
             :initarg    :type)
   (%storage :accessor   asm-storage
             :initform   nil
             :initarg    :storage)
   (%lexicon :accessor   asm-lexicon
             :initform   nil
             :initarg    :lexicon)
   (%domains :accessor   asm-domains
             :initform   nil
             :initarg    :domains)
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

(defgeneric types-of (assembler)
  (:documentation "Fetch an assembler's types."))

(defgeneric %derive-domains (assembler &rest params) ;; TODO: Remove this method
  (:documentation "Determine storage domains for a given assembler."))

(defgeneric of-lexicon (assembler key &optional value)
  (:documentation "Fetch lexical get/setter for given assembler."))

(defgeneric of-storage (assembler key)
  (:documentation "Fetch storage object for given assembler."))

(defgeneric storage-type-p (assembler type &rest keys)
  (:documentation "Confirm membership of symbol(s) in a given storage type of an assembler."))

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

(defmethod storage-type-p ((assembler assembler) type &rest keys)
  (let ((type-domain (getf (asm-storage assembler) type)))
    (loop :for key :in keys :always (position key type-domain))))

(defmethod of-decoder ((assembler assembler-encoding) key &optional value)
  (if value (setf (gethash key (asm-enc-decoder assembler)) value)
      (gethash key (asm-enc-decoder assembler))))

(defmethod of-battery ((assembler assembler-masking) key &optional value)
  (if value (setf (gethash key (asm-msk-battery assembler)) value)
      (gethash key (asm-msk-battery assembler))))

(defmethod types-of ((item t))
  (declare (ignore item))
  nil)

(defmethod types-of ((assembler assembler))
  (append (call-next-method)
          (asm-type assembler)))

(defmethod %derive-domains ((assembler assembler) &rest params) ;; TODO: remove
  (let ((derived-ranges))
    (loop :for p :in params
          :do (destructuring-bind (qualifier &rest bindings) p
                (when (or (eq t qualifier)
                          (member qualifier (asm-type assembler)))
                  (loop :for b :in bindings
                        :do (setf (getf derived-ranges (first b))
                                  (max (second b) (or (getf derived-ranges (first b))
                                                      0)))))))
    ;; (print (list :der derived-ranges))
    (setf (asm-domains assembler)
          (loop :for (key length) :on derived-ranges :by #'cddr
                :collect (cons key (loop :for i :below length
                                         :unless (member i (rest (assoc key (asm-reserve assembler))))
                                           :collect i))))))

(defmacro derive-domains (assembler &rest params) ;; TODO: remove
  `(%derive-domains ,assembler ,@(loop :for p :in params :collect `(quote ,p))))

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
  (let ((args (gensym))
        (wrap-body (or (rest (assoc :wrap-body params)) #'identity))
        (api-access-sym (asm-program-api assembler))
        (api-access (asm-program-api-access assembler))
        (operands (case action (of-lexicon operands) (t))))
    ;; (print (list :abc action mnemonic operands body))
    (multiple-value-bind (content key is-constant) (call-next-method)
      ;; (print (list :gg content key action params operands))
      (list action (rest (assoc :assembler-sym params))
            key ;; (intern (string mnemonic) "KEYWORD")
            ;; (let ((content (funcall wrap-body (call-next-method))))
            (if is-constant content
                `(lambda ,(cons api-access-sym operands)
                   (flet ((,api-access (&rest ,args) (apply ,api-access-sym ,args)))
                     (declare (ignorable (function ,api-access)))
                     ;; (declare (ignorable ,api-access))
                     ,@(funcall wrap-body content))))))))

(defmethod extend-clauses ((assembler assembler) mnemonic operands params body)
  (declare (ignore assembler mnemonic operands params))
  body)

(defmethod clause-processor ((assembler assembler) action mnemonic operands params body)
  (declare (ignore assembler action operands params))
  (values body ;; mnemonic
          (intern (string mnemonic) "KEYWORD")))

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

(defmethod specify-ops ((assembler assembler) op-symbol operands params operations)
  "A simple scheme for implementing operations - the (specop) content is directly placed within functions in the lexicon hash table."
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
                                          (append (list (cons :wrap-body
                                                              (lambda (body)
                                                                `((let ((,co-symbol ,index)) ,@body)))))
                                                  params)
                                          operations))))))
        ((assoc :tabular params)
         ;; tabular parameters are used to specify many opcodes for ISAs like Z80 and 6502 along
         ;; with more recent one-byte instruction sets like WebAssembly
         (destructuring-bind (mode &rest properties) (rest (assoc :tabular params))
           (declare (ignore properties))
           (case mode (:cross-adding
                       (cons 'progn (process-clause-matrix assembler op-symbol
                                                           operands params operations))))))
        (t (clause-processor assembler 'of-lexicon op-symbol operands params operations))))

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

(defun qualify-operand (operand type)
  type)

(defun derive-operand (operand type)
  operand)

(defun verbalize-operand (spec)
  (format nil "~a~%" spec))

(defmacro determine (mnemonic &rest specs)
  `(determine-in-context ,(list :qualify #'qualify-operand :verbalize #'verbalize-operand
                                :derive #'derive-operand)
                         ,mnemonic ,@specs))

(defun complete-dforms (msym dsym form)
  (typecase form
    (atom form)
    (list (if (not (eql dsym (first form)))
              (loop :for item :in form :collect (complete-dforms msym dsym item))
              (append (list dsym msym)
                      (loop :for item :in (rest form)
                            :collect (complete-dforms msym dsym item)))))))
                           

(defmacro determine-in-context (utils mnemonic specs &optional bindings &rest body)
  (destructuring-bind (&key qualify derive verbalize &allow-other-keys) utils
    (labels ((process-level (body bindings specs)
               (if (not bindings)
                   body (if (not (listp (first bindings)))
                            (process-level body (rest bindings) (rest specs))
                            `((multiple-value-bind ,(first bindings)
                                  ,(funcall derive (caar specs) (cadar specs))
                                ,@(process-level body (rest bindings) (rest specs))))))))
      (let* ((mnem-length (length (string mnemonic)))
             (op-strings (loop :for spec :in specs :collect (string (first spec))))
             (op-max-length (reduce #'max (loop :for string :in op-strings :collect (length string)))))
        `(if ,(cons 'and (loop :for spec :in specs
                               :collect (destructuring-bind (operand &rest types) spec
                                          (cons 'or (loop :for type :in types
                                                          :collect (funcall qualify operand type))))))
             ,(if body `(let ,(if bindings (loop :for b :in bindings :for s :in specs
                                                 :when (and b (symbolp b))
                                                   :collect (list b (funcall derive (first s)
                                                                             (second s)))))
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

#|

(determine andi ((op0 gpr (adr)) (op1 (imm 16)))
          (en0 (en1 en2)))

(IF (AND (OR GPR (ADR)) (OR (IMM 16)))
    NIL
    (ERROR "
ANDI OP0 - GPR
           (ADR)
     OP1 -      (IMM 16)
"))



|#

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
  (let ((reserved) (domains (asm-domains assembler)))
    (loop :for item :in items
          :collect (destructuring-bind (symbol type &key bind &allow-other-keys) item
                     (let* ((type-spec (getf domains type))
                            (rset (assoc type reserved))
                            (out-item (if bind (typecase type-spec
                                                 (list     (and (position bind type-spec :test #'eq)
                                                                bind))
                                                 (function (funcall type-spec bind)))
                                          (let ((options (set-difference type-spec (rest rset))))
                                            ;; (print (list :aa domains type-spec (rest rset)))
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
                         (:exmode (or (rest (assoc :exmode params))
                                      (first (asm-exmodes assembler))))
                         (:label (destructuring-bind (offset-bits field-length symbol) args
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
                              ;; (print (list :bi build-instruction))
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
            :do (let* ((match) (this-interval) ;; (ipatterns)
                       (sub-count 0)
                       (reader (lambda (in)
                                 (lambda (count)
                                   (let ((this-count sub-count))
                                     (incf sub-count count)
                                     (read-words (+ point this-count in) count)))))
                       (ivindex (min (1- (length intervals)) (- (length array) point 1)))
                       (pattern (read-words point (aref intervals ivindex))))
                  ;; (print (list :dd deltas))
                  
                  (loop :for ix :below ivindex :for delta :in deltas
                        :do (setf pattern (ash pattern (- delta)))
                            (let ((match? (interpret-element assembler pattern
                                                             (funcall reader (aref intervals ix)))))
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
    (loop :until match :for unmasker :being :the :hash-values :of (asm-msk-battery assembler)
          :do (let ((attempt (funcall unmasker ipattern reader)))
                (when attempt (setf match attempt))))
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
       (funcall (lambda ,(cons output-sym input-bindings) ,@body)
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
;;     (asm-domains)
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

;; (defmacro bitmanifest (string &rest clauses)
;;   (let ((base (gensym)) (bits (gensym)) (mapper (gensym)) (input (gensym)))
;;     `(multiple-value-bind (,base ,bits ,mapper) (bitmapper ,string)
;;        ;; ,@(loop for c :in clauses :collect `(format t "~v,'0B~%" 24 (funcall ,mapper ,@c)))
;;        (funcall (if (>= 8 ,bits) #'identity (lambda (,input) (vectorize (ceiling ,bits 8) ,input)))
;;                 (reduce #'+ (list ,base ,@(loop for c :in clauses :collect `(funcall ,mapper ,@(rest c)))))))))

;; (defmacro defop (symbol operands lexicon &rest specs)
;;   (let* ((provisions (rest (assoc :provisions specs)))
;;          (ins-part (rest (assoc :instructions specs)))
;;          (ins-meta (rest (assoc :with ins-part)))
;;          (opcon-process (symbol-function (rest (assoc :opcons ins-meta))))
;;          (ins-main))
    
;;     (let ((ins-list ins-part))
;;       (loop :while (and ins-list (not ins-main))
;;             :do (if (keywordp (caar ins-list))
;;                     (setf ins-list (rest ins-list))
;;                     (setf ins-main ins-list))))
    
;;     `(setf (gethash ,(intern (string symbol) "KEYWORD") ,lexicon)
;;            (lambda ,operands
;;              (symbol-macrolet ,provisions
;;                (cond ,@(loop :for in :in ins-main
;;                              :collect (destructuring-bind (manifest conditions) in
;;                                         (list (cons 'and (funcall opcon-process operands conditions
;;                                                                   (rest (assoc :priority ins-meta))))
;;                                               (cons 'join manifest))))
;;                      (t "Invalid operation.")))))))

;; (defmacro specify-assembler (name macro-symbol &body params)
;;   (let* ((subpar (gensym)) (ops (gensym)) (template (gensym)) (s (gensym)) (associated)
;;          (joined (loop :for p :in params
;;                        :collect (progn (push (first p) associated)
;;                                        `(append ',p (rest (assoc ,(first p) ,template)))))))
;;     ;; (print (list :a associated))
;;     `(progn (proclaim '(special ,name))
;;             (setf (macro-function ',name)
;;                   (lambda (,subpar &rest ,ops)
;;                     (let ((,template (rest ,subpar)))
;;                       (append (list ',macro-symbol
;;                                     (cons :with (append (list ,@joined)
;;                                                         (loop :for ,s :in ,template
;;                                                               :when (not (member (first ,s) ',associated
;;                                                                                  :test #'eq))
;;                                                                 :collect ,s))))
;;                               ,ops)))))))

;; (let ((series-names) (derived-domains))
;;   (dotimes (n (/ (length (getf *x86-storage* :gpr)) 8))
;;     (push (reg-series (nth (1+ (* 8 n)) (getf *x86-storage* :gpr))) series-names))
;;   (setf series-names (reverse series-names))
  
;;   (defmethod of-storage ((assembler assembler-x86) &rest params)
;;     (unless derived-domains (derive-domains))
;;     (destructuring-bind (type &key series width index) params
;;       (let ((series-index (if (not series)
;;                               0 (1+ (* 8 (position series series-names)))))
;;             (type-list (getf *x86-storage* type))
;;             (width-index (case type (:gpr (position width '(8 16 32 64)))
;;                                (:vcr (position width '(128 256 512)))
;;                                (t 0)))
;;             (storage-index (if index (1+ (* 2 index))
;;                                (+ series-index width-index))))
;;         (if )
;;         (nth type-list (if index (1+ (* 2 index))
;;                            (+ series-index width-index))))))

;;   (defmethod locate ((assembler assembler-x86) params)
;;     (loop :for p :in params
;;           :collect (destructuring-bind (type &rest options) p
;;                      (cond type (:gpr ))))))

;; (defmethod compose ((assembler assembler-x86) params &rest expressions)
;;   (loop :for e :in expressions :collect 1 2 3))

;; (format t "~v,'0X~%" 40 (asm-op-add (getf *x86-registers* :al) (getf *x86-registers* :cl)))

#|

(defmacro define-op (symbol args &body body)
  `(defun ,(intern (format nil "ASM-OP-~a" (string symbol)))
       ,(loop :for arg :in args :collect (if (listp arg) (first arg) arg))
     ,@(loop :for arg :in args :when (listp arg)
             :collect (destructuring-bind (sym &rest types) arg
                        `(unless (or ,@(loop :for arg-clause :in (rest arg)
                                             :collect `(and (typep ,sym ',(case (first arg-clause)
                                                                            (:gpr 'x86-gpregister)
                                                                            (:vrg 'x86-vcregister)
                                                                            (:mem 'x86-mem-access)
                                                                            (:imm `(unsigned-byte
                                                                                    ,(reduce
                                                                                      #'max
                                                                                      (rest arg-clause))))))
                                                            ,@(if (member (first arg-clause)
                                                                          '(:mem :imm) :test #'eq)
                                                                  nil `((member (,(case (first arg-clause)
                                                                                    (:gpr 'reg-width)
                                                                                    (:vrg 'reg-width))
                                                                                 ,sym)
                                                                                '(,@(rest arg-clause))
                                                                                :test #'=))))))
                           (error "Invalid operand."))))
     ,@body))
  
(define-op mov
    ((op0 (:gpr 8 16 32 64) (:mem 8 16 32 64) (:imm 8 16 32 64))
     (op1 (:gpr 8 16 32 64) (:mem 8 16 32 64) (:imm 8 16 32 64)))
  (let ((minimm (if (not (numberp op1))
                    nil (loop :for i :in '((unsigned-byte 8)  (unsigned-byte 16)
                                           (unsigned-byte 32) (unsigned-byte 64))
                              :when (typep op1 i) :return (second i))))
        (rex-required (or (and (typep op0 'x86-register)
                               (or (= 64 (reg-width op0))
                                   (<  7 (reg-index op0))))
                          (and (typep op1 'x86-register)
                               (or (= 64 (reg-width op1))
                                   (<  7 (reg-index op1)))))))
    (print (list :mn minimm))
    (join (if (not rex-required) nil (determine-pfrex t op0 op1)
              ;; (prefix-rex t (and (typep op0 'register) (< 7 (reg-index op0)))
              ;;             nil (and (typep op1 'register) (< 7 (reg-index op1))))
              )
          (if (numberp op1)
              (if (and (typep op0 'x86-register)
                       (> 8 (reg-index op0)))
                  (join (+ #xB0 ;; opcode
                           (if (= 8 (reg-width op0)) 0 8)
                           (logand #b111 (reg-index op0)))
                        op1)
                  (if (typep op0 'x86-mem-access)
                      (join (+ #xC6 ;; opcode
                               (if (= 8 (reg-width op0)) 0 1))
                            (field-modrm :mod 0 :reg 0 :rm (if (typep op0 'x86-register)
                                                               (reg-index op0)
                                                               0))
                            op1)))
              (if (typep op1 'x86-mem-access)
                  (join (+ #x88 (if (= 8 (reg-width op0)) 0 1))
                        (determine-modrm op1 op0)
                        (determine-sib op0))
                  (join (+ #x8A (if (= 8 (reg-width op1)) 0 1))
                        (determine-modrm op0 op1)
                        (determine-sib op1)))
              ))))

(define-op add
    ((op1 (:gpr 8 16 32 64) (:mem 8 16 32 64) (:imm 8 16 32))
     (op2 (:gpr 8 16 32 64) (:mem 8 16 32 64) (:imm 8 16 32)))
  (let ((minimm (if (not (numberp op2))
                    nil (loop :for i :in '((unsigned-byte 8)  (unsigned-byte 16)
                                           (unsigned-byte 32) (unsigned-byte 64))
                              :when (typep op2 i) :return (second i)))))
    (print (list :mn minimm))
    (join (if (not (or (and (typep op1 'register)
                            (or (= 64 (reg-width op1))
                                (< 7 (reg-index op1))))
                       (and (typep op2 'register)
                            (= 64 (reg-width op2))
                            (< 7 (reg-index op2)))))
              nil (prefix-rex t (and (typep op1 'register) (< 7 (reg-index op1)))
                              nil (and (typep op2 'register) (< 7 (reg-index op2)))))
          (if (numberp op2)
              (join (if (eq :a (reg-series op1)) ;; 4 is opcode for A-series registers
                        (+ #x04 (if (= 8 (reg-width op1)) 0 1))
                        ;; #x80-series opcodes are for adding an immediate value to a register
                        (join (+ #x80 (if (/= 8 minimm)
                                          1 (if (= minimm (reg-width op1)) 0 3)))
                              (determine-modrm nil op1)
                              (determine-sib op1)))
                    op2)
              (if (typep op2 'x86-mem-access)
                  (join (+ #x02 (if (= 8 (reg-width op1)) 0 1))
                        (determine-modrm op1 op2))
                  (join (+ #x00 (if (= 8 (reg-width op2)) 0 1))
                        (determine-modrm op2 op1)))
              ))))
  
(define-op sub
    ((op1 (:gpr 8 16 32 64) (:mem 8 16 32 64) (:imm 8 16 32))
     (op2 (:gpr 8 16 32 64) (:mem 8 16 32 64) (:imm 8 16 32)))
  (let ((minimm (if (not (numberp op2))
                    nil (loop :for i :in '((unsigned-byte 8)  (unsigned-byte 16)
                                           (unsigned-byte 32) (unsigned-byte 64))
                              :when (typep op2 (list 'unsigned-byte i)) :return (second i)))))
    (join (if (not (or (and (typep op1 'register)
                            (or (= 64 (reg-width op1))
                                (< 7 (reg-index op1))))
                       (and (typep op2 'register)
                            (= 64 (reg-width op2))
                            (< 7 (reg-index op2)))))
              nil (prefix-rex :wide t :rex (and (typep op1 'register) < 7 (reg-index op1))
                              :iex nil
                              :rbex (and (typep op2 'register) < 7 (reg-index op2))))
          (if (numberp op2)
              (join (if (eq :a (reg-series op1)) ;; 4 is opcode for A-series registers
                        (+ #x2C (if (= 8 (reg-width op1)) 0 1))
                        ;; #x80-series opcodes are for adding an immediate value to a register
                        (join (+ #x80 (if (/= 8 minimm)
                                          1 (if (= minimm (reg-width op1))
                                                0 3)))
                              (determine-modrm nil op1)
                              (determine-sib op1)))
                    op2)
              (if (typep op2 'x86-mem-access)
                  (join (+ #x2A (if (= 8 (reg-width op1) 0 1)))
                        (determine-modrm op1 op2))
                  (join (+ #x28 (if (= 8 (reg-width op2) 0 1)))
                        (determine-modrm op2 op1)))))))

(define-op pshufb
    ((op1 (:vrg 64 128 256))
     (op2 (:vrg 64 128 256) (:mem 64 128 256))
     &optional
     (order (:vrg 64 128 256) (:mem 64 128 256)))
  (let ((mac (if (typep op2 'x86-mem-access)
                 op2 (if (typep order 'x86-mem-access)
                         order nil))))
    (print (list :mm mac op1 op2))
    (join (if (typep op2 'x86-mem-access)
              (if (or order (not (= 128 (reg-width op1))))
                  (error "Invalid.")
                  (field #x66 #x0F #x38))
              (determine-pfvex op1 order op2 :long t :prefix #x66 :map #x0F38))

          #x00 ;; opcode
          (determine-modrm op1 op2)
          (determine-sib mac)
          )))

;; (format t "~v,'0B~%" 40 (asm-op-pshufb (getf *x86-registers* :ymm0) (getf *x86-registers* :ymm1) (getf *x86-registers* :ymm2)))

(define-op pshufd
    ((op1 (:vrg 64 128 256))
     (op2 (:vrg 64 128 256) (:mem))
     (order (:imm 8)))
  (let ((mac (if (typep op2 'x86-mem-access) op2 nil)))
    (print (list :mm mac op1 op2))
    (join (if (typep op2 'x86-mem-access)
              (if (= 128 (reg-width op1))
                  (field #x66 #x0F)
                  (error "Invalid."))
              (determine-pfvex op1 order op2 :long t :prefix #x66 :map #x0F))
          #x70 ;; opcode
          (determine-modrm op1 op2)
          (determine-sib mac)
          order
          )))

(define-op vpblendd
    ((op1 (:vrg 64 128 256))
     (op2 (:vrg 64 128 256) (:mem))
     (op3 (:vrg 64 128 256) (:mem))
     (order (:imm 8)))
  (let ((mac (if (typep op3 'x86-mem-access) op3 nil)))
    (print (list :mm mac op1 op2))
    (join (determine-pfvex op1 op3 op2 :long t :prefix #x66 :map #x0F3A :wide 0)
          #x02 ;; opcode
          (determine-modrm op1 op3)
          (determine-sib mac)
          order
          )))

|#

;; (join (if (typep op2 'x86-mem-access)
;;           (if (or order (not (= 128 (reg-width op1))))
;;               (error "Invalid.")
;;               (field #x66 #x0F #x70))
;;           (prefix-vex (t :prefix #x66 :map #x0F :length (reg-width op1)
;;                          :third-op (reg-index order)
;;                          :rex :iex :rbex)))
;;       (prefix-vex long-prefix &key rex iex rbex map wide (length 128) third-op prefix)
;;       #x70 ;; opcode
;;       (field-modrm :mode #b11 :reg1 (reg-index op1) :reg2 (reg-index op2))
;;       (let ((max (if (typep op2 'x86-mem-access)
;;                      op2 (if (typep order 'x86-mem-access)
;;                              order nil))))
;;         (if (not max) nil (field-sib :index (mac-addr max) :base (x86mac-offset max)
;;                                      :scale (x86mac-scale max))))))

;; (define-op pshufd
;;     ((op1 (:vrg 64 128 256) (:mem 64 128 256))
;;      (op2 (:vrg 64 128 256) (:mem 64 128 256))
;;      (order (:imm 8)))
;;   (join (sum (field 0 0 0 0)
;;              (prefix-vex :type 3 :length (if (= 256 (reg-width op1)) 256 0))
;;              (operand-modrm))))

