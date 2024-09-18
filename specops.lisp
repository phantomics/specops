;;;; specops.lisp

(in-package #:specops)

(defun find-width (number unit)
  (let ((width 0) (shift (- unit)))
    (loop :until (zerop number) :do (setf number (ash number shift))
                                    (incf width))
    width))

(defun serialize (item unit collector)
  (let ((mask (1- (ash 1 unit)))) ;; TODO: find a way to create the mask just once?
    (flet ((decompose (number starting-width)
             (let ((shift (- (* unit starting-width))))
               (loop :for i :below starting-width
                     :do (funcall collector (logand mask (ash number shift)))
                         (incf shift unit)))))
      (if (integerp item) ;; values that fit within a byte are just pushed on
          (if (zerop (ash item (- unit)))
              (funcall collector item)
              (decompose item (1- (find-width item unit))))
          (if (and (consp item) (not (listp (rest item))))
              ;; handle cons cells encoding width and value, like (3 . 5) → #x000005
              (destructuring-bind (width &rest value) item
                (decompose value (1- width)))
              (if (vectorp item)
                  (loop :for i :across item :do (funcall collector i))
                  (error "Attempted to serialize incompatible value - must be an integer, a vector")))))))

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
    (values (loop :for pair :in (rest (assoc :static params)) ;; values
                  :do (destructuring-bind (sym value) pair
                        (let* ((insym (intern (string-upcase sym) "KEYWORD"))
                               (index (loop :for s :in symbols :for ix :from 0
                                            :when (eq s insym) :return ix)))
                          ;; (when reverse-match (push insym static-segments))
                          (if index (incf base (ash value (nth index segments)))
                              (error "Invalid key for static base value increment."))))
                  :finally (return base))
            (cons 0 (loop :for s :in segments :collect (abs (- s bits))))
            symbols bits)))

(defmacro masque (string &rest assignments)
  (let* ((base-sym (gensym))
         (params (if (listp (caar assignments)) (first assignments) nil))
         (assignments (if (not params) assignments (rest assignments))))
    (multiple-value-bind (base segments symbols) (quantify-mask-string string params)
      ;; (print (list :ss segments symbols clauses params))

      (let ((steps (loop :for assignment :in assignments
                         :collect (destructuring-bind (key value) assignment
                                    (let ((index (position (intern (string-upcase key) "KEYWORD")
                                                           symbols)))
                                      (when (not index)
                                        (error "Symbol ~a not found in mask ~a." key string))
                                      (let ((length (- (nth (1+ index) segments) (nth index segments))))
                                        ;; (format t "~v,'0B~%" 16 (ash (1- (ash 1 length)) (nth index segments)))
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

;; (loop :for c :across string :for ix :from 0 :when (not (char= c #\.)) ;; period is used as a spacer
;;       :do (if (position c "01" :test #'char=)
;;               (progn (when (and symbols (not (null (first symbols))))
;;                        (push nil symbols)
;;                        (push bits segments))
;;                      (when (char= c #\1) (incf base))) ;; set constant 1 bits
;;               (when (or (not symbols) (not (eq (intern (string-upcase c) "KEYWORD") (first symbols))))
;;                 (push (intern (string-upcase c) "KEYWORD") symbols)
;;                 (print (list :bit bits c))
;;                 (push bits segments)))
;;           ;; shift the number to the left unless this is the last digit
;;           (unless (= ix (1- (length string))) (setf base (ash base 1)))
;;           (incf bits))

;; (when (assoc :static params)
;;   (loop :for item :in (rest (assoc :static params))
;;         :do (destructuring-bind (sym number) item
;;               (let* ((insym (intern (string-upcase sym) "KEYWORD"))
;;                      (index (loop :for s :in symbols :for ix :from 0
;;                                   :when (eq s insym) :return ix)))
;;                 ;; (when reverse-match (push insym static-segments))
;;                 (incf base (ash number (nth index segments)))))))

(defmacro mqbase (name opcsym mnesym args string &body body)
  ;; this is a currying macro for masque, allowing the creation
  ;; of a specialized masque with static contents for some fields
  (destructuring-bind (static-specs clauses &optional disassembler) body
    (let ((dsarg (gensym))
          (static-form (loop :for ss :in static-specs
                             :collect (if (not (eq :static (first ss)))
                                          (list 'quote ss)
                                          `(list :static ,@(loop :for spec :in (rest ss)
                                                                 :collect `(list ',(first spec)
                                                                                 ,(second spec))))))))
      `(defmacro ,name (,opcsym ,mnesym)
         (list (list 'lambda ',args
                     (append (list 'masque ,string)
                             ;; generate a template incorporating the :static values
                             ;; into a (masque) expansion within the defined macro
                             (list (list ,@static-form))
                             ',clauses))
               ,@(if (not disassembler)
                     nil `((list 'lambda (list ',dsarg)
                                 (list 'unmasque ,string ',dsarg (list ,@static-form)
                                       ',(mapcar #'first clauses) ;; ',disassembler
                                       (list ,@(loop :for item :in disassembler
                                                     :collect (if (and (symbolp item)
                                                                       (eql item mnesym))
                                                                  `(intern (string ,item) "KEYWORD")
                                                                  `(quote ,item)))))))))))))

(defclass register ()
  ((%name  :accessor reg-name
           :initform nil
           :initarg :name
           :documentation "The register's name.")
   (%index :accessor reg-index
           :initform nil
           :initarg :index
           :documentation "The register's index.")
   (%width :accessor reg-width
           :initform nil
           :initarg :width
           :documentation "The register's width."))
  (:documentation "Generic class for registers."))

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
             :initform   8
             :initarg    :breadth)
   (%joiner  :accessor   asm-joiner
             :allocation :class
             :initform   #'joinb
             :initarg    :joiner))
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

(defgeneric %derive-domains (assembler &rest params)
  (:documentation "Determine storage domains for a given assembler."))

(defgeneric of-lexicon (assembler key &optional value)
  (:documentation "Fetch lexical get/setter for given assembler."))

(defgeneric of-storage (assembler key)
  (:documentation "Fetch storage object for given assembler."))

(defgeneric storage-type-p (assembler type &rest keys)
  (:documentation "Confirm membership of symbol(s) in a given storage type of an assembler."))

(defgeneric specify-ops (assembler asm-symbol op-symbol operands params)
  (:documentation "Specify operations for members of a given assembler's class."))

(defgeneric qualify-ops (assembler operands form order)
  (:documentation "Qualify operations for members of a given assembler's class."))

(defgeneric clause-processor (assembler assembler-symbol)
  (:documentation "Process opcode specification clauses for an assembler."))

(defgeneric locate (assembler assembler-sym params)
  (:documentation "Locate available storage for use by a program."))

(defgeneric reserve (assembler &rest params)
  (:documentation "Reserve a storage location for use by a program."))

(defgeneric compose (assembler params expression)
  (:documentation "A function composing an instruction for an assembler, translating the symbols in an instruction into specific values and class instances."))

(defgeneric interpret (assembler params array)
  (:documentation "A function converting operation codes for a given ISA into a human-readable assembly language."))

(defgeneric interpret-element (assembler ipattern reader)
  (:documentation "A function to interpret an individual opcode or other element.")
  (:method-combination or))

(defgeneric of-decoder (assembler-encoding key value)
  (:documentation "Encoding getter/setter for an encoding assembler."))

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

(defmethod of-decoder ((assembler assembler-encoding) key value)
  (if value (setf (gethash key (asm-enc-decoder assembler)) value)
      (gethash key (asm-enc-decoder assembler))))

(defmethod of-battery ((assembler assembler-masking) key value)
  (if value (setf (gethash key (asm-msk-battery assembler)) value)
      (gethash key (asm-msk-battery assembler))))

(defmethod types-of ((item t))
  (declare (ignore item))
  nil)

(defmethod types-of ((assembler assembler))
  (append (call-next-method)
          (asm-type assembler)))

(defmethod %derive-domains ((assembler assembler) &rest params)
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

(defmacro derive-domains (assembler &rest params)
  `(%derive-domains ,assembler ,@(loop :for p :in params :collect `(quote ,p))))

(defmacro specops (symbol operands assembler &body params)
  (specify-ops (symbol-value assembler)
               assembler symbol operands params))

(defmethod clause-processor ((assembler assembler) assembler-symbol)
  (declare (ignore assembler))
  (lambda (mnemonic operands body params)
    (flet ((add-alias (form)
             (let ((items (gensym)))
               (if (not (assoc :type-matcher params))
                   form `(labels ((,(second (assoc :type-matcher params)) (&rest ,items)
                                    (intersection ,items (types-of ,assembler-symbol))))
                           ,form)))))
      ;; (print (list :mn mnemonic body))
      `(of-lexicon ,assembler-symbol ,(intern (string mnemonic) "KEYWORD")
                   ,(add-alias `(lambda ,operands ,@body))))))

(defun process-clause-matrix (assembler asm-sym operands matrix params)
  (declare (ignore params))
  (let ((clauses) ;; (varops (remove '&optional operands))
        (clause-processor (clause-processor assembler asm-sym)))
    (symbol-macrolet ((opcode (+ (first cellx) (caar row))))
      (loop :for row :in (rest matrix) :for row-index :from 0
            :do (loop :for cell :in (rest row) :for col-index :from 0
                      :for cellx :in (cdar matrix)
                      :do (destructuring-bind (&optional ins &rest opr) cell
                            (when ins (if (assoc ins clauses)
                                          (when (not (atom (second (assoc ins clauses))))
                                            ;; don't push an item if it's another possible
                                            ;; opcode for an instruction without operands,
                                            ;; as it's redundant and it interferes with the
                                            ;; clause organization
                                            (push (list opr opcode) (rest (assoc ins clauses))))
                                          (push (if opr (list ins (list opr opcode))
                                                    (list ins opcode))
                                                clauses))))))
      ;; (print (list :clau clauses))
      (loop :for c :in clauses :append (funcall clause-processor c operands)))))

(defmethod specify-ops ((assembler assembler) asm-sym op-symbol operands items)
  "A simple scheme for implementing operations - the (specop) content is directly placed within functions in the lexicon hash table."
  (let* ((params (if (not (and (listp (first items)) (listp (caar items))
                               (keywordp (caaar items))))
                     nil (first items)))
         (operations (if (not params) items (rest items)))
         ;; (provisions (rest (assoc :provisions params)))
         )
    ;; (print (list :par params))
    (cond ((assoc :combine params)
           (destructuring-bind (co-symbol join-by indexer &rest combinators)
               (rest (assoc :combine params))
             (cons 'progn (loop :for co :in combinators :for i :from 0
                                :collect (let ((comp-str (case join-by
                                                           (:appending (format nil "~a~a" op-symbol co))))
                                               (index (funcall (case indexer (:by-index #'identity))
                                                               i)))
                                          `(setf (gethash ,(intern comp-str "KEYWORD")
                                                          (asm-lexicon ,asm-sym))
                                                  (lambda ,operands
                                                    (let ((,co-symbol ,index))
                                                      ,@operations))))))))
          ((assoc :tabular params)
           (destructuring-bind (mode &rest properties) (rest (assoc :tabular params))
             ;; (print (list :mo mode))
             (case mode (:cross-adding
                         (cons 'progn (process-clause-matrix assembler asm-sym ;; `(asm-lexicon ,asm-sym)
                                                             operands operations properties))))))
          ((and (not operands) (= 1 (length operations)))
           ;; `(setf (gethash ,(intern (string op-symbol) "KEYWORD")
           ;;                 (asm-lexicon ,asm-sym))
           ;;        ,(first operations))
           `(of-lexicon ,asm-sym ,(intern (string op-symbol) "KEYWORD")
                        ,(first operations)))
          (t (funcall (clause-processor assembler asm-sym)
                      op-symbol operands operations params)))))

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

(defparameter *default-segment* 1000)

(defmethod compose ((assembler assembler) params expression)
  (let* ((unit (asm-breadth assembler))
         (codes (make-array *default-segment* :element-type (list 'unsigned-byte unit)
                                             :initial-element 0 :adjustable t :fill-pointer 0))
         (marked-points) (tag-points) (codes-length))
    (flet ((add-value (value)
             (unless (< (fill-pointer codes) (1- (length codes)))
               (adjust-array codes (+ *default-segment* (length codes)) :initial-element 0))
             (vector-push value codes)))
      (loop :for item :in expression
            :do (typecase item
                  (symbol (push (cons item (fill-pointer codes)) marked-points))
                  (list   (destructuring-bind (instruction &rest operands) item
                            (let ((build-instruction (of-lexicon assembler instruction)))
                              (multiple-value-bind (code properties)
                                  (if (not (functionp build-instruction))
                                      build-instruction (apply build-instruction operands))
                                ;; (print (list :co code))
                                (if (functionp code)
                                    (let ((breadth (or (getf properties :breadth) 0)))
                                      (push (append (list (fill-pointer codes) code)
                                                    properties)
                                            tag-points)
                                      (dotimes (i breadth) (add-value 0)))
                                    (serialize code unit #'add-value))))))))
      (setf codes-length (fill-pointer codes))
      ;; (print (list :mm marked-points))
      (loop :for tag-spec :in tag-points
            :do (destructuring-bind (point function &rest properties) tag-spec
                  (let ((value (apply function
                                      (cons point (loop :for b :in (getf properties :bindings)
                                                        :collect (rest (assoc b marked-points)))))))
                    (setf (fill-pointer codes) point)
                    (serialize value unit #'add-value))))
      (let ((output (make-array codes-length :element-type (list 'unsigned-byte unit))))
        (loop :for i :below codes-length :do (setf (aref output i) (aref codes i)))
        output))))

;; (defmethod compose ((assembler assembler) params expression)
;;   (print (list :par params))
;;   (destructuring-bind (instruction &rest operands) expression
;;     (let ((build-instruction (gethash instruction (asm-lexicon assembler))))
;;       (if (not (functionp build-instruction))
;;           build-instruction (apply build-instruction operands)))))

(defmethod %assemble ((assembler assembler) assembler-sym params expressions)
  (let ((compose-params))
    `(let ,(locate assembler assembler-sym (rest (assoc :store (rest params))))
       (compose ,assembler-sym ,compose-params
                (list ,@(loop :for e :in expressions
                              :collect (if (not (and (listp e) (keywordp (first e))))
                                           e (cons 'list e))))))))

(defmacro assemble (assembler params &rest expressions)
  (%assemble (symbol-value assembler) assembler params expressions))

(defmethod interpret ((assembler assembler) params array)
  (let* ((etype (let ((element-type (array-element-type array)))
                  (unless (and (listp element-type)
                               (eq 'unsigned-byte (first element-type)))
                    (error "Invalid array."))
                  (second element-type)))
         (to-read (/ etype (asm-breadth assembler)))
         (intervals (loop :for segment :in (asm-msk-segment assembler)
                          :collect (ash etype (- (+ 2 segment)))))
         (disassembled) (point 0))
    (labels ((read-words (from count)
               (let ((value 0))
                 (loop :for c :below (* count to-read)
                       :do (setf value (ash  value etype))
                           (incf value (aref array (+ c from))))
                 value)))
      (loop :while (< point (1- (length array)))
            :do (let* ((match) (this-interval) (ipatterns) (total 0) (sub-count 0)
                       (reader (lambda (in)
                                 (lambda (count)
                                   (let ((this-count sub-count))
                                     (incf sub-count count)
                                     (read-words (+ point this-count in) count))))))
                  (loop :for in :in intervals :do (push (read-words point in) ipatterns))
                  (loop :for in :in intervals :for ip :in ipatterns :until match
                        :do (setf match (interpret-element assembler ip (funcall reader in)))
                            (when match (setf this-interval in)))
                  ;; (print (list match point (length array)))
                  (if match (progn (push match disassembled)
                                   (setf point (+ point sub-count this-interval)))
                      (error "Undecipherable instruction!"))))
      (reverse disassembled))))

(defmethod interpret-element or ((assembler assembler-encoding) ipattern reader)
  (let ((match (gethash ipattern (asm-enc-decoder assembler))))
    (if (not (functionp match)) match (identity (funcall match reader)))))

(defmethod interpret-element or ((assembler assembler-masking) ipattern reader)
  (let ((match))
    (loop  :until match :for unmasker :being :the :hash-values :of (asm-msk-battery assembler)
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

