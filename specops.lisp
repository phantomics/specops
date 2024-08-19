;;;; specops.lisp

(in-package #:specops)

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
           :documentation "The register's width.")))

(defclass mem-access ()
  ((%width :accessor mac-width
           :initform nil
           :initarg  :width)
   (%bsreg :accessor mac-bsreg
           :initform nil
           :initarg  :bsreg)))

(defun join (&rest items)
  (let ((collected))
    (loop :for item :in items :when item ;; ignore null items
          :do (if (numberp item) ;; values that fit within a byte are just pushed on
                  (if (zerop (ash item -8))
                      (push item collected)
                      (let ((sub-collected))
                        (loop :until (zerop item)
                              :do (push (logand item #xFF) sub-collected)
                                  (setf item (ash item -8)))
                        (setf collected (append (reverse sub-collected) collected))))
                  (if (and (consp item) (not (listp (rest item))))
                      ;; handle cons cells encoding width and value, like (3 . 5) → #x000005
                      (destructuring-bind (width &rest value) item
                        (let ((sub-collected))
                          (loop :for i :below width :do (push (logand value #xFF) sub-collected)
                                                        (setf value (ash value -8)))
                          (setf collected (append (reverse sub-collected) collected))))
                      (when (vectorp item)
                        (loop :for i :across item :do (push i collected))))))
    (make-array (length collected) :element-type '(unsigned-byte 8) :initial-contents (reverse collected))))

(defun flipbits (b &optional (n 32))
  (let ((r 0))
    (dotimes (x n r)
      (setq r (logior (ash r 1) (logand b 1))
            b (ash b -1)))))

(defmacro masque (string &rest clauses)
  (let ((segments) (symbols) (base 0) (bits 0) (base-sym (gensym)))
    (loop :for c :across string :for ix :from 0 :when (not (char= c #\.)) ;; period is used as a spacer
          :do (if (position c "01" :test #'char=)
                  (when (char= c #\1) (incf base)) ;; set constant 1 bits
                  (when (or (not symbols) (not (eq (intern (string-upcase c) "KEYWORD") (first symbols))))
                    (push (intern (string-upcase c) "KEYWORD") symbols)
                    (push bits segments)))
              ;; shift the number to the left unless this is the last digit
              (unless (= ix (1- (length string))) (setf base (ash base 1)))
              (incf bits))
    (setf segments (cons 0 (loop :for s :in segments :collect (abs (- s bits)))))
    ;; (print (list :ss segments symbols))
    `(let ((,base-sym ,base))
       ,@(loop :for clause :in clauses
               :collect (let* ((insym (intern (string-upcase (first clause)) "KEYWORD"))
                               (index (loop :for s :in symbols :for ix :from 0
                                            :when (eq s insym) :return ix))
                               (length (- (nth (1+ index) segments) (nth index segments))))
                          `(incf ,base-sym (ash (logand ,(second clause) ,(1- (ash 1 length)))
                                                ,(nth index segments)))))
       ,base-sym)))

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
             :allocation :class
             :initform   nil
             :initarg    :domains)
   (%reserve :accessor   asm-reserve
             :initform   nil
             :initarg    :reserve)))

(defgeneric types-of (assembler)
  (:documentation "Fetch an assembler's types."))

(defgeneric %derive-domains (assembler &rest params)
  (:documentation "Determine storage domains for a given assembler."))

(defgeneric of-lexicon (assembler)
  (:documentation "Fetch lexical get/setter for given assembler."))

(defgeneric of-storage (assembler &rest params)
  (:documentation "Fetch storage object for given assembler."))

(defgeneric specify-ops (assembler asm-symbol op-symbol operands params)
  (:documentation "Specify operations for members of a given assembler's class."))

(defgeneric qualify-ops (assembler operands form order)
  (:documentation "Qualify operations for members of a given assembler's class."))

(defgeneric locate (assembler &rest params)
  (:documentation "Refer to an available storage location for an assembler."))

(defgeneric compose (assembler params expression)
  (:documentation "Compose an instruction for an assembler."))

(defgeneric %assemble (assembler params expressions)
  (:documentation "Fetch storage object for given assembler."))

(defmethod of-lexicon ((assembler assembler))
  (lambda (key &optional value)
    (if value (setf (gethash key (asm-lexicon assembler)) value)
        (gethash key (asm-lexicon assembler)))))

(defmethod types-of ((item t))
  (declare (ignore item))
  nil)

(defmethod types-of ((assembler assembler))
  (append (call-next-method)
          (asm-type assembler)))

(defmethod %derive-domains ((assembler assembler) &rest params)
  (declare (ignore assembler params))
  nil)

(defmacro derive-domains (assembler &rest params)
  `(%derive-domains ,assembler ,@(loop :for p :in params :collect `(quote ,p))))

(defmacro specops (symbol operands assembler &body params)
  (specify-ops (symbol-value assembler)
               assembler (intern (string symbol) "KEYWORD") operands params))

(defmacro assemble (assembler &body rest)
  `(%assemble ,assembler ,@rest))

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
;; (map-bits "11000101.WvvvvLpp")

;; (map-bits "11000100.RXBmmmmm.WvvvvLpp"
;;           (r (if r-extend 0 1)) (x (if x-extend 0 1)) (b (if b-extend 0 1))
;;           (m m-value) (w (if width 1 0))
;;           (v (flipbits third-op 4)) (l (if (= 256 length) 1 0)))

;; (bitmanifest "11000100.RXBmmmmm.WvvvvLpp"
;;              (:R 1) (:X 0) (:B 1)
;;              (:|m| 3) (:W 1)
;;              (:|v| (flipbits 12)) (:L 1))

;; (defun prefix-rex (&optional is-wide mregx sindx mrmx-sbasx)
;;   (+ #b01000000 ;; initial nibble is a fixed 0100 pattern
;;      ;; fifth bit indicates use of 64-bit register(s)
;;      (if is-wide    #b00001000 0)
;;      ;; sixth bit indicates extension of the MODRM.reg field
;;      (if mregx      #b00000100 0)
;;      ;; seventh bit indicates extension of the SIB.index field
;;      (if sindx      #b00000010 0)
;;      ;; eighth bit indicates extension of the MODRM.rm field or SIB.base field
;;      (if mrmx-sbasx #b00000001 0)))

;; (defun field-modrm (&key mode reg1 reg2 address opex)
;;   (masque "MMRRROOO"
;;           (m (case mode (:reg #b11) (0 #b00) (1 #b01) (2 #b10)))
;;           (r (or opex (if (numberp reg1) ;; R field may contain an opcode extension
;;                           reg1 (floor (reg-index reg1) 8))))
;;           (o (if reg2 (if (numberp reg2)
;;                           reg2 (floor (reg-index reg2) 8))
;;                  address))))

;; (defun field-modrm (&key mod reg rm)
;;   (masque "MMRRROOO"
;;           (m mod)
;;           (r reg
;;           (o rm))))

;; (defun field-sib (&key scale index base)
;;   (masque "SSIIIBBB"
;;           (s (case scale  ;; bits 0-1 determine scaling factor in bytes ranging from 2^0-3
;;                (1 #b00) (2 #b01) (4 #b10) (8 #b11)))
;;           (i (if (numberp scale) ;; bits 2-4 determine index register (extendable via REX)
;;                  scale (floor (reg-index index) 8)))
;;           (b (if (numberp base) ;; bits 5-7 determine displacement register (extendable via REX)
;;                  base (floor (reg-index base) 8)))))

;; (defun operand-modrm (mode-or-disp &key register extension operand)
;;   (+ (case mode-or-disp (0 0) (1 1) (4 2) (:reg 3) (t (error "Invalid displacement value.")))
;;      (or extension (if register (mod (register-index register) 8)))
;;      (case mode-or-disp (:reg (register-index operand))
;;            (t 0))))

;; (if (not (and (typep op1 'x86-gpregister) (member (reg-width op1) '(8 16 32 64))))

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

