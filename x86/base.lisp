;;;; base.lisp

(in-package #:specops.x86)

;; (defclass x86-register (register)
;;   ((%series :accessor reg-series
;;             :initform nil
;;             :initarg  :series
;;             :documentation "The register's series.")))

;; (defclass x86-gpregister (x86-register)
;;   ())

;; (defclass x86-vcregister (x86-register)
;;   ())

(defclass mas-x86 (mas-based mas-indexed mas-scaling-displaced)
  ((%half-width :accessor mas-x86-half-width
                :initform nil
                :initarg  :half-width
                :documentation "The register's name."))
  (:documentation "Memory access scheme for x86 ISA featuring base, index and scaled displacement."))

(defvar *assembler-prototype-x86*)

(defvar *x86-layout*
  (list :gpr '(:a   :c   :d   :b   :sp  :bp  :di  :si  :r8  :r9  :r10 :r11 :r12 :r13 :r14 :r15)
        :vcr '(:m0  :m1  :m2  :m3  :m4  :m5  :m6  :m7  :m8  :m9  :m10 :m11 :m12 :m13 :m14 :m15
               :m16 :m17 :m18 :m19 :m20 :m21 :m22 :m23 :m24 :m25 :m26 :m27 :m28 :m29 :m30 :m31)
        :gpr-hb '(:ah :ch :dh :bh)))

(defvar *storage-domains-x86*
  (list :gpr '(:a   :c   :d   :b   :sp  :bp  :di  :si)))

(defvar *storage-domains-x86-64*
  (list :gpr '(:a   :c   :d   :b   :sp  :bp  :di  :si  :r8  :r9  :r10 :r11 :r12 :r13 :r14 :r15)
        :gpr-hb '(:ah :ch :dh :bh)))

(defvar *storage-domains-x86-64-avx*
  (list :gpr '(:a   :c   :d   :b   :sp  :bp  :di  :si  :r8  :r9  :r10 :r11 :r12 :r13 :r14 :r15)
        :vcr '(:m0  :m1  :m2  :m3  :m4  :m5  :m6  :m7  :m8  :m9  :m10 :m11 :m12 :m13 :m14 :m15)
        :gpr-hb '(:ah :ch :dh :bh)))

(defvar *storage-domains-x86-64-avx-512*
  (list :gpr '(:a   :c   :d   :b   :sp  :bp  :di  :si  :r8  :r9  :r10 :r11 :r12 :r13 :r14 :r15)
        :vcr '(:m0  :m1  :m2  :m3  :m4  :m5  :m6  :m7  :m8  :m9  :m10 :m11 :m12 :m13 :m14 :m15
               :m16 :m17 :m18 :m19 :m20 :m21 :m22 :m23 :m24 :m25 :m26 :m27 :m28 :m29 :m30 :m31)
        :gpr-hb '(:ah :ch :dh :bh)))

(defun gprix (item)
  (or (position item (getf *x86-layout* :gpr))
      (position item (getf *x86-layout* :gpr-hb))))

(defun vcrix (item)
  (position item (getf *x86-layout* :vcr)))

;; (defun fprix (index)
;;   (position index (getf *x86-layout* :fpr)))

(defun regix (item)
  (or (gprix item) (vcrix item)))

(defun gpr-p (item)
  (and (keywordp item) (gprix item)))

(defun vcr-p (item)
  (and (keywordp item) (vcrix item)))

(defun gpr-hb-p (item)
  (and (keywordp item)
       (position item (getf *x86-layout* :gpr-hb))))

(deftype gpr () `(satisfies gpr-p))

(deftype vcr () `(satisfies vcr-p))

(deftype gpr-hb () `(satisfies gpr-hb-p))

;; (defun mas-simple-p  (item)
;;   (and (typep item 'mas-x86) (not (mas-displ item))))

;; (deftype mas-simple  () `(satisfies mas-simple-p))

(defun b+ (gpr-sym)
  "Express an _H upper-byte register - an 8-bit register coincident with the top half of a 16-bit register. The argument is a register spec that otherwise, given as an argument to an 8-bit operation, would express a _L lower-byte register."
  (if (keywordp item)
      (let ((gprix (position item (getf *x86-layout* :gpr))))
        (if (and gprix (> 4 gprix))
            (nth gprix (getf *x86-layout* :gpr-hb))))))

(defun @ (base &rest options)
  "Specify a memory access scheme with base and, optionally, index, displacement and scale."
  (let* ((index   (if (gpr-p (first options)) (first options) nil))
         (options (if index (rest options) options))
         (displ   (if (integerp (first options)) (first options) nil)))
    (make-instance 'mas-x86 :base base :index index :displ displ :scale (if displ (or (second options) 1)))))

(defun @/ (&rest args)
  "Specify a 32-bit memory address - only applicable in 64-bit mode and even then seldom used."
  (let ((mas (apply #'@ args)))
    (setf (mas-x86-half-width mas) t)
    mas))

;; (defun prefix-rex (&optional wide rex iex rbex)
;;   (masque "0100.WRXB"
;;           (w (if wide 1 0))   ;; bit 4 indicates use of 64-bit register(s)
;;           (r (if  rex 1 0))   ;; bit 5 indicates extension of the MODRM.reg field
;;           (x (if  iex 1 0))   ;; bit 6 indicates extension of the SIB.index field
;;           (b (if rbex 1 0)))) ;; bit 7 indicates extension of MODRM.rm or SIB.base field

;; (defun prefix-vex (long-prefix &key r x b map wide (length 128) third-op prefix)
;;   (if long-prefix
;;       (masque "11000100.RXBmmmmm.WvvvvLpp"
;;               (r (if (zerop r) 1 0)) ;; bit 1.0 indicates inverse extension of the MODRM.reg field
;;               (x (if (zerop x) 1 0)) ;; bit 1.1 indicates inverse extension of the SIB.index field
;;               (b (if (zerop b) 1 0)) ;; bit 1.2 indicates inverse extension of MODRM.rm or SIB.base field
;;               (m (if (> 4 map)  ;; map field contains operator extension index 1, 2 or 3
;;                      map (case map (#x0F #b01) (#x0F35 #b10) (#x0F38 #b11) (t 0))))
;;               (w (if wide 1 0)) ;; bit 2.0 indicates 64-bit width of integer operand
;;               (v (if third-op (flipbits third-op) 0)) ;; v field addresses third operand (reversed)
;;               (l (if (= length 256) 1 0)) ;; bit 2.5 denotes use of 256-bit registers
;;               ;; prefix field expresses opcode prefix if needed
;;               (p (case prefix (#x66 #b01) (#xF3 #b10) (#xF2 #b11) (t 0))))
;;       (masque "11000101.WvvvvLpp"
;;               (w (if wide 1 0)) ;; bit 2.0 indicates 64-bit width of integer operand
;;               (v (if third-op (flipbits third-op) 0)) ;; v field addresses third operand (reversed)
;;               (l (if (= length 256) 1 0)) ;; bit 2.5 denotes use of 256-bit registers
;;               ;; prefix field expresses opcode prefix if needed
;;               (p (case prefix (#x66 #b01) (#xF3 #b10) (#xF2 #b11) (t 0))))))

;; (defun prefix-evex (&key rex iex rbex map wide (length 128)
;;                       third-op prefix merge br-control op3-extend)
;;   (masque "01100010.RXBŔ00mm.Wvvvv1pp.ZLLḂṼaaa"
;;           (r (if  rex 0 1)) ;; bit 1.0 indicates inverse extension of the MODRM.reg field
;;           (x (if  iex 0 1)) ;; bit 1.1 indicates inverse extension of the SIB.index field
;;           (b (if rbex 0 1)) ;; bit 1.2 indicates inverse extension of MODRM.rm or SIB.base field
;;           (ŕ (if rbex 0 1)) ;; bit 1.3 indicates further inverse extension of MODRM.reg field
;;           (m (if (> 4 map)  ;; map field contains operator extension index 1, 2 or 3
;;                  map (case map (#x0F #b01) (#x0F35 #b10) (#x0F38 #b11) (t 0))))
;;           (w (if wide 1 0)) ;; bit 2.0 indicates 64-bit width of integer operand
;;           (v (if third-op (flipbits third-op) 0)) ;; v field addresses third operand (reversed)
;;           ;; prefix field expresses opcode prefix if needed
;;           (p (case prefix (#x66 #b01) (#xF3 #b10) (#xF2 #b11) (t 0)))
;;           (z (if merge 1 0)) ;; bit 3.0 toggles merge function
;;           (l (+ (if (= length 512) #b10 0) (if (= length 256) #b01 0)))
;;           (ḃ (if br-control 1 0))
;;           ;; bits 3.1-2 determine use of 256-bit or 512-bit registers
;;           (ṽ (if op3-extend 1 0))
;;           (a 0)))


(defun determine-modrm (op0 op1)
  ;; (print (list :oo op0 op1))
  (masque "MMRRROOO"
          (m (if (gpr-p op1)
                 #b11 (if (or (not (mas-displ op1)) (zerop (mas-displ op1)))
                          ;; no displacement and a BP register base forces use of a zero 8-bit disp.
                          (if (eq :bp (reg-series (mas-base op1)))
                              #b01 #b00)
                          (if (> 256 (mas-displ op1)) ;; choose 8 or 32-bit displacement
                              #b01 #b10))))
          (r (if (numberp op0)
                 op0 (if (not (gpr-p op0))
                         0 (logand #b111 (regix op0)))))
          (o (if (numberp op1)
                 op1 (if (gpr-p op1)
                         (logand #b111 (regix op1))
                         (if (not (typep op1 'mas-x86))
                             0 (if (not (mas-base op1))
                                   #b101 ;; determines 32-bit displacement-only mode
                                   (if (mas-index op1) ;; #b100 determines SIB addressing
                                       #b100 (logand #b111 (regix (mas-base op1)))))))))))

(defun determine-sib (mac)
  (if (or (not (typep mac 'mas-x86))
          (not (mas-index mac)))
      nil (masque "SSIIIBBB"
                  (s (mas-sdisp-scale mac))
                  (i (logand #b111 (regix (mas-index mac))))
                  (b (logand #b111 (regix (mas-base mac)))))))

(defun determine-pfsize (width op0 op1 mode)
  (let ((reg-size (case mode
                    (:r16 (eq width :d))
                    (:p16 (eq width :d))
                    (:p32 (eq width :w))
                    (:l64 (eq width :w))))
        (adr-size (case mode
                    (:l64 (or (and (typep op0 'mas-x86)
                                   (mas-x86-half-width op0))
                              (and (typep op1 'mas-x86)
                                   (mas-x86-half-width op1)))))))
    (if reg-size (if adr-size #x6766 #x66)
        (if adr-size #x67 nil)))

;; (defun determine-pfasize (width op0 op1 mode)
;;   (case mode
;;     (:r16 (if (eq width :d) #x67 nil))
;;     (:p32 (if (eq width :w) #x67 nil))
;;     (:l64 (if (eq width :w) #x67 nil))))

(defun determine-pfrex (width op0 op1)
  (let ((flags (+ (if (eq :q width) 8 0) ;; 8-bit indicates 64-bit width
                  (if (not (gpr-p op0))  ;; 4-bit is upper bit of extended GPR 0 index
                      0 (ash (ash (regix op0) -3) 2))
                  (if (not (and (typep op1 'mas-x86) ;; 2-bit is upper bit of extended GPR 1 index
                                (mas-base op1) (mas-index op1)))
                      0 (ash (ash (regix (mas-index op1)) -3) 1))
                  (if (gpr-p op0)        ;; 1-bit is upper bit of extended GPR 1/MAS 1 indec
                      (ash (regix op0) -3)
                      (if (not (and (typep op0 'mas-x86) (mas-base op1)))
                          0 (ash (regix (mas-base op1)) -3))))))
    (if (not (zerop flags))
        (masque "0100.FFFF" (f flags))
        (if (not (eq :b width))
            nil (let ((gprix0 (gprix op0))
                      (gprix1 (gprix op1)))
                  (if (or (and gprix0 (> 4 gprix0))
                          (and gprix1 (> 4 gprix1)))
                      (if (not (or (gpr-hb-p op0) (gpr-hb-p op1)))
                          #b01000000
                          (error "The 8-bit registers SPL, BPL, DIL and SIL may not be addressed in the same instruction as the upper-byte registers AL, CL, DL and BL."))))))))
   
(defun determine-pfvex (op0 op1 op2 &key long map prefix)
  (let ((sib-index (and (typep op1 'mas-x86)
                        (ash (mas-index op1) -3))))
    (if long
        (masque "11000100.RXBmmmmm.WvvvvLpp"
                (r (if (zerop (logand #b1000 (regix op0)))               1 0))
                ;; bit 1.0 indicates inverse extension of the MODRM.reg field: op0 index upper bit
                (x (if (or (not sib-index) (zerop sib-index))                1 0))
                ;; bit 1.1 indicates inverse extension of the SIB.index field: index register upper bit
                (b (if (or (not sib-index) (zerop (ash (mas-base op1) -3))) 1 0))
                ;; bit 1.2 indicates inverse extension of MODRM.rm or SIB.base field: base register upper bit
                (m (case map (#x0F #b01) (#x0F35 #b10) (#x0F38 #b11) (t map)))
                ;; map field contains operator extension index 1-3, may be mapped to codes or direct
                (w (if (= 64 (reg-width op0)) 1 0)) ;; bit 2.0 indicates 64-bit width of integer operand
                (v (if op2 (flipbits (regix op2)) 0)) ;; v field addresses third operand (reversed)
                (l (if (= 256 (reg-width op0)) 1 0)) ;; bit 2.5 denotes use of 256-bit registers
                ;; prefix field expresses opcode prefix if needed
                (p (case prefix (#x66 #b01) (#xF3 #b10) (#xF2 #b11) (t 0))))
        (masque "11000101.WvvvvLpp"
                (w (if (= 64 (reg-width op0)) 1 0)) ;; bit 2.0 indicates 64-bit width of integer operand
                (v (if op2 (flipbits (regix op2)) 0))
                ;; v field addresses third operand (reversed)
                (l (if (= 256 (reg-width op0)) 1 0)) ;; bit 2.5 denotes use of 256-bit registers
                ;; prefix field expresses opcode prefix if needed
                (p (case prefix (#x66 #b01) (#xF3 #b10) (#xF2 #b11) (t 0)))))))

(defun determine-pfevex (op0 op1 op2 &key map br-control prefix merge)
  (let ((sib-index (or (and (typep op1 'mas-x86)
                            (ash (mas-index op1) -3)))))
    (masque "01100010.RXBŔ00mm.Wvvvv1pp.ZllḂṼaaa"
            (r (if (zerop (logand #b1000 (regix op0)))               1 0))
            ;; bit 1.0 indicates inverse extension of the MODRM.reg field: op0 index upper bit
            (x (if (or (not sib-index) (zerop sib-index))                1 0))
            ;; bit 1.1 indicates inverse extension of the SIB.index field: index register upper bit
            (b (if (or (not sib-index) (zerop (ash (mas-base op1) -3))) 1 0))
            ;; bit 1.2 indicates inverse extension of MODRM.rm or SIB.base field: base register upper bit
            (ŕ (if (zerop (logand #b10000 (regix op0)))              1 0))
            ;; bit 1.3 indicates further inverse extension of MODRM.reg field
            (m (case map (#x0F #b01) (#x0F35 #b10) (#x0F38 #b11) (t map)))
            ;; map field contains operator extension index 1-3, may be mapped to codes or direct
            (w (if (= 64 (reg-width op0)) 1 0)) ;; bit 2.0 indicates 64-bit width of integer operand
            (v (if op2 (flipbits (logand #b1111 (regix op2))) 0))
            ;; v field addresses third operand (reversed)
            ;; prefix field expresses opcode prefix if needed
            (p (case prefix (#x66 #b01) (#xF3 #b10) (#xF2 #b11) (t 0)))
            (z (if merge 1 0)) ;; bit 3.0 toggles merge function
            (l (if (= 512 (reg-width op0)) #b10 (if (= 256 (reg-width op0)) #b01 0)))
            ;; bit 3.1-2 determine use of 256 or 512-bit registers
            (ḃ (if br-control 1 0))
            ;; prefix field expresses opcode prefix if needed
            (ṽ (if (zerop (logand #b10000 (regix op2))) 1 0))
            (a 0))))

;; (defvar *x86-lexicon* (make-hash-table :test #'eq))

(defclass assembler-x86 (assembler)
  ((%storage :accessor   asm-storage
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :storage)
   (%lexicon :accessor   asm-lexicon
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :lexicon)
   (%domains :accessor   asm-domains
             :initform   *storage-domains-x86*
             :allocation :class
             :initarg    :domains)
   (%exmodes :accessor   asm-exmodes
             :allocation :class
             :initform   '(:r16 :p16 :p32 :l64)
             :initarg    :exmodes)))

(defmethod qualify-ops ((assembler assembler-x86) operands form order)
  (declare (ignore assembler))
  ;; (print (list :or order))
  (let ((wix (gensym)))
    (flet ((operand-determine (o spec)
             (destructuring-bind (type &optional qualifier) spec
               (symbol-macrolet
                   ((  w-case `(eq ,o ,qualifier))
                    (gpr-case `(and (gpr-p ,o)
                                    ,@(typecase qualifier
                                        (keyword `((eq ,qualifier ,o)))
                                        (integer `((=  ,qualifier (reg-width ,o)))))))
                    (vcr-case `(and (vcr-p ,o)
                                    ,@(typecase qualifier
                                       (keyword `((eq ,qualifier ,o)))
                                       (integer `((=  ,qualifier (reg-width ,o)))))))
                    (mem-case `(and (typep ,o 'mas-x86)
                                    ,@(typecase qualifier
                                       (keyword `((eq ,qualifier ,o)))
                                       (integer `((=  ,qualifier (reg-width ,o))))))))
                 (case type
                   (:w   (list   w-case))
                   (:gpr (list gpr-case))
                   (:vcr (list vcr-case))
                   (:mem (list mem-case))
                   (:gxm `((or ,gpr-case ,mem-case)))
                   (:vxm `((or ,vcr-case ,mem-case)))
                   (:imm `((and (typep ,o 'integer)
                                ,@(if qualifier `((< ,o ,(expt 2 qualifier))))))))))))
      (loop :for index :in order
            :append (let ((f (nth index form)) (o (nth index operands)))
                      (typecase f
                        (integer `((and (integerp ,o) (= ,f ,o))))
                        ;; (keyword (case f
                        ;;            (:w   (list   w-case))
                        ;;            (:gpr `((gpr-p ,o)))
                        ;;            (:vcr `((vcr-p ,o)))
                        ;;            (:mem `((typep ,o 'mas-x86)))
                        ;;            (:gxm `((or (gpr-p ,o) (typep ,o 'mas-x86))))
                        ;;            (:vxm `((or (vcr-p ,o) (typep ,o 'mas-x86))))
                        ;;            (:imm `((typep ,o 'integer)))))
                        (list    (operand-determine o f))))))))

(defmethod specify-ops ((assembler assembler-x86) op-symbol operands params operations)
  "A simple scheme for implementing operations - the (specop) content is directly placed within functions in the lexicon hash table."
  ;; (print (list :par op-symbol operands params))
  (let ((provisions (rest (assoc :provisions params))))
    (clause-processor
     assembler 'of-lexicon op-symbol operands params
     `((symbol-macrolet ,provisions
         (cond ,@(loop :for op :in operations
                       :collect (destructuring-bind (manifest conditions) op
                                  (list (cons 'and (qualify-ops assembler operands conditions
                                                                (rest (assoc :priority params))))
                                        (cons 'list manifest))))
               (t "Invalid operation.")))))))

;; (defmethod initialize-instance :after ((assembler assembler-x86) &key)
;;   (derive-domains assembler (t (:gpr  8))
;;                   (:x86-64 (:gpr 16))
;;                   (:avx    (:vcr  8))
;;                   (:avx2   (:vcr 16))
;;                   (:avx512 (:vcr 32))))

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

;; (defun genopcons-x86 (operands form order)
;;   "Generate operand conditions for x86 architecture."
;;   (loop :for index :in order
;;         :append (let ((f (nth index form)) (o (nth index operands)))
;;                   (if (integerp f)
;;                       `((and (integerp ,o) (= ,f ,o)))
;;                       (if (not f)
;;                           nil (destructuring-bind (type qualifier) f
;;                                 (symbol-macrolet
;;                                     ((gpr-case `(and (typep ,o 'x86-gpregister)
;;                                                      ,(typecase qualifier
;;                                                         (keyword `(eq ,qualifier (reg-name  ,o)))
;;                                                         (integer `(=  ,qualifier (reg-width ,o))))))
;;                                      (vcr-case `(and (typep ,o 'x86-vcregister)
;;                                                      ,(typecase qualifier
;;                                                         (keyword `(eq ,qualifier (reg-name  ,o)))
;;                                                         (integer `(=  ,qualifier (reg-width ,o))))))
;;                                      (mem-case `(and (typep ,o 'mas-x86)
;;                                                      ,(typecase qualifier
;;                                                         (keyword `(eq ,qualifier (reg-name  ,o)))
;;                                                         (integer `(=  ,qualifier (reg-width ,o)))))))
;;                                   (case type
;;                                     (:gpr (list gpr-case))
;;                                     (:vcr (list vcr-case))
;;                                     (:mem (list mem-case))
;;                                     (:gxm `((or ,gpr-case ,mem-case)))
;;                                     (:vxm `((or ,vcr-case ,mem-case)))
;;                                     (:imm `((and (typep ,o 'integer)
;;                                                  (< ,o ,(expt 2 qualifier)))))))))))))

;; (defmethod specify-ops ((assembler assembler-x86) asm-sym op-symbol operands items)
;;   "A simple scheme for implementing operations - the (specop) content is directly placed within functions in the lexicon hash table."
;;   (print (list :par asm-sym op-symbol op-symbol operands items))
;;   (let* ((params (if (not (and (listp (first items)) (listp (caar items))
;;                                (keywordp (caaar items))))
;;                      nil (first items)))
;;          (operations (if (not params) items (rest items)))
;;          (provisions (rest (assoc :provisions params))))
;;     ;; (print (list :op operations params))
    
;;     `(setf (gethash ,(intern (string op-symbol) "KEYWORD")
;;                     (asm-lexicon ,asm-sym))
;;            (lambda ,operands
;;              (symbol-macrolet ,provisions
;;                (cond ,@(loop :for op :in operations
;;                              :collect (destructuring-bind (manifest mconditions) op
;;                                         (list (cons 'and (qualify-ops assembler operands conditions
;;                                                                       (rest (assoc :priority params))))
;;                                               (cons 'join manifest))))
;;                      (t "Invalid operation.")))))))

;; (defun determine-pfrex (width op0 op1 &optional null-if-zero)
;;   (let ((flags (+ (if (eq :q width) 8 0) ;; 8-bit indicates 64-bit width
;;                   (if (not (gpr-p op0))  ;; 4-bit is upper bit of extended GPR 0 index
;;                       0 (ash (ash (regix op0) -3) 2))
;;                   (if (not (and (typep op1 'mas-x86) ;; 2-bit is upper bit of extended GPR 1 index
;;                                 (mas-base op1) (mas-index op1)))
;;                       0 (ash (ash (regix (mas-index op1)) -3) 1))
;;                   (if (gpr-p op0)        ;; 1-bit is upper bit of extended GPR 1/MAS 1 indec
;;                       (ash (regix op0) -3)
;;                       (if (not (and (typep op0 'mas-x86) (mas-base op1)))
;;                           0 (ash (regix (mas-base op1)) -3))))))
;;     (if (and null-if-zero (zerop flags))
;;         nil (masque "0100.FFFF" (f flags)))))

;; (defun determine-pfrex (wide op0 op1 &key szpx-reg szpx-addr)
;;   (masque "0100.WRXB"
;;           (w (if wide 1 (if (or (and (gpr-p op0)
;;                                      (= 64 (reg-width op0)))
;;                                 (and (gpr-p op1)
;;                                      (= 64 (reg-width op1))))
;;                             1 0)))
;;           (r (if (not (gpr-p op0))
;;                  0 (ash (regix op0) -3)))
;;           (x (if (not (and (typep op1 'mas-x86)
;;                            (mas-base op1) (mas-index op1)))
;;                  0 (ash (regix (mas-index op1)) -3)))
;;           (b (if (gpr-p op0)
;;                  (ash (regix op0) -3)
;;                  (if (not (and (typep op0 'mas-x86)
;;                                (mas-base op1)))
;;                      0 (ash (regix (mas-base op1)) -3))))))



;; (defmethod specify-ops ((assembler assembler) op-symbol operands params operations)
;;   "A simple scheme for implementing operations - the (specop) content is directly placed within functions in the lexicon hash table."
;;   (cond ((assoc :combine params)
;;          ;; combinatoric parameters are used to specify mnemonics and opcodes that can be derived
;;          ;; by combining lists of base components, such as the Xcc conditional instructions seen
;;          ;; in many ISAs, where many different conditions share a common numeric and mnemonic base
;;          (destructuring-bind (co-symbol join-by indexer &rest combinators)
;;              (rest (assoc :combine params))
;;            (cons 'progn (loop :for co :in combinators :for i :from 0
;;                               :collect (let ((comp-sym (case join-by
;;                                                          (:appending (intern (format nil "~a~a" op-symbol co)
;;                                                                              "KEYWORD"))))
;;                                              (index (funcall (case indexer (:by-index #'identity))
;;                                                              i)))
;;                                          (clause-processor
;;                                           assembler 'of-lexicon comp-sym operands
;;                                           (append (list (cons :wrap-body
;;                                                               (lambda (body)
;;                                                                 `((let ((,co-symbol ,index)) ,@body)))))
;;                                                   params)
;;                                           operations))))))
;;         ((assoc :tabular params)
;;          ;; tabular parameters are used to specify many opcodes for ISAs like Z80 and 6502 along
;;          ;; with more recent one-byte instruction sets like WebAssembly
;;          (destructuring-bind (mode &rest properties) (rest (assoc :tabular params))
;;            (declare (ignore properties))
;;            (case mode (:cross-adding
;;                        (cons 'progn (process-clause-matrix assembler op-symbol
;;                                                            operands params operations))))))
;;         ;; ((and (not operands) (= 1 (length operations)))
;;         ;;  ;; `(setf (gethash ,(intern (string op-symbol) "KEYWORD")
;;         ;;  ;;                 (asm-lexicon ,asm-sym))
;;         ;;  ;;        ,(first operations))
;;         ;;  `(of-lexicon ,asm-sym ,(intern (string op-symbol) "KEYWORD")
;;         ;;               ,(first operations)))
;;         (t (clause-processor assembler 'of-lexicon op-symbol operands params operations))))

;; (let ((gpr-series-names) (vcr-series-names))
;;   (dotimes (n (/ (length (getf *x86-storage* :gpr)) 8))
;;     (push (reg-series (nth (1+ (* 8 n)) (getf *x86-storage* :gpr))) gpr-series-names))
  
;;   (dotimes (n (/ (length (getf *x86-storage* :vcr)) 6))
;;     (push (reg-series (nth (1+ (* 6 n)) (getf *x86-storage* :vcr))) vcr-series-names))

;;   (setf gpr-series-names (reverse gpr-series-names)
;;         vcr-series-names (reverse vcr-series-names))

;;   (defun series-index (type series-id)
;;     (case type (:gpr (position series-id gpr-series-names))
;;           (:vcr (position series-id vcr-series-names)))))

;; (defmethod of-storage ((assembler assembler-x86) &rest params)
;;   (destructuring-bind (type &key series width series-index) params
;;     ;; (print (list :in width type series-index))
;;     (let* ((series-index (or series-index (if (not series) 0 (series-index type series))))
;;            (type-list (getf *x86-storage* type))
;;            (storage-index (+ 1 (* 4 series-index) (* 2 width))))
;;       ;; (print (list :ss storage-index width type))
;;       (if (not (member series-index (assoc type (asm-domains assembler))))
;;           nil (values (nth storage-index type-list)
;;                       (+ series-index width))))))

;; (defmethod locate ((assembler assembler-x86) items)
;;   (let ((domains (copy-tree (asm-domains assembler)))
;;         (bound (loop :for item :in items :when (member :bind item :test #'eq) :collect item))
;;         (unbound (loop :for item :in items :unless (member :bind item :test #'eq) :collect item)))
;;     (print (list :bu bound unbound))
;;     (append (loop :for item :in bound
;;                   :collect (destructuring-bind (symbol type &rest params) item
;;                              ;; (print (list :dom domains))
;;                              (list symbol (let ((out-index (series-index type (getf params :bind))))
;;                                             (setf (rest (assoc type domains))
;;                                                   (remove out-index (rest (assoc type domains))))
;;                                             out-index))))
;;             (loop :for item :in unbound
;;                   :collect (destructuring-bind (symbol type &rest params) item
;;                              (list symbol (let ((random-index (nth (random (length (rest (assoc type domains))))
;;                                                                    (rest (assoc type domains)))))
;;                                             (setf (rest (assoc type domains))
;;                                                   (remove random-index (rest (assoc type domains))))
;;                                             random-index)))))))

;; (loop :for item :in items
;;       :collect (destructuring-bind (symbol type &rest params) item
;;                  ;; (print (list :dom domains))
;;                  (list symbol (if (getf params :bind)
;;                                   (let ((out-index (series-index type (getf params :bind))))
;;                                     (setf (rest (assoc type domains))
;;                                           (remove out-index (rest (assoc type domains))))
;;                                     out-index)
;;                                   (let ((random-index (nth (random (length (rest (assoc type domains))))
;;                                                            (rest (assoc type domains)))))
;;                                     (setf (rest (assoc type domains))
;;                                           (remove random-index (rest (assoc type domains))))
;;                                     random-index)))))))

;; (defmethod compose ((assembler assembler-x86) params expression)
;;   (print (list :ar params))
;;   (destructuring-bind (op &rest props) expression
;;     (if (listp op)
;;         (loop :for item :in expression :collect (compose assembler params item))
;;         (if (not (keywordp op))
;;             (compose assembler params (macroexpand op))
;;             (let (;; (width (if (not (keywordp (first props)))
;;                   ;;            nil (position (first props) #(:b :w :d :q) :test #'eq)))
;;                   ;; (props (if (not width) props (rest props)))
;;                   (bindings (rest (assoc :store params))))
;;               (print (list :bi bindings params props expression
;;                            (gethash op (asm-lexicon assembler))))
;;               (apply (gethash op (asm-lexicon assembler)) props
;;                      ;; (loop :for p :in props
;;                      ;;       :collect (typecase p
;;                      ;;                  ;; (symbol (of-storage assembler :gpr :width width
;;                      ;;                  ;;                     :series-index (second (assoc p bindings))))
;;                      ;;                  (t p)))
;;                      ))))))

#|
(assemble *assembler-prototype-x86*
 ((:exmode . :l64) (:store (abc :gpr) (def :gpr :bind :a)))
  (:add :w abc 10)
  (:add :w def 33))
|#

;; (specify-assembler asm-x86 assemble
;;                    (:options :x86-64)
;;                    (:lexicon *x86-lexicon*))

;; (assemble-x86
;;  (:with (:options :x86-64 :avx2)
;;         (:input )
;;         (:store(abc :gpr) (def :gpr :binding-series :a) (vec :vcr)))
;;  (:mov abc 10)
;;  (:mov def 33))

(defun w (width value)
  (if (and (not (zerop (ash value (ash (1- width) 3)))))
      (cons width value)
      (if (zerop (ash value (ash width 3)))
          value (logand value (1- (ash 1 width))))))
