;;;; base.lisp

(in-package #:specops.system-z)

;; this keyword-indexed register file doesn't reflect standard practice in writing
;; Z assembly, where registers are typically referenced simply by numeric indices, but it is
;; offered to support greater clarity in expressing code

(defvar *z-layout*
  (list :gp '(:r0  :r1  :r2  :r3  :r3  :r5  :r6  :r7  :r8  :r9  :r10 :r11 :r12 :r13 :r14 :r15)
        :vc '(:v0  :v1  :v2  :v3  :v4  :v5  :v6  :v7  :v8  :v9  :v10 :v11 :v12 :v13 :v14 :v15
              :v16 :v17 :v18 :v19 :v20 :v21 :v22 :v23 :v24 :v25 :v26 :v27 :v28 :v29 :v30 :v31)))

(defun rix (register &optional type)
  (typecase register
    (register (of-register-type-index register type))
    (integer  (if (<= 0 register (1- (length (getf *z-layout* type)))) register nil))
    (keyword  (if type (values (position register (getf *z-layout* type))
                               type)
                  (let ((position) (type-found))
                    (loop :for (type-key names) :on *z-layout* :by #'cddr :until position
                          :do (setf position   (position register names)
                                    type-found type-key))
                    (values position type-found))))))

(defun z-vcr-loix (register)
  (logand #xF (rix register :vc)))

;; (defun vrmsbits (v1 &optional v2 v3 v4)
;;   "Get most significant bits from indices of vector registers passed as operands to a vector instruction. Used to populate the RXB fields of vector instruction codes."
;;   (let ((iv1 (rix v1 :vc)) (iv2 (rix v2 :vc)) (iv3 (rix v3 :vc)) (iv4 (rix v4 :vc)))
;;     (+ (if iv1 (ash (logand #b10000 iv1) -1) 0)
;;        (if iv2 (ash (logand #b10000 iv2) -2) 0)
;;        (if iv3 (ash (logand #b10000 iv3) -3) 0)
;;        (if iv4 (ash (logand #b10000 iv4) -4) 0))))

(defun vrmsbits (v1 &optional (v2 0) (v3 0) (v4 0))
  "Get most significant bits from indices of vector registers passed as operands to a vector instruction. Used to populate the RXB fields of vector instruction codes."
  (declare (type bit v1 v2 v3 v4))
  (+ (ash v1 3) (ash v2 2) (ash v3 1) v4))

(defun gpr-p (item) (rix item :gp))

(defun vcr-p (item) (rix item :vc))

(deftype gpr  () `(satisfies gpr-p))

(deftype vcr  () `(satisfies gpr-p))

(defclass mas-z (mas-based mas-indexed mas-displaced)
  ())

(defun @ (base &optional index displacement)
  (let ((ibase (rix base :gp)) (iindex (rix index :gp)))
    (if (not (and ibase (<= 0 ibase 15)))
        (error "Memory access must use a base register in the 0-15 range.")
        (make-instance 'mas-z :base ibase :index iindex :displ displacement))))

(defun @% (base &optional displacement)
  (if displacement (@ base nil displacement)) ;; two arguments yield a base/displacement MAS
      (@ 0 nil base)) ;; one argument produces a MAS with only displacement

(defun derive-mas (base &optional index displacement)
  `(@ ,@(if base (list base) nil) ,@(if index (list index) nil)
      ,@(if displacement (list displacement) nil)))

(defclass mas-z-ranged (mas-z)
  ((%length :accessor mas-z-ranged-length
            :initform nil
            :initarg  :length))
  (:documentation "A ranged memory access scheme, defining access to a range of memory."))

(defclass mas-z-disparate (mas-z)
  ()
  (:documentation "A disparate memory access scheme, defining access memory at disparate points within a range for the purpose of gather/scatter vector operations."))

(defun @* (base length &optional displacement)
  (if (not (<= 0 base 15))
      (error "Memory access must use a base register in the 0-15 range.")
      (make-instance 'mas-z-ranged :base base :length length :displ displacement)))

(defun derive-masr (base length &optional displacement)
  `(@* ,@(if base (list base) nil) ,@(if length (list length) nil)
       ,@(if displacement (list displacement) nil)))

(defun @~ (base indices displacement)
  (let ((ibase (rix base :gp)) (iindices (rix indices :vc)))
    (if (not (<= 0 base 15))
        (error "Memory access must use a base register in the 0-15 range.")
        (make-instance 'mas-z-ranged :base ibase :index iindices :displ displacement))))

(defun derive-masd (base indices &optional displacement)
  `(@~ ,@(if base (list base) nil) ,@(if indices (list indices) nil)
       ,@(if displacement (list displacement) nil)))

(defgeneric z-masd-rs12 (mas-z))

(defgeneric z-masd-lo12 (mas-z))

(defgeneric encoded-mras-length (mas))

(defmethod z-masd-rs12 ((mas-z mas-z))
  (ash (mas-displ mas-z) -12))

(defmethod z-masd-lo12 ((mas-z mas-z))
  (logand #xFFF (mas-displ mas-z)))

(defmethod encoded-mras-length ((mas mas-z-ranged))
  (if (zerop (mas-z-ranged-length mas))
      0 (1- (mas-z-ranged-length mas))))

(defun mas-bd12-p (item)
  (and (typep item 'mas-z) (mas-displ item) (zerop (ash (mas-displ item) -12))))
  
(defun mas-bd20-p (item)
  (and (typep item 'mas-z) (mas-displ item) (zerop (ash (mas-displ item) -20))))

(defun mas-bid12-p (item)
  (and (typep item 'mas-z) (mas-displ item) (mas-index item)
       (zerop (ash (mas-displ item) -12))))
  
(defun mas-bid20-p (item)
  (and (typep item 'mas-z) (mas-displ item) (mas-index item)
       (zerop (ash (mas-displ item) -20))))

(defun mas-ranged-p (item)
  (and (typep item 'mas-z-ranged)))

(defun mas-disparate-p (item)
  (and (typep item 'mas-z-disparate)))

(deftype mas-bd12      () `(satisfies mas-bd12-p))

(deftype mas-bd20      () `(satisfies mas-bd20-p))

(deftype mas-bid12     () `(satisfies mas-bid12-p))

(deftype mas-bid20     () `(satisfies mas-bid20-p))

(deftype mas-ranged    () `(satisfies mas-ranged-p))

(deftype mas-disparate () `(satisfies mas-disparate-p))

(defun encode-location (location)
  (values (mas-base location)
          (or (mas-displ location) 0)
          (typecase location
            (mas-ranged    (encoded-mras-length      location))
            (mas-disparate (logand #b1111 (mas-index location))) ;; low 4 bits of vector register index
            (t                            (mas-index location)))
          (and (typep location 'mas-disparate) ;; high bit of vector register indext
               (ash (logand #b10000 (mas-index location)) -4))))

(defun add-longdisp (hi4 lo12)
  (+ lo12 (ash hi4 12)))

(defun derive-vra (arg index msbits)
  (+ index (ash (logand msbits (ash 1 arg)) (- 4 arg))))

(defvar *assembler-prototype-z*)

(defclass assembler-z (assembler-masking)
  ((%storage :accessor   asm-storage
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :storage)
   (%lexicon :accessor   asm-lexicon
             :allocation :class
             :initform   (make-hash-table :test #'eq)
             :initarg    :lexicon)
   (%domains :accessor   asm-domains
             :initform   nil
             :initarg    :domains)
   (%breadth :accessor   asm-breadth
             :allocation :class
             :initform   16
             :initarg    :breadth)
   (%joiner  :accessor   asm-joiner
             :allocation :class
             :initform   #'joinw
             :initarg    :joiner)
   (%segment :accessor   asm-msk-segment
             :initform   '(1 2 3)
             :initarg    :breadth)
   (%battery :accessor   asm-msk-battery
             :allocation :class
             :initform   (make-hash-table)
             :initarg    :battery)))

(defmacro rs4 (number)
  (list 'ash number -4))

(defmacro rs8 (number)
  (list 'ash number -8))

(defmacro lo4 (number)
  (list 'logand number #x0F))

(defmacro lo8 (number)
  (list 'logand number #xFF))

(defmacro specop-z (mnemonic format opcode)
  (destructuring-bind (assemble-fn &optional disassemble-fn) (macroexpand (list format opcode mnemonic))
    `(progn (of-lexicon *assembler-prototype-z* ,(intern (string mnemonic) "KEYWORD")
                        ,assemble-fn)
            ,@(if (not disassemble-fn)
                  nil `((of-battery *assembler-prototype-z* ,(intern (string mnemonic) "KEYWORD")
                                    ,disassemble-fn))))))

#|

(assemble *assembler-prototype-z*
  (:adr 1 1) (:sr 10 7))

(assemble *assembler-prototype-z*
  (:adr 1 1) 
  (:sr 10 7) 
  (:a 3 (@ 2 3 50))
  (:or 3 5)
  (:nr 10 12)
  (:lr 2 8)
  (:ar 3 5)
  (:ah 10 (@ 1 8 166))
  (:sr 5 2)
  (:lr 6 7)
  (:or 9 8))

|#

(setf *assembler-prototype-z* (make-instance 'assembler-z))

(defun qualify-operand (operand type)
  (typecase type
    (symbol (case type ((gpr vcr mas-bd12 mas-bd20 mas-bid12 mas-bid20 mas-disparate)
                        `(typep ,operand ',type))
                  ('nil `(not ,operand))))
    (list (destructuring-bind (type &rest qualities) type
            (case type
              (imm          `(and (imm-value ,operand)
                                  (< (imm-value ,operand) ,(expt 2 (first qualities)))))
              (mas-ranged   `(and (mas-ranged-p ,operand)
                                  (zerop (ash (mas-z-ranged-length ,operand)
                                              (- ,(first qualities))))))
              (label-offset `(or (integerp ,operand) (symbolp ,operand))))))))

(defun verbalize-operand (spec)
  (format nil "~a~%" (typecase spec
                       (symbol (case spec
                                 (gpr "general purpose register : any")
                                 (vcr "vector register : any")
                                 (mas-bd12                "memory access : (base,disp12)")
                                 (mas-bd20                "memory access : (base,disp20)")
                                 (mas-bid12               "memory access : (base,index,disp12)")
                                 (mas-bid20               "memory access : (base,index,disp20)")
                                 (mas-disparate "disparate memory access : (base,vector,disp12)")))
                       (list (destructuring-bind (type &rest qualities) spec
                               (case type
                                 (imm (format nil "immediate value : ~d bits" (first qualities)))
                                 (mas-ranged    (format nil "ranged memory access : (base,length~a,disp12)"
                                                        (first qualities)))
                                 (label-offset "label / offset")))))))

(defun derive-operand (spec)
  (let ((index (gensym)))
    (destructuring-bind (operand &rest types) spec
      (cons 'cond (loop :for type :in types
                        :collect
                        (list (qualify-operand operand type)
                              (typecase type
                                (symbol (case type
                                          (gpr `(rix ,operand :gp))
                                          (vcr `(let ((,index (rix ,operand :vc)))
                                                  (values      (logand #b01111 ,index)
                                                          (ash (logand #b10000 ,index) -4))))
                                          ((mas-bd12 mas-bd20 mas-bid12 mas-bid20 mas-disparate)
                                           `(encode-location ,operand))))
                                (list (destructuring-bind (type &rest qualities) type
                                        (case type
                                          (imm `(imm-value ,operand))
                                          (mas-ranged   `(encoded-mras-length ,operand))
                                          (label-offset (destructuring-bind (api-fnsym length offset) qualities
                                                          (list api-fnsym :label length
                                                                offset operand)))))))))))))

(defmacro determine (mnemonic specs &optional bindings &body body)
  "For System Z, the (determine) macro is generated during the expansion of the (specop-z) macro as specified by the :determine-by parameter given to the (mqbase) macro, whose forms are below. It works differently than for M68K and SuperH, as the (determine) form is not written in code but is rather used as an intermediate form during expansion of instruction spec macros. It has the same function: to facilitate at once processing, error-checking and documenting of instructions."
  (append (list 'determine-in-context (list :qualify #'qualify-operand :verbalize #'verbalize-operand
                                            :derive #'derive-operand)
                mnemonic specs bindings)
          body))

;; IBM's docs count the operands from 1 and these functions do the same

(mqbase zformat-e opc mne ()
    "AAAAAAAA"
  ((:static (a opc)))
  ()
  (list mne))

(mqbase zformat-i opc mne (i1)
    "AAAAAAAA.IIIIIIII"
  ((:static (a opc))
   (:determine-by determine ((i1 (imm 8))) (im1)))
  ((i im1))
  (list mne i))

(mqbase zformat-ie opc mne (i1 i2)
    "AAAAAAAA.AAAAAAAA.00000000.IIIIJJJJ"
  ((:static (a opc))
   (:determine-by determine ((i1 (imm 4)) (i2 (imm 4))) (im1 im2)))
  ((i im1) (j im2))
  (list mne i j))

(mqbase zformat-mii opc mne (m1 ri2 ri3)
    "AAAAAAAA.MMMMRRRR.RRRRRRRR.IIIIIIII.IIIIIIII.IIIIIIII"
  ((:static (a opc))
   (:determine-by determine ((m1 (imm 4)) (ri2 (label-offset of-program 12 12))
                             (ri3 (label-offset of-program 24 24)))
                  (im1 lb2 lb3)))
  ((m im1) (r lb2) (i lb3))
  (list mne m r i))

(mqbase zformat-ri-a opc mne (r1 i2)
    "AAAAAAAA.RRRRZZZZ.IIIIIIII.IIIIIIII"
  ((:static (a (rs4 opc)) (z (lo4 opc)))
   (:determine-by determine ((r1 gpr) (i2 (imm 16))) (ix1 im2)))
  ;; ((r (rix r1 :gp)) (i i2))
  ((r ix1) (i im2))
  (list mne r i))

(mqbase zformat-ri-b opc mne (r1 ri2)
    "AAAAAAAA.RRRRZZZZ.IIIIIIII.IIIIIIII"
  ((:static (a (rs4 opc)) (z (lo4 opc)))
   (:determine-by determine ((r1 gpr) (ri2 (label-offset of-program 16 16))) (ix1 lb2)))
  ((r ix1) (i lb2))
  (list mne r i))

(mqbase zformat-ri-c opc mne (m1 ri2)
    "AAAAAAAA.MMMMZZZZ.IIIIIIII.IIIIIIII"
  ((:static (a (rs4 opc)) (z (lo4 opc)))
   (:determine-by determine ((m1 (imm 4)) (ri2 (label-offset of-program 16 16))) (im1 lb2)))
  ((m im1) (i lb2))
  (list mne m i))

(mqbase zformat-rie-a opc mne (r1 i2 m3)
    "AAAAAAAA.RRRR0000.IIIIIIII.IIIIIIII.MMMM0000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((r1 gpr) (i2 (imm 16)) (m3 (imm 4))) (ix1 im2 im3)))
  ((r ix1) (i im2) (m im3))
  (list mne r i m))

(mqbase zformat-rie-b opc mne (r1 r2 m3 ri4)
    "AAAAAAAA.RRRRSSSS.IIIIIIII.IIIIIIII.MMMM0000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((r1 gpr) (r2 gpr) (m3 (imm 4)) (ri4 (label-offset of-program 16 16)))
                  (ix1 ix2 im3 lb4)))
  ((r ix1) (s ix2) (i lb4) (m im3))
  (list mne r s m i))

(mqbase zformat-rie-c opc mne (r1 i2 m3 ri4)
    "AAAAAAAA.RRRRMMMM.IIIIIIII.IIIIIIII.JJJJJJJJ.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((r1 gpr) (i2 (imm 8)) (m3 (imm 4)) (ri4 (label-offset of-program 16 16)))
                  (ix1 im2 im3 lb4)))
  ((r ix1) (m im3) (i lb4) (j im2))
  (list mne r j m i))

(mqbase zformat-rie-d opc mne (r1 i2 r3)
    "AAAAAAAA.RRRRSSSS.IIIIIIII.IIIIIIII.00000000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((r1 gpr) (i2 (imm 8)) (r3 gpr)) (ix1 im2 ix3)))
  ((r ix1) (s ix3) (i im2))
  (list mne r i s))

(mqbase zformat-rie-e opc mne (r1 ri2 r3)
    "AAAAAAAA.RRRRSSSS.IIIIIIII.IIIIIIII.00000000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((r1 gpr) (ri2 (label-offset of-program 16 16)) (r3 gpr)) (ix1 lb2 ix3)))
  ((r ix1) (s ix3) (i lb2))
  (list mne r i s))

(mqbase zformat-rie-f opc mne (r1 r2 i3 i4 i5)
    "AAAAAAAA.RRRRSSSS.IIIIIIII.JJJJJJJJ.KKKKKKKK.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((r1 gpr) (r2 gpr) (i3 (imm 8)) (i4 (imm 8)) (i5 (imm 8)))
                  (ix1 ix2 im3 im4 im5)))
  ((r ix1) (s ix2) (i im3) (j im4) (k im5))
  (list mne r s i j k))

(mqbase zformat-rie-g opc mne (r1 i2 m3)
    "AAAAAAAA.RRRRMMMM.IIIIIIII.IIIIIIII.00000000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((r1 gpr) (i2 (imm 16)) (m3 (imm 4))) (ix1 im2 im3)))
  ((r ix1) (m im3) (i im2))
  (list mne r i m))

(mqbase zformat-ril-a opc mne (r1 i2)
    "AAAAAAAA.RRRRZZZZ.IIIIIIII.IIIIIIII.IIIIIIII.IIIIIIII"
  ((:static (a (rs4 opc)) (z (lo4 opc)))
   (:determine-by determine ((r1 gpr) (i2 (imm 32))) (ix1 im2)))
  ((r ix1) (i im2))
  (list mne r i))

(mqbase zformat-ril-b opc mne (r1 ri2)
    "AAAAAAAA.RRRRZZZZ.IIIIIIII.IIIIIIII.IIIIIIII.IIIIIIII"
  ((:static (a (rs4 opc)) (z (lo4 opc)))
   (:determine-by determine ((r1 gpr) (ri2 (label-offset of-program 16 32))) (ix1 lb2)))
  ((r ix1) (i lb2))
  (list mne r i))

(mqbase zformat-ril-c opc mne (m1 ri2)
    "AAAAAAAA.MMMMZZZZ.IIIIIIII.IIIIIIII.IIIIIIII.IIIIIIII"
  ((:static (a (rs4 opc)) (z (lo4 opc)))
   (:determine-by determine ((m1 (imm 4)) (ri2 (label-offset of-program 16 32))) (im1 lb2)))
  ((m im1) (i lb2))
  (list mne m i))

(mqbase zformat-ris opc mne (r1 i2 m3 bd4)
    "AAAAAAAA.RRRRMMMM.BBBBDDDD.DDDDDDDD.IIIIIIII.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((r1 gpr) (i2 (imm 8)) (m3 (imm 4)) (bd4 mas-bd12)) (ix1 im2 im3 (b4 d4))))
  ((r ix1) (m im3) (b b4) (d d4) (i im2))
  (list mne r i m (derive-mas b nil d)))

(mqbase zformat-rr opc mne (r1 r2)
    "AAAAAAAA.RRRRSSSS"
  ((:static (a opc))
   (:determine-by determine ((r1 gpr) (r2 gpr)) (ix1 ix2)))
  ((r ix1) (s ix2))
  (list mne r s))

(mqbase zformat-rrd opc mne (r1 r2 r3)
    "AAAAAAAA.AAAAAAAA.QQQQ0000.RRRRSSSS"
  ((:static (a opc))
   (:determine-by determine ((r1 gpr) (r2 gpr) (r3 gpr)) (ix1 ix2 ix3)))
  ((q ix1) (r ix2) (s ix3))
  (list mne q r s))

(mqbase zformat-rre opc mne (&optional r1 r2)
    "AAAAAAAA.AAAAAAAA.00000000.RRRRSSSS"
  ((:static (a opc))
   (:determine-by determine ((r1 gpr) (r2 gpr)) (ix1 ix2)))
  ((r ix1) (s ix2))
  (list mne r s))

(mqbase zformat-rrf-a opc mne (r1 r2 r3 &optional m4)
    "AAAAAAAA.AAAAAAAA.QQQQMMMM.RRRRSSSS"
  ((:static (a opc))
   (:determine-by determine ((r1 gpr) (r2 gpr) (r3 gpr) (m4 nil (imm 4))) (ix1 ix2 ix3 im4)))
  ((q ix3) (m (or im4 0)) (r ix1) (s ix2))
  (list mne r s q m))

(mqbase zformat-rrf-b opc mne (r1 r2 r3 &optional m4)
    "AAAAAAAA.AAAAAAAA.QQQQMMMM.RRRRSSSS"
  ((:static (a opc))
   (:determine-by determine ((r1 gpr) (r2 gpr) (r3 gpr) (m4 nil (imm 4))) (ix1 ix2 ix3 im4)))
  ((q ix3) (m (or im4 0)) (r ix1) (s ix2))
  (list mne r s q m))

(mqbase zformat-rrf-c opc mne (r1 r2 &optional m3 m4)
    "AAAAAAAA.AAAAAAAA.MMMMNNNN.RRRRSSSS"
  ((:static (a opc))
   (:determine-by determine ((r1 gpr) (r2 gpr) (m3 nil (imm 4)) (m4 nil (imm 4))) (ix1 ix2 im3 im4)))
  ((m (or im3 0)) (n (or im4 0)) (r ix1) (s ix2))
  (list mne r s m n))

(mqbase zformat-rrf-d opc mne (r1 r2 &optional m3 m4)
    "AAAAAAAA.AAAAAAAA.MMMMNNNN.RRRRSSSS"
  ((:static (a opc))
   (:determine-by determine ((r1 gpr) (r2 gpr) (m3 nil (imm 4)) (m4 nil (imm 4))) (ix1 ix2 im3 im4)))
  ((m (or im3 0)) (n (or im4 0)) (r ix1) (s ix2))
  (list mne r s m n))

(mqbase zformat-rrf-e opc mne (r1 r2 &optional m3 m4)
    "AAAAAAAA.AAAAAAAA.MMMMNNNN.RRRRSSSS"
  ((:static (a opc))
   (:determine-by determine ((r1 gpr) (r2 gpr) (m3 nil (imm 4)) (m4 nil (imm 4))) (ix1 ix2 im3 im4)))
  ((m (or im3 0)) (n (or im4 0)) (r ix1) (s ix2))
  (list mne r s m n))

(mqbase zformat-rrs opc mne (r1 r2 m3 bd4)
    "AAAAAAAA.RRRRSSSS.BBBBDDDD.DDDDDDDD.MMMM0000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((r1 gpr) (r2 gpr) (m3 (imm 4)) (bd4 mas-bd12)) (ix1 ix2 im3 (b4 d4))))
  ((r ix1) (s ix2) (b b4) (d d4) (m im3))
  (list mne r s m (derive-mas b nil d)))

(mqbase zformat-rs-a opc mne (r1 bd2 &optional r3)
    "AAAAAAAA.RRRRSSSS.BBBBDDDD.DDDDDDDD"
  ((:static (a opc))
   (:determine-by determine ((r1 gpr) (bd2 mas-bd12) (r3 nil gpr)) (ix1 (b2 d2) ix3)))
  ((r ix1) (s (or ix3 0)) (b b2) (d d2))
  (list mne r (derive-mas b nil d) s))

(mqbase zformat-rs-b opc mne (r1 bd2 m3)
    "AAAAAAAA.RRRRMMMM.BBBBDDDD.DDDDDDDD"
  ((:static (a opc))
   (:determine-by determine ((r1 gpr) (bd2 mas-bd12) (m3 (imm 4))) (ix1 (b2 d2) im3)))
  ((r ix1) (m im3) (b b2) (d d2))
  (list mne r (derive-mas b nil d) m))

(mqbase zformat-rsi opc mne (r1 ri2 r3)
    "AAAAAAAA.RRRRSSSS.IIIIIIII.IIIIIIII"
  ((:static (a opc))
   (:determine-by determine ((r1 gpr) (ri2 (label-offset of-program 16 16)) (r3 gpr)) (ix1 lb2 ix3)))
  ((r ix1) (s ix3) (i lb2))
  (list mne r i s))

(mqbase zformat-rsl-a opc mne (bld1)
    "AAAAAAAA.LLLL0000.BBBBDDDD.DDDDDDDD.00000000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((bld1 (mas-ranged 4))) ((b1 l1 d1))))
  ((l l1) (b b1) (d d1))
  (list mne (derive-masr b l d)))

(mqbase zformat-rsl-b opc mne (bld1)
    "AAAAAAAA.LLLLLLLL.BBBBDDDD.DDDDDDDD.00000000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((bld1 (mas-ranged 4))) ((b1 l1 d1))))
  ((l l1) (b b1) (d d1))
  (list mne (derive-masr b l d)))

(mqbase zformat-rsy-a opc mne (r1 bdd2 r3)
    "AAAAAAAA.RRRRSSSS.BBBBDDDD.DDDDDDDD.HHHHHHHH.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((r1 gpr) (bdd2 mas-bd20) (r3 gpr)) (ix1 (b2 d2) ix3)))
  ((r ix1) (s ix3) (b b2) (d (z-masd-lo12 d2)) (h (z-masd-rs12 d2)))
  (list mne r (derive-mas b nil (add-longdisp h d)) s))

(mqbase zformat-rsy-b opc mne (r1 bdd2 m3)
    "AAAAAAAA.RRRRMMMM.BBBBDDDD.DDDDDDDD.HHHHHHHH.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((r1 gpr) (bdd2 mas-bd20) (m3 (imm 4))) (ix1 (b2 d2) im3)))
  ((r ix1) (m im3) (b b2) (d (z-masd-lo12 d2)) (h (z-masd-rs12 d2)))
  (list mne r (derive-mas b nil (add-longdisp h d)) m))

(mqbase zformat-rx-a opc mne (r1 bdx2)
    "AAAAAAAA.RRRRXXXX.BBBBDDDD.DDDDDDDD"
  ((:static (a opc))
   (:determine-by determine ((r1 gpr) (bdx2 mas-bid12)) (ix1 (b2 d2 ix2))))
  ((r ix1) (x ix2) (b b2) (d d2))
  (list mne r (derive-mas b x d)))

(mqbase zformat-rx-b opc mne (m1 bdx2)
    "AAAAAAAA.MMMMXXXX.BBBBDDDD.DDDDDDDD"
  ((:static (a opc))
   (:determine-by determine ((m1 (imm 4)) (bdx2 mas-bid12)) (im1 (b2 d2 ix2))))
  ((m im1) (x ix2) (b b2) (d d2))
  (list mne m (derive-mas b x d)))

(mqbase zformat-rxe opc mne (r1 bdx2 &optional m3)
    "AAAAAAAA.RRRRXXXX.BBBBDDDD.DDDDDDDD.MMMM0000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((r1 gpr) (bdx2 mas-bid12) (m3 nil (imm 4))) (ix1 (b2 d2 ix2) im3)))
  ((r ix1) (x ix2) (b b2) (d d2) (m (or im3 0)))
  (list mne r (derive-mas b x d) m))

(mqbase zformat-rxf opc mne (r1 bdx2 r3)
    "AAAAAAAA.RRRRXXXX.BBBBDDDD.DDDDDDDD.SSSS0000.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((r1 gpr) (bdx2 mas-bid12) (r3 gpr)) (ix1 (b2 d2 ix2) ix3)))
  ((r ix3) (x ix2) (b b2) (d d2) (s ix1))
  (list mne r (derive-mas b x d) s))

(mqbase zformat-rxy-a opc mne (r1 bddx2)
    "AAAAAAAA.RRRRXXXX.BBBBDDDD.DDDDDDDD.HHHHHHHH.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((r1 gpr) (bddx2 mas-bid20)) (ix1 (b2 dd2 ix2))))
  ((r ix1) (x ix2) (b b2) (d (z-masd-lo12 dd2)) (h (z-masd-rs12 dd2)))
  (list mne r (derive-mas b x (add-longdisp h d))))

(mqbase zformat-rxy-b opc mne (m1 bddx2)
    "AAAAAAAA.MMMMXXXX.BBBBDDDD.DDDDDDDD.HHHHHHHH.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((m1 (imm 4)) (bddx2 mas-bid20)) (im1 (b2 dd2 ix2))))
  ((m im1) (x ix2) (b b2) (d (z-masd-lo12 dd2)) (h (z-masd-rs12 dd2)))
  (list mne m (derive-mas b x (add-longdisp h d))))

(mqbase zformat-s opc mne (&optional bd1)
    "AAAAAAAA.AAAAAAAA.BBBBDDDD.DDDDDDDD"
  ((:static (a opc))
   (:determine-by determine ((bd1 nil mas-bd12)) ((b1 d1))))
  ((b (or b1 0)) (d (or d1 0)))
  (list mne (derive-mas b nil d)))

(mqbase zformat-si opc mne (bd1 &optional i2)
    "AAAAAAAA.IIIIIIII.BBBBDDDD.DDDDDDDD"
  ((:static (a opc))
   (:determine-by determine ((bd1 mas-bd12) (i2 nil (imm 8))) ((b1 d1) im2)))
  ((i (or im2 0)) (b b1) (d d1))
  (list mne (derive-mas b nil d) i))

(mqbase zformat-sil opc mne (bd1 i2)
    "AAAAAAAA.AAAAAAAA.BBBBDDDD.DDDDDDDD.IIIIIIII.IIIIIIII"
  ((:static (a opc))
   (:determine-by determine ((bd1 mas-bd12) (i2 (imm 16))) ((b1 d1) im2)))
  ((b b1) (d d1) (i im2))
  (list mne (derive-mas b nil d) i))

(mqbase zformat-siy opc mne (bdd1 i2)
    "AAAAAAAA.IIIIIIII.BBBBDDDD.DDDDDDDD.HHHHHHHH.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((bdd1 mas-bd20) (i2 (imm 8))) ((b1 dd1) im2)))
  ((i im2) (b b1) (d (z-masd-lo12 dd1)) (h (z-masd-rs12 dd1)))
  (list mne (derive-mas b nil (add-longdisp h d)) i))

(mqbase zformat-smi opc mne (m1 ri2 bd3)
    "AAAAAAAA.MMMM0000.BBBBDDDD.DDDDDDDD.IIIIIIII.IIIIIIII"
  ((:static (a opc))
   (:determine-by determine ((m1 (imm 4)) (ri2 (label-offset of-program 32 16)) (bd3 mas-bd12))
                  (im1 lb2 (b3 d3))))
  ((m im1) (b b3) (d d3) (i lb2))
  (list mne m i (derive-mas b nil d)))

(mqbase zformat-ss-a opc mne (bld1 bd2)
    "AAAAAAAA.LLLLLLLL.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
  ((:static (a opc))
   (:determine-by determine ((bld1 (mas-ranged 8)) (bd2 mas-bd12)) ((b1 l1 d1) (b2 d2))))
  ((l l1) (b b1) (d d1) (e b2) (f d2))
  (list mne (derive-masr b l d) (derive-mas e nil f)))

(mqbase zformat-ss-b opc mne (bld1 bld2)
    "AAAAAAAA.LLLLMMMM.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
  ((:static (a opc))
   (:determine-by determine ((bld1 (mas-ranged 4)) (bld2 (mas-ranged 4))) ((b1 l1 d1) (b2 l2 d2))))
  ((l l1) (m l2) (b b1) (d d1) (e b2) (f d2))
  (list mne (derive-masr b l d) (derive-masr e m f)))

(mqbase zformat-ss-c opc mne (bld1 bd2 i3)
    "AAAAAAAA.LLLLIIII.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
  ((:static (a opc))
   (:determine-by determine ((bld1 (mas-ranged 4)) (bd2 mas-bd12) (i3 (imm 4))) ((b1 l1 d1) (b2 d2) im3)))
  ((l l1) (i im3) (b b1) (d d1) (e b2) (f d2))
  (list mne (derive-masr b l d) (derive-mas e nil f) i))

(mqbase zformat-ss-d opc mne (r1 bd2 bd3 r4)
    "AAAAAAAA.RRRRSSSS.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
  ((:static (a opc))
   (:determine-by determine ((r1 gpr) (bd2 mas-bd12) (bd3 mas-bd12) (r4 gpr)) (ix1 (b2 d2) (b3 d3) ix4)))
  ((r ix1) (s ix4) (b b2) (d d2) (e b3) (f d3))
  (list mne r (derive-mas b nil d) (derive-mas e nil f) s))

(mqbase zformat-ss-e opc mne (r1 bd2 r3 bd4)
    "AAAAAAAA.RRRRSSSS.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
  ((:static (a opc))
   (:determine-by determine ((r1 gpr) (bd2 mas-bd12) (r3 gpr) (bd4 mas-bd12)) (ix1 (b2 d2) ix3 (b4 d4))))
  ((r ix1) (s ix3) (b b2) (d d2) (e b4) (f d4))
  (list mne r (derive-mas b nil d) s (derive-mas e nil f)))

(mqbase zformat-ss-f opc mne (bd1 bld2)
    "AAAAAAAA.LLLLLLLL.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
  ((:static (a opc))
   (:determine-by determine ((bd1 mas-bd12) (bld2 (mas-ranged 8))) ((b1 d1) (b2 l2 d2))))
  ((l l2) (b b1) (d d1) (e b2) (f d2))
  (list mne (derive-mas b nil d) (derive-masr e l f)))

(mqbase zformat-sse opc mne (bd1 bd2)
    "AAAAAAAA.AAAAAAAA.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
  ((:static (a opc))
   (:determine-by determine ((bd1 mas-bd12) (bd2 mas-bd12)) ((b1 d1) (b2 d2))))
  ((b b1) (d d1) (e b2) (f d2))
  (list mne (derive-mas b nil d) (derive-mas e nil f)))

(mqbase zformat-ssf opc mne (bd1 bd2 r3)
    "AAAAAAAA.RRRRZZZZ.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
  ((:static (a (rs4 opc)) (z (lo4 opc)))
   (:determine-by determine ((bd1 mas-bd12) (bd2 mas-bd12) (r3 gpr)) ((b1 d1) (b2 d2) ix3)))
  ((r ix3) (b b1) (d d1) (e b2) (f d2))
  (list mne (derive-mas b nil d) (derive-mas e nil f) r))

(mqbase zformat-vri-a opc mne (v1 i2 &optional m3)
    "AAAAAAAA.VVVV0000.IIIIIIII.IIIIIIII.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (i2 (imm 16)) (m3 nil (imm 4))) ((ixl1 ixh1) im2 im3)))
  ((v ixl1) (i im2) (m (or im3 0)) (y (vrmsbits ixh1)))
  (list mne (derive-vra 0 v y) i m))

(mqbase zformat-vri-b opc mne (v1 i2 i3 m4)
    "AAAAAAAA.VVVV0000.IIIIIIII.JJJJJJJJ.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (i2 (imm 8)) (i3 (imm 8)) (m4 (imm 4))) ((ixl1 ixh1) im2 im3 im4)))
  ((v ixl1) (i im2) (j im3) (m im4) (y (vrmsbits ixh1)))
  (list mne (derive-vra 0 v y) i j m))

(mqbase zformat-vri-c opc mne (v1 i2 v3 m4)
    "AAAAAAAA.VVVVWWWW.IIIIIIII.IIIIIIII.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (i2 (imm 16)) (v3 vcr) (m4 (imm 4)))
                  ((ixl1 ixh1) im2 (ixl3 ixh3) im4)))
  ((v ixl1) (w ixl3) (i im2) (m im4) (y (vrmsbits ixh1 0 ixh3)))
  (list mne (derive-vra 0 v y) i (derive-vra 2 w y) m))

(mqbase zformat-vri-d opc mne (v1 v2 v3 i4 &optional m5)
    "AAAAAAAA.UUUUVVVV.WWWW0000.IIIIIIII.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (v2 vcr) (v3 vcr) (i4 (imm 8)) (m5 nil (imm 4)))
                  ((ixl1 ixh1) (ixl2 ixh2) (ixl3 ixh3) im4 im5)))
  ((u ixl1) (v ixl2) (w ixl3) (i im4) (m (or im5 0)) (y (vrmsbits ixh1 ixh2 ixh3)))
  (list mne (derive-vra 0 u y) (derive-vra 1 v y) (derive-vra 2 w y) i m))

(mqbase zformat-vri-e opc mne (v1 v2 i3 m4 m5)
    "AAAAAAAA.VVVVWWWW.IIIIIIII.IIIIMMMM.NNNNYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (v2 vcr) (i3 (imm 12)) (m4 (imm 4)) (m5 (imm 4)))
                  ((ixl1 ixh1) (ixl2 ixh2) im3 im4 im5)))
  ((v ixl1) (w ixl2) (i im3) (m im5) (n im4) (y (vrmsbits ixh1 ixh2)))
  (list mne (derive-vra 0 v y) (derive-vra 1 w y) i m n))

(mqbase zformat-vri-f opc mne (v1 v2 v3 i4 m5)
    "AAAAAAAA.UUUUVVVV.WWWW0000.MMMMIIII.IIIIYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (v2 vcr) (v3 vcr) (i4 (imm 8)) (m5 (imm 4)))
                  ((ixl1 ixh1) (ixl2 ixh2) (ixl3 ixh3) im4 im5)))
  ((u ixl1) (v ixl2) (w ixl3) (m im5) (i im4) (y (vrmsbits ixh1 ixh2 ixh3)))
  (list mne (derive-vra 0 u y) (derive-vra 1 v y) (derive-vra 2 w y) i m))

(mqbase zformat-vri-g opc mne (v1 v2 i3 i4 m5)
    "AAAAAAAA.VVVVWWWW.IIIIIIII.MMMMJJJJ.JJJJYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (v2 vcr) (i3 (imm 8)) (i4 (imm 8)) (m5 (imm 4)))
                  ((ixl1 ixh1) (ixl2 ixh2) im3 im4 im5)))
  ((v ixl1) (w ixl2) (i im4) (m im5) (j im3) (y (vrmsbits ixh1 ixh2)))
  (list mne (derive-vra 0 v y) (derive-vra 1 w y) j i m))

(mqbase zformat-vri-h opc mne (v1 i2 i3)
    "AAAAAAAA.VVVV0000.IIIIIIII.IIIIIIII.JJJJYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (i2 (imm 16)) (i3 (imm 4))) ((ixl1 ixh1) im2 im3)))
  ((v ixl1) (i im2) (j im3) (y (vrmsbits ixh1)))
  (list mne (derive-vra 0 v y) i j))

(mqbase zformat-vri-i opc mne (v1 r2 i3 m4)
    "AAAAAAAA.VVVVRRRR.00000000.MMMMIIII.IIIIYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (r2 gpr) (i3 (imm 8)) (m4 (imm 4))) ((ixl1 ixh1) ix2 im3 im4)))
  ((v ixl1) (r ix2) (m im4) (i im3) (y (vrmsbits ixh1)))
  (list mne (derive-vra 0 v y) r i m))

(mqbase zformat-vrr-a opc mne (v1 v2 &optional m3 m4 m5)
    "AAAAAAAA.VVVVWWWW.00000000.MMMMNNNN.PPPPYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (v2 vcr) (m3 nil (imm 4)) (m4 nil (imm 4)) (m5 nil (imm 4)))
                  ((ixl1 ixh1) (ixl2 ixh2) im3 im4 im5)))
  ((v ixl1) (w ixl2) (m (or im5 0)) (n (or im4 0)) (p (or im3 0)) (y (vrmsbits ixh1 ixh2)))
  (list mne (derive-vra 0 v y) (derive-vra 1 w y) p n m))

(mqbase zformat-vrr-b opc mne (v1 v2 v3 &optional m4 m5)
    "AAAAAAAA.UUUUVVVV.WWWW0000.MMMM0000.NNNNYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (v2 vcr) (v3 vcr) (m4 nil (imm 4)) (m5 nil (imm 4)))
                  ((ixl1 ixh1) (ixl2 ixh2) (ixl3 ixh3) im4 im5)))
  ((u ixl1) (v ixl2) (w ixl3) (m (or im5 0)) (n (or im4 0)) (y (vrmsbits ixh1 ixh2 ixh3)))
  (list mne (derive-vra 0 u y) (derive-vra 1 v y) (derive-vra 2 w y) n m))

(mqbase zformat-vrr-c opc mne (v1 v2 v3 &optional m4 m5 m6)
    "AAAAAAAA.UUUUVVVV.WWWW0000.MMMMNNNN.PPPPYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (v2 vcr) (v3 vcr) (m4 nil (imm 4)) (m5 nil (imm 4)) (m6 nil (imm 4)))
                  ((ixl1 ixh1) (ixl2 ixh2) (ixl3 ixh3) im4 im5 im6)))
  ((u ixl1) (v ixl2) (w ixl3) (m (or im6 0)) (n (or im5 0)) (p (or im4 0)) (y (vrmsbits ixh1 ixh2 ixh3)))
  (list mne (derive-vra 0 u y) (derive-vra 1 v y) (derive-vra 2 w y) p n m))

(mqbase zformat-vrr-d opc mne (v1 v2 v3 v4 &optional m5 m6)
    "AAAAAAAA.SSSSUUUU.VVVVMMMM.NNNN0000.WWWWYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (v2 vcr) (v3 vcr) (v4 vcr) (m5 nil (imm 4)) (m6 nil (imm 4)))
                  ((ixl1 ixh1) (ixl2 ixh2) (ixl3 ixh3) (ixl4 ixh4) im5 im6)))
  ((s ixl1) (u ixl2) (v ixl3) (m (or im5 0)) (n (or im6 0)) (w ixl4) (y (vrmsbits ixh1 ixh2 ixh3 ixh4)))
  (list mne (derive-vra 0 s y) (derive-vra 1 u y) (derive-vra 2 v y)
        (derive-vra 3 w y) m n))

(mqbase zformat-vrr-e opc mne (v1 v2 v3 v4 m5 m6)
    "AAAAAAAA.SSSSUUUU.VVVVMMMM.0000NNNN.WWWWYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (v2 vcr) (v3 vcr) (v4 vcr) (m5 (imm 4)) (m6 (imm 4)))
                  ((ixl1 ixh1) (ixl2 ixh2) (ixl3 ixh3) (ixl4 ixh4) im5 im6)))
  ((s ixl1) (u ixl2) (v ixl3) (m im6) (n im5) (w ixl4) (y (vrmsbits ixh1 ixh2 ixh3 ixh4)))
  (list mne (derive-vra 0 s y) (derive-vra 1 u y) (derive-vra 2 v y)
        (derive-vra 3 w y) n m))

(mqbase zformat-vrr-f opc mne (v1 r2 r3)
    "AAAAAAAA.VVVVRRRR.SSSS0000.00000000.0000YYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (r2 gpr) (r3 gpr)) ((ixl1 ixh1) ix2 ix3)))
  ((v ixl1) (r ix2) (s ix3) (y (vrmsbits ixh1)))
  (list mne (derive-vra 0 v y) r s))

(mqbase zformat-vrr-g opc mne (v1)
    "AAAAAAAA.0000VVVV.00000000.00000000.0000YYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr)) ((ixl1 ixh1))))
  ((v ixl1) (y (vrmsbits ixh1)))
  (list mne (derive-vra 0 v y)))

(mqbase zformat-vrr-h opc mne (v1 v2 m3)
    "AAAAAAAA.0000VVVV.WWWW0000.MMMM0000.0000YYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (v2 vcr) (m3 (imm 4))) ((ixl1 ixh1) (ixl2 ixh2) im3)))
  ((v ixl1) (w ixl2) (m im3) (y (vrmsbits ixh1 ixh2)))
  (list mne (derive-vra 0 v y) (derive-vra 1 w y) m))

(mqbase zformat-vrr-i opc mne (r1 v2 m3 &optional m4)
    "AAAAAAAA.RRRRVVVV.00000000.MMMMNNNN.0000YYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((r1 gpr) (v2 vcr) (m3 (imm 4)) (m4 nil (imm 4))) (ix1 (ixl2 ixh2) im3 im4)))
  ((r ix1) (v ixl2) (m im3) (n (or im4 0)) (y (vrmsbits 0 ixh2)))
  (list mne r (derive-vra 1 v y) m n))

(mqbase zformat-vrs-a opc mne (v1 bd2 v3 &optional m4)
    "AAAAAAAA.VVVVWWWW.BBBBDDDD.DDDDDDDD.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (bd2 mas-bd12) (v3 vcr) (m4 nil (imm 4)))
                  ((ixl1 ixh1) (b2 d2) (ixl3 ixh3) im4)))
  ((v ixl1) (w ixl3) (b b2) (d d2) (m (or im4 0)) (y (vrmsbits ixh1 0 ixh3)))
  (list mne (derive-vra 0 v y) (derive-mas b nil d) (derive-vra 2 w y) m))

(mqbase zformat-vrs-b opc mne (v1 bd2 r3 &optional m4)
    "AAAAAAAA.VVVVRRRR.BBBBDDDD.DDDDDDDD.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (bd2 mas-bd12) (r3 gpr) (m4 nil (imm 4)))
                  ((ixl1 ixh1) (b2 d2) ix3 im4)))
  ((v ixl1) (r ix3) (b b2) (d d2) (m (or im4 0)) (y (vrmsbits ixh1)))
  (list mne (derive-vra 0 v y) (derive-mas b nil d) r m))

(mqbase zformat-vrs-c opc mne (r1 bd2 v3 m4)
    "AAAAAAAA.RRRRVVVV.BBBBDDDD.DDDDDDDD.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((r1 gpr) (bd2 mas-bd12) (v3 vcr) (m4 (imm 4)))
                  (ix1 (b2 d2) (ixl3 ixh3) im4)))
  ((r ix1) (v ixl3) (b b2) (d d2) (m im4) (y (vrmsbits 0 0 ixh3)))
  (list mne r (derive-mas b nil d) (derive-vra 2 v y) m))

(mqbase zformat-vrs-d opc mne (v1 bd2 r3)
    "AAAAAAAA.0000RRRR.BBBBDDDD.DDDDDDDD.VVVVYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (bd2 mas-bd12) (r3 gpr)) ((ixl1 ixh1) (b2 d2) ix3)))
  ((r ix3) (b b2) (d d2) (v ixl1) (y (vrmsbits ixh1)))
  (list mne (derive-vra 0 v y) (derive-mas b nil d) r))

(mqbase zformat-vrv opc mne (v1 bdv2 &optional m3)
    "AAAAAAAA.VVVVWWWW.BBBBDDDD.DDDDDDDD.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (bdv2 mas-disparate) (m3 nil (imm 4)))
                  ((ixl1 ixh1) (b2 d2 ixl2 ixh2) im3)))
  ((v ixl1) (w ixl2) (b b2) (d d2) (m (or im3 0)) (y (vrmsbits ixh1 ixh2)))
  (list mne (derive-vra 0 v y) (derive-masd b (derive-vra 1 w y) d) m))

(mqbase zformat-vrx opc mne (v1 bdx2 &optional m3)
    "AAAAAAAA.VVVVXXXX.BBBBDDDD.DDDDDDDD.MMMMYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (bdx2 mas-bid12) (m3 nil (imm 4))) ((ixl1 ixh1) (b2 d2 ix2) im3)))
  ((v ixl1) (x ix2) (b b2) (d d2) (m (or im3 0)) (y (vrmsbits ixh1)))
  (list mne (derive-vra 0 v y) (derive-mas b x d) m))

(mqbase zformat-vsi opc mne (v1 bd2 i3)
    "AAAAAAAA.IIIIIIII.BBBBDDDD.DDDDDDDD.VVVVYYYY.ZZZZZZZZ"
  ((:static (a (rs8 opc)) (z (lo8 opc)))
   (:determine-by determine ((v1 vcr) (bd2 mas-bd12) (i3 (imm 8))) ((ixl1 ixh1) (b2 d2) im3)))
  ((i im3) (b b2) (d d2) (v ixl1) (y (vrmsbits ixh1)))
  (list mne (derive-vra 0 v y) (derive-mas b nil d) i))






#|

(defun zformat-e (opc)
  opc)

(defun zformat-i (opc imm)
  (masque "CCCCCCCC.NNNNNNNN"
          (c opc) (n imm)))

(defun zformat-ie (opc imm1 imm2)
  (masque "CCCCCCCC.CCCCCCCC.00000000.MMMMNNNN"
          (c opc) (m imm1) (n imm2)))

(defun zformat-mii (opc m1 ri1 ri2)
  (masque "CCCCCCCC.MMMMRRRR.RRRRRRRR.IIIIIIII.IIIIIIII.IIIIIIII"
          (c opc) (m m1) (r ri1) (i ri2)))

(defun zformat-ri-abc (opc r op iri)
  (masque "CCCCCCCC.RRRROOOO.IIIIIIII.IIIIIIII"
          (c opc) (r r) (o op) (i iri)))

(defun zformat-rie-a (opc r1 i2 m3)
  (masque "CCCCCCCC.RRRR0000.IIIIIIII.IIIIIIII.MMMM0000.DDDDDDDD"
          (c opc) (r r1) (i i2) (m m3) (d (lo8 opc))))

(defun zformat-rie-b (opc r1 r2 m3 ri4)
  (masque "CCCCCCCC.RRRRSSSS.IIIIIIII.IIIIIIII.MMMM0000.DDDDDDDD"
          (c opc) (r r1) (s r2) (i ri4) (m m3) (d (lo8 opc))))

(defun zformat-rie-c (opc r1 i2 m3 ri4)
  (masque "CCCCCCCC.RRRRMMMM.IIIIIIII.IIIIIIII.JJJJJJJJ.DDDDDDDD"
          (c (hi8 opc)) (r r1) (m m3) (i ri4) (j i2) (d (lo8 opc))))

(defun zformat-rie-de (opc r1 i2 r3)
  (masque "CCCCCCCC.RRRRSSSS.IIIIIIII.IIIIIIII.00000000.DDDDDDDD"
          (c (hi8 opc)) (r r1) (s r3) (i i2) (d (lo8 opc))))

(defun zformat-rie-f (opc r1 r2 i3 i4 i5)
  (masque "CCCCCCCC.RRRRSSSS.IIIIIIII.JJJJJJJJ.KKKKKKKK.DDDDDDDD"
          (c (hi8 opc)) (r r1) (s r2) (i i3) (j i4) (k i5) (d (lo8 opc))))

(defun zformat-rie-g (opc r1 i2 m3)
  (masque "CCCCCCCC.RRRRMMMM.IIIIIIII.IIIIIIII.00000000.DDDDDDDD"
          (c opc) (r r1) (m m3) (i i2) (d (lo8 opc))))

(defun zformat-ril-a (opc r1 op i2)
  (masque "CCCCCCCC.RRRRPPPP.IIIIIIII.IIIIIIII.IIIIIIII.IIIIIIII"
          (c opc) (r r1) (p op) (i i2) (d (lo8 opc))))

(defun zformat-ril-b (opc r1 op ri2)
  (masque "CCCCCCCC.RRRRPPPP.IIIIIIII.IIIIIIII.IIIIIIII.IIIIIIII"
          (c opc) (r r1) (p op) (i ri2) (d (lo8 opc))))

(defun zformat-ril-c (opc m1 op ri2)
  (masque "CCCCCCCC.RRRRPPPP.IIIIIIII.IIIIIIII.IIIIIIII.IIIIIIII"
          (c opc) (r m1) (p op) (i ri2) (d (lo8 opc))))

(defun zformat-ris (opc r1 i2 m3 b4 d4)
  (masque "CCCCCCCC.RRRRMMMM.BBBBDDDD.DDDDDDDD.IIIIIIII.DDDDDDDD"
          (c opc) (r r1) (m m3) (b b4) (d d4) (i i2) (d (lo8 opc))))

(defun zformat-rr (opc r1 r2)
  (masque "CCCCCCCC.RRRRSSSS"
          (c opc) (r r1) (s r2)))

(defun zformat-rrd (opc r1 r2 r3)
  (masque "CCCCCCCC.CCCCCCCC.RRRR0000.SSSSTTTT"
          (c opc) (r r1) (s r2) (t r2)))

(defun zformat-rre (opc r1 r2)
  (masque "CCCCCCCC.CCCCCCCC.00000000.RRRRSSSS"
          (c opc) (r r1) (s r2)))

(defun zformat-rrf-ab (opc r1 r2 r3 m4)
  (masque "CCCCCCCC.CCCCCCCC.RRRRMMMM.SSSSTTTT"
          (c opc) (r r3) (m m4) (s r1) (t r2)))

(defun zformat-rrf-ce (opc r1 r2 m3 m4)
  (masque "CCCCCCCC.CCCCCCCC.MMMMNNNN.RRRRSSSS"
          (c opc) (m m3) (n m4) (r r1) (s r2)))

(defun zformat-rrs (opc r1 r2 m3 b4 d4)
  (masque "CCCCCCCC.RRRRSSSS.BBBBDDDD.DDDDDDDD.MMMM0000.XXXXXXXX"
          (c (hi8 opc)) (r r1) (s r2) (b b4) (d d4) (m m3) (x (lo8 opc))))

(defun zformat-rs-a (opc r1 b2 d2 r3)
  (masque "CCCCCCCC.RRRRSSSS.BBBBDDDD.DDDDDDDD"
          (c opc) (r r1) (s r3) (b b2) (d d2)))

(defun zformat-rs-b (opc r1 b2 d2 m3)
  (masque "CCCCCCCC.RRRRMMMM.BBBBDDDD.DDDDDDDD"
          (c opc) (r r1) (m m3) (b b2) (d d2)))

(defun zformat-rsi (opc r1 ri2 r3)
  (masque "CCCCCCCC.RRRRSSSS.IIIIIIII.IIIIIIII"
          (c opc) (r r1) (s r3) (i ri2)))

(defun zformat-rsl-a (opc l1 b2 d2)
  (masque "CCCCCCCC.LLLL0000.BBBBDDDD.DDDDDDDD.00000000.XXXXXXXX"
          (c (hi8 opc)) (l l1) (b b2) (d d2) (x (lo8 opc))))

(defun zformat-rsl-b (opc l1 b2 d2)
  (masque "CCCCCCCC.LLLLLLLL.BBBBDDDD.DDDDDDDD.00000000.XXXXXXXX"
          (c (hi8 opc)) (l l1) (b b2) (d d2) (x (lo8 opc))))

(defun zformat-rsy-a (opc r1 b2 r3 dl2 dh2)
  (masque "CCCCCCCC.RRRRSSSS.BBBBDDDD.DDDDDDDD.HHHHHHHH.XXXXXXXX"
          (c (hi8 opc)) (r r1) (s r3) (b b2) (d dl2) (h dh2) (x (lo8 opc))))

(defun zformat-rsy-b (opc r1 b2 m3 dl2 dh2)
  (masque "CCCCCCCC.RRRRMMMM.BBBBDDDD.DDDDDDDD.HHHHHHHH.XXXXXXXX"
          (c (hi8 opc)) (r r1) (m m3) (b b2) (d dl2) (h dh2) (x (lo8 opc))))

(defun zformat-rx-a (opc r1 b2 d2 x2)
  (masque "CCCCCCCC.RRRRXXXX.BBBBDDDD.DDDDDDDD"
          (c opc) (r r1) (x x2) (b b2) (d d2)))

(defun zformat-rx-b (opc m1 b2 d2 x2)
  (masque "CCCCCCCC.MMMMXXXX.BBBBDDDD.DDDDDDDD"
          (c opc) (m m1) (x x2) (b b2) (d d2)))

(defun zformat-rxe (opc r1 b2 d2 x2 m3)
  (masque "CCCCCCCC.RRRRXXXX.BBBBDDDD.DDDDDDDD.MMMM0000.XXXXXXXX"
          (c (hi8 opc)) (r r1) (x x2) (b b2) (d d2) (m m3) (x (lo8 opc))))

(defun zformat-rxf (opc r1 b2 d2 x2 r3)
  (masque "CCCCCCCC.RRRRXXXX.BBBBDDDD.DDDDDDDD.SSSS0000.XXXXXAXXX"
          (c (hi8 opc)) (r r3) (x x2) (b b2) (d d2) (s r1) (x (lo8 opc))))

(defun zformat-rxy-a (opc r1 b2 dl2 dh2 x2)
  (masque "CCCCCCCC.RRRRXXXX.BBBBDDDD.DDDDDDDD.HHHHHHHH.XXXXXXXX"
          (c (hi8 opc)) (r r1) (x x2) (b b2) (d dl2) (h dh2) (x (lo8 opc))))

(defun zformat-rxy-b (opc m1 b2 dl2 dh2 x2)
  (masque "CCCCCCCC.MMMMXXXX.BBBBDDDD.DDDDDDDD.HHHHHHHH.XXXXXXXX"
          (c (hi8 opc)) (m m1) (x x2) (b b2) (d dl2) (h dh2) (x (lo8 opc))))

(defun zformat-s (opc b2 d2)
  (masque "CCCCCCCC.CCCCCCCC.BBBBDDDD.DDDDDDDD"
          (c opc) (b b2) (d d2)))

(defun zformat-si (opc b1 d1 i2)
  (masque "CCCCCCCC.IIIIIIII.BBBBDDDD.DDDDDDDD"
          (c opc) (i i2) (b b1) (d d1)))

(defun zformat-sil (opc b1 d1 i1)
  (masque "CCCCCCCC.CCCCCCCC.BBBBDDDD.DDDDDDDD.IIIIIIII.IIIIIIII"
          (c opc) (b b1) (d d1) (i i2)))

(defun zformat-siy (opc b1 dl1 dh1 i2)
  (masque "CCCCCCCC.IIIIIIII.BBBBDDDD.DDDDDDDD.HHHHHHHH.XXXXXXXX"
          (c (hi8 opc)) (i i2) (b b1) (d dl1) (h dh1) (x (lo8 opc))))

(defun zformat-smi (opc m1 ri2 b3 d3)
  (masque "CCCCCCCC.MMMM0000.BBBBDDDD.DDDDDDDD.RRRRRRRR.RRRRRRRR"
          (c opc) (m m1) (b b3) (d d3) (r ri2)))

(defun zformat-ss-a (opc b1 d1 l1 b2 d2)
  (masque "CCCCCCCC.LLLLLLLL.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
          (c opc) (l l1) (b b1) (d d1) (e b2) (f d2)))

(defun zformat-ss-b (opc b1 d1 l1 b2 d2 l2)
  (masque "CCCCCCCC.LLLLMMMM.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
          (c opc) (l l1) (m l2) (b b1) (d d1) (e b2) (f d2)))

(defun zformat-ss-c (opc b1 d1 l1 b2 d2 i3)
  (masque "CCCCCCCC.LLLLIIII.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
          (c opc) (l l1) (i i3) (b b1) (d d1) (e b2) (f d2)))

(defun zformat-ss-d (opc b1 d1 r1 b2 d2 r3)
  (masque "CCCCCCCC.RRRRSSSS.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
          (c opc) (r r1) (s r3) (b b1) (d d1) (e b2) (f d2)))

(defun zformat-ss-e (opc r1 b2 d2 r3 b4 d4)
  (masque "CCCCCCCC.RRRRSSSS.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
          (c opc) (r r1) (s r3) (b b2) (d d2) (e b4) (f d4)))

(defun zformat-ss-f (opc b1 d1 b2 d2 l2)
  (masque "CCCCCCCC.LLLLLLLL.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
          (c opc) (l l2) (b b1) (d d1) (e b2) (f d2)))

(defun zformat-sse (opc b1 d1 b2 d2 l2)
  (masque "CCCCCCCC.CCCCCCCC.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
          (c opc) (b b1) (d d1) (e b2) (f d2)))

(defun zformat-ssf (opc op b1 d1 b2 d2 r3)
  (masque "CCCCCCCC.RRRRPPPP.BBBBDDDD.DDDDDDDD.EEEEFFFF.FFFFFFFF"
          (c opc) (r r3) (p op) (b b1) (d d1) (e b2) (f d2)))

(defun zformat-vri-a (opc rxb v1 i2 m3)
  (masque "CCCCCCCC.VVVV0000.IIIIIIII.IIIIIIII.MMMMRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (i i2) (m m3) (r rxb) (d (lo8 opc))))

(defun zformat-vri-b (opc rxb v1 i2 i3 m4)
  (masque "CCCCCCCC.VVVV0000.IIIIIIII.JJJJJJJJ.MMMMRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (i i2) (j i3) (m m4) (r rxb) (d (lo8 opc))))

(defun zformat-vri-c (opc rxb v1 i2 v3 i3 m4)
  (masque "CCCCCCCC.VVVVWWWW.IIIIIIII.IIIIIIII.MMMMRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v3) (i i2) (m m4) (r rxb) (d (lo8 opc))))

(defun zformat-vri-d (opc rxb v1 v2 v3 i4 m5)
  (masque "CCCCCCCC.VVVVWWWW.XXXX0000.IIIIIIII.MMMMRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (x v3) (i i4) (m m5) (r rxb) (d (lo8 opc))))

(defun zformat-vri-e (opc rxb v1 v2 i3 m4 m5)
  (masque "CCCCCCCC.VVVVWWWW.IIIIIIII.IIIIMMMM.NNNNRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (i i3) (m m5) (n m4) (r rxb) (d (lo8 opc))))

(defun zformat-vri-f (opc rxb v1 v2 v3 i4 m5)
  (masque "CCCCCCCC.VVVVWWWW.XXXX0000.MMMMIIII.IIIIRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (x v3) (m m5) (i i4) (r rxb) (d (lo8 opc))))

(defun zformat-vri-g (opc rxb v1 v2 i3 i4 m5)
  (masque "CCCCCCCC.VVVVWWWW.IIIIIIII.MMMMJJJJ.JJJJRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (i i4) (m m5) (j i3) (r rxb) (d (lo8 opc))))

(defun zformat-vri-h (opc rxb v1 v2 i3 i4 m5)
  (masque "CCCCCCCC.VVVV0000.IIIIIIII.IIIIIIII.JJJJRRRR.DDDDDDDD"
          (c opc) (v v1) (i i2) (j i3) (r rxb) (d (lo8 opc))))

(defun zformat-vri-i (opc rxb v1 r2 i3 m4)
  (masque "CCCCCCCC.VVVVRRRR.00000000.MMMMIIII.IIIIRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (r r2) (m m4) (i i3) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-a (opc rxb v1 v2 m3 m4 m5)
  (masque "CCCCCCCC.VVVVWWWW.00000000.MMMMNNNN.PPPPRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (m m5) (n m4) (p m3) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-b (opc rxb v1 v2 v3 m4 m5)
  (masque "CCCCCCCC.VVVVWWWW.XXXX0000.MMMM0000.NNNNRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (x v3) (m m5) (n m4) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-c (opc rxb v1 v2 v3 m4 m5 m6)
  (masque "CCCCCCCC.VVVVWWWW.XXXX0000.MMMMNNNN.PPPPRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (x v3) (m m6) (n m5) (p m4) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-d (opc rxb v1 v2 v3 m4 m5 m6)
  (masque "CCCCCCCC.VVVVWWWW.XXXXMMMM.NNNN0000.VVVVRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (x v3) (m m5) (n m6) (v v4) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-e (opc rxb v1 v2 v3 v4 m5 m6)
  (masque "CCCCCCCC.VVVVWWWW.XXXXMMMM.0000NNNN.VVVVRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (x v3) (m m6) (n m5) (v v4) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-f (opc rxb v1 r2 r3)
  (masque "CCCCCCCC.VVVVRRRR.SSSS0000.00000000.0000RRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (r r2) (s r3) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-g (opc rxb v1)
  (masque "CCCCCCCC.0000VVVV.00000000.00000000.0000RRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-h (opc rxb v1 v2 m3)
  (masque "CCCCCCCC.0000VVVV.WWWW0000.MMMM0000.0000RRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (m m3) (r rxb) (d (lo8 opc))))

(defun zformat-vrr-i (opc rxb r1 v2 m3 m4)
  (masque "CCCCCCCC.RRRRVVVV.00000000.MMMMNNNN.0000RRRR.DDDDDDDD"
          (c (hi8 opc)) (r r1) (v v2) (m m3) (n m4) (r rxb) (d (lo8 opc))))

(defun zformat-vrs-a (opc rxb v1 b2 d2 v3 m4)
  (masque "CCCCCCCC.VVVVWWWW.BBBBDDDD.DDDDDDDD.MMMMRRRR.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v3) (b b2) (d d2) (m m4) (r rxb) (d (lo8 opc))))

(defun zformat-vrs-b (opc rxb v1 b2 d2 r3 m4)
  (masque "CCCCCCCC.VVVVRRRR.BBBBDDDD.DDDDDDDD.MMMMXXXX.DDDDDDDD"
          (c (hi8 opc)) (v v1) (r r3) (b b2) (d d2) (m m4) (x rxb) (d (lo8 opc))))

(defun zformat-vrs-c (opc rxb r1 b2 d2 v3 m4)
  (masque "CCCCCCCC.RRRRVVVV.BBBBDDDD.DDDDDDDD.MMMMXXXX.DDDDDDDD"
          (c (hi8 opc)) (r r1) (v v3) (b b2) (d d2) (m m4) (x rxb) (d (lo8 opc))))

(defun zformat-vrs-d (opc rxb v1 b2 d2 r3)
  (masque "CCCCCCCC.0000RRRR.BBBBDDDD.DDDDDDDD.VVVVXXXX.DDDDDDDD"
          (c (hi8 opc)) (r r3) (b b2) (d d2) (v v1) (x rxb) (d (lo8 opc))))

(defun zformat-vrv (opc rxb v1 b2 d2 v2 m3)
  (masque "CCCCCCCC.VVVVWWWW.BBBBDDDD.DDDDDDDD.MMMMXXXX.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w v2) (b b2) (d d2) (m m3) (x rxb) (d (lo8 opc))))

(defun zformat-vrx (opc rxb v1 b2 d2 x2 m3)
  (masque "CCCCCCCC.VVVVWWWW.BBBBDDDD.DDDDDDDD.MMMMXXXX.DDDDDDDD"
          (c (hi8 opc)) (v v1) (w x2) (b b2) (d d2) (m m3) (x rxb) (d (lo8 opc))))

(defun zformat-vsi (opc rxb v1 b2 d2 i3)
  (masque "CCCCCCCC.IIIIIIII.BBBBDDDD.DDDDDDDD.VVVVXXXX.DDDDDDDD"
          (c (hi8 opc)) (i i3) (b b2) (d d2) (v v1) (x rxb) (d (lo8 opc))))
|#
