;;;; base.lisp

(in-package #:specops.x86)

(defclass x86-register (register)
  ((%series :accessor reg-series
            :initform nil
            :initarg :series
            :documentation "The register's series.")))

(defclass x86-gpregister (x86-register)
  ())

(defclass x86-vcregister (x86-register)
  ())

(defclass mas-x86 (mas-based mas-indexed mas-scaling-displaced)
  () (:documentation "Memory access scheme for x86 ISA featuring base, index and scaled displacement."))

(defvar *assembler-prototype-x86*)

(defvar *x86-storage-2*
  (list :gpr #(:a :c :d :b :sp :bp :di :si :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15)
        :vcr #(:m0  :m1  :m2  :m3  :m4  :m5  :m6  :m7  :m8  :m9  :m10 :m11 :m12 :m13 :m14 :m15
               :m16 :m17 :m18 :m19 :m20 :m21 :m22 :m23 :m24 :m25 :m26 :m27 :m28 :m29 :m30 :m31)
        ))

(defvar *x86-storage*)

(setf *x86-storage*
      (list :gpr
            (list :al    (make-instance 'x86-gpregister :name :al    :width   8 :series :a    :index  0)
                  :ax    (make-instance 'x86-gpregister :name :ax    :width  16 :series :a    :index  0)
                  :eax   (make-instance 'x86-gpregister :name :eax   :width  32 :series :a    :index  0)
                  :rax   (make-instance 'x86-gpregister :name :rax   :width  64 :series :a    :index  0)
                  :cl    (make-instance 'x86-gpregister :name :cl    :width   8 :series :c    :index  1)
                  :cx    (make-instance 'x86-gpregister :name :cx    :width  16 :series :c    :index  1)
                  :ecx   (make-instance 'x86-gpregister :name :ecx   :width  32 :series :c    :index  1)
                  :rcx   (make-instance 'x86-gpregister :name :rcx   :width  64 :series :c    :index  1)
                  :dl    (make-instance 'x86-gpregister :name :dl    :width   8 :series :d    :index  2)
                  :dx    (make-instance 'x86-gpregister :name :dx    :width  16 :series :d    :index  2)
                  :edx   (make-instance 'x86-gpregister :name :edx   :width  32 :series :d    :index  2)
                  :rdx   (make-instance 'x86-gpregister :name :rdx   :width  64 :series :d    :index  2)
                  :bl    (make-instance 'x86-gpregister :name :bl    :width   8 :series :b    :index  3)
                  :bx    (make-instance 'x86-gpregister :name :bx    :width  16 :series :b    :index  3)
                  :ebx   (make-instance 'x86-gpregister :name :ebx   :width  32 :series :b    :index  3)
                  :rbx   (make-instance 'x86-gpregister :name :rbx   :width  64 :series :b    :index  3)
                  :spl   (make-instance 'x86-gpregister :name :bl    :width   8 :series :sp   :index  4)
                  :sp    (make-instance 'x86-gpregister :name :bx    :width  16 :series :sp   :index  4)
                  :esp   (make-instance 'x86-gpregister :name :ebx   :width  32 :series :sp   :index  4)
                  :rsp   (make-instance 'x86-gpregister :name :rbx   :width  64 :series :sp   :index  4)
                  :bpl   (make-instance 'x86-gpregister :name :bl    :width   8 :series :bp   :index  5)
                  :bp    (make-instance 'x86-gpregister :name :bx    :width  16 :series :bp   :index  5)
                  :ebp   (make-instance 'x86-gpregister :name :ebx   :width  32 :series :bp   :index  5)
                  :rbp   (make-instance 'x86-gpregister :name :rbx   :width  64 :series :bp   :index  5)
                  :dil   (make-instance 'x86-gpregister :name :bl    :width   8 :series :di   :index  6)
                  :di    (make-instance 'x86-gpregister :name :bx    :width  16 :series :di   :index  6)
                  :edi   (make-instance 'x86-gpregister :name :ebx   :width  32 :series :di   :index  6)
                  :rdi   (make-instance 'x86-gpregister :name :rbx   :width  64 :series :di   :index  6)
                  :sil   (make-instance 'x86-gpregister :name :bl    :width   8 :series :si   :index  7)
                  :si    (make-instance 'x86-gpregister :name :bx    :width  16 :series :si   :index  7)
                  :rsi   (make-instance 'x86-gpregister :name :ebx   :width  32 :series :si   :index  7)
                  :rsi   (make-instance 'x86-gpregister :name :rbx   :width  64 :series :si   :index  7)
                  :r8b   (make-instance 'x86-gpregister :name :r8b   :width   8 :series :r8   :index  8)
                  :r8w   (make-instance 'x86-gpregister :name :r8w   :width  16 :series :r8   :index  8)
                  :r8d   (make-instance 'x86-gpregister :name :r8d   :width  32 :series :r8   :index  8)
                  :r8    (make-instance 'x86-gpregister :name :r8    :width  64 :series :r8   :index  8)
                  :r9b   (make-instance 'x86-gpregister :name :r9b   :width   8 :series :r9   :index  9)
                  :r9w   (make-instance 'x86-gpregister :name :r9w   :width  16 :series :r9   :index  9)
                  :r9d   (make-instance 'x86-gpregister :name :r9d   :width  32 :series :r9   :index  9)
                  :r9    (make-instance 'x86-gpregister :name :r9    :width  64 :series :r9   :index  9)
                  :r10b  (make-instance 'x86-gpregister :name :r10b  :width   8 :series :r10  :index 10)
                  :r10w  (make-instance 'x86-gpregister :name :r10w  :width  16 :series :r10  :index 10)
                  :r10d  (make-instance 'x86-gpregister :name :r10d  :width  32 :series :r10  :index 10)
                  :r10   (make-instance 'x86-gpregister :name :r10   :width  64 :series :r10  :index 10)
                  :r11b  (make-instance 'x86-gpregister :name :r11b  :width   8 :series :r11  :index 11)
                  :r11w  (make-instance 'x86-gpregister :name :r11w  :width  16 :series :r11  :index 11)
                  :r11d  (make-instance 'x86-gpregister :name :r11d  :width  32 :series :r11  :index 11)
                  :r11   (make-instance 'x86-gpregister :name :r11   :width  64 :series :r11  :index 11)
                  :r12b  (make-instance 'x86-gpregister :name :r12b  :width   8 :series :r12  :index 12)
                  :r12w  (make-instance 'x86-gpregister :name :r12w  :width  16 :series :r12  :index 12)
                  :r12d  (make-instance 'x86-gpregister :name :r12d  :width  32 :series :r12  :index 12)
                  :r12   (make-instance 'x86-gpregister :name :r12   :width  64 :series :r12  :index 12)
                  :r13b  (make-instance 'x86-gpregister :name :r13b  :width   8 :series :r13  :index 13)
                  :r13w  (make-instance 'x86-gpregister :name :r13w  :width  16 :series :r13  :index 13)
                  :r13d  (make-instance 'x86-gpregister :name :r13d  :width  32 :series :r13  :index 13)
                  :r13   (make-instance 'x86-gpregister :name :r13   :width  64 :series :r13  :index 13)
                  :r14b  (make-instance 'x86-gpregister :name :r14b  :width   8 :series :r14  :index 14)
                  :r14w  (make-instance 'x86-gpregister :name :r14w  :width  16 :series :r14  :index 14)
                  :r14d  (make-instance 'x86-gpregister :name :r14d  :width  32 :series :r14  :index 14)
                  :r14   (make-instance 'x86-gpregister :name :r14   :width  64 :series :r14  :index 14)
                  :r15b  (make-instance 'x86-gpregister :name :r15b  :width   8 :series :r15  :index 15)
                  :r15w  (make-instance 'x86-gpregister :name :r15w  :width  16 :series :r15  :index 15)
                  :r15d  (make-instance 'x86-gpregister :name :r15d  :width  32 :series :r15  :index 15)
                  :r15   (make-instance 'x86-gpregister :name :r15   :width  64 :series :r15  :index 15))
            :vcr
            (list :xmm0  (make-instance 'x86-vcregister :name :xmm0  :width 128 :series :xm0  :index  0)
                  :ymm0  (make-instance 'x86-vcregister :name :ymm0  :width 256 :series :xm0  :index  0)
                  :zmm0  (make-instance 'x86-vcregister :name :zmm0  :width 512 :series :xm0  :index  0)
                  :xmm1  (make-instance 'x86-vcregister :name :xmm1  :width 128 :series :xm1  :index  1)
                  :ymm1  (make-instance 'x86-vcregister :name :ymm1  :width 256 :series :xm1  :index  1)
                  :zmm1  (make-instance 'x86-vcregister :name :zmm1  :width 512 :series :xm1  :index  1)
                  :xmm2  (make-instance 'x86-vcregister :name :xmm2  :width 128 :series :xm2  :index  2)
                  :ymm2  (make-instance 'x86-vcregister :name :ymm2  :width 256 :series :xm2  :index  2)
                  :zmm2  (make-instance 'x86-vcregister :name :zmm2  :width 512 :series :xm2  :index  2)
                  :xmm3  (make-instance 'x86-vcregister :name :xmm3  :width 128 :series :xm3  :index  3)
                  :ymm3  (make-instance 'x86-vcregister :name :ymm3  :width 256 :series :xm3  :index  3)
                  :zmm3  (make-instance 'x86-vcregister :name :zmm3  :width 512 :series :xm3  :index  3)
                  :xmm4  (make-instance 'x86-vcregister :name :xmm4  :width 128 :series :xm4  :index  4)
                  :ymm4  (make-instance 'x86-vcregister :name :ymm4  :width 256 :series :xm4  :index  4)
                  :zmm4  (make-instance 'x86-vcregister :name :zmm4  :width 512 :series :xm4  :index  4)
                  :xmm5  (make-instance 'x86-vcregister :name :xmm5  :width 128 :series :xm5  :index  5)
                  :ymm5  (make-instance 'x86-vcregister :name :ymm5  :width 256 :series :xm5  :index  5)
                  :zmm5  (make-instance 'x86-vcregister :name :zmm5  :width 512 :series :xm5  :index  5)
                  :xmm6  (make-instance 'x86-vcregister :name :xmm6  :width 128 :series :xm6  :index  6)
                  :ymm6  (make-instance 'x86-vcregister :name :ymm6  :width 256 :series :xm6  :index  6)
                  :zmm6  (make-instance 'x86-vcregister :name :zmm6  :width 512 :series :xm6  :index  6)
                  :xmm7  (make-instance 'x86-vcregister :name :xmm7  :width 128 :series :xm7  :index  7)
                  :ymm7  (make-instance 'x86-vcregister :name :ymm7  :width 256 :series :xm7  :index  7)
                  :zmm7  (make-instance 'x86-vcregister :name :zmm7  :width 512 :series :xm7  :index  7)
                  :xmm8  (make-instance 'x86-vcregister :name :xmm8  :width 128 :series :xm8  :index  8)
                  :ymm8  (make-instance 'x86-vcregister :name :ymm8  :width 256 :series :xm8  :index  8)
                  :zmm8  (make-instance 'x86-vcregister :name :zmm8  :width 512 :series :xm8  :index  8)
                  :xmm9  (make-instance 'x86-vcregister :name :xmm9  :width 128 :series :xm9  :index  9)
                  :ymm9  (make-instance 'x86-vcregister :name :ymm9  :width 256 :series :xm9  :index  9)
                  :zmm9  (make-instance 'x86-vcregister :name :zmm9  :width 512 :series :xm9  :index  9)
                  :xmm10 (make-instance 'x86-vcregister :name :xmm10 :width 128 :series :xm10 :index 10)
                  :ymm10 (make-instance 'x86-vcregister :name :ymm10 :width 256 :series :xm10 :index 10)
                  :zmm10 (make-instance 'x86-vcregister :name :zmm10 :width 512 :series :xm10 :index 10)
                  :xmm11 (make-instance 'x86-vcregister :name :xmm11 :width 128 :series :xm11 :index 11)
                  :ymm11 (make-instance 'x86-vcregister :name :ymm11 :width 256 :series :xm11 :index 11)
                  :zmm11 (make-instance 'x86-vcregister :name :zmm11 :width 512 :series :xm11 :index 11)
                  :xmm12 (make-instance 'x86-vcregister :name :xmm12 :width 128 :series :xm12 :index 12)
                  :ymm12 (make-instance 'x86-vcregister :name :ymm12 :width 256 :series :xm12 :index 12)
                  :zmm12 (make-instance 'x86-vcregister :name :zmm12 :width 512 :series :xm12 :index 12)
                  :xmm13 (make-instance 'x86-vcregister :name :xmm13 :width 128 :series :xm13 :index 13)
                  :ymm13 (make-instance 'x86-vcregister :name :ymm13 :width 256 :series :xm13 :index 13)
                  :zmm13 (make-instance 'x86-vcregister :name :zmm13 :width 512 :series :xm13 :index 13)
                  :xmm14 (make-instance 'x86-vcregister :name :xmm14 :width 128 :series :xm14 :index 14)
                  :ymm14 (make-instance 'x86-vcregister :name :ymm14 :width 256 :series :xm14 :index 14)
                  :zmm14 (make-instance 'x86-vcregister :name :zmm14 :width 512 :series :xm14 :index 14)
                  :xmm15 (make-instance 'x86-vcregister :name :xmm15 :width 128 :series :xm15 :index 15)
                  :ymm15 (make-instance 'x86-vcregister :name :ymm15 :width 256 :series :xm15 :index 15)
                  :zmm15 (make-instance 'x86-vcregister :name :zmm15 :width 512 :series :xm15 :index 15)
                  :xmm16 (make-instance 'x86-vcregister :name :xmm16 :width 128 :series :xm16 :index 16)
                  :ymm16 (make-instance 'x86-vcregister :name :ymm16 :width 256 :series :xm16 :index 16)
                  :zmm16 (make-instance 'x86-vcregister :name :zmm16 :width 512 :series :xm16 :index 16)
                  :xmm17 (make-instance 'x86-vcregister :name :xmm17 :width 128 :series :xm17 :index 17)
                  :ymm17 (make-instance 'x86-vcregister :name :ymm17 :width 256 :series :xm17 :index 17)
                  :zmm17 (make-instance 'x86-vcregister :name :zmm17 :width 512 :series :xm17 :index 17)
                  :xmm18 (make-instance 'x86-vcregister :name :xmm18 :width 128 :series :xm18 :index 18)
                  :ymm18 (make-instance 'x86-vcregister :name :ymm18 :width 256 :series :xm18 :index 18)
                  :zmm18 (make-instance 'x86-vcregister :name :zmm18 :width 512 :series :xm18 :index 18)
                  :xmm19 (make-instance 'x86-vcregister :name :xmm19 :width 128 :series :xm19 :index 19)
                  :ymm19 (make-instance 'x86-vcregister :name :ymm19 :width 256 :series :xm19 :index 19)
                  :zmm19 (make-instance 'x86-vcregister :name :zmm19 :width 512 :series :xm19 :index 19)
                  :xmm20 (make-instance 'x86-vcregister :name :xmm20 :width 128 :series :xm20 :index 20)
                  :ymm20 (make-instance 'x86-vcregister :name :ymm20 :width 256 :series :xm20 :index 20)
                  :zmm20 (make-instance 'x86-vcregister :name :zmm20 :width 512 :series :xm20 :index 20)
                  :xmm21 (make-instance 'x86-vcregister :name :xmm21 :width 128 :series :xm21 :index 21)
                  :ymm21 (make-instance 'x86-vcregister :name :ymm21 :width 256 :series :xm21 :index 21)
                  :zmm21 (make-instance 'x86-vcregister :name :zmm21 :width 512 :series :xm21 :index 21)
                  :xmm22 (make-instance 'x86-vcregister :name :xmm22 :width 128 :series :xm22 :index 22)
                  :ymm22 (make-instance 'x86-vcregister :name :ymm22 :width 256 :series :xm22 :index 22)
                  :zmm22 (make-instance 'x86-vcregister :name :zmm22 :width 512 :series :xm22 :index 22)
                  :xmm23 (make-instance 'x86-vcregister :name :xmm23 :width 128 :series :xm23 :index 23)
                  :ymm23 (make-instance 'x86-vcregister :name :ymm23 :width 256 :series :xm23 :index 23)
                  :zmm23 (make-instance 'x86-vcregister :name :zmm23 :width 512 :series :xm23 :index 23)
                  :xmm24 (make-instance 'x86-vcregister :name :xmm24 :width 128 :series :xm24 :index 24)
                  :ymm24 (make-instance 'x86-vcregister :name :ymm24 :width 256 :series :xm24 :index 24)
                  :zmm24 (make-instance 'x86-vcregister :name :zmm24 :width 512 :series :xm24 :index 24)
                  :xmm25 (make-instance 'x86-vcregister :name :xmm25 :width 128 :series :xm25 :index 25)
                  :ymm25 (make-instance 'x86-vcregister :name :ymm25 :width 256 :series :xm25 :index 25)
                  :zmm25 (make-instance 'x86-vcregister :name :zmm25 :width 512 :series :xm25 :index 25)
                  :xmm26 (make-instance 'x86-vcregister :name :xmm26 :width 128 :series :xm26 :index 26)
                  :ymm26 (make-instance 'x86-vcregister :name :ymm26 :width 256 :series :xm26 :index 26)
                  :zmm26 (make-instance 'x86-vcregister :name :zmm26 :width 512 :series :xm26 :index 26)
                  :xmm27 (make-instance 'x86-vcregister :name :xmm27 :width 128 :series :xm27 :index 27)
                  :ymm27 (make-instance 'x86-vcregister :name :ymm27 :width 256 :series :xm27 :index 27)
                  :zmm27 (make-instance 'x86-vcregister :name :zmm27 :width 512 :series :xm27 :index 27)
                  :xmm28 (make-instance 'x86-vcregister :name :xmm28 :width 128 :series :xm28 :index 28)
                  :ymm28 (make-instance 'x86-vcregister :name :ymm28 :width 256 :series :xm28 :index 28)
                  :zmm28 (make-instance 'x86-vcregister :name :zmm28 :width 512 :series :xm28 :index 28)
                  :xmm29 (make-instance 'x86-vcregister :name :xmm29 :width 128 :series :xm29 :index 29)
                  :ymm29 (make-instance 'x86-vcregister :name :ymm29 :width 256 :series :xm29 :index 29)
                  :zmm29 (make-instance 'x86-vcregister :name :zmm29 :width 512 :series :xm29 :index 29)
                  :xmm30 (make-instance 'x86-vcregister :name :xmm30 :width 128 :series :xm30 :index 30)
                  :ymm30 (make-instance 'x86-vcregister :name :ymm30 :width 256 :series :xm30 :index 30)
                  :zmm30 (make-instance 'x86-vcregister :name :zmm30 :width 512 :series :xm30 :index 30)
                  :xmm31 (make-instance 'x86-vcregister :name :xmm31 :width 128 :series :xm31 :index 31)
                  :ymm31 (make-instance 'x86-vcregister :name :ymm31 :width 256 :series :xm31 :index 31)
                  :zmm31 (make-instance 'x86-vcregister :name :zmm31 :width 512 :series :xm31 :index 31))
            
            ;; :es   (make-instance 'x86-sgregister :name :es   :width 64  :series :s   :index 0)
            ;; :cs   (make-instance 'x86-sgregister :name :cs   :width 8   :series :s   :index 1)
            ;; :ss   (make-instance 'x86-sgregister :name :ss   :width 16  :series :s   :index 2)
            ;; :ds   (make-instance 'x86-sgregister :name :ds   :width 32  :series :s   :index 3)
            ;; :fs   (make-instance 'x86-sgregister :name :fs   :width 64  :series :s   :index 4)
            ;; :gs   (make-instance 'x86-sgregister :name :gs   :width 64  :series :s   :index 5)
            
            ))

(defun prefix-rex (&optional wide rex iex rbex)
  (masque "0100.WRXB"
          (w (if wide 1 0))   ;; bit 4 indicates use of 64-bit register(s)
          (r (if  rex 1 0))   ;; bit 5 indicates extension of the MODRM.reg field
          (x (if  iex 1 0))   ;; bit 6 indicates extension of the SIB.index field
          (b (if rbex 1 0)))) ;; bit 7 indicates extension of MODRM.rm or SIB.base field

(defun prefix-vex (long-prefix &key r x b map wide (length 128) third-op prefix)
  (if long-prefix
      (masque "11000100.RXBmmmmm.WvvvvLpp"
              (r (if (zerop r) 1 0)) ;; bit 1.0 indicates inverse extension of the MODRM.reg field
              (x (if (zerop x) 1 0)) ;; bit 1.1 indicates inverse extension of the SIB.index field
              (b (if (zerop b) 1 0)) ;; bit 1.2 indicates inverse extension of MODRM.rm or SIB.base field
              (m (if (> 4 map)  ;; map field contains operator extension index 1, 2 or 3
                     map (case map (#x0F #b01) (#x0F35 #b10) (#x0F38 #b11) (t 0))))
              (w (if wide 1 0)) ;; bit 2.0 indicates 64-bit width of integer operand
              (v (if third-op (flipbits third-op) 0)) ;; v field addresses third operand (reversed)
              (l (if (= length 256) 1 0)) ;; bit 2.5 denotes use of 256-bit registers
              ;; prefix field expresses opcode prefix if needed
              (p (case prefix (#x66 #b01) (#xF3 #b10) (#xF2 #b11) (t 0))))
      (masque "11000101.WvvvvLpp"
              (w (if wide 1 0)) ;; bit 2.0 indicates 64-bit width of integer operand
              (v (if third-op (flipbits third-op) 0)) ;; v field addresses third operand (reversed)
              (l (if (= length 256) 1 0)) ;; bit 2.5 denotes use of 256-bit registers
              ;; prefix field expresses opcode prefix if needed
              (p (case prefix (#x66 #b01) (#xF3 #b10) (#xF2 #b11) (t 0))))))

(defun prefix-evex (&key rex iex rbex map wide (length 128)
                      third-op prefix merge br-control op3-extend)
  (masque "01100010.RXBŔ00mm.Wvvvv1pp.ZLLḂṼaaa"
          (r (if  rex 0 1)) ;; bit 1.0 indicates inverse extension of the MODRM.reg field
          (x (if  iex 0 1)) ;; bit 1.1 indicates inverse extension of the SIB.index field
          (b (if rbex 0 1)) ;; bit 1.2 indicates inverse extension of MODRM.rm or SIB.base field
          (ŕ (if rbex 0 1)) ;; bit 1.3 indicates further inverse extension of MODRM.reg field
          (m (if (> 4 map)  ;; map field contains operator extension index 1, 2 or 3
                 map (case map (#x0F #b01) (#x0F35 #b10) (#x0F38 #b11) (t 0))))
          (w (if wide 1 0)) ;; bit 2.0 indicates 64-bit width of integer operand
          (v (if third-op (flipbits third-op) 0)) ;; v field addresses third operand (reversed)
          ;; prefix field expresses opcode prefix if needed
          (p (case prefix (#x66 #b01) (#xF3 #b10) (#xF2 #b11) (t 0)))
          (z (if merge 1 0)) ;; bit 3.0 toggles merge function
          (l (+ (if (= length 512) #b10 0) (if (= length 256) #b01 0)))
          (ḃ (if br-control 1 0))
          ;; bits 3.1-2 determine use of 256-bit or 512-bit registers
          (ṽ (if op3-extend 1 0))
          (a 0)))


(defun determine-modrm (op0 op1)
  ;; (print (list :oo op0 op1))
  (masque "MMRRROOO"
          (m (if (typep op1 'x86-register)
                 #b11 (if (or (not (x86mac-displ op1)) (zerop (x86mac-displ op1)))
                          ;; no displacement and a BP register base forces use of a zero 8-bit disp.
                          (if (eq :bp (reg-series (mac-bsreg op1)))
                              #b01 #b00)
                          (if (> 256 (x86mac-displ op1)) ;; choose 8 or 32-bit displacement
                              #b01 #b10))))
          (r (if (numberp op0)
                 op0 (if (not (typep op0 'x86-register))
                         0 (logand #b111 (reg-index op0)))))
          (o (if (numberp op1)
                 op1 (if (typep op1 'x86-register)
                         (logand #b111 (reg-index op1))
                         (if (not (typep op1 'x86-mem-access))
                             0 (if (not (mac-bsreg op1))
                                   #b101 ;; determines 32-bit displacement-only mode
                                   (if (x86mac-inreg op1) ;; #b100 determines SIB addressing
                                       #b100 (logand #b111 (reg-index (mac-bsreg op1)))))))))))

(defun determine-sib (mac)
  (if (or (not (typep mac 'x86-mem-access))
          (not (x86mac-inreg mac)))
      nil (masque "SSIIIBBB"
                  (s (x86mac-scale mac))
                  (i (logand #b111 (reg-index (x86mac-inreg mac))))
                  (b (logand #b111 (reg-index (mac-bsreg mac)))))))

(defun determine-pfrex (wide op0 op1)
  (masque "0100.WRXB"
          (w (if wide 1 (if (or (and (typep op0 'x86-register)
                                     (= 64 (reg-width op0)))
                                (and (typep op1 'x86-register)
                                     (= 64 (reg-width op1))))
                            1 0)))
          (r (if (not (typep op0 'x86-register))
                 0 (ash (reg-index op0) -3)))
          (x (if (not (and (typep op1 'x86-mem-access)
                           (mac-bsreg op1) (x86mac-inreg op1)))
                 0 (ash (reg-index (x86mac-inreg op1)) -3)))
          (b (if (typep op0 'x86-register)
                 (ash (reg-index op0) -3)
                 (if (not (and (typep op0 'x86-mem-access)
                               (mac-bsreg op1)))
                     0 (ash (reg-index (mac-bsreg op1)) -3))))))

(defun determine-pfvex (op0 op1 op2 &key long map prefix)
  (let ((sib-index (and (typep op1 'x86-mem-access)
                        (ash (x86mac-inreg op1) -3))))
    (if long
        (masque "11000100.RXBmmmmm.WvvvvLpp"
                (r (if (zerop (logand #b1000 (reg-index op0)))               1 0))
                ;; bit 1.0 indicates inverse extension of the MODRM.reg field: op0 index upper bit
                (x (if (or (not sib-index) (zerop sib-index))                1 0))
                ;; bit 1.1 indicates inverse extension of the SIB.index field: index register upper bit
                (b (if (or (not sib-index) (zerop (ash (mac-bsreg op1) -3))) 1 0))
                ;; bit 1.2 indicates inverse extension of MODRM.rm or SIB.base field: base register upper bit
                (m (case map (#x0F #b01) (#x0F35 #b10) (#x0F38 #b11) (t map)))
                ;; map field contains operator extension index 1-3, may be mapped to codes or direct
                (w (if (= 64 (reg-width op0)) 1 0)) ;; bit 2.0 indicates 64-bit width of integer operand
                (v (if op2 (flipbits (reg-index op2)) 0)) ;; v field addresses third operand (reversed)
                (l (if (= 256 (reg-width op0)) 1 0)) ;; bit 2.5 denotes use of 256-bit registers
                ;; prefix field expresses opcode prefix if needed
                (p (case prefix (#x66 #b01) (#xF3 #b10) (#xF2 #b11) (t 0))))
        (masque "11000101.WvvvvLpp"
                (w (if (= 64 (reg-width op0)) 1 0)) ;; bit 2.0 indicates 64-bit width of integer operand
                (v (if op2 (flipbits (reg-index op2)) 0))
                ;; v field addresses third operand (reversed)
                (l (if (= 256 (reg-width op0)) 1 0)) ;; bit 2.5 denotes use of 256-bit registers
                ;; prefix field expresses opcode prefix if needed
                (p (case prefix (#x66 #b01) (#xF3 #b10) (#xF2 #b11) (t 0)))))))

(defun determine-pfevex (op0 op1 op2 &key map br-control prefix merge)
  (let ((sib-index (or (and (typep op1 'x86-mem-access)
                            (ash (x86mac-inreg op1) -3)))))
    (masque "01100010.RXBŔ00mm.Wvvvv1pp.ZllḂṼaaa"
            (r (if (zerop (logand #b1000 (reg-index op0)))               1 0))
            ;; bit 1.0 indicates inverse extension of the MODRM.reg field: op0 index upper bit
            (x (if (or (not sib-index) (zerop sib-index))                1 0))
            ;; bit 1.1 indicates inverse extension of the SIB.index field: index register upper bit
            (b (if (or (not sib-index) (zerop (ash (mac-bsreg op1) -3))) 1 0))
            ;; bit 1.2 indicates inverse extension of MODRM.rm or SIB.base field: base register upper bit
            (ŕ (if (zerop (logand #b10000 (reg-index op0)))              1 0))
            ;; bit 1.3 indicates further inverse extension of MODRM.reg field
            (m (case map (#x0F #b01) (#x0F35 #b10) (#x0F38 #b11) (t map)))
            ;; map field contains operator extension index 1-3, may be mapped to codes or direct
            (w (if (= 64 (reg-width op0)) 1 0)) ;; bit 2.0 indicates 64-bit width of integer operand
            (v (if op2 (flipbits (logand #b1111 (reg-index op2))) 0))
            ;; v field addresses third operand (reversed)
            ;; prefix field expresses opcode prefix if needed
            (p (case prefix (#x66 #b01) (#xF3 #b10) (#xF2 #b11) (t 0)))
            (z (if merge 1 0)) ;; bit 3.0 toggles merge function
            (l (if (= 512 (reg-width op0)) #b10 (if (= 256 (reg-width op0)) #b01 0)))
            ;; bit 3.1-2 determine use of 256 or 512-bit registers
            (ḃ (if br-control 1 0))
            ;; prefix field expresses opcode prefix if needed
            (ṽ (if (zerop (logand #b10000 (reg-index op2))) 1 0))
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
             :initform   nil
             :allocation :class
             :initarg    :domains)))

(defmethod initialize-instance :after ((assembler assembler-x86) &key)
  (derive-domains assembler (t (:gpr  8))
                  (:x86-64 (:gpr 16))
                  (:avx    (:vcr  8))
                  (:avx2   (:vcr 16))
                  (:avx512 (:vcr 32))))

(defmacro defop (symbol operands lexicon &rest specs)
  (let* ((provisions (rest (assoc :provisions specs)))
         (ins-part (rest (assoc :instructions specs)))
         (ins-meta (rest (assoc :with ins-part)))
         (opcon-process (symbol-function (rest (assoc :opcons ins-meta))))
         (ins-main))
    
    (let ((ins-list ins-part))
      (loop :while (and ins-list (not ins-main))
            :do (if (keywordp (caar ins-list))
                    (setf ins-list (rest ins-list))
                    (setf ins-main ins-list))))
    
    `(setf (gethash ,(intern (string symbol) "KEYWORD") ,lexicon)
           (lambda ,operands
             (symbol-macrolet ,provisions
               (cond ,@(loop :for in :in ins-main
                             :collect (destructuring-bind (manifest conditions) in
                                        (list (cons 'and (funcall opcon-process operands conditions
                                                                  (rest (assoc :priority ins-meta))))
                                              (cons 'join manifest))))
                     (t "Invalid operation.")))))))

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
;;                                      (mem-case `(and (typep ,o 'x86-mem-access)
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

(defmethod qualify-ops ((assembler assembler-x86) operands form order)
  (declare (ignore assembler))
  ;; (print (list :or order))
  (loop :for index :in order
        :append (let ((f (nth index form)) (o (nth index operands)))
                  (if (integerp f)
                      `((and (integerp ,o) (= ,f ,o)))
                      (if (not f)
                          nil (destructuring-bind (type qualifier) f
                                (symbol-macrolet
                                    ((gpr-case `(and (typep ,o 'x86-gpregister)
                                                     ,(typecase qualifier
                                                        (keyword `(eq ,qualifier (reg-name  ,o)))
                                                        (integer `(=  ,qualifier (reg-width ,o))))))
                                     (vcr-case `(and (typep ,o 'x86-vcregister)
                                                     ,(typecase qualifier
                                                        (keyword `(eq ,qualifier (reg-name  ,o)))
                                                        (integer `(=  ,qualifier (reg-width ,o))))))
                                     (mem-case `(and (typep ,o 'x86-mem-access)
                                                     ,(typecase qualifier
                                                        (keyword `(eq ,qualifier (reg-name  ,o)))
                                                        (integer `(=  ,qualifier (reg-width ,o)))))))
                                  (case type
                                    (:gpr (list gpr-case))
                                    (:vcr (list vcr-case))
                                    (:mem (list mem-case))
                                    (:gxm `((or ,gpr-case ,mem-case)))
                                    (:vxm `((or ,vcr-case ,mem-case)))
                                    (:imm `((and (typep ,o 'integer)
                                                 (< ,o ,(expt 2 qualifier)))))))))))))

(defmethod specify-ops ((assembler assembler-x86) asm-sym op-symbol operands items)
  "A simple scheme for implementing operations - the (specop) content is directly placed within functions in the lexicon hash table."
  (let* ((params (if (not (and (listp (first items)) (listp (caar items))
                               (keywordp (caaar items))))
                     nil (first items)))
         (operations (if (not params) items (rest items)))
         (provisions (rest (assoc :provisions params))))
    ;; (print (list :op operations params))
    
    `(setf (gethash ,(intern (string op-symbol) "KEYWORD")
                    (asm-lexicon ,asm-sym))
           (lambda ,operands
             (symbol-macrolet ,provisions
               (cond ,@(loop :for op :in operations
                             :collect (destructuring-bind (manifest conditions) op
                                        (list (cons 'and (qualify-ops assembler operands conditions
                                                                      (rest (assoc :priority params))))
                                              (cons 'join manifest))))
                     (t "Invalid operation.")))))))

(let ((gpr-series-names) (vcr-series-names))
  (dotimes (n (/ (length (getf *x86-storage* :gpr)) 8))
    (push (reg-series (nth (1+ (* 8 n)) (getf *x86-storage* :gpr))) gpr-series-names))
  
  (dotimes (n (/ (length (getf *x86-storage* :vcr)) 6))
    (push (reg-series (nth (1+ (* 6 n)) (getf *x86-storage* :vcr))) vcr-series-names))

  (setf gpr-series-names (reverse gpr-series-names)
        vcr-series-names (reverse vcr-series-names))

  (defun series-index (type series-id)
    (case type (:gpr (position series-id gpr-series-names))
          (:vcr (position series-id vcr-series-names)))))

(defmethod of-storage ((assembler assembler-x86) &rest params)
  (destructuring-bind (type &key series width series-index) params
    ;; (print (list :in width type series-index))
    (let* ((series-index (or series-index (if (not series) 0 (series-index type series))))
           (type-list (getf *x86-storage* type))
           (storage-index (+ 1 (* 4 series-index) (* 2 width))))
      ;; (print (list :ss storage-index width type))
      (if (not (member series-index (assoc type (asm-domains assembler))))
          nil (values (nth storage-index type-list)
                      (+ series-index width))))))

(defmethod locate ((assembler assembler-x86) items)
  (let ((domains (copy-tree (asm-domains assembler)))
        (bound (loop :for item :in items :when (member :bind item :test #'eq) :collect item))
        (unbound (loop :for item :in items :unless (member :bind item :test #'eq) :collect item)))
    (print (list :bu bound unbound))
    (append (loop :for item :in bound
                  :collect (destructuring-bind (symbol type &rest params) item
                             ;; (print (list :dom domains))
                             (list symbol (let ((out-index (series-index type (getf params :bind))))
                                            (setf (rest (assoc type domains))
                                                  (remove out-index (rest (assoc type domains))))
                                            out-index))))
            (loop :for item :in unbound
                  :collect (destructuring-bind (symbol type &rest params) item
                             (list symbol (let ((random-index (nth (random (length (rest (assoc type domains))))
                                                                   (rest (assoc type domains)))))
                                            (setf (rest (assoc type domains))
                                                  (remove random-index (rest (assoc type domains))))
                                            random-index)))))))

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

(defmethod compose ((assembler assembler-x86) params expression)
  (destructuring-bind (op &rest props) expression
    (if (listp op)
        (loop :for item :in expression :collect (compose assembler params item))
        (if (not (keywordp op))
            (compose assembler params (macroexpand op))
            (let* ((width (if (not (keywordp (first props)))
                              nil (position (first props) #(:b :w :d :q) :test #'eq)))
                   (props (if (not width) props (rest props)))
                   (bindings (rest (assoc :bindings params))))
              (print (list :bi bindings width))
              (apply (gethash op (asm-lexicon assembler))
                     (loop :for p :in props
                           :collect (typecase p
                                      (symbol (of-storage assembler :gpr :width width
                                                          :series-index (second (assoc p bindings))))
                                      (t p)))))))))

#|
(assemble *assembler-prototype-x86*
 (:with (:store (abc :gpr) (def :gpr :bind :a)))
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
