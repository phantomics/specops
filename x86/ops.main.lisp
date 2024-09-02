;;;; ops.main.lisp

(in-package #:specops.x86)

;; Intel PDF pg. 588 has guide to opcode table abbreviations

(specops mov (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rix (logand #b111 (reg-index op0))) (rex (determine-pfrex nil op0 op1))
    (rex.w (determine-pfrex t op0 op1)) (modz0 (determine-modrm 0 op0))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)))
   (:priority                  0           1))
  ((         #x88 mod10 sib0) ((:gxm    8) (:gpr    8)))
  ((rex      #x88 mod10 sib0) ((:gxm    8) (:gpr    8)))
  ((         #x89 mod10 sib0) ((:gxm   16) (:gpr   16)))
  ((         #x89 mod10 sib0) ((:gxm   32) (:gpr   32)))
  ((rex.w    #x89 mod10 sib0) ((:gxm   64) (:gpr   64)))
  ((         #x8A mod10 sib0) ((:gpr    8) (:gxm    8)))
  ((rex      #x8A mod10 sib0) ((:gpr    8) (:gxm    8)))
  ((         #x8B mod10 sib0) ((:gpr   16) (:gxm   16)))
  ((         #x8B mod10 sib0) ((:gpr   32) (:gxm   32)))
  ((rex.w    #x8B mod10 sib0) ((:gpr   64) (:gxm   64)))
  ;; segment register instructions ommitted
  ((         #xA0           ) ((:gpr  :al) (:off    8)))
  ((rex.w    #xA0           ) ((:gpr  :al) (:off    8)))
  ((         #xA1           ) ((:gpr  :ax) (:off   16)))
  ((         #xA1           ) ((:gpr :eax) (:off   32)))
  ((rex.w    #xA1           ) ((:gpr :rax) (:off   64)))
  ((         #xA2           ) ((:off    8) (:gpr  :al)))
  ((rex.w    #xA2           ) ((:off    8) (:gpr  :al)))
  ((         #xA3           ) ((:off   16) (:gpr  :ax)))
  ((         #xA3           ) ((:off   32) (:gpr :eax)))
  ((rex.w    #xA3           ) ((:off   64) (:gpr :rax)))
  ((      (+ #xB0  rix)     ) ((:gpr    8) (:imm    8)))
  ((rex   (+ #xB0  rix)     ) ((:gpr    8) (:imm    8)))
  ((      (+ #xB8  rix)     ) ((:gpr   16) (:imm   16)))
  ((      (+ #xB8  rix)     ) ((:gpr   32) (:imm   32)))
  ((rex.w (+ #xB8  rix)     ) ((:gpr   64) (:imm   64)))
  ((         #xC6 modz0 sib0) ((:gxm    8) (:imm    8)))
  ((rex      #xC6 modz0 sib0) ((:gxm    8) (:imm    8)))
  ((         #xC7 modz0 sib0) ((:gxm   16) (:imm   16)))
  ((         #xC7 modz0 sib0) ((:gxm   32) (:imm   32)))
  ((rex.w    #xC7 modz0 sib0) ((:gxm   64) (:imm   64))))

(specops movsx (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex.w (determine-pfrex t op0 op1))
    (mod01 (determine-modrm op0 op1)) (sib1 (determine-sib op1)))
   (:priority                 0         1))
  ((      #x0FBE mod01 sib1) ((:gpr 16) (:gxm  8)))
  ((      #x0FBE mod01 sib1) ((:gpr 32) (:gxm  8)))
  ((rex.w #x0FBE mod01 sib1) ((:gpr 64) (:gxm  8)))
  ((      #x0FBF mod01 sib1) ((:gpr 32) (:gxm 16)))
  ((rex.w #x0FBF mod01 sib1) ((:gpr 64) (:gxm 16))))

(specops movsxd (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex.w (determine-pfrex t op0 op1))
    (mod01 (determine-modrm op0 op1)) (sib1 (determine-sib op1)))
   (:priority               0         1))
  ((      #x63 mod01 sib1) ((:gpr 16) (:gxm 16)))
  ((      #x63 mod01 sib1) ((:gpr 32) (:gxm 32)))
  ((rex.w #x63 mod01 sib1) ((:gpr 64) (:gxm 32))))

(specops add (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex nil op0 op1)) 
    (rex.w (determine-pfrex t op0 op1)) (modz0 (determine-modrm 0 op0))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                         1           0))
  ((      #x04                 op1 ) ((:gpr  :al) (:imm  8)))
  ((      #x05            (w 2 op1)) ((:gpr  :ax) (:imm 16)))
  ((      #x05            (w 4 op1)) ((:gpr :eax) (:imm 32)))
  ((rex.w #x05            (w 4 op1)) ((:gpr :rax) (:imm 32)))
  ((      #x80 modz0 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((rex   #x80 modz0 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((      #x81 modz0 sib0 (w 2 op1)) ((:gxm   16) (:imm 16)))
  ((      #x81 modz0 sib0 (w 4 op1)) ((:gxm   32) (:imm 32)))
  ((rex.w #x81 modz0 sib0 (w 4 op1)) ((:gxm   64) (:imm 32)))
  ((      #x83 modz0 sib0      op1 ) ((:gxm   16) (:imm  8)))
  ((      #x83 modz0 sib0      op1 ) ((:gxm   32) (:imm  8)))
  ((rex.w #x83 modz0 sib0      op1 ) ((:gxm   64) (:imm  8)))
  ((      #x00 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((rex   #x00 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((      #x01 mod10 sib0          ) ((:gxm   16) (:gpr 16)))
  ((      #x01 mod10 sib0          ) ((:gxm   32) (:gpr 32)))
  ((rex.w #x01 mod10 sib0          ) ((:gxm   64) (:gpr 64)))
  ((      #x02 mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((rex   #x02 mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((      #x03 mod01 sib1          ) ((:gpr   16) (:gxm 16)))
  ((      #x03 mod01 sib1          ) ((:gpr   32) (:gxm 32)))
  ((rex.w #x03 mod01 sib1          ) ((:gpr   64) (:gxm 64))))

(specops adc (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex nil op0 op1)) 
    (rex.w (determine-pfrex t op0 op1)) (mod20 (determine-modrm 2 op0))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                         1           0))
  ((      #x14                 op1 ) ((:gpr  :al) (:imm  8)))
  ((      #x15            (w 2 op1)) ((:gpr  :ax) (:imm 16)))
  ((      #x15            (w 4 op1)) ((:gpr :eax) (:imm 32)))
  ((rex.w #x15            (w 4 op1)) ((:gpr :rax) (:imm 32)))
  ((      #x80 mod20 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((rex   #x80 mod20 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((      #x81 mod20 sib0 (w 2 op1)) ((:gxm   16) (:imm 16)))
  ((      #x81 mod20 sib0 (w 4 op1)) ((:gxm   32) (:imm 32)))
  ((rex.w #x81 mod20 sib0 (w 4 op1)) ((:gxm   64) (:imm 32)))
  ((      #x83 mod20 sib0      op1 ) ((:gxm   16) (:imm  8)))
  ((      #x83 mod20 sib0      op1 ) ((:gxm   32) (:imm  8)))
  ((rex.w #x83 mod20 sib0      op1 ) ((:gxm   64) (:imm  8)))
  ((      #x10 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((rex   #x10 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((      #x11 mod10 sib0          ) ((:gxm   16) (:gpr 16)))
  ((      #x11 mod10 sib0          ) ((:gxm   32) (:gpr 32)))
  ((rex.w #x11 mod10 sib0          ) ((:gxm   64) (:gpr 64)))
  ((      #x12 mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((rex   #x12 mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((      #x13 mod01 sib1          ) ((:gpr   16) (:gxm 16)))
  ((      #x13 mod01 sib1          ) ((:gpr   32) (:gxm 32)))
  ((rex.w #x13 mod01 sib1          ) ((:gpr   64) (:gxm 64))))

(specops sub (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex nil op0 op1))
    (rex.w (determine-pfrex t op0 op1)) (mod50 (determine-modrm 5 op0))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                         1           0))
  ((      #x2C                 op1 ) ((:gpr  :al) (:imm  8)))
  ((      #x2D            (w 2 op1)) ((:gpr  :ax) (:imm 16)))
  ((      #x2D            (w 4 op1)) ((:gpr :eax) (:imm 32)))
  ((rex.w #x2D            (w 8 op1)) ((:gpr :rax) (:imm 32)))
  ((      #x80 mod50 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((rex   #x80 mod50 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((      #x81 mod50 sib0 (w 2 op1)) ((:gxm   16) (:imm 16)))
  ((      #x81 mod50 sib0 (w 4 op1)) ((:gxm   32) (:imm 32)))
  ((rex.w #x81 mod50 sib0 (w 4 op1)) ((:gxm   64) (:imm 64)))
  ((      #x83 mod50 sib0      op1 ) ((:gxm   16) (:imm  8)))
  ((      #x83 mod50 sib0      op1 ) ((:gxm   32) (:imm  8)))
  ((rex.w #x83 mod50 sib0      op1 ) ((:gxm   64) (:imm  8)))
  ((      #x28 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((rex   #x28 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((      #x29 mod10 sib0          ) ((:gxm   16) (:gpr 16)))
  ((      #x29 mod10 sib0          ) ((:gxm   32) (:gpr 32)))
  ((rex.w #x29 mod10 sib0          ) ((:gxm   64) (:gpr 64)))
  ((      #x2A mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((rex   #x2A mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((      #x2B mod01 sib1          ) ((:gpr   16) (:gxm 16)))
  ((      #x2B mod01 sib1          ) ((:gpr   32) (:gxm 32)))
  ((rex.w #x2B mod01 sib1          ) ((:gpr   64) (:gxm 64))))

(specops sbb (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex nil op0 op1))
    (rex.w (determine-pfrex t op0 op1)) (mod30 (determine-modrm 3 op0))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                         1           0))
  ((      #x1C                 op1 ) ((:gpr  :al) (:imm  8)))
  ((      #x1D            (w 2 op1)) ((:gpr  :ax) (:imm 16)))
  ((      #x1D            (w 4 op1)) ((:gpr :eax) (:imm 32)))
  ((rex.w #x1D            (w 4 op1)) ((:gpr :rax) (:imm 32)))
  ((      #x80 mod30 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((rex   #x80 mod30 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((      #x81 mod30 sib0 (w 2 op1)) ((:gxm   16) (:imm 16)))
  ((      #x81 mod30 sib0 (w 4 op1)) ((:gxm   32) (:imm 32)))
  ((rex.w #x81 mod30 sib0 (w 4 op1)) ((:gxm   64) (:imm 64)))
  ((      #x83 mod30 sib0      op1 ) ((:gxm   16) (:imm  8)))
  ((      #x83 mod30 sib0      op1 ) ((:gxm   32) (:imm  8)))
  ((rex.w #x83 mod30 sib0      op1 ) ((:gxm   64) (:imm  8)))
  ((      #x18 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((rex   #x18 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((      #x19 mod10 sib0          ) ((:gxm   16) (:gpr 16)))
  ((      #x19 mod10 sib0          ) ((:gxm   32) (:gpr 32)))
  ((rex.w #x19 mod10 sib0          ) ((:gxm   64) (:gpr 64)))
  ((      #x1A mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((rex   #x1A mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((      #x1B mod01 sib1          ) ((:gpr   16) (:gxm 16)))
  ((      #x1B mod01 sib1          ) ((:gpr   32) (:gxm 32)))
  ((rex.w #x1B mod01 sib1          ) ((:gpr   64) (:gxm 64))))

(specops aas () *assembler-prototype-x86*
  ((#x3F) ()))

(specops mul (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex nil op0 op1)) (rex.w (determine-pfrex t op0 op1))
    (mod40 (determine-modrm 4 op0)) (sib0 (determine-sib op0)))
   (:priority               0))
  ((      #xF6 mod40 sib0) ((:gxm  8)))
  ((rex   #xF6 mod40 sib0) ((:gxm  8)))
  ((      #xF7 mod40 sib0) ((:gxm 16)))
  ((      #xF7 mod40 sib0) ((:gxm 32)))
  ((rex.w #xF7 mod40 sib0) ((:gxm 64))))

(specops aam (&optional op0) *assembler-prototype-x86*
  ((:priority    0))
  ((#xD4   op0) ((:imm  8)))
  ((#xD40A    )  ()))

(specops div (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex nil op0 op1)) (rex.w (determine-pfrex t op0 op1))
    (mod60 (determine-modrm 6 op0)) (sib0 (determine-sib op0)))
   (:priority               0))
  ((      #xF6 mod60 sib0) ((:gxm  8)))
  ((rex   #xF6 mod60 sib0) ((:gxm  8)))
  ((      #xF7 mod60 sib0) ((:gxm 16)))
  ((      #xF7 mod60 sib0) ((:gxm 32)))
  ((rex.w #xF7 mod60 sib0) ((:gxm 64))))

(specops neg (op0) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex nil op0 nil)) (rex.w (determine-pfrex t op0 nil))
    (mod30 (determine-modrm 3 op0)) (sib0 (determine-sib op0)))
   (:priority               0))
  ((      #xF6 mod30 sib0) ((:gxm  8)))
  ((rex   #xF6 mod30 sib0) ((:gxm  8)))
  ((      #xF7 mod30 sib0) ((:gxm 16)))
  ((      #xF7 mod30 sib0) ((:gxm 32)))
  ((rex.w #xF7 mod30 sib0) ((:gxm 64))))

(specops and (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex nil op0 op1)) 
    (rex.w (determine-pfrex t op0 op1)) (mod40 (determine-modrm 4 op0))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                         1           0))
  ((      #x24                 op1 ) ((:gpr  :al) (:imm  8)))
  ((      #x25            (w 2 op1)) ((:gpr  :ax) (:imm 16)))
  ((      #x25            (w 4 op1)) ((:gpr :eax) (:imm 32)))
  ((rex.w #x25            (w 4 op1)) ((:gpr :rax) (:imm 32)))
  ((      #x80 mod40 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((rex   #x80 mod40 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((      #x81 mod40 sib0 (w 2 op1)) ((:gxm   16) (:imm 16)))
  ((      #x81 mod40 sib0 (w 4 op1)) ((:gxm   32) (:imm 32)))
  ((rex.w #x81 mod40 sib0 (w 4 op1)) ((:gxm   64) (:imm 32)))
  ((      #x83 mod40 sib0 (w 2 op1)) ((:gxm   16) (:imm  8)))
  ((      #x83 mod40 sib0 (w 4 op1)) ((:gxm   32) (:imm  8)))
  ((rex.w #x83 mod40 sib0 (w 4 op1)) ((:gxm   64) (:imm  8)))
  ((      #x20 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((rex   #x20 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((      #x21 mod10 sib0          ) ((:gxm   16) (:gpr 16)))
  ((      #x21 mod10 sib0          ) ((:gxm   32) (:gpr 32)))
  ((rex.w #x21 mod10 sib0          ) ((:gxm   64) (:gpr 64)))
  ((      #x22 mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((rex   #x22 mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((      #x23 mod01 sib1          ) ((:gpr   16) (:gxm 16)))
  ((      #x23 mod01 sib1          ) ((:gpr   32) (:gxm 32)))
  ((rex.w #x23 mod01 sib1          ) ((:gpr   64) (:gxm 64))))

(specops or (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex nil op0 op1)) 
    (rex.w (determine-pfrex t op0 op1)) (modo0 (determine-modrm 1 op0))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                         0           1))
  ((      #x0C                 op1 ) ((:gpr  :al) (:imm  8)))
  ((      #x0D            (w 2 op1)) ((:gpr  :ax) (:imm 16)))
  ((      #x0D            (w 4 op1)) ((:gpr :eax) (:imm 32)))
  ((rex.w #x0D            (w 4 op1)) ((:gpr :rax) (:imm 32)))
  ((      #x80 modo0 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((rex   #x80 modo0 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((      #x81 modo0 sib0 (w 2 op1)) ((:gxm   16) (:imm 16)))
  ((      #x81 modo0 sib0 (w 4 op1)) ((:gxm   32) (:imm 32)))
  ((rex.w #x81 modo0 sib0 (w 4 op1)) ((:gxm   64) (:imm 32)))
  ((      #x83 modo0 sib0 (w 2 op1)) ((:gxm   16) (:imm  8)))
  ((      #x83 modo0 sib0 (w 4 op1)) ((:gxm   32) (:imm  8)))
  ((rex.w #x83 modo0 sib0 (w 4 op1)) ((:gxm   32) (:imm  8)))
  ((      #x08 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((rex   #x08 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((      #x09 mod10 sib0          ) ((:gxm   16) (:gpr 16)))
  ((      #x09 mod10 sib0          ) ((:gxm   32) (:gpr 32)))
  ((rex.w #x09 mod10 sib0          ) ((:gxm   64) (:gpr 64)))
  ((      #x0A mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((rex   #x0A mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((      #x0B mod01 sib1          ) ((:gpr   16) (:gxm 16)))
  ((      #x0B mod01 sib1          ) ((:gpr   32) (:gxm 32)))
  ((rex.w #x0B mod01 sib1          ) ((:gpr   64) (:gxm 64))))

(specops xor (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex nil op0 op1)) 
    (rex.w (determine-pfrex t op0 op1)) (mod60 (determine-modrm 6 op0))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                         1           0))
  ((      #x34                 op1 ) ((:gpr  :al) (:imm  8)))
  ((      #x35            (w 2 op1)) ((:gpr  :ax) (:imm 16)))
  ((      #x35            (w 4 op1)) ((:gpr :eax) (:imm 32)))
  ((rex.w #x35            (w 4 op1)) ((:gpr :rax) (:imm 32)))
  ((      #x80 mod60 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((rex   #x80 mod60 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((      #x81 mod60 sib0 (w 2 op1)) ((:gxm   16) (:imm 16)))
  ((      #x81 mod60 sib0 (w 4 op1)) ((:gxm   32) (:imm 32)))
  ((rex.w #x81 mod60 sib0 (w 4 op1)) ((:gxm   64) (:imm 32)))
  ((      #x83 mod60 sib0 (w 2 op1)) ((:gxm   16) (:imm 16)))
  ((      #x83 mod60 sib0 (w 4 op1)) ((:gxm   32) (:imm 32)))
  ((rex.w #x83 mod60 sib0 (w 4 op1)) ((:gxm   32) (:imm 32)))
  ((      #x30 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((rex   #x30 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((      #x31 mod10 sib0          ) ((:gxm   16) (:gpr 16)))
  ((      #x31 mod10 sib0          ) ((:gxm   32) (:gpr 32)))
  ((rex.w #x31 mod10 sib0          ) ((:gxm   64) (:gpr 64)))
  ((      #x32 mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((rex   #x32 mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((      #x33 mod01 sib1          ) ((:gpr   16) (:gxm 16)))
  ((      #x33 mod01 sib1          ) ((:gpr   32) (:gxm 32)))
  ((rex.w #x33 mod01 sib1          ) ((:gpr   64) (:gxm 64))))

(specops sal (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex nil op0 op1)) (rex.w (determine-pfrex t op0 op1))
    (mod40 (determine-modrm 4 op0)) (sib0 (determine-sib op0)))
   (:priority                   0         1))
  ((      #xD0 mod40 sib0    ) ((:gxm  8) 1         ))
  ((rex   #xD0 mod40 sib0    ) ((:gxm  8) 1         ))
  ((      #xD2 mod40 sib0    ) ((:gxm  8) (:gpr :cl)))
  ((rex   #xD2 mod40 sib0    ) ((:gxm  8) (:gpr :cl)))
  ((      #xC0 mod40 sib0 op1) ((:gxm  8) (:imm   8)))
  ((rex   #xC0 mod40 sib0 op1) ((:gxm  8) (:imm   8)))
  ((      #xD1 mod40 sib0    ) ((:gxm 16) 1         ))
  ((      #xD3 mod40 sib0    ) ((:gxm 16) (:gpr :cl)))
  ((      #xC1 mod40 sib0 op1) ((:gxm 16) (:imm   8)))
  ((      #xD1 mod40 sib0    ) ((:gxm 32) 1         ))
  ((rex.w #xD1 mod40 sib0    ) ((:gxm 64) 1         ))
  ((      #xD3 mod40 sib0    ) ((:gxm 32) (:gpr :cl)))
  ((rex.w #xD3 mod40 sib0    ) ((:gxm 64) (:gpr :cl)))
  ((      #xC1 mod40 sib0 op1) ((:gxm 32) (:imm   8)))
  ((rex.w #xC1 mod40 sib0 op1) ((:gxm 64) (:imm   8))))

(specops sar (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex nil op0 op1)) (rex.w (determine-pfrex t op0 op1))
    (mod70 (determine-modrm 4 op0)) (sib0 (determine-sib op0)))
   (:priority                   0         1))
  ((      #xD0 mod70 sib0    ) ((:gxm  8) 1        ))
  ((rex   #xD0 mod70 sib0    ) ((:gxm  8) 1        ))
  ((      #xD2 mod70 sib0    ) ((:gxm  8) (:gpr :cl)))
  ((rex   #xD2 mod70 sib0    ) ((:gxm  8) (:gpr :cl)))
  ((      #xC0 mod70 sib0 op1) ((:gxm  8) (:imm   8)))
  ((rex   #xC0 mod70 sib0 op1) ((:gxm  8) (:imm   8)))
  ((      #xD1 mod70 sib0    ) ((:gxm 16) 1        ))
  ((      #xD3 mod70 sib0    ) ((:gxm 16) (:gpr :cl)))
  ((      #xC1 mod70 sib0 op1) ((:gxm 16) (:imm   8)))
  ((      #xD1 mod70 sib0    ) ((:gxm 32) 1        ))
  ((rex.w #xD1 mod70 sib0    ) ((:gxm 64) 1        ))
  ((      #xD3 mod70 sib0    ) ((:gxm 32) (:gpr :cl)))
  ((rex.w #xD3 mod70 sib0    ) ((:gxm 64) (:gpr :cl)))
  ((      #xC1 mod70 sib0 op1) ((:gxm 32) (:imm   8)))
  ((rex.w #xC1 mod70 sib0 op1) ((:gxm 64) (:imm   8))))

(specops shr (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex nil op0 op1)) (rex.w (determine-pfrex t op0 op1))
    (mod50 (determine-modrm 4 op0)) (sib0 (determine-sib op0)))
   (:priority                   0         1))
  ((      #xD0 mod50 sib0    ) ((:gxm  8) 1         ))
  ((rex   #xD0 mod50 sib0    ) ((:gxm  8) 1         ))
  ((      #xD2 mod50 sib0    ) ((:gxm  8) (:gpr :cl)))
  ((rex   #xD2 mod50 sib0    ) ((:gxm  8) (:gpr :cl)))
  ((      #xC0 mod50 sib0 op1) ((:gxm  8) (:imm   8)))
  ((rex   #xC0 mod50 sib0 op1) ((:gxm  8) (:imm   8)))
  ((      #xD1 mod50 sib0    ) ((:gxm 16) 1         ))
  ((      #xD3 mod50 sib0    ) ((:gxm 16) (:gpr :cl)))
  ((      #xC1 mod50 sib0 op1) ((:gxm 16) (:imm   8)))
  ((      #xD1 mod50 sib0    ) ((:gxm 32) 1         ))
  ((rex.w #xD1 mod50 sib0    ) ((:gxm 64) 1         ))
  ((      #xD3 mod50 sib0    ) ((:gxm 32) (:gpr :cl)))
  ((rex.w #xD3 mod50 sib0    ) ((:gxm 64) (:gpr :cl)))
  ((      #xC1 mod50 sib0 op1) ((:gxm 32) (:imm   8)))
  ((rex.w #xC1 mod50 sib0 op1) ((:gxm 64) (:imm   8))))

(specops cmp (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex nil op0 op1)) 
    (rex.w (determine-pfrex t op0 op1)) (mod70 (determine-modrm 7 op0))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                         1           0))
  ((      #x3C                 op1 ) ((:gpr  :al) (:imm  8)))
  ((      #x3D            (w 2 op1)) ((:gpr  :ax) (:imm 16)))
  ((      #x3D            (w 4 op1)) ((:gpr :eax) (:imm 32)))
  ((rex.w #x3D            (w 4 op1)) ((:gpr :rax) (:imm 32)))
  ((      #x80 mod70 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((rex   #x80 mod70 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((      #x81 mod70 sib0 (w 2 op1)) ((:gxm   16) (:imm 16)))
  ((      #x81 mod70 sib0 (w 4 op1)) ((:gxm   32) (:imm 32)))
  ((rex.w #x81 mod70 sib0 (w 4 op1)) ((:gxm   64) (:imm 32)))
  ((      #x83 mod70 sib0 (w 2 op1)) ((:gxm   16) (:imm 16)))
  ((      #x83 mod70 sib0 (w 4 op1)) ((:gxm   32) (:imm 32)))
  ((rex.w #x83 mod70 sib0 (w 4 op1)) ((:gxm   32) (:imm 32)))
  ((      #x38 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((rex   #x38 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((      #x39 mod10 sib0          ) ((:gxm   16) (:gpr 16)))
  ((      #x39 mod10 sib0          ) ((:gxm   32) (:gpr 32)))
  ((rex.w #x39 mod10 sib0          ) ((:gxm   64) (:gpr 64)))
  ((      #x3A mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((rex   #x3A mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((      #x3B mod01 sib1          ) ((:gpr   16) (:gxm 16)))
  ((      #x3B mod01 sib1          ) ((:gpr   32) (:gxm 32)))
  ((rex.w #x3B mod01 sib1          ) ((:gpr   64) (:gxm 64))))

(specops ja (op0) *assembler-prototype-x86*
  ((:priority     0))
   ((#x77   op0) ((:imm  8)))
   ((#x0F87 op0) ((:imm 16)))
   ((#x0F87 op0) ((:imm 32))))

(specops jae (op0) *assembler-prototype-x86*
  ((:priority    0))
  ((#x73   op0) ((:imm  8)))
  ((#x0F83 op0) ((:imm 16)))
  ((#x0F83 op0) ((:imm 32))))

(specops jb (op0) *assembler-prototype-x86*
  ((:priority    0))
  ((#x72   op0) ((:imm  8)))
  ((#x0F82 op0) ((:imm 16)))
  ((#x0F82 op0) ((:imm 32))))

(specops jbe (op0) *assembler-prototype-x86*
  ((:priority    0))
  ((#x76   op0) ((:imm  8)))
  ((#x0F86 op0) ((:imm 16)))
  ((#x0F86 op0) ((:imm 32))))

(specops jc (op0) *assembler-prototype-x86*
  ((:priority    0))
  ((#x72   op0) ((:imm  8)))
  ((#x0F82 op0) ((:imm 16)))
  ((#x0F82 op0) ((:imm 32))))

(specops jcxz (op0) *assembler-prototype-x86*
  ((:priority 0))
  ((#xE3op0) ((:imm  8))))

(specops jecxz (op0) *assembler-prototype-x86*
  ((:priority  0))
  ((#xE3 op0) ((:imm  8))))

(specops jrcxz (op0) *assembler-prototype-x86*
  ((:priority  0))
  ((#xE3 op0) ((:imm  8))))

(specops je (op0) *assembler-prototype-x86*
  ((:priority    0))
  ((#x74   op0) ((:imm  8)))
  ((#x0F84 op0) ((:imm 16)))
  ((#x0F84 op0) ((:imm 32))))

(specops jg (op0) *assembler-prototype-x86*
  ((:priority    0))
  ((#x7F   op0) ((:imm  8)))
  ((#x0F8F op0) ((:imm 16)))
  ((#x0F8F op0) ((:imm 32))))

(specops jge (op0) *assembler-prototype-x86*
  ((:priority    0))
  ((#x7D   op0) ((:imm  8)))
  ((#x0F8D op0) ((:imm 16)))
  ((#x0F8D op0) ((:imm 32))))

(specops jl (op0) *assembler-prototype-x86*
  ((:priority    0))
  ((#x7C   op0) ((:imm  8)))
  ((#x0F8C op0) ((:imm 16)))
  ((#x0F8C op0) ((:imm 32))))

(specops jna (op0) *assembler-prototype-x86*
  ((:priority    0))
  ((#x76   op0) ((:imm  8)))
  ((#x0F86 op0) ((:imm 16)))
  ((#x0F86 op0) ((:imm 32))))

(specops jle (op0) *assembler-prototype-x86*
  ((:priority    0))
  ((#x7E   op0) ((:imm  8)))
  ((#x0F8E op0) ((:imm 16)))
  ((#x0F8E op0) ((:imm 32))))

;; ...more to come

(specops ret (op0) *assembler-prototype-x86*
  ((:priority  0))
  ((#xC2 op0) ((:imm 16))) ;; needs work
  ((#xCA op0) ((:imm 16)))
  ((#xC3 op0))
  ((#xCB op0)))

(specops lea (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex.w (determine-pfrex t op0 op1))
    (mod01 (determine-modrm op0 op1)) (sib1 (determine-sib op1)))
   (:priority               0         1))
  ((      #x8D mod01 sib1) ((:gpr 16) (:mem 16)))
  ((      #x8D mod01 sib1) ((:gpr 32) (:mem 32)))
  ((rex.w #x8D mod01 sib1) ((:gpr 64) (:mem 64))))

(specops popcnt (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex.w (determine-pfrex t op0 op1))
    (mod01 (determine-modrm op0 op1)) (sib1 (determine-sib op1)))
   (:priority                      0         1))
  ((#xF3       #x0FB8 mod01 sib1) ((:gpr 16) (:gxm 16)))
  ((#xF3       #x0FB8 mod01 sib1) ((:gpr 32) (:gxm 32)))
  ((#xF3 rex.w #x0FB8 mod01 sib1) ((:gpr 64) (:gxm 64))))

(specops pshufb (op0 op1 &optional op2) *assembler-prototype-x86*
  ((:provisions
    (vex  (determine-pfvex  op0 op2 op1 :prefix #x66 :map #x0F38 :long t))
    (evex (determine-pfevex op0 op2 op1 :prefix #x66 :map #x0F38))
    (mod01 (determine-modrm op0 op1)) (mod02 (determine-modrm op0 op2))
    (sib1 (determine-sib op1)) (sib2 (determine-sib op2)))
   (:priority                    0          1          2))
   ;; ((        #x0F3800 mod01 sib1) ((:vcr 64)  (:vxm  64)           ))
  ((     #x660F3800 mod01 sib1) ((:vcr 128) (:vxm 128)           ))
  (( vex       #x00 mod02 sib2) ((:vcr 128) (:vcr 128) (:vxm 128)))
  (( vex       #x00 mod02 sib2) ((:vcr 256) (:vcr 256) (:vxm 256)))
  ((evex       #x00 mod02 sib2) ((:vcr 128) (:vcr 128) (:vxm 128)))
  ((evex       #x00 mod02 sib2) ((:vcr 256) (:vcr 256) (:vxm 256)))
  ((evex       #x00 mod02 sib2) ((:vcr 512) (:vcr 512) (:vxm 512))))

(specops pshufd (op0 op1 op2) *assembler-prototype-x86*
  ((:provisions
    ( vex (determine-pfvex  op0 op1 op2 :prefix #x66 :map #x0F :long t))
    (evex (determine-pfevex op0 op1 op2 :prefix #x66 :map #x0F))
    (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                      0          1          2))
  ((     #x660F70 mod01 sib1 op2) ((:vcr 128) (:vxm 128) (:imm 8)))
  (( vex     #x70 mod01 sib1 op2) ((:vcr 128) (:vcr 128) (:imm 8)))
  (( vex     #x70 mod01 sib1 op2) ((:vcr 256) (:vcr 256) (:imm 8)))
  ((evex     #x70 mod01 sib1 op2) ((:vcr 128) (:vcr 128) (:imm 8)))
  ((evex     #x70 mod01 sib1 op2) ((:vcr 256) (:vcr 256) (:imm 8)))
  ((evex     #x70 mod01 sib1 op2) ((:vcr 512) (:vcr 512) (:imm 8))))

(specops vpblendd (op0 op1 op2 op3) *assembler-prototype-x86*
  ((:provisions
    (vex (determine-pfvex op0 op1 op2 :prefix #x66 :map #x0F3A :long t))
    (mod01 (determine-modrm op0 op1)) (sib1 (determine-sib op1)))
   (:priority                 0          1          2          3))
  ((vex #x02 mod01 sib1 op3) ((:vcr 128) (:vcr 128) (:vxm 128) (:imm 8)))
  ((vex #x02 mod01 sib1 op3) ((:vcr 256) (:vcr 256) (:vxm 256) (:imm 8))))


;; https://wiki.raptorcs.com/w/images/f/f5/PowerISA_public.v3.1.pdf pg. 12

(defun powerformat-a
  (masque "PPPPPPAA.AAABBBBB.CCCCCDDD.DDXXXXXR"))
