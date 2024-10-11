;;;; ops.main.lisp

(in-package #:specops.x86)

;; Intel PDF pg. 588 has guide to opcode table abbreviations

(specops mov (op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rix (logand #b111 (reg-index op0))) (rex (determine-pfrex w op0 op1))
    (sz? (determine-pfsize w op0 op1 (of-program :exmode)))
    (modz0 (determine-modrm 0 op0)) (mod10 (determine-modrm op1 op0))
    (mod01 (determine-modrm op0 op1)) (sib0 (determine-sib op0)))
   (:priority                  0           1))
  ;; ((rex    #x88 mod10 sib0) ((:gxm    8) (:gpr    8)))
  ((sz? rex    #x88 mod10 sib0) ((:w :b) (:gxm   ) (:gpr   )))
  ((sz? rex    #x89 mod10 sib0) ((:w :w) (:gxm   ) (:gpr   )))
  ((sz? rex    #x89 mod10 sib0) ((:w :d) (:gxm   ) (:gpr   )))
  ((sz? rex    #x89 mod10 sib0) ((:w :q) (:gxm   ) (:gpr   )))
  ;; ((rex    #x8A mod10 sib0) ((:gpr    8) (:gxm    8)))
  ((sz? rex    #x8A mod10 sib0) ((:w :b) (:gpr   ) (:gxm   )))
  ((sz? rex    #x8B mod10 sib0) ((:w :w) (:gpr   ) (:gxm   )))
  ((sz? rex    #x8B mod10 sib0) ((:w :d) (:gpr   ) (:gxm   )))
  ((sz? rex    #x8B mod10 sib0) ((:w :q) (:gpr   ) (:gxm   )))
  ;; segment register instructions ommitted
  ;; ((rex    #xA0           ) ((:gpr  :al) (:off    8)))
  ((    rex    #xA0           ) ((:w :b) (:gpr :a) (:off   )))
  ((sz? rex    #xA1           ) ((:w :w) (:gpr :a) (:off   )))
  ((sz? rex    #xA1           ) ((:w :d) (:gpr :a) (:off   )))
  ((    rex    #xA1           ) ((:w :q) (:gpr :a) (:off   )))
  ;; ((rex    #xA2           ) ((:off    8) (:gpr  :al)))
  ((    rex    #xA2           ) ((:w :b) (:off   ) (:gpr :a)))
  ((sz? rex    #xA3           ) ((:w :w) (:off   ) (:gpr :a)))
  ((sz? rex    #xA3           ) ((:w :d) (:off   ) (:gpr :a)))
  ((    rex    #xA3           ) ((:w :q) (:off   ) (:gpr :a)))
  ;; ((rex (+ #xB0  rix)     ) ((:gpr    8) (:imm    8)))
  ((sz? rex (+ #xB0 rix)      ) ((:w :b) (:gpr   ) (:imm   )))
  ((sz? rex (+ #xB8 rix)      ) ((:w :w) (:gpr   ) (:imm   )))
  ((sz? rex (+ #xB8 rix)      ) ((:w :d) (:gpr   ) (:imm   )))
  ((sz? rex (+ #xB8 rix)      ) ((:w :q) (:gpr   ) (:imm   )))
  ;; ((sz? rex    #xC6 modz0 sib0) ((:gxm    8) (:imm    8)))
  ((sz? rex    #xC6 modz0 sib0) ((:w :b) (:gxm   ) (:imm   )))
  ((sz? rex    #xC7 modz0 sib0) ((:w :w) (:gxm   ) (:imm   )))
  ((sz? rex    #xC7 modz0 sib0) ((:w :d) (:gxm   ) (:imm   )))
  ((sz? rex    #xC7 modz0 sib0) ((:w :q) (:gxm   ) (:imm   ))))

(specops add (w op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex w op0 op1)) (modz0 (determine-modrm 0 op0))
    (sz? (determine-pfsize w op0 op1 (of-program :exmode)))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                           0       2         1))
  ((    rex #x04                 op1 ) ((:w :b) (:gpr :a) (:imm   )))
  ((sz? rex #x05            (w 2 op1)) ((:w :w) (:gpr :a) (:imm   )))
  ((sz? rex #x05            (w 4 op1)) ((:w :d) (:gpr :a) (:imm   )))
  ((    rex #x05            (w 4 op1)) ((:w :q) (:gpr :a) (:imm   )))
  ;; ((           #x80 modz0 sib0      op1 ) ((:w :b) (:gxm   ) (:imm  )))
  ((sz? rex #x80 modz0 sib0      op1 ) ((:w :b) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 modz0 sib0      op1 ) ((:w :w) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 modz0 sib0      op1 ) ((:w :d) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 modz0 sib0      op1 ) ((:w :q) (:gxm   ) (:imm  8)))
  ((sz? rex #x81 modz0 sib0 (w 2 op1)) ((:w :w) (:gxm   ) (:imm 16)))
  ((sz? rex #x81 modz0 sib0 (w 4 op1)) ((:w :d) (:gxm   ) (:imm 32)))
  ((sz? rex #x81 modz0 sib0 (w 4 op1)) ((:w :q) (:gxm   ) (:imm 64)))
  ;; ((           #x00 mod10 sib0          ) ((:w :b) (:gxm   ) (:gpr  )))
  ((sz? rex #x00 mod10 sib0          ) ((:w :b) (:gxm   ) (:gpr   )))
  ((sz? rex #x01 mod10 sib0          ) ((:w :w) (:gxm   ) (:gpr   )))
  ((sz? rex #x01 mod10 sib0          ) ((:w :d) (:gxm   ) (:gpr   )))
  ((sz? rex #x01 mod10 sib0          ) ((:w :q) (:gxm   ) (:gpr   )))
  ;; ((           #x02 mod01 sib1          ) ((:w :b) (:gpr   ) (:gxm  )))
  ((sz? rex #x02 mod01 sib1          ) ((:w :b) (:gpr   ) (:gxm   )))
  ((sz? rex #x03 mod01 sib1          ) ((:w :w) (:gpr   ) (:gxm   )))
  ((sz? rex #x03 mod01 sib1          ) ((:w :d) (:gpr   ) (:gxm   )))
  ((sz? rex #x03 mod01 sib1          ) ((:w :q) (:gpr   ) (:gxm   ))))

(specops adc (w op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex w op0 op1)) (mod20 (determine-modrm 2 op0))
    (sz? (determine-pfrsize w op0 op1 (of-program :exmode)))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                           0       2         1))
  ((    rex #x14                 op1 ) ((:w :b) (:gpr :a) (:imm   )))
  ((sz? rex #x15            (w 2 op1)) ((:w :w) (:gpr :a) (:imm   )))
  ((sz? rex #x15            (w 4 op1)) ((:w :d) (:gpr :a) (:imm   )))
  ((    rex #x15            (w 4 op1)) ((:w :q) (:gpr :a) (:imm   )))
  ;; ((      #x80 mod20 sib0      op1 ) ((:gxm    8) (:imm)))
  ((sz? rex #x80 mod20 sib0      op1 ) ((:w :b) (:gxm   ) (:imm  8)))
  ((sz? rex #x81 mod20 sib0 (w 2 op1)) ((:w :w) (:gxm   ) (:imm 16)))
  ((sz? rex #x81 mod20 sib0 (w 4 op1)) ((:w :d) (:gxm   ) (:imm 32)))
  ((sz? rex #x81 mod20 sib0 (w 4 op1)) ((:w :q) (:gxm   ) (:imm 64)))
  ((sz? rex #x83 mod20 sib0      op1 ) ((:w :w) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 mod20 sib0      op1 ) ((:w :d) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 mod20 sib0      op1 ) ((:w :q) (:gxm   ) (:imm  8)))
  ;; ((      #x10 mod10 sib0          ) ((:gxm    8) (:gpr)))
  ((sz? rex #x10 mod10 sib0          ) ((:w :b) (:gxm   ) (:gpr   )))
  ((sz? rex #x11 mod10 sib0          ) ((:w :w) (:gxm   ) (:gpr   )))
  ((sz? rex #x11 mod10 sib0          ) ((:w :d) (:gxm   ) (:gpr   )))
  ((sz? rex #x11 mod10 sib0          ) ((:w :q) (:gxm   ) (:gpr   )))
  ;; ((      #x12 mod01 sib1          ) ((:gpr    8) (:gxm)))
  ((sz? rex #x12 mod01 sib1          ) ((:w :b) (:gpr   ) (:gxm   )))
  ((sz? rex #x13 mod01 sib1          ) ((:w :w) (:gpr   ) (:gxm   )))
  ((sz? rex #x13 mod01 sib1          ) ((:w :d) (:gpr   ) (:gxm   )))
  ((sz? rex #x13 mod01 sib1          ) ((:w :q) (:gpr   ) (:gxm   ))))

(specops sub (w op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex nil op0 op1)) (mod50 (determine-modrm 5 op0))
    (sz? (determine-pfrsize w op0 op1 (of-program :exmode)))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                           0       2         1))
  ((    rex #x2C                 op1 ) ((:w :b) (:gpr :a) (:imm   )))
  ((sz? rex #x2D            (w 2 op1)) ((:w :w) (:gpr :a) (:imm   )))
  ((sz? rex #x2D            (w 4 op1)) ((:w :d) (:gpr :a) (:imm   )))
  ((    rex #x2D            (w 8 op1)) ((:w :q) (:gpr :a) (:imm   )))
  ;; ((      #x80 mod50 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((sz? rex #x80 mod50 sib0      op1 ) ((:w :b) (:gxm   ) (:imm  8)))
  ((sz? rex #x81 mod50 sib0 (w 2 op1)) ((:w :w) (:gxm   ) (:imm 16)))
  ((sz? rex #x81 mod50 sib0 (w 4 op1)) ((:w :d) (:gxm   ) (:imm 32)))
  ((sz? rex #x81 mod50 sib0 (w 4 op1)) ((:w :q) (:gxm   ) (:imm 64)))
  ((sz? rex #x83 mod50 sib0      op1 ) ((:w :w) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 mod50 sib0      op1 ) ((:w :d) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 mod50 sib0      op1 ) ((:w :q) (:gxm   ) (:imm  8)))
  ;; ((      #x28 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((sz? rex #x28 mod10 sib0          ) ((:w :b) (:gxm   ) (:gpr   )))
  ((sz? rex #x29 mod10 sib0          ) ((:w :w) (:gxm   ) (:gpr   )))
  ((sz? rex #x29 mod10 sib0          ) ((:w :d) (:gxm   ) (:gpr   )))
  ((sz? rex #x29 mod10 sib0          ) ((:w :q) (:gxm   ) (:gpr   )))
  ;; ((      #x2A mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((sz? rex #x2A mod01 sib1          ) ((:w :b) (:gpr   ) (:gxm   )))
  ((sz? rex #x2B mod01 sib1          ) ((:w :w) (:gpr   ) (:gxm   )))
  ((sz? rex #x2B mod01 sib1          ) ((:w :d) (:gpr   ) (:gxm   )))
  ((sz? rex #x2B mod01 sib1          ) ((:w :q) (:gpr   ) (:gxm   ))))

(specops sbb (w op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex w op0 op1)) (mod30 (determine-modrm 3 op0))
    (sz? (determine-pfrsize w op0 op1 (of-program :exmode)))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                           0       2         1))
  ((    rex #x1C                 op1 ) ((:w :b) (:gpr :a) (:imm   )))
  ((sz? rex #x1D            (w 2 op1)) ((:w :w) (:gpr :a) (:imm   )))
  ((sz? rex #x1D            (w 4 op1)) ((:w :d) (:gpr :a) (:imm   )))
  ((    rex #x1D            (w 4 op1)) ((:w :q) (:gpr :a) (:imm   )))
  ;; ((      #x80 mod30 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((sz? rex #x80 mod30 sib0      op1 ) ((:w :b) (:gxm   ) (:imm  8)))
  ((sz? rex #x81 mod30 sib0 (w 2 op1)) ((:w :w) (:gxm   ) (:imm 16)))
  ((sz? rex #x81 mod30 sib0 (w 4 op1)) ((:w :d) (:gxm   ) (:imm 32)))
  ((sz? rex #x81 mod30 sib0 (w 4 op1)) ((:w :q) (:gxm   ) (:imm 64)))
  ((sz? rex #x83 mod30 sib0      op1 ) ((:w :w) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 mod30 sib0      op1 ) ((:w :d) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 mod30 sib0      op1 ) ((:w :q) (:gxm   ) (:imm  8)))
  ;; ((      #x18 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((sz? rex #x18 mod10 sib0          ) ((:w :b) (:gxm   ) (:gpr   )))
  ((sz? rex #x19 mod10 sib0          ) ((:w :w) (:gxm   ) (:gpr   )))
  ((sz? rex #x19 mod10 sib0          ) ((:w :d) (:gxm   ) (:gpr   )))
  ((sz? rex #x19 mod10 sib0          ) ((:w :q) (:gxm   ) (:gpr   )))
  ;; ((      #x1A mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((sz? rex #x1A mod01 sib1          ) ((:w :b) (:gpr   ) (:gxm   )))
  ((sz? rex #x1B mod01 sib1          ) ((:w :w) (:gpr   ) (:gxm   )))
  ((sz? rex #x1B mod01 sib1          ) ((:w :d) (:gpr   ) (:gxm   )))
  ((sz? rex #x1B mod01 sib1          ) ((:w :q) (:gpr   ) (:gxm   ))))

(specops aas () *assembler-prototype-x86*
  ((#x3F) ()))

(specops mul (w op0) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex w op0 op1))
    (sz? (determine-pfrsize w op0 op1 (of-program :exmode)))
    (mod40 (determine-modrm 4 op0)) (sib0 (determine-sib op0)))
   (:priority                 0       1))
  ;; ((      #xF6 mod40 sib0) ((:gxm  8)))
  ((sz? rex #xF6 mod40 sib0) ((:w :b) (:gxm  8)))
  ((sz? rex #xF7 mod40 sib0) ((:w :w) (:gxm 16)))
  ((sz? rex #xF7 mod40 sib0) ((:w :d) (:gxm 32)))
  ((sz? rex #xF7 mod40 sib0) ((:w :q) (:gxm 64))))

;; (specops aam (&optional op0) *assembler-prototype-x86*
;;   ((:priority    0))
;;   ((#xD4   op0) ((:imm  8)))
;;   ((#xD40A    )  ()))

(specops div (w op0) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex w op0 op1))
    (sz? (determine-pfrsize w op0 op1 (of-program :exmode)))
    (mod60 (determine-modrm 6 op0)) (sib0 (determine-sib op0)))
   (:priority                 0       1))
  ;; ((      #xF6 mod60 sib0) ((:gxm  8)))
  ((sz? rex #xF6 mod60 sib0) ((:w :b) (:gxm  8)))
  ((sz? rex #xF7 mod60 sib0) ((:w :w) (:gxm 16)))
  ((sz? rex #xF7 mod60 sib0) ((:w :d) (:gxm 32)))
  ((sz? rex #xF7 mod60 sib0) ((:w :q) (:gxm 64))))

(specops neg (w op0) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex w op0 nil))
    (sz? (determine-pfrsize w op0 op1 (of-program :exmode)))
    (mod30 (determine-modrm 3 op0)) (sib0 (determine-sib op0)))
   (:priority                 0))
  ;; ((rex #xF6 mod30 sib0) ((:gxm  8)))
  ((sz? rex #xF6 mod30 sib0) ((:w :b) (:gxm  8)))
  ((sz? rex #xF7 mod30 sib0) ((:w :w) (:gxm 16)))
  ((sz? rex #xF7 mod30 sib0) ((:w :d) (:gxm 32)))
  ((sz? rex #xF7 mod30 sib0) ((:w :q) (:gxm 64))))

(specops and (w op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex w op0 op1)) 
    ;; (rex.w (determine-pfrex t op0 op1))
    (sz? (determine-pfrsize w op0 op1 (of-program :exmode)))
    (mod40 (determine-modrm 4 op0))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                           0       2         1))
  ((    rex #x24                 op1 ) ((:w :b) (:gpr :a) (:imm   )))
  ((sz? rex #x25            (w 2 op1)) ((:w :w) (:gpr :a) (:imm   )))
  ((sz? rex #x25            (w 4 op1)) ((:w :d) (:gpr :a) (:imm   )))
  ((    rex #x25            (w 4 op1)) ((:w :q) (:gpr :a) (:imm   )))
  ;; ((sz? rex #x80 mod40 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((sz? rex #x80 mod40 sib0      op1 ) ((:w :b) (:gxm   ) (:imm  8)))
  ((sz? rex #x81 mod40 sib0 (w 2 op1)) ((:w :w) (:gxm   ) (:imm 16)))
  ((sz? rex #x81 mod40 sib0 (w 4 op1)) ((:w :d) (:gxm   ) (:imm 32)))
  ((sz? rex #x81 mod40 sib0 (w 4 op1)) ((:w :q) (:gxm   ) (:imm 32)))
  ((sz? rex #x83 mod40 sib0      op1 ) ((:w :w) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 mod40 sib0      op1 ) ((:w :d) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 mod40 sib0      op1 ) ((:w :q) (:gxm   ) (:imm  8)))
  ;; ((sz? rex #x20 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((sz? rex #x20 mod10 sib0          ) ((:w :b) (:gxm   ) (:gpr   )))
  ((sz? rex #x21 mod10 sib0          ) ((:w :w) (:gxm   ) (:gpr   )))
  ((sz? rex #x21 mod10 sib0          ) ((:w :d) (:gxm   ) (:gpr   )))
  ((sz? rex #x21 mod10 sib0          ) ((:w :q) (:gxm   ) (:gpr   )))
  ;; ((sz? rex #x22 mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((sz? rex #x22 mod01 sib1          ) ((:w :b) (:gpr   ) (:gxm   )))
  ((sz? rex #x23 mod01 sib1          ) ((:w :w) (:gpr   ) (:gxm   )))
  ((sz? rex #x23 mod01 sib1          ) ((:w :d) (:gpr   ) (:gxm   )))
  ((sz? rex #x23 mod01 sib1          ) ((:w :q) (:gpr   ) (:gxm   ))))

(specops or (w op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex w op0 op1)) (modo0 (determine-modrm 1 op0))
    (sz? (determine-pfrsize w op0 op1 (of-program :exmode)))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                           0       2         1))
  ((    rex #x0C                 op1 ) ((:w :b) (:gpr :a) (:imm   )))
  ((sz? rex #x0D            (w 2 op1)) ((:w :w) (:gpr :a) (:imm   )))
  ((sz? rex #x0D            (w 4 op1)) ((:w :d) (:gpr :a) (:imm   )))
  ((    rex #x0D            (w 4 op1)) ((:w :q) (:gpr :a) (:imm   )))
  ;; ((      #x80 modo0 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((sz? rex #x80 modo0 sib0      op1 ) ((:w :b) (:gxm   ) (:imm  8)))
  ((sz? rex #x81 modo0 sib0 (w 2 op1)) ((:w :w) (:gxm   ) (:imm 16)))
  ((sz? rex #x81 modo0 sib0 (w 4 op1)) ((:w :d) (:gxm   ) (:imm 32)))
  ((sz? rex #x81 modo0 sib0 (w 4 op1)) ((:w :q) (:gxm   ) (:imm 32)))
  ((sz? rex #x83 modo0 sib0      op1 ) ((:w :w) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 modo0 sib0      op1 ) ((:w :d) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 modo0 sib0      op1 ) ((:w :q) (:gxm   ) (:imm  8)))
  ;; ((      #x08 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((sz? rex #x08 mod10 sib0          ) ((:w :b) (:gxm   ) (:gpr   )))
  ((sz? rex #x09 mod10 sib0          ) ((:w :w) (:gxm   ) (:gpr   )))
  ((sz? rex #x09 mod10 sib0          ) ((:w :d) (:gxm   ) (:gpr   )))
  ((sz? rex #x09 mod10 sib0          ) ((:w :q) (:gxm   ) (:gpr   )))
  ;; ((      #x0A mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((sz? rex #x0A mod01 sib1          ) ((:w :b) (:gpr   ) (:gxm   )))
  ((sz? rex #x0B mod01 sib1          ) ((:w :w) (:gpr   ) (:gxm   )))
  ((sz? rex #x0B mod01 sib1          ) ((:w :d) (:gpr   ) (:gxm   )))
  ((sz? rex #x0B mod01 sib1          ) ((:w :q) (:gpr   ) (:gxm   ))))

(specops xor (w op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex w op0 op1)) (mod60 (determine-modrm 6 op0))
    (sz? (determine-pfrsize w op0 op1 (of-program :exmode)))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                         1           0))
  ((    rex #x34                 op1 ) ((:w :b) (:gpr :a) (:imm   )))
  ((sz? rex #x35            (w 2 op1)) ((:w :w) (:gpr :a) (:imm   )))
  ((sz? rex #x35            (w 4 op1)) ((:w :d) (:gpr :a) (:imm   )))
  ((    rex #x35            (w 4 op1)) ((:w :q) (:gpr :a) (:imm   )))
  ;; ((rex #x80 mod60 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((sz? rex #x80 mod60 sib0      op1 ) ((:w :b) (:gxm   ) (:imm  8)))
  ((sz? rex #x81 mod60 sib0 (w 2 op1)) ((:w :w) (:gxm   ) (:imm 16)))
  ((sz? rex #x81 mod60 sib0 (w 4 op1)) ((:w :d) (:gxm   ) (:imm 32)))
  ((sz? rex #x81 mod60 sib0 (w 4 op1)) ((:w :q) (:gxm   ) (:imm 32)))
  ((sz? rex #x83 mod60 sib0      op1 ) ((:w :w) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 mod60 sib0      op1 ) ((:w :d) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 mod60 sib0      op1 ) ((:w :q) (:gxm   ) (:imm  8)))
  ;; ((rex #x30 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((sz? rex #x30 mod10 sib0          ) ((:w :b) (:gxm   ) (:gpr   )))
  ((sz? rex #x31 mod10 sib0          ) ((:w :w) (:gxm   ) (:gpr   )))
  ((sz? rex #x31 mod10 sib0          ) ((:w :d) (:gxm   ) (:gpr   )))
  ((sz? rex #x31 mod10 sib0          ) ((:w :q) (:gxm   ) (:gpr   )))
  ;; ((    #x32 mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((sz? rex #x32 mod01 sib1          ) ((:w :b) (:gpr   ) (:gxm   )))
  ((sz? rex #x33 mod01 sib1          ) ((:w :w) (:gpr   ) (:gxm   )))
  ((sz? rex #x33 mod01 sib1          ) ((:w :d) (:gpr   ) (:gxm   )))
  ((sz? rex #x33 mod01 sib1          ) ((:w :q) (:gpr   ) (:gxm   ))))

(specops sal (w op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex w op0 op1))
    (sz? (determine-pfrsize w op0 op1 (of-program :exmode)))
    (mod40 (determine-modrm 4 op0)) (sib0 (determine-sib op0)))
   (:priority                   0         1))
  ;; ((      #xD0 mod40 sib0    ) ((:gxm  8) 1         ))
  ((sz? rex #xD0 mod40 sib0    ) ((:w :b) (:gxm) 1         ))
  ;; ((      #xD2 mod40 sib0    ) ((:gxm  8) (:gpr :cl)))
  ((sz? rex #xD2 mod40 sib0    ) ((:w :b) (:gxm) (:gpr :cl)))
  ;; ((rex #xC0 mod40 sib0 op1) ((:gxm  8) (:imm   8)))
  ((sz? rex #xC0 mod40 sib0 op1) ((:w :b) (:gxm) (:imm   8)))
  ((sz? rex #xD1 mod40 sib0    ) ((:w :w) (:gxm) 1         ))
  ((sz? rex #xD3 mod40 sib0    ) ((:w :w) (:gxm) (:gpr :cl)))
  ((sz? rex #xC1 mod40 sib0 op1) ((:w :w) (:gxm) (:imm   8)))
  ((sz? rex #xD1 mod40 sib0    ) ((:w :d) (:gxm) 1         ))
  ((sz? rex #xD1 mod40 sib0    ) ((:w :q) (:gxm) 1         ))
  ((sz? rex #xD3 mod40 sib0    ) ((:w :d) (:gxm) (:gpr :cl)))
  ((sz? rex #xD3 mod40 sib0    ) ((:w :q) (:gxm) (:gpr :cl)))
  ((sz? rex #xC1 mod40 sib0 op1) ((:w :d) (:gxm) (:imm   8)))
  ((sz? rex #xC1 mod40 sib0 op1) ((:w :q) (:gxm) (:imm   8))))

(specops sar (w op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex w op0 op1))
    (sz? (determine-pfrsize w op0 op1 (of-program :exmode)))
    (mod70 (determine-modrm 4 op0)) (sib0 (determine-sib op0)))
   (:priority                   0         1))
  ;; ((rex #xD0 mod70 sib0    ) ((:gxm  8) 1        ))
  ((sz? rex #xD0 mod70 sib0    ) ((:w :b) (:gxm) 1         ))
  ;; ((rex #xD2 mod70 sib0    ) ((:gxm  8) (:gpr :cl)))
  ((sz? rex #xD2 mod70 sib0    ) ((:w :b) (:gxm) (:gpr :cl)))
  ;; ((rex #xC0 mod70 sib0 op1) ((:gxm  8) (:imm   8)))
  ((sz? rex #xC0 mod70 sib0 op1) ((:w :b) (:gxm) (:imm   8)))
  ((sz? rex #xD1 mod70 sib0    ) ((:w :w) (:gxm) 1         ))
  ((sz? rex #xD3 mod70 sib0    ) ((:w :w) (:gxm) (:gpr :cl)))
  ((sz? rex #xC1 mod70 sib0 op1) ((:w :w) (:gxm) (:imm   8)))
  ((sz? rex #xD1 mod70 sib0    ) ((:w :d) (:gxm) 1         ))
  ((sz? rex #xD1 mod70 sib0    ) ((:w :q) (:gxm) 1         ))
  ((sz? rex #xD3 mod70 sib0    ) ((:w :d) (:gxm) (:gpr :cl)))
  ((sz? rex #xD3 mod70 sib0    ) ((:w :q) (:gxm) (:gpr :cl)))
  ((sz? rex #xC1 mod70 sib0 op1) ((:w :d) (:gxm) (:imm   8)))
  ((sz? rex #xC1 mod70 sib0 op1) ((:w :q) (:gxm) (:imm   8))))

(specops shr (w op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex w op0 op1))
    (sz? (determine-pfrsize w op0 op1 (of-program :exmode)))
    (mod50 (determine-modrm 4 op0)) (sib0 (determine-sib op0)))
   (:priority                     0       2      1))
  ;; ((      #xD0 mod50 sib0    ) ((:gxm  8) 1         ))
  ((sz? rex #xD0 mod50 sib0    ) ((:w :b) (:gxm) 1         ))
  ;; ((      #xD2 mod50 sib0    ) ((:gxm  8) (:gpr :cl)))
  ((sz? rex #xD2 mod50 sib0    ) ((:w :b) (:gxm) (:gpr :cl)))
  ;; ((      #xC0 mod50 sib0 op1) ((:gxm  8) (:imm   8)))
  ((sz? rex #xC0 mod50 sib0 op1) ((:w :b) (:gxm) (:imm   8)))
  ((sz? rex #xD1 mod50 sib0    ) ((:w :w) (:gxm) 1         ))
  ((sz? rex #xD3 mod50 sib0    ) ((:w :w) (:gxm) (:gpr :cl)))
  ((sz? rex #xC1 mod50 sib0 op1) ((:w :w) (:gxm) (:imm   8)))
  ((sz? rex #xD1 mod50 sib0    ) ((:w :d) (:gxm) 1         ))
  ((sz? rex #xD1 mod50 sib0    ) ((:w :q) (:gxm) 1         ))
  ((sz? rex #xD3 mod50 sib0    ) ((:w :d) (:gxm) (:gpr :cl)))
  ((sz? rex #xD3 mod50 sib0    ) ((:w :q) (:gxm) (:gpr :cl)))
  ((sz? rex #xC1 mod50 sib0 op1) ((:w :d) (:gxm) (:imm   8)))
  ((sz? rex #xC1 mod50 sib0 op1) ((:w :q) (:gxm) (:imm   8))))

(specops cmp (w op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex w op0 op1)) (mod70 (determine-modrm 7 op0))
    (sz? (determine-pfrsize w op0 op1 (of-program :exmode)))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                           0       2         1))
  ((    rex #x3C                 op1 ) ((:w :b) (:gpr :a) (:imm   )))
  ((sz? rex #x3D            (w 2 op1)) ((:w :w) (:gpr :a) (:imm   )))
  ((sz? rex #x3D            (w 4 op1)) ((:w :d) (:gpr :a) (:imm   )))
  ((    rex #x3D            (w 4 op1)) ((:w :q) (:gpr :a) (:imm   )))
  ;; ((sz? rex #x80 mod70 sib0      op1 ) ((:gxm    8) (:imm  8)))
  ((sz? rex #x80 mod70 sib0      op1 ) ((:w :b) (:gxm   ) (:imm  8)))
  ((sz? rex #x81 mod70 sib0 (w 2 op1)) ((:w :w) (:gxm   ) (:imm 16)))
  ((sz? rex #x81 mod70 sib0 (w 4 op1)) ((:w :d) (:gxm   ) (:imm 32)))
  ((sz? rex #x81 mod70 sib0 (w 4 op1)) ((:w :q) (:gxm   ) (:imm 32)))
  ((sz? rex #x83 mod70 sib0      op1 ) ((:w :w) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 mod70 sib0      op1 ) ((:w :d) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 mod70 sib0      op1 ) ((:w :q) (:gxm   ) (:imm  8)))
  ;; ((sz? rex #x38 mod10 sib0          ) ((:gxm    8) (:gpr  8)))
  ((sz? rex #x38 mod10 sib0          ) ((:w :b) (:gxm   ) (:gpr   )))
  ((sz? rex #x39 mod10 sib0          ) ((:w :w) (:gxm   ) (:gpr   )))
  ((sz? rex #x39 mod10 sib0          ) ((:w :d) (:gxm   ) (:gpr   )))
  ((sz? rex #x39 mod10 sib0          ) ((:w :q) (:gxm   ) (:gpr   )))
  ;; ((sz? rex #x3A mod01 sib1          ) ((:gpr    8) (:gxm  8)))
  ((sz? rex #x3A mod01 sib1          ) ((:w :b) (:gpr   ) (:gxm   )))
  ((sz? rex #x3B mod01 sib1          ) ((:w :w) (:gpr   ) (:gxm   )))
  ((sz? rex #x3B mod01 sib1          ) ((:w :d) (:gpr   ) (:gxm   )))
  ((sz? rex #x3B mod01 sib1          ) ((:w :q) (:gpr   ) (:gxm   ))))

(specops ja (w op0) *assembler-prototype-x86*
  ((:priority     0       1))
   ((#x77   op0) ((:w :b) (:imm  8)))
   ((#x0F87 op0) ((:w :w) (:imm 16)))
   ((#x0F87 op0) ((:w :d) (:imm 32))))

(specops jae (w op0) *assembler-prototype-x86*
  ((:priority    0       1))
  ((#x73   op0) ((:w :b) (:imm  8)))
  ((#x0F83 op0) ((:w :w) (:imm 16)))
  ((#x0F83 op0) ((:w :d) (:imm 32))))

(specops jb (w op0) *assembler-prototype-x86*
  ((:priority    0       1))
  ((#x72   op0) ((:w :b) (:imm  8)))
  ((#x0F82 op0) ((:w :w) (:imm 16)))
  ((#x0F82 op0) ((:w :d) (:imm 32))))

(specops jbe (w op0) *assembler-prototype-x86*
  ((:priority    0       1))
  ((#x76   op0) ((:w :b) (:imm  8)))
  ((#x0F86 op0) ((:w :w) (:imm 16)))
  ((#x0F86 op0) ((:w :d) (:imm 32))))

(specops jc (w op0) *assembler-prototype-x86*
  ((:priority    0       1))
  ((#x72   op0) ((:w :b) (:imm  8)))
  ((#x0F82 op0) ((:w :w) (:imm 16)))
  ((#x0F82 op0) ((:w :d) (:imm 32))))

(specops jcxz (w op0) *assembler-prototype-x86*
  ((:priority 0       1))
  ((#xE3 op0) ((:w :b) (:imm  8))))

(specops jecxz (w op0) *assembler-prototype-x86*
  ((:priority  0       1))
  ((#xE3 op0) ((:w :b) (:imm  8))))

(specops jrcxz (w op0) *assembler-prototype-x86*
  ((:priority  0       1))
  ((#xE3 op0) ((:w :b) (:imm  8))))

(specops je (w op0) *assembler-prototype-x86*
  ((:priority    0       1))
  ((#x74   op0) ((:w :b) (:imm  8)))
  ((#x0F84 op0) ((:w :w) (:imm 16)))
  ((#x0F84 op0) ((:w :d) (:imm 32))))

(specops jg (w op0) *assembler-prototype-x86*
  ((:priority    0       1))
  ((#x7F   op0) ((:w :b) (:imm  8)))
  ((#x0F8F op0) ((:w :w) (:imm 16)))
  ((#x0F8F op0) ((:w :d) (:imm 32))))

(specops jge (w op0) *assembler-prototype-x86*
  ((:priority    0       1))
  ((#x7D   op0) ((:w :b) (:imm  8)))
  ((#x0F8D op0) ((:w :w) (:imm 16)))
  ((#x0F8D op0) ((:w :d) (:imm 32))))

(specops jl (w op0) *assembler-prototype-x86*
  ((:priority    0       1))
  ((#x7C   op0) ((:w :b) (:imm  8)))
  ((#x0F8C op0) ((:w :w) (:imm 16)))
  ((#x0F8C op0) ((:w :d) (:imm 32))))

(specops jna (w op0) *assembler-prototype-x86*
  ((:priority    0       1))
  ((#x76   op0) ((:w :b) (:imm  8)))
  ((#x0F86 op0) ((:w :w) (:imm 16)))
  ((#x0F86 op0) ((:w :d) (:imm 32))))

(specops jle (w op0) *assembler-prototype-x86*
  ((:priority    0       1))
  ((#x7E   op0) ((:w :b) (:imm  8)))
  ((#x0F8E op0) ((:w :w) (:imm 16)))
  ((#x0F8E op0) ((:w :d) (:imm 32))))

;; ...more to come

;; (specops ret (op0) *assembler-prototype-x86*
;;   ((:priority  0))
;;   ((#xC2 op0) ((:imm 16))) ;; needs work
;;   ((#xCA op0) ((:imm 16)))
;;   ((#xC3 op0))
;;   ((#xCB op0)))

(specops lea (w op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex.w (determine-pfrex w op0 op1))
    (mod01 (determine-modrm op0 op1)) (sib1 (determine-sib op1)))
   (:priority                 0       1      2))
  ((sz? rex #x8D mod01 sib1) ((:w :w) (:gpr) (:mem)))
  ((sz? rex #x8D mod01 sib1) ((:w :w) (:gpr) (:mem)))
  ((sz? rex #x8D mod01 sib1) ((:w :q) (:gpr) (:mem))))

(specops popcnt (w op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex w op0 op1))
    (mod01 (determine-modrm op0 op1)) (sib1 (determine-sib op1)))
   (:priority                    0       1      2))
  ((#xF3 rex #x0FB8 mod01 sib1) ((:w :w) (:gpr) (:gxm)))
  ((#xF3 rex #x0FB8 mod01 sib1) ((:w :w) (:gpr) (:gxm)))
  ((#xF3 rex #x0FB8 mod01 sib1) ((:w :q) (:gpr) (:gxm))))

(specops pshufb (w op0 op1 &optional op2) *assembler-prototype-x86*
  ((:provisions
    ( vex (determine-pfvex  op0 op2 op1 :prefix #x66 :map #x0F38 :long t))
    (evex (determine-pfevex op0 op2 op1 :prefix #x66 :map #x0F38))
    (mod01 (determine-modrm op0 op1)) (mod02 (determine-modrm op0 op2))
    (sib1 (determine-sib op1)) (sib2 (determine-sib op2)))
   (:priority                    0       1      2))
   ;; ((        #x0F3800 mod01 sib1) ((:vcr 64)  (:vxm  64)           ))
  ((     #x660F3800 mod01 sib1) ((:w :x) (:vcr) (:vxm)       ))
  (( vex       #x00 mod02 sib2) ((:w :x) (:vcr) (:vcr) (:vxm)))
  (( vex       #x00 mod02 sib2) ((:w :y) (:vcr) (:vcr) (:vxm)))
  ((evex       #x00 mod02 sib2) ((:w :x) (:vcr) (:vcr) (:vxm)))
  ((evex       #x00 mod02 sib2) ((:w :y) (:vcr) (:vcr) (:vxm)))
  ((evex       #x00 mod02 sib2) ((:w :z) (:vcr) (:vcr) (:vxm))))

(specops pshufd (w op0 op1 op2) *assembler-prototype-x86*
  ((:provisions
    ( vex (determine-pfvex  op0 op1 op2 :prefix #x66 :map #x0F :long t))
    (evex (determine-pfevex op0 op1 op2 :prefix #x66 :map #x0F))
    (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                      0       1      2      3))
  ((     #x660F70 mod01 sib1 op2) ((:w :x) (:vcr) (:vxm) (:imm 8)))
  (( vex     #x70 mod01 sib1 op2) ((:w :x) (:vcr) (:vcr) (:imm 8)))
  (( vex     #x70 mod01 sib1 op2) ((:w :y) (:vcr) (:vcr) (:imm 8)))
  ((evex     #x70 mod01 sib1 op2) ((:w :x) (:vcr) (:vcr) (:imm 8)))
  ((evex     #x70 mod01 sib1 op2) ((:w :y) (:vcr) (:vcr) (:imm 8)))
  ((evex     #x70 mod01 sib1 op2) ((:w :z) (:vcr) (:vcr) (:imm 8))))

(specops vpblendd (w op0 op1 op2 op3) *assembler-prototype-x86*
  ((:provisions
    (vex (determine-pfvex op0 op1 op2 :prefix #x66 :map #x0F3A :long t))
    (mod01 (determine-modrm op0 op1)) (sib1 (determine-sib op1)))
   (:priority                 0       1          2          3          4))
  ((vex #x02 mod01 sib1 op3) ((:w :x) (:vcr 128) (:vcr 128) (:vxm 128) (:imm 8)))
  ((vex #x02 mod01 sib1 op3) ((:w :y) (:vcr 256) (:vcr 256) (:vxm 256) (:imm 8))))
