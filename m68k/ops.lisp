;;;; ops.lisp

(in-package #:specops.m68k)

(specop ori-ccr (op0) *assembler-prototype-m68k*
  "00000000.00111100" op0)

(specop ori-sr (op0) *assembler-prototype-m68k*
  "00000000.01111100" op0)

(specop ori (w op0) *assembler-prototype-m68k*
  "00000000.SSMMMXXX"
  ((s (determine-width w))
   (m (determine-amode op0))
   (x (reg-index op0))))

(specop andi-ccr (w op0) *assembler-prototype-m68k*
  "00000010.00111100" op0)

(specop andi-sr (w op0) *assembler-prototype-m68k*
  "00000010.01111100" op0)

(specop andi (w op0) *assembler-prototype-m68k*
  "00000010.SSMMMXXX"
  ((s (determine-width w))
   (m (determine-addressing-mode op0))
   (x (reg-index op0)))
  op0)

(specop subi (w op0) *assembler-prototype-m68k*
  "00000100.SSMMMXXX"
  ((s (determine-width w))
   (m (determine-addressing-mode op0))
   (x (reg-index op0)))
  op0)

(specop addi (w op0) *assembler-prototype-m68k*
  "00000110.SSMMMXXX"
  ((s (determine-width w))
   (m (determine-addressing-mode op0))
   (x (reg-index op0)))
  op0)

(specop eori-ccr (op0) *assembler-prototype-m68k*
  "00001010.00111100" op0)

(specop eori-sr (op0) *assembler-prototype-m68k*
  "00001010.01111100" op0)

(specop eori (w op0) *assembler-prototype-m68k*
  "00001010.SSMMMXXX"
  ((s (determine-width w))
   (m (determine-addressing-mode op0))
   (x (reg-index op0)))
  op0)

(specop cmpi (w op0) *assembler-prototype-m68k*
  "00001100.SSMMMXXX"
  ((s (determine-width w))
   (m (determine-addressing-mode op0))
   (x (reg-index op0)))
  op0)

(specop btst (op0 op1) *assembler-prototype-m68k*
  "0000RRRV.00MMMXXX"
  ((r (if (numberp op0) #b100 (reg-index op1)))
   (v (if (numberp op0) 0 1))
   (m (determine-addressing-mode op0))
   (x (reg-index op0)))
  op0)

(specop bchg (op0) *assembler-prototype-m68k*
  "0000RRRV.01MMMXXX"
  ((r (if (numberp op0) #b100 (reg-index op1)))
   (v (if (numberp op0) 0 1))
   (m (determine-addressing-mode op0))
   (x (reg-index op0)))
  op0)

(specop bclr (op0) *assembler-prototype-m68k*
  "0000RRRV.10MMMXXX"
  ((r (if (numberp op0) #b100 (reg-index op1)))
   (v (if (numberp op0) 0 1))
   (m (determine-addressing-mode op0))
   (x (reg-index op0)))
  op0)

(specop bset (op0) *assembler-prototype-m68k* "0000RRRV.11MMMXXX" ((r
(if (numberp op0) #b100 (reg-index op1))) (v (if (numberp op0) 0 1))
(m (determine-addressing-mode op0)) (x (reg-index op0))) op0)

(specop movep (w op0 op1) *assembler-prototype-m68k*
  "0000RRR1.DS001XXX"
  ((r (if (typep op0 'm68k-gpregister)
          (reg-index op0) (reg-index op1)))
   (d (if (typep op0 'm68k-mem-access) 1 0))
   (s (case w (:w 0) (:l 1)))
   (x (if (typep op0 'm68k-gpregister)
          (reg-index op1) (reg-index op0))))
  op0)

(specop movea (w op0) *assembler-prototype-m68k*
  "00SSRRR0.10MMMXXX"
  ((s (determine-width w t))
   (r (if (numberp op0) #b100 (reg-index op1)))
   (v (if (numberp op0) 0 1))
   (m (determine-addressing-mode op0))
   (x (reg-index op0)))
  op0)

(specop move (w op0) *assembler-prototype-m68k*
  "00SSYYYA.AAMMMXXX"
  ((s (determine-width w t))
   (y (reg-index op1))
   (a (determine-addressing-mode op1))
   (m (determine-addressing-mode op0))
   (x (reg-index op0))))

;; move from and to...

(specop negx (w op0) *assembler-prototype-m68k*
  "01000000.SSMMMXXX"
  ((s (determine-width w))
   (m (determine-amode op0))
   (x (reg-index op0))))

(specop clr (w op0) *assembler-prototype-m68k*
  "01000010.SSMMMXXX"
  ((s (determine-width w))
   (m (determine-amode op0))
   (x (reg-index op0))))

(specop neg (w op0) *assembler-prototype-m68k*
  "01000100.SSMMMXXX"
  ((s (determine-width w))
   (m (determine-amode op0))
   (x (reg-index op0))))

(specop not (w op0) *assembler-prototype-m68k*
  "01000110.SSMMMXXX"
  ((s (determine-width w))
   (m (determine-amode op0))
   (x (reg-index op0))))

(specop ext (w op0) *assembler-prototype-m68k*
  "01001000.1S000XXX"
  ((s (case w (:w 0) (:l 1)))
   (x (reg-index op0))))

(specop nbcd (op0) *assembler-prototype-m68k*
  "01001000.00MMMXXX"
  ((m (determine-amode op0))
   (x (reg-index op0))))

(specop swap (op0) *assembler-prototype-m68k*
  "01001000.01000XXX"
  ((x (reg-index op0))))

(specop pea (op0) *assembler-prototype-m68k*
  "01001000.01MMMXXX"
  ((m (determine-amode op0))
   (x (reg-index op0))))

(specop illegal () *assembler-prototype-m68k*
  "01001010.11111100")

(specop tas (op0) *assembler-prototype-m68k*
  "01001010.11MMMXXX"
  ((m (determine-amode op0))
   (x (reg-index op0))))

(specop tst (w op0) *assembler-prototype-m68k*
  "01001010.SSMMMXXX"
  ((s (determine-width w))
   (m (determine-amode op0))
   (x (reg-index op0))))
