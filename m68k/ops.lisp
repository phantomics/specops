;;;; ops.lisp

(in-package #:specops.m68k)

;; (specops ori-ccr (op0) *assembler-prototype-m68k*
;;   (join (masque "00000000.00111100")
;;         op0))

;; (specops ori-sr (op0) *assembler-prototype-m68k*
;;   (join (masque "00000000.01111100")
;;         op0))

(specops ori (w op0 op1) *assembler-prototype-m68k*
  (cond ((and (typep op0 'm68k-spregister)
              (eq :ccr (reg-name op0)))
         (join (masque "00000000.00111100")
               op0))
        ((and (typep op0 'm68k-spregister)
              (eq :sr (reg-name op0)))
         (join (masque "00000000.01111100")
               op0))
        ((typep op0 'm68k-gpregister)
         (join (masque "00000000.SSMMMXXX"
                       (s (determine-width w))
                       (m (determine-amode op0))
                       (x (reg-index op0)))
               op1))))

;; (specops andi-ccr (w op0) *assembler-prototype-m68k*
;;   (join (masque "00000010.00111100")
;;         op0))

;; (specops andi-sr (w op0) *assembler-prototype-m68k*
;;   (join (masque "00000010.01111100")
;;         op0))

(specops andi (w op0 op1) *assembler-prototype-m68k*
  (cond ((and (typep op0 'm68k-spregister)
              (eq :ccr (reg-name op0)))
         (join (masque "00000010.00111100")
               op1))
        ((and (typep op0 'm68k-spregister)
              (eq :sr (reg-name op0)))
         (join (masque "00000010.01111100")
               op1))
        ((typep op0 'm68k-gpregister)
         (join (masque "00000010.SSMMMXXX"
                       (s (determine-width w))
                       (m (determine-amode op0))
                       (x (reg-index op0)))
               op1))))

(specops subi (w op0) *assembler-prototype-m68k*
  (join (masque "00000100.SSMMMXXX"
                (s (determine-width w))
                (m (determine-amode op0))
                (x (reg-index op0)))
        op0))

(specops addi (w op0 op1) *assembler-prototype-m68k*
  (join (masque "00000110.SSMMMXXX"
                (s (determine-width w))
                (m (determine-amode op0))
                (x (reg-index op0)))
        op1))

;; (specops eori-ccr (op0) *assembler-prototype-m68k*
;;   (join (masque "00001010.00111100")
;;         op0))

;; (specops eori-sr (op0) *assembler-prototype-m68k*
;;   (join (masque "00001010.01111100")
;;         op0))

(specops eori (w op0) *assembler-prototype-m68k*
  (cond ((and (typep op0 'm68k-spregister)
              (eq :ccr (reg-name op0)))
         (join (masque "00001010.00111100")
               op0))
        ((and (typep op0 'm68k-spregister)
              (eq :sr (reg-name op0)))
         (join (masque "00001010.01111100")
               op0))
        ((typep op0 'm68k-gpregister)
         (join (masque "00001010.SSMMMXXX"
                       (s (determine-width w))
                       (m (determine-amode op0))
                       (x (reg-index op0)))
               op0))))

(specops cmpi (w op0) *assembler-prototype-m68k*
  (join (masque "00001100.SSMMMXXX"
                (s (determine-width w))
                (m (determine-amode op0))
                (x (reg-index op0)))
        op0))

(specops btst (op0 op1) *assembler-prototype-m68k*
  (join (masque "0000RRRV.00MMMXXX"
                (r (if (numberp op0) #b100 (reg-index op1)))
                (v (if (numberp op0) 0 1))
                (m (determine-amode op0))
                (x (reg-index op0)))
        op0))

(specops bchg (op0) *assembler-prototype-m68k*
  (join (masque "0000RRRV.01MMMXXX"
                (r (if (numberp op0) #b100 (reg-index op1)))
                (v (if (numberp op0) 0 1))
                (m (determine-amode op0))
                (x (reg-index op0)))
        op0))

(specops bclr (op0) *assembler-prototype-m68k*
  (join (masque "0000RRRV.10MMMXXX"
                (r (if (numberp op0) #b100 (reg-index op1)))
                (v (if (numberp op0) 0 1))
                (m (determine-amode op0))
                (x (reg-index op0)))
        op0))

(specops bset (op0 op1) *assembler-prototype-m68k*
  (join (masque "0000RRRV.11MMMXXX"
                (r (if (numberp op0) #b100 (reg-index op1)))
                (v (if (numberp op0) 0 1))
                (m (determine-amode op0))
                (x (reg-index op0)))
        op0))

(specops movep (w op0 op1) *assembler-prototype-m68k*
  (join (masque "0000RRR1.DS001XXX"
                (r (if (typep op0 'm68k-gpregister)
                       (reg-index op0) (reg-index op1)))
                (d (if (typep op0 'm68k-mem-access) 1 0))
                (s (case w (:w 0) (t 1)))
                (x (if (typep op0 'm68k-gpregister)
                       (reg-index op1) (reg-index op0))))
        op0))

(specops movea (w op0 op1) *assembler-prototype-m68k*
  (join (masque "00SSRRR0.10MMMXXX"
                (s (determine-width w t))
                (r (if (numberp op0) #b100 (reg-index op1)))
                (m (determine-amode op0))
                (x (reg-index op0)))
        op0))

(specops move (w op0 op1) *assembler-prototype-m68k*
  (cond ((or (and (typep  op0 'm68k-spregister)
                  (eq    :usp (reg-name op0))
                  (typep  op1 'm68k-memory-access))
             (and (typep  op1 'm68k-spregister)
                  (eq    :usp (reg-name op1))
                  (typep  op0 'm68k-memory-access)))
         (masque "01001110.0110DAAA"
                 (d (if (typep op0 'm68k-spregister) 0 1))
                 (a (reg-index (if (typep op1 'm68k-spregister) op0 op1)))))
        ((and (typep op0 'm68k-spregister) ;; move from SR
              (eq    :sr (reg-name op0)))
         (masque "01000000.11MMMXXX"
                 (m (determine-amode op1))
                 (x (reg-index op1))))
        ((and (typep op1 'm68k-spregister) ;; move to SR
              (eq    :sr (reg-name op1)))
         (masque "01000110.11MMMXXX"
                 (m (determine-amode op1))
                 (x (reg-index op1))))
        ((typep op0 'm68k-gpregister)
         (masque "00SSYYYA.AAMMMXXX"
                 (s (determine-width w t))
                 (y (reg-index op1))
                 (a (determine-amode op1))
                 (m (determine-amode op0))
                 (x (reg-index op0))))))

;; move from and to...

(specops negx (w op0) *assembler-prototype-m68k*
  (join (masque "01000000.SSMMMXXX"
                (s (determine-width w))
                (m (determine-amode op0))
                (x (reg-index       op0)))))

(specops clr (w op0) *assembler-prototype-m68k*
  (join (masque "01000010.SSMMMXXX"
                (s (determine-width w))
                (m (determine-amode op0))
                (x (reg-index       op0)))))

(specops neg (w op0) *assembler-prototype-m68k*
  (join (masque "01000100.SSMMMXXX"
                (s (determine-width w))
                (m (determine-amode op0))
                (x (reg-index       op0)))))

(specops not (w op0) *assembler-prototype-m68k*
  (join (masque "01000110.SSMMMXXX"
                (s (determine-width w))
                (m (determine-amode op0))
                (x (reg-index       op0)))))

(specops ext (w op0) *assembler-prototype-m68k*
  (join (masque "01001000.1S000XXX"
                (s (case w (:w 0) (:l 1)))
                (x (reg-index op0)))))

(specops nbcd (op0) *assembler-prototype-m68k*
  (join (masque "01001000.00MMMXXX"
                (m (determine-amode op0))
                (x (reg-index op0)))))

(specops swap (op0) *assembler-prototype-m68k*
  (join (masque "01001000.01000XXX"
                (x (reg-index op0)))))

(specops pea (op0) *assembler-prototype-m68k*
  (join (masque "01001000.01MMMXXX"
                (m (determine-amode op0))
                (x (reg-index op0)))))

(specops illegal () *assembler-prototype-m68k*
  (join (masque "01001010.11111100")))

(specops tas (op0) *assembler-prototype-m68k*
  (join (masque "01001010.11MMMXXX"
                (m (determine-amode op0))
                (x (reg-index op0)))))

(specops tst (w op0) *assembler-prototype-m68k*
  (join (masque "01001010.SSMMMXXX"
                (s (determine-width w))
                (m (determine-amode op0))
                (x (reg-index op0)))))

(specops trap (vector) *assembler-prototype-m68k*
  (join (masque "01001110.0100VVVV"
                (v vector))))

(specops link (op0 op1) *assembler-prototype-m68k*
  (join (masque "01001110.01010AAA"
                (a (reg-index op0)))
        op1))

(specops unlk (op0) *assembler-prototype-m68k*
  (join (masque "01001110.01011AAA"
                (a (reg-index op0)))))

(specops reset () *assembler-prototype-m68k*
  (join (masque "01001110.01110000")))

(specops nop () *assembler-prototype-m68k*
  (join (masque "01001110.01110001")))

(specops stop (op0) *assembler-prototype-m68k*
  (join (masque "01001110.01110001")
        op0))

(specops rte () *assembler-prototype-m68k*
  (join (masque "01001110.01110011")))

(specops rts () *assembler-prototype-m68k*
  (join (masque "01001110.01110101")))

(specops trapv () *assembler-prototype-m68k*
  (join (masque "01001110.01110110")))

(specops rtr () *assembler-prototype-m68k*
  (join (masque "01001110.01110111")))

(specops jsr (op0) *assembler-prototype-m68k*
  (join (masque "01001110.10MMMXXX"
                (m (determine-amode op0))
                (x (reg-index op0)))))

(specops jmp (op0) *assembler-prototype-m68k*
  (join (masque "01001110.11MMMXXX"
                (m (determine-amode op0))
                (x (reg-index op0)))))

(specops movem (op0) *assembler-prototype-m68k*
  (join (masque "01001D00.1SMMMXXX"
                ;; direction ?? how does it work
                (s (case w (:w 0) (:l 1)))
                (m (determine-amode op0))
                (x (reg-index op0)))))

(specops lea (op0 op1) *assembler-prototype-m68k*
  (join (masque "0100AAA1.11MMMXXX"
                ;; memory address
                (m (determine-amode op0))
                (x (reg-index op0)))))

(specops chk (op0 op1) *assembler-prototype-m68k*
  (join (masque "0100DDD1.10MMMXXX"
                ;; memory address
                (d (reg-index op1))
                (m (determine-amode op0))
                (x (reg-index op0)))))

(specops addq (op0 op1) *assembler-prototype-m68k*
  (join (masque "0101DDD0.SSMMMXXX"
                (d op0)
                (s (determine-width w))
                (m (determine-amode op1))
                (x (reg-index op1)))))

(specops subq (op0 op1) *assembler-prototype-m68k*
  (join (masque "0101DDD1.SSMMMXXX"
                (d op0)
                (s (determine-width w))
                (m (determine-amode op1))
                (x (reg-index op1)))))

(specops s (op0) *assembler-prototype-m68k*
  ((:combine condition :appending :by-index
             :t :f :hi :ls :cc :cs :ne :eq :vc :vs :pl :mi :ge :lt :gt :le))
  (join (masque "0101CCCC.11MMMXXX"
                (c condition)
                (m (determine-amode op0))
                (x (reg-index op0)))))

(specops db (op0) *assembler-prototype-m68k*
  ((:combine condition :appending :by-index
             :t :f :hi :ls :cc :cs :ne :eq :vc :vs :pl :mi :ge :lt :gt :le))
  (join (masque "0101CCCC.11001XXX"
                (c condition)
                (x (reg-index op0)))))

