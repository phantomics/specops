;;;; ops.lisp

(in-package #:specops.m68k)

(specops ori (w op0 op1) *assembler-prototype-m68k*
  (cond ((special-reg-p op0 :ccr)
         (joinw (masque "00000000.00111100")
                op0))
        ((special-reg-p op0 :sr)
         (joinw (masque "00000000.01111100")
                op0))
        ((typep op0 'm68k-gpregister)
         (joinw (masque "00000000.SSMMMXXX"
                        (s (determine-width w))
                        (m (determine-amode op0))
                        (x (base-or-reg-index op0)))
                op1))))

(specops andi (w op0 op1) *assembler-prototype-m68k*
  (cond ((special-reg-p op0 :ccr)
         (joinw (masque "00000010.00111100")
                op1))
        ((special-reg-p op0 :sr)
         (joinw (masque "00000010.01111100")
                op1))
        ((typep op0 'm68k-gpregister)
         (joinw (masque "00000010.SSMMMXXX"
                        (s (determine-width w))
                        (m (determine-amode op0))
                        (x (base-or-reg-index op0)))
                op1))))

(specops subi (w op0) *assembler-prototype-m68k*
  (joinw (masque "00000100.SSMMMXXX"
                 (s (determine-width w))
                 (m (determine-amode op0))
                 (x (base-or-reg-index op0)))
         op0))

(specops addi (w op0 op1) *assembler-prototype-m68k*
  (joinw (masque "00000110.SSMMMXXX"
                 (s (determine-width w))
                 (m (determine-amode op0))
                 (x (base-or-reg-index op0)))
         op1))

(specops eori (w op0) *assembler-prototype-m68k*
  (cond ((special-reg-p op0 :ccr)
         (joinw (masque "00001010.00111100")
                op0))
        ((special-reg-p op0 :sr)
         (joinw (masque "00001010.01111100")
                op0))
        ((typep op0 'm68k-gpregister)
         (joinw (masque "00001010.SSMMMXXX"
                        (s (determine-width w))
                        (m (determine-amode op0))
                        (x (base-or-reg-index op0)))
                op0))))

(specops cmpi (w op0) *assembler-prototype-m68k*
  (joinw (masque "00001100.SSMMMXXX"
                 (s (determine-width   w))
                 (m (determine-amode   op0))
                 (x (base-or-reg-index op0)))
         op0))

(specops btst (op0 op1) *assembler-prototype-m68k*
  (if (numberp op0)
      (joinw (masque "00001000.00MMMXXX"
                     (m (determine-amode   op1))
                     (x (base-or-reg-index op1)))
             op0)
      (joinw (masque "0000DDD1.00MMMXXX"
                     (d (reg-index         op0))
                     (m (determine-amode   op1))
                     (x (base-or-reg-index op1))))))

(specops bchg (op0 op1) *assembler-prototype-m68k*
  (if (numberp op0)
      (joinw (masque "00001000.01MMMXXX"
                     (m (determine-amode   op1))
                     (x (base-or-reg-index op1)))
             op0)
      (joinw (masque "0000DDD1.01MMMXXX"
                     (d (reg-index         op0))
                     (m (determine-amode   op1))
                     (x (base-or-reg-index op1))))))

(specops bclr (op0 op1) *assembler-prototype-m68k*
  (if (numberp op0)
      (joinw (masque "00001000.10MMMXXX"
                     (m (determine-amode   op1))
                     (x (base-or-reg-index op1)))
             op0)
      (joinw (masque "0000DDD1.10MMMXXX"
                     (d (reg-index         op0))
                     (m (determine-amode   op1))
                     (x (base-or-reg-index op1))))))

(specops bset (op0 op1) *assembler-prototype-m68k*
  (if (numberp op0)
      (joinw (masque "00001000.11MMMXXX"
                     (m (determine-amode   op1))
                     (x (base-or-reg-index op1)))
             op0)
      (joinw (masque "0000DDD1.11MMMXXX"
                     (d (reg-index         op0))
                     (m (determine-amode   op1))
                     (x (base-or-reg-index op1))))))

(specops movep (w op0 op1) *assembler-prototype-m68k*
  (joinw (masque "0000RRR1.DS001XXX"
                 (r (if (typep op0 'm68k-gpregister)
                        (reg-index op0) (reg-index op1)))
                 (d (if (typep op0 'm68k-gpregister) 1 0))
                 (s (case w (:w 0) (t 1)))
                 (x (if (typep op0 'm68k-gpregister)
                        (reg-index op1) (reg-index op0))))
         op0))

(specops movea (w op0 op1) *assembler-prototype-m68k*
  (joinw (masque "00SSRRR0.10MMMXXX"
                 (s (determine-width w t))
                 (r (reg-index         op1))
                 (m (determine-amode   op0))
                 (x (base-or-reg-index op0)))))


(specops move (w op0 op1) *assembler-prototype-m68k*
  (cond ((or (and (special-reg-p op0 :usp)
                  (typep  op1 'm68k-memory-access))
             (and (special-reg-p op1 :usp)
                  (typep  op0 'm68k-memory-access)))
         (masque "01001110.0110DAAA"
                 (d (if (typep op0 'm68k-spregister) 0 1))
                 (a (reg-index (if (typep op1 'm68k-spregister) op0 op1)))))
        ((special-reg-p op0 :sr) ;; move from SR
         (masque "01000000.11MMMXXX"
                 (m (determine-amode op1))
                 (x (reg-index op1))))
        ((special-reg-p op1 :sr) ;; move to SR
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
  (joinw (masque "01000000.SSMMMXXX"
                 (s (determine-width   w))
                 (m (determine-amode   op0))
                 (x (base-or-reg-index op0)))))

(specops clr (w op0) *assembler-prototype-m68k*
  (joinw (masque "01000010.SSMMMXXX"
                 (s (determine-width   w))
                 (m (determine-amode   op0))
                 (x (base-or-reg-index op0)))))

(specops neg (w op0) *assembler-prototype-m68k*
  (joinw (masque "01000100.SSMMMXXX"
                 (s (determine-width   w))
                 (m (determine-amode   op0))
                 (x (base-or-reg-index op0)))))

(specops not (w op0) *assembler-prototype-m68k*
  (joinw (masque "01000110.SSMMMXXX"
                 (s (determine-width   w))
                 (m (determine-amode   op0))
                 (x (base-or-reg-index op0)))))

(specops ext (w op0) *assembler-prototype-m68k*
  (joinw (masque "01001000.1S000XXX"
                 (s (determine-width-bit w))
                 (x (reg-index           op0)))))

(specops nbcd (op0) *assembler-prototype-m68k*
  (joinw (masque "01001000.00MMMXXX"
                 (m (determine-amode   op0))
                 (x (base-or-reg-index op0)))))

(specops swap (op0) *assembler-prototype-m68k*
  (joinw (masque "01001000.01000XXX"
                 (x (reg-index op0)))))

(specops pea (op0) *assembler-prototype-m68k*
  (joinw (masque "01001000.01MMMXXX"
                 (m (determine-amode   op0))
                 (x (base-or-reg-index op0)))))

(specops illegal () *assembler-prototype-m68k*
  (joinw (masque "01001010.11111100")))

(specops tas (op0) *assembler-prototype-m68k*
  (joinw (masque "01001010.11MMMXXX"
                 (m (determine-amode   op0))
                 (x (base-or-reg-index op0)))))

(specops tst (w op0) *assembler-prototype-m68k*
  (joinw (masque "01001010.SSMMMXXX"
                 (s (determine-width   w))
                 (m (determine-amode   op0))
                 (x (base-or-reg-index op0)))))

(specops trap (vector) *assembler-prototype-m68k*
  (joinw (masque "01001110.0100VVVV"
                 (v vector))))

(specops link (op0 op1) *assembler-prototype-m68k*
  (joinw (masque "01001110.01010AAA"
                 (a (reg-index op0)))
         op1))

(specops unlk (op0) *assembler-prototype-m68k*
  (joinw (masque "01001110.01011AAA"
                 (a (reg-index op0)))))

(specops reset () *assembler-prototype-m68k*
  (joinw (masque "01001110.01110000")))

(specops nop () *assembler-prototype-m68k*
  (joinw (masque "01001110.01110001")))

(specops stop (op0) *assembler-prototype-m68k*
  (joinw (masque "01001110.01110001")
         op0))

(specops rte () *assembler-prototype-m68k*
  (joinw (masque "01001110.01110011")))

(specops rts () *assembler-prototype-m68k*
  (joinw (masque "01001110.01110101")))

(specops trapv () *assembler-prototype-m68k*
  (joinw (masque "01001110.01110110")))

(specops rtr () *assembler-prototype-m68k*
  (joinw (masque "01001110.01110111")))

(specops jsr (op0) *assembler-prototype-m68k*
  (joinw (masque "01001110.10MMMXXX"
                 (m (determine-amode   op0))
                 (x (base-or-reg-index op0)))))

(specops jmp (op0) *assembler-prototype-m68k*
  (joinw (masque "01001110.11MMMXXX"
                 (m (determine-amode   op0))
                 (x (base-or-reg-index op0)))))

(specops movem (op0) *assembler-prototype-m68k*
  (joinw (masque "01001D00.1SMMMXXX"
                 ;; direction ?? how does it work
                 ;; !!! needs mask for registers to move
                 (s (case w (:w 0) (:l 1)))
                 (m (determine-amode op0))
                 (x (base-or-reg-index op0)))))

(specops lea (op0 op1) *assembler-prototype-m68k*
  (joinw (masque "0100AAA1.11MMMXXX"
                 ;; memory address
                 (m (determine-amode op0))
                 (x (reg-index op0)))))

(specops chk (op0 op1) *assembler-prototype-m68k*
  (joinw (masque "0100DDD1.10MMMXXX"
                 ;; memory address
                 (d (reg-index op1))
                 (m (determine-amode op0))
                 (x (reg-index op0)))))

(specops addq (op0 op1) *assembler-prototype-m68k*
  (joinw (masque "0101DDD0.SSMMMXXX"
                 (d op0)
                 (s (determine-width w))
                 (m (determine-amode op1))
                 (x (reg-index op1)))))

(specops subq (op0 op1) *assembler-prototype-m68k*
  (joinw (masque "0101DDD1.SSMMMXXX"
                 (d op0)
                 (s (determine-width w))
                 (m (determine-amode op1))
                 (x (reg-index op1)))))

(specops s (op0) *assembler-prototype-m68k*
  ((:combine condition :appending :by-index
    :t :f :hi :ls :cc :cs :ne :eq :vc :vs :pl :mi :ge :lt :gt :le))
  (joinw (masque "0101CCCC.11MMMXXX"
                 (c condition)
                 (m (determine-amode op0))
                 (x (reg-index op0)))))

(specops db (op0 op1) *assembler-prototype-m68k*
  ((:combine condition :appending :by-index
    :t :f :hi :ls :cc :cs :ne :eq :vc :vs :pl :mi :ge :lt :gt :le))
  (joinw (masque "0101CCCC.11001XXX"
                 (c condition)
                 (x (reg-index op0)))
         op1))

(specops bra (op0) *assembler-prototype-m68k*
  (if (zerop (ash op0 -8))
      (joinw (masque "01100000.DDDDDDDD"
                     (d op0)))
      (joinw (masque "01100000.00000000") ;; case for 16-bit displacement
             op0)))

(specops bsr (op0) *assembler-prototype-m68k*
  (if (zerop (ash op0 -8))
      (joinw (masque "01100001.DDDDDDDD"
                     (d op0)))
      (joinw (masque "01100001.00000000") ;; case for 16-bit displacement
             op0)))

(specops b (op0) *assembler-prototype-m68k*
  ((:combine condition :appending :by-index
    :t :f :hi :ls :cc :cs :ne :eq :vc :vs :pl :mi :ge :lt :gt :le))
  (joinw (masque "0101CCCC.DDDDDDDD"
                 (c condition)
                 (d op0)))) ;;; ?? join extended D-value?

(specops moveq (op0 op1) *assembler-prototype-m68k*
  (joinw (masque "0111AAA0.DDDDDDDD"
                 (a (m (determine-amode op1)))
                 (d op0))))

(specops divu (op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1000AAA0.11MMMXXX"
                 (a (m (determine-amode op1)))
                 (m (determine-amode op0))
                 (x (reg-index op0)))))

(specops divs (op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1000AAA1.11MMMXXX"
                 (a (m (determine-amode op1)))
                 (m (determine-amode op0))
                 (x (reg-index op0)))))

(specops sbcd (op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1000AAA1.0000MXXX"
                 (a (if (typep op1 'm68k-gpregister)
                        (reg-index op1)
                        (mas-base  op1)))
                 (m (if (typep op1 'm68k-gpregister)
                        0 1))
                 (x (if (typep op0 'm68k-gpregister)
                        (reg-index op0)
                        (mas-base  op0))))))

(specops or (w op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1000AAAD.SSMMMXXX"
                 (a (if (typep op1 'm68k-gpregister)
                        (reg-index op1)
                        (reg-index op0)))
                 (d (if (typep op1 'm68k-gpregister)
                        0 1))
                 (s (determine-width w))
                 (m (determine-amode op0))
                 (x (cond ((typep op1 'm68k-mas)
                           (mas-base op1))
                          ((typep op0 'm68k-mas)
                           (mas-base op0))
                          (t (reg-index op1)))))))

(specops sub (w op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1001AAAD.SSMMMXXX"
                 (a (if (typep op1 'm68k-gpregister)
                        (reg-index op1)
                        (reg-index op0)))
                 (d (if (typep op1 'm68k-gpregister)
                        0 1))
                 (s (determine-width w))
                 (m (determine-amode op0))
                 (x ((cond ((typep op1 'm68k-mas)
                            (mas-base op1))
                           ((typep op0 'm68k-mas)
                            (mas-base op0))
                           (t (reg-index op1))))))))

(specops subx (w op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1001AAA1.SS00MXXX"
                 (a (if (typep op1 'm68k-gpregister)
                        (reg-index op1)
                        (reg-index op0)))
                 (s (determine-width w))
                 (m (if (typep op1 'm68k-gpregister)
                        0 1))
                 (x (cond ((typep op1 'm68k-mas)
                           (mas-base op1))
                          ((typep op0 'm68k-mas)
                           (mas-base op0))
                          (t (reg-index op1)))))))

(specops suba (w op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1001AAAS.11MMMXXX"
                 (a (if (typep op1 'm68k-gpregister)
                        (reg-index op1)
                        (reg-index op0)))
                 (s (determine-width-bit w))
                 (m (if (typep op1 'm68k-gpregister)
                        0 1))
                 (x (cond ((typep op1 'm68k-mas)
                           (mas-base op1))
                          ((typep op0 'm68k-mas)
                           (mas-base op0))
                          (t (reg-index op1)))))))

(specops eor (w op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1011AAA1.SSMMMXXX"
                 (a (reg-index op0))
                 (s (determine-width w))
                 (m (determine-amode op1))
                 (x (base-or-reg-index op1)))))

(specops cmpm (w op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1011AAA1.SS001XXX"
                 (a (reg-index (mas-base op0)))
                 (s (determine-width w))
                 (x (reg-index (mas-base op1))))))

(specops cmp (w op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1011DDD0.SSMMMXXX"
                 (d (reg-index (mas-base op1)))
                 (s (determine-width w))
                 (m (determine-amode op1))
                 (x (if (typep op1 'm68k-mas)
                        (reg-index (mas-base op1))
                        (reg-index op1))))))

(specops cmpa (w op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1011AAAS.11MMMXXX"
                 (a (reg-index (mas-base op1)))
                 (s (determine-width-bit w))
                 (m (determine-amode op0))
                 (x (base-or-reg-index op0)))))

(specops mulu (op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1100DDD0.11MMMXXX"
                 (d (reg-index op1))
                 (m (determine-amode op0))
                 (x (if (typep op0 'm68k-mas)
                        (reg-index (mas-base op0))
                        (reg-index op0))))))

(specops muls (op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1100DDD1.11MMMXXX"
                 (d (reg-index op1))
                 (m (determine-amode op0))
                 (x (base-or-reg-index op0)))))

(specops abcd (op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1100XXX1.0000MYYY"
                 (d (reg-index op1))
                 (m (if (typep op1 'm68k-gpregister)
                        0 1))
                 (x (base-or-reg-index op0)))))

(specops exg (op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1100XXX1.MM00NYYY"
                 (x (reg-index op1))
                 (m (determine-amode op1))
                 (n (determine-amode op0))
                 (y (base-or-reg-index op0)))))

(specops and (op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1100AAAD.SSMMMXXX"
                 (a (if (typep op1 'm68k-gpregister)
                        (reg-index op1)
                        (reg-index op0)))
                 (d (if (typep op1 'm68k-gpregister)
                        0 1))
                 (s (determine-width w))
                 (m (determine-amode op0))
                 (x (cond ((typep op1 'm68k-mas)
                           (mas-base op1))
                          ((typep op0 'm68k-mas)
                           (mas-base op0))
                          (t (reg-index op1)))))))

(specops add (op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1101AAAD.SSMMMXXX"
                 (a (if (typep op1 'm68k-gpregister)
                        (reg-index op1)
                        (reg-index op0)))
                 (d (if (typep op1 'm68k-gpregister)
                        0 1))
                 (s (determine-width w))
                 (m (determine-amode op0))
                 (x (cond ((typep op1 'm68k-mas)
                           (mas-base op1))
                          ((typep op0 'm68k-mas)
                           (mas-base op0))
                          (t (reg-index op1)))))))

(specops addx (op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1101AAA1.SS00MXXX"
                 (a (if (typep op1 'm68k-gpregister)
                        (reg-index op1)
                        (reg-index op0)))
                 (s (determine-width w))
                 (m (if (typep op1 'm68k-gpregister)
                        0 1))
                 (x (cond ((typep op1 'm68k-mas)
                           (mas-base op1))
                          ((typep op0 'm68k-mas)
                           (mas-base op0))
                          (t (reg-index op1)))))))

(specops adda (op0 op1) *assembler-prototype-m68k*
  (joinw (masque "1101AAAS.11MMMXXX"
                 (a (if (typep op1 'm68k-gpregister)
                        (reg-index op1)
                        (reg-index op0)))
                 (s (determine-width-bit w))
                 (m (if (typep op1 'm68k-gpregister)
                        0 1))
                 (x (cond ((typep op1 'm68k-mas)
                           (reg-index (mas-base op1)))
                          ((typep op0 'm68k-mas)
                           (reg-index (mas-base op0)))
                          (t (reg-index op1)))))))

(specops as (w op0 &optional op1) *assembler-prototype-m68k*
  ((:combine direction :appending :by-index :r :l))
  (if op1
      (joinw (masque "1110RRRD.SSM00XXX"
                     (r (if (numberp op0)
                            op0 (reg-index op0)))
                     (d direction)
                     (s (determine-width w))
                     (m (determine-amode op0))
                     (x (reg-index op1))))
      (joinw (masque "1110000D.11MMMXXX"
                     (d direction)
                     (m (determine-amode op0))
                     (x (base-or-reg-index op0))))))

(specops ls (w op0 &optional op1) *assembler-prototype-m68k*
  ((:combine direction :appending :by-index :r :l))
  (if op1
      (joinw (masque "1110RRRD.SSM01XXX"
                     (r (if (numberp op0)
                            op0 (reg-index op0)))
                     (d direction)
                     (s (determine-width w))
                     (m (determine-amode op0))
                     (x (reg-index op1))))
      (joinw (masque "1110001D.11MMMXXX"
                     (d direction)
                     (m (determine-amode op0))
                     (x (base-or-reg-index op0))))))

(specops rox (w op0 &optional op1) *assembler-prototype-m68k*
  ((:combine direction :appending :by-index :r :l))
  (if op1
      (joinw (masque "1110RRRD.SSM10XXX"
                     (r (if (numberp op0)
                            op0 (reg-index op0)))
                     (d direction)
                     (s (determine-width w))
                     (m (determine-amode op0))
                     (x (reg-index op1))))
      (joinw (masque "1110010D.11MMMXXX"
                     (d direction)
                     (m (determine-amode op0))
                     (x (base-or-reg-index op0))))))

(specops ro (w op0 &optional op1) *assembler-prototype-m68k*
  ((:combine direction :appending :by-index :r :l))
  (if op1
      (joinw (masque "1110RRRD.SSM11XXX"
                     (r (if (numberp op0)
                            op0 (reg-index op0)))
                     (d direction)
                     (s (determine-width w))
                     (m (determine-amode op0))
                     (x (reg-index op1))))
      (joinw (masque "1110011D.11MMMXXX"
                     (d direction)
                     (m (determine-amode op0))
                     (x (base-or-reg-index op0))))))

;; (specops rox (op0) *assembler-prototype-m68k*
;;   ((:combine direction :appending :by-index :r :l))
;;   (joinw (masque "1110010D.11MMMXXX"
;;                  (d direction)
;;                  (m (determine-amode op0))
;;                  (x (base-or-reg-index op0)))))

;; (specops ro (op0) *assembler-prototype-m68k*
;;   ((:combine direction :appending :by-index :r :l))
;;   (joinw (masque "1110011D.11MMMXXX"
;;                  (d direction)
;;                  (m (determine-amode op0))
;;                  (x (base-or-reg-index op0)))))

