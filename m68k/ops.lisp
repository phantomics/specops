;;;; ops.lisp

(in-package #:specops.m68k)

(specop ori (w op0 op1)
  (cond ((eq op1 :ccr)
         (joinw (masque "00000000.00111100")
                op0))
        ((eq op1 :sr)
         (joinw (masque "00000000.01111100")
                op0))
        ((storage-type-p :gpr op1)
         (address (op1) ((index1 amode1))
           (joinw (masque "00000000.SSMMMXXX"
                          (s (determine-width w))
                          (m amode1) (x index1))
                  op0)))))

(readop ori (word read)
  (unmasque "00000000.SSMMMXXX" word (s m x)
    (list :ori (derive-width s) (funcall read 1)
          (derive-location m x))))

(specop andi (w op0 op1)
  (cond ((eq op1 :ccr)
         (joinw (masque "00000010.00111100")
                op0))
        ((eq op1 :sr)
         (joinw (masque "00000010.01111100")
                op0))
        ((storage-type-p :gpr op1)
         (address (op1) ((index1 amode1))
           (joinw (masque "00000010.SSMMMXXX"
                          (s (determine-width w))
                          (m amode1) (x index1))
                  op0)))))

(readop (masque "00000010.00111100") (read)
  (list :andi (funcall read 1) :ccr))

(readop (masque "00000010.01111100") (read)
  (list :andi (funcall read 1) :sr))

(readop andi (word read)
  (unmasque "00000010.SSMMMXXX" word (s m x)
    (list :andi (derive-width s) (funcall read 1)
          (derive-location m x))))

(specop subi (w op0 op1)
  (address (op1) ((index1 amode1))
    (joinw (masque "00000100.SSMMMXXX"
                   (s (determine-width w))
                   (m amode1) (x index1))
           op0)))

(readop subi (word read)
  (unmasque "00000100.SSMMMXXX" word (s m x)
    (list :subi (derive-width s) (funcall read 1)
          (derive-location m x))))

(specop addi (w op0 op1)
  (address (op1) ((index1 amode1))
    (joinw (masque "00000110.SSMMMXXX"
                   (s (determine-width w))
                   (m amode1) (x index1))
           op0)))

(readop addi (word read)
  (unmasque "00000110.SSMMMXXX" word (s m x)
    (list :addi (derive-width s) (funcall read 1)
          (derive-location m x))))

(specop eori (w op0 op1)
  (cond ((eq op1 :ccr)
         (joinw (masque "00001010.00111100")
                op0))
        ((eq op1 :sr)
         (joinw (masque "00001010.01111100")
                op0))
        ((typep op0 'm68k-gpregister)
         (address (op1) ((index1 amode1))
           (joinw (masque "00001010.SSMMMXXX"
                          (s (determine-width w))
                          (m amode1) (x index1))
                  op0)))))

(readop (masque "00001010.00111100") (read)
  (list :eori (funcall read 1) :ccr))

(readop (masque "00001010.01111100") (read)
  (list :eori (funcall read 1) :sr))

(readop eori (word read)
  (unmasque "00001010.SSMMMXXX" word (s m x)
    (list :eori (derive-width s) (funcall read 1)
          (derive-location m x))))

(specop cmpi (w op0 op1)
  (address (op1) ((index1 amode1))
    (joinw (masque "00001100.SSMMMXXX"
                   (s (determine-width w))
                   (m amode1) (x index1))
           op0)))

(readop cmpi (word read)
  (unmasque "00001010.SSMMMXXX" word (s m x)
    (list :cmpi (derive-width s) (funcall read 1)
          (derive-location m x))))

(specop btst (op0 op1)
  (if (numberp op0)
      (address (op1) ((index1 amode1))
        (joinw (masque "00001000.00MMMXXX"
                       (m amode1) (x index1))
               op0))
      (address (op0 op1) ((index0) (index1 amode1))
        (joinw (masque "0000DDD1.00MMMXXX"
                       (d index0) (m amode1) (x index1))))))

(readop btst-n (word read)
  (unmasque "00001000.00MMMXXX" word (m x)
    (list :btst (funcall read 1) (derive-location m x))))

(readop btst-r (word read)
  (unmasque "0000DDD1.00MMMXXX" word (d m x)
    (list :btst d (derive-location m x))))

(specop bchg (op0 op1)
  (if (numberp op0)
      (address (op1) ((index1 amode1))
        (joinw (masque "00001000.01MMMXXX"
                       (m amode1) (x index1))
               op0))
      (address (op0 op1) ((index0) (index1 amode1))
        (joinw (masque "0000DDD1.01MMMXXX"
                       (d index0) (m amode1) (x index1))))))

(readop bchg-n (word read)
  (unmasque "00001000.01MMMXXX" word (m x)
    (list :bchg (funcall read 1) (derive-location m x))))

(readop bchg-r (word read)
  (unmasque "0000DDD1.01MMMXXX" word (d m x)
    (list :bchg d (derive-location m x))))

(specop bclr (op0 op1)
  (if (numberp op0)
      (address (op1) ((index1 amode1))
        (joinw (masque "00001000.10MMMXXX"
                       (m amode1) (x index1))
               op0))
      (address (op0 op1) ((index0) (index1 amode1))
        (joinw (masque "0000DDD1.10MMMXXX"
                       (d index0) (m amode1) (x index1))))))

(readop bclr-n (word read)
  (unmasque "00001000.10MMMXXX" word (d m x)
    (list :bclr (funcall read 1) (derive-location m x))))

(readop bclr-r (word read)
  (unmasque "0000DDD1.10MMMXXX" word (d m x)
    (list :bclr d (derive-location m x))))

(specop bset (op0 op1)
  (if (numberp op0)
      (address (op1) ((index1 amode1))
        (joinw (masque "00001000.11MMMXXX"
                       (m amode1) (x index1))
               op0))
      (address (op0 op1) ((index0) (index1 amode1))
        (joinw (masque "0000DDD1.11MMMXXX"
                       (d index0) (m amode1) (x index1))))))

(readop bset-n (word read)
  (unmasque "00001000.11MMMXXX" word (d m x)
    (list :bset (funcall read 1) (derive-location m x))))

(readop bset-r (word read)
  (unmasque "0000DDD1.11MMMXXX" word (d m x)
    (list :bset d (derive-location m x))))

;; (specop movep (w op0 op1)
;;   (address (op0 op1) ((index0) (index1)) ;; FIX TYPE
;;     (joinw (masque "0000RRR1.DS001XXX"
;;                    (r (if (typep op0 'm68k-gpregister) index0 index1))
;;                    (d (if (typep op0 'm68k-gpregister) 1      0))
;;                    (s (determine-width-bit w))
;;                    (x (if (typep op0 'm68k-gpregister) index1 index0)))
;;            op0)))

(specop movea (w op0 op1)
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "00SSRRR0.10MMMXXX"
                   (s (determine-width w t))
                   (r index1) (m amode0) (x index0)))))

(readop movea (word read)
  (unmasque "0000DDD1.11MMMXXX" word (d m x)
    (list :movea (derive-location m x) d)))

;; (specop move (w op0 op1)
;;   (cond ((or (and (eq op0 :usp)
;;                   (typep  op1 'm68k-mas))
;;              (and (eq op1 :usp)
;;                   (typep  op0 'm68k-mas)))
;;          (masque "01001110.0110DAAA"
;;                  (d (if (eq op0 :usp) 0 1))
;;                  (a (reg-index (if (typep op1 'm68k-spregister) op0 op1))))) ;; ***
;;         ((eq op0 :sr) ;; move from SR
;;          (address (op1) ((index1 amode1))
;;            (masque "01000000.11MMMXXX"
;;                    (m amode1) (x index1))))
;;         ((eq op1 :sr) ;; move to SR
;;          (address (op0) ((index0 amode0))
;;            (masque "01000110.11MMMXXX"
;;                    (m amode0) (x index0))))
;;         ((typep op0 'm68k-gpregister)
;;          (masque "00SSYYYA.AAMMMXXX"
;;                  (s (determine-width w t))
;;                  (y (reg-index op1))
;;                  (a (determine-amode op1))
;;                  (m (determine-amode op0))
;;                  (x (reg-index op0))))))

;; move from and to...

(specop negx (w op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01000000.SSMMMXXX"
                   (s (determine-width w))
                   (m amode0) (x index0)))))

(readop negx (word read)
  (unmasque "01000000.SSMMMXXX" word (s m x)
    (list :negx (derive-width s) (derive-location m x))))

(specop clr (w op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01000010.SSMMMXXX"
                   (s (determine-width w))
                   (m amode0) (x index0)))))

(readop clr (word read)
  (unmasque "01000010.SSMMMXXX" word (s m x)
    (list :clr (derive-width s) (derive-location m x))))

(specop neg (w op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01000100.SSMMMXXX"
                   (s (determine-width w))
                   (m amode0) (x index0)))))

(readop neg (word read)
  (unmasque "01000100.SSMMMXXX" word (s m x)
    (list :neg (derive-width s) (derive-location m x))))

(specop not (w op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01000110.SSMMMXXX"
                   (s (determine-width w))
                   (m amode0) (x index0)))))

(readop not (word read)
  (unmasque "01000110.SSMMMXXX" word (s m x)
    (list :not (derive-width s) (derive-location m x))))

(specop ext (w op0)
  (address (op0) ((index0))
    (joinw (masque "01001000.1S000XXX"
                   (s (determine-width-bit w))
                   (x index0)))))

(readop ext (word read)
  (unmasque "01001000.1S000XXX" word (s x)
    (list :ext (derive-width-bit s) (derive-location 0 x))))

(specop nbcd (op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01001000.00MMMXXX"
                   (m amode0) (x index0)))))

(readop nbcd (word read)
  (unmasque "01000110.SSMMMXXX" word (s m x)
    (list :nbcd (derive-width s) (derive-location m x))))

(specop swap (op0)
  (address (op0) ((index0))
    (joinw (masque "01001000.01000XXX"
                   (x index0)))))

(readop swap (word read)
  (unmasque "01001000.01000XXX" word (x)
    (list :swap (derive-location 0 x))))
  
(specop pea (op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01001000.01MMMXXX"
                   (m amode0) (x index0)))))

(readop pea (word read)
  (unmasque "01001000.01MMMXXX" word (m x)
    (list :pea (derive-location m x))))

(specop illegal ()
  (joinw (masque "01001010.11111100")))

(readop (masque "01001010.11111100") (read)
  (list :illegal))

(specop tas (op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01001010.11MMMXXX"
                   (m amode0) (x index0)))))

(readop tas (word read)
  (unmasque "01001010.11MMMXXX" word (m x)
    (list :tas (derive-location m x))))

(specop tst (w op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01001010.SSMMMXXX"
                   (s (determine-width w))
                   (m amode0) (x index0)))))

(readop tst (word read)
  (unmasque "01001010.SSMMMXXX" word (s m x)
    (list :tst (derive-width s) (derive-location m x))))

(specop trap (vector)
  (joinw (masque "01001110.0100VVVV"
                 (v vector))))

(readop trap (word read)
  (unmasque "01001110.0100VVVV" word (v)
    (list :trap v)))

(specop link (op0 op1)
  (address (op0) ((index0))
    (joinw (masque "01001110.01010AAA"
                   (a index0))
           op1)))

(readop link (word read)
  (unmasque "01001110.01010AAA" word (a)
    (list :link (derive-location 1 a) (funcall read 1))))

(specop unlk (op0)
  (address (op0) ((index0))
    (joinw (masque "01001110.01011AAA"
                   (a index0)))))

(readop unlk (word read)
  (unmasque "01001110.01011AAA" word (a)
    (list :unlk (derive-location 1 a))))

(specop reset ()
  (joinw (masque "01001110.01110000")))

(readop (masque "01001110.01110000") (read)
  (list :reset))

(specop nop ()
  (joinw (masque "01001110.01110001")))

(readop (masque "01001110.01110001") (read)
  (list :nop))

(specop stop (op0)
  (joinw (masque "01001110.01110001")
         op0))

(readop (masque "01001110.01110001") (read)
  (list :stop (funcall read 1)))

(specop rte ()
  (joinw (masque "01001110.01110011")))

(readop (masque "01001110.01110011") (read)
  (list :rte))

(specop rts ()
  (joinw (masque "01001110.01110101")))

(readop (masque "01001110.01110101") (read)
  (list :rts))

(specop trapv ()
  (joinw (masque "01001110.01110110")))

(readop (masque "01001110.01110110") (read)
  (list :trapv))

(specop rtr ()
  (joinw (masque "01001110.01110111")))

(readop (masque "01001110.01110111") (read)
  (list :rtr))

(specop jsr (op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01001110.10MMMXXX"
                   (m amode0) (x index0)))))

(readop jsr (word read)
  (unmasque "01001110.10MMMXXX" word (m x)
    (list :jsr (derive-location m x))))

(specop jmp (op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01001110.11MMMXXX"
                   (m amode0) (x index0)))))

(readop jmp (word read)
  (unmasque "01001110.11MMMXXX" word (m x)
    (list :jmp (derive-location m x))))

(specop movem (op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01001D00.1SMMMXXX"
                   ;; direction ?? how does it work
                   ;; *** needs mask for registers to move
                   (s (determine-width-bit w))
                   (m amode0) (x index0)))))

;; *** readop movem

(specop lea (op0 op1)
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "0100AAA1.11MMMXXX"
                   (a index1) (m amode0) (x index0)))))

(readop lea (word read)
  (unmasque "0100AAA1.11MMMXXX" word (a m x)
    (list :lea (derive-location m x) (derive-location 1 a))))

(specop chk (op0 op1)
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "0100DDD1.10MMMXXX"
                   ;; memory address
                   (d index1) (m amode0) (x index0)))))

(readop chk (word read)
  (unmasque "0100DDD1.10MMMXXX" word (d m x)
    (list :chk (derive-location m x) (derive-location 1 d))))

;; note: the immediate value is encoded in the bitfield as 1-7 if the number is the same,
;; but encoded as 0 if the number is 8, amounting to (logand #b111 n)
(specop addq (w op0 op1)
  (address (op1) ((index1 amode1))
    (joinw (masque "0101NNN0.SSMMMXXX"
                   (n (logand #b111 op0)) (s (determine-width w))
                   (m amode1) (x index1)))))

(readop addq (word read)
  (unmasque "0101NNN0.SSMMMXXX" word (n m x)
    (list :addq (derive-width s) (if (zerop n) 8 n) (derive-location m x))))

#|
(lambda (word)
  (unmasque "0101DDD0.SSMMMXXX" word (d s m x)
            (list :addq (case s (0 :b) (1 :w) (2 :l))
                  d (build-m68k-mas m x))))

|#

(specop subq (op0 op1)
  (address (op1) ((index1 amode1))
    (joinw (masque "0101NNN1.SSMMMXXX"
                   (n (logand #b111 op0)) (s (determine-width w))
                   (m amode1) (x index1)))))

(readop subq (word read)
  (unmasque "0101NNN1.SSMMMXXX" word (n m x)
    (list :subq (derive-width s) (if (zerop n) 8 n) (derive-location m x))))

(specop s (op0)
  ((:combine condition :appending :by-index
    :t :f :hi :ls :cc :cs :ne :eq :vc :vs :pl :mi :ge :lt :gt :le))
  (address (op0) ((index0 amode0))
    (joinw (masque "0101CCCC.11MMMXXX"
                   (c condition) (m amode0) (x index0)))))

(readop s (word read)
  (unmasque "0101CCCC.11MMMXXX" word (c m x)
    (list (nth c '(:st :sf :shi :sls :scc :scs :sne :seq :svc :svs :spl :smi :sge :slt :sgt :sle))
          (derive-location m x))))

(specop db (op0 op1)
  ((:combine condition :appending :by-index
    :t :f :hi :ls :cc :cs :ne :eq :vc :vs :pl :mi :ge :lt :gt :le))
  (address (op0) ((index0))
    (joinw (masque "0101CCCC.11001XXX"
                   (c condition) (x index0))
           op1)))

(readop db (word read)
  (unmasque "0101CCCC.11001XXX" word (c x)
    (list (nth c '(:dbt  :dbf  :dbhi :dbls :dbcc :dbcs :dbne :dbeq
                   :dbvc :dbvs :dbpl :dbmi :dbge :dblt :dbgt :dble))
          (derive-location 0 x)
          (funcall read 1))))

(specop bra (op0)
  (assert (integerp op0) (op0)
          "BRA can only take an integer as operand.")
  (if (zerop (ash op0 -8))
      (joinw (masque "01100000.DDDDDDDD"
                     (d op0)))
      (joinw (masque "01100000.00000000") ;; case for 16-bit displacement
             op0)))

(readop bra (word read)
  (unmasque "01100000.DDDDDDDD" word (d)
    (list :bra d)))

(readop (masque "01100000.00000000") (read)
  (list :bra (funcall read 1)))

(specop bsr (op0)
  (assert (integerp op0) (op0)
          "BSR can only take an integer as operand.")
  (if (zerop (ash op0 -8))
      (joinw (masque "01100001.DDDDDDDD"
                     (d op0)))
      (joinw (masque "01100001.00000000") ;; case for 16-bit displacement
             op0)))

(readop bsr (word read)
  (unmasque "01100000.DDDDDDDD" word (d)
    (list :bsr d)))

(readop (masque "01100001.00000000") (read)
  (list :bsr (funcall read 1)))

(specop b (op0) ;; *** !!! Is there a possibility for a 16-bit offset in the next byte?
  ((:combine condition :appending :by-index
    :t :f :hi :ls :cc :cs :ne :eq :vc :vs :pl :mi :ge :lt :gt :le))
  (joinw (masque "0101CCCC.NNNNNNNN"
                 (c condition) (n (if (zerop (ash op0 -8)) op0 0)))
         (if (zerop (ash op0 -8)) nil op0)))

(readop b (word read)
  (unmasque "0101CCCC.NNNNNNNN" word (c n)
    (list (nth c '(:bt :bf :bhi :bls :bcc :bcs :bne :beq :bvc :bvs :bpl :bmi :bge :blt :bgt :ble))
          (if (not (zerop n))
              n (funcall read 1)))))

(specop moveq (op0 op1)
  (address (op1) ((index1))
    (joinw (masque "0111DDD0.NNNNNNNN"
                   (d index1) (n op0)))))

(readop moveq (word read)
  (unmasque "0111DDD0.NNNNNNNN" word (d n)
    (list :moveq n (derive-location 0 d))))

(specop divu (op0 op1)
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "1000AAA0.11MMMXXX"
                   (a index1) (m amode0) (x index0)))))

(readop divu (word read)
  (unmasque "1000AAA0.11MMMXXX" word (a m x)
    (list :divu (derive-location m x) (derive-location 0 a))))

(specop divs (op0 op1)
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "1000AAA1.11MMMXXX"
                   (a index1) (m amode0) (x index0)))))

(readop divs (word read)
  (unmasque "1000AAA1.11MMMXXX" word (a m x)
    (list :divs (derive-location m x) (derive-location 0 a))))

(specop sbcd (op0 op1)
  ;; CHECK FOR D or -A
  (address (op0 op1) ((index0) (index1))
    (joinw (masque "1000AAA1.0000MXXX"
                   (a index1)
                   (m (if (typep op1 'm68k-mas) 1 0))
                   (x index0)))))

(readop sbcd (word read)
  (unmasque "1000AAA1.0000MXXX" word (a m x)
    (list :sbcd (derive-location (* m #b100) x)
          (derive-location (* m #b100) a))))

(specop or (w op0 op1)
  (address (op0 op1) ((index0 amode0) (index1 amode1))
    (joinw (masque "1000AAAD.SSMMMXXX"
                   (a (if (typep op1 'm68k-mas) index0 index1))
                   (d (if (typep op1 'm68k-mas) 1      0))
                   (s (determine-width w))
                   (m (if (typep op1 'm68k-mas) amode1 amode0))
                   (x (if (typep op1 'm68k-mas) index1 index0))))))

(readop or (word read)
  (unmasque "1000AAAD.SSMMMXXX" word (a d s m x)
    (list :or (derive-width s) (if (zerop d) (derive-location 0 a) (derive-location m x))
          (if (zerop d) (derive-location m x) (derive-location 0 a)))))

(specop sub (w op0 op1)
  (address (op0 op1) ((index0 amode0) (index1 amode1))
    (joinw (masque "1001AAAD.SSMMMXXX"
                   (a (if (typep op1 'm68k-mas) index0 index1))
                   (d (if (typep op1 'm68k-mas) 1      0))
                   (s (determine-width w))
                   (m (if (typep op1 'm68k-mas) amode1 amode0))
                   (x (if (typep op1 'm68k-mas) index1 index0))))))

(readop sub (word read)
  (unmasque "1000AAAD.SSMMMXXX" word (a d s m x)
    (list :or (derive-width s) (if (zerop d) (derive-location 0 a) (derive-location m x))
          (if (zerop d) (derive-location m x) (derive-location 0 a)))))

(specop subx (w op0 op1)
  ;; CONTROL FOR BOTH BEING ADDRESS OR DATA REGISTERS
  (assert (storage-type-p :gpr op0 op1) (op0 op1)
          "SUBX operands ~a and ~a must both be data registers or post-decrementing address registers.")
  (address (op0 op1) ((index0) (index1))
    (joinw (masque "1001AAA1.SS00MXXX"
                   (a index1) (s (determine-width w))
                   (m (if (typep op1 'm68k-mas) 1 0))
                   (x index0)))))

(specop suba (w op0 op1)
  (assert (storage-type-p :adr op1) (op1)
          "SUBA operand 1 must be an address register.")
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "1001AAAS.11MMMXXX"
                   (a index1) (s (determine-width-bit w))
                   (m amode0) (x index0)))))

(specop eor (w op0 op1)
  (address (op0 op1) ((index0) (index1 amode1))
    (joinw (masque "1011AAA1.SSMMMXXX"
                   (a index0) (s (determine-width w))
                   (m amode1) (x index1)))))

(specop cmpm (w op0 op1)
  ;; CONTROL FOR ADDRESS REGISTERS - ALWAYS POSTINCREMENT
  (address (op0 op1) ((index0) (index1))
    (joinw (masque "1011AAA1.SS001XXX"
                   (a index1) (s (determine-width w))
                   (x index0)))))

(specop cmp (w op0 op1)
  (assert (storage-type-p :gpr op1) (op1)
          "CMP operand 1 must be a data register.")
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "1011DDD0.SSMMMXXX"
                   (d index1) (s (determine-width w))
                   (m (determine-amode op1))
                   (m amode0) (x index0)))))

(specop cmpa (w op0 op1)
  (assert (storage-type-p :adr op1) (op1)
          "CMPA operand 1 must be an address register.")
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "1011AAAS.11MMMXXX"
                   (a index1) (s (determine-width-bit w))
                   (m amode0) (x index0)))))


(specop mulu (op0 op1)
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "1100DDD0.11MMMXXX"
                   (d index1) (m amode0) (x index0)))))

(specop muls (op0 op1)
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "1100DDD1.11MMMXXX"
                   (d index1) (m amode0) (x index0)))))

(specop abcd (op0 op1)
  ;; CONTROL FOR BOTH Dx OR BOTH Ax
  (address (op0 op1) ((index0) (index1))
    (joinw (masque "1100XXX1.0000MYYY"
                   (x index1)
                   (m (if (typep op1 'm68k-mas) 1 0))
                   (y index0)))))

(specop exg (op0 op1)
  (address (op0 op1) ((index0) (index1))
    (joinw (masque "1100XXX1.MMMMMYYY"
                   (x index0)
                   (m (if (storage-type-p :gpr op0)
                          (if (storage-type-p :gpr op1)
                              #b01000 #b10001)
                          #b01001))
                   (y index1)))))

(specop and (op0 op1)
  (address (op0 op1) ((index0 amode0) (index1 amode1))
    (joinw (masque "1100AAAD.SSMMMXXX"
                   (a (if (typep op1 'm68k-mas) index0 index1))
                   (d (if (typep op1 'm68k-mas) 1      0))
                   (s (determine-width w))
                   (m (if (typep op1 'm68k-mas) amode1 amode0))
                   (x (if (typep op1 'm68k-mas) index1 index0))))))

(specop add (op0 op1)
  (address (op0 op1) ((index0 amode0) (index1 amode1))
    (joinw (masque "1101AAAD.SSMMMXXX"
                   (a (if (typep op1 'm68k-mas) index0 index1))
                   (d (if (typep op1 'm68k-mas) 1      0))
                   (s (determine-width w))
                   (m (if (typep op1 'm68k-mas) amode1 amode0))
                   (x (if (typep op1 'm68k-mas) index1 index0))))))

(specop addx (op0 op1)
  ;; CONTROL FOR TWO DATA OR TWO ADDRESS REGS
  (address (op0 op1) ((index0) (index1))
    (joinw (masque "1101AAA1.SS00MXXX"
                   (a index1) (s (determine-width w))
                   (m (if (typep op1 'm68k-mas) 1 0))
                   (x index0)))))

(specop adda (op0 op1)
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "1101AAAS.11MMMXXX"
                   (a index1) (s (determine-width-bit w))
                   (m amode0) (x index0)))))

(specop as (w op0 &optional op1)
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

(specop ls (w op0 &optional op1)
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

(specop rox (w op0 &optional op1)
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

(specop ro (w op0 &optional op1)
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

;; (specops rox (op0)
;;   ((:combine direction :appending :by-index :r :l))
;;   (joinw (masque "1110010D.11MMMXXX"
;;                  (d direction)
;;                  (m (determine-amode op0))
;;                  (x (base-or-reg-index op0)))))

;; (specops ro (op0)
;;   ((:combine direction :appending :by-index :r :l))
;;   (joinw (masque "1110011D.11MMMXXX"
;;                  (d direction)
;;                  (m (determine-amode op0))
;;                  (x (base-or-reg-index op0)))))

