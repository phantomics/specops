;;;; ops.lisp

(in-package #:specops.m68k)

(specop ori (w op0 op1)
  (cond ((eq op1 :ccr)
         (joinw (masque "00000000.00111100")
                op0))
        ((eq op1 :sr)
         (joinw (masque "00000000.01111100")
                op0))
        ((typep op1 'm68k-gpregister)
         (address (op1) ((index1 amode1))
           (joinw (masque "00000000.SSMMMXXX"
                          (s (determine-width w))
                          (m amode1) (x index1))
                  op0)))))

(of-battery *assembler-prototype-m68k* :ori
            (lambda (word read-words)
              (unmasque "00000000.SSMMMXXX" word (s m x)
                (list :ori (derive-width s) (read-words 1)
                      (derive-location m x)))))

(specop andi (w op0 op1)
  (cond ((eq op1 :ccr)
         (joinw (masque "00000010.00111100")
                op0))
        ((eq op1 :sr)
         (joinw (masque "00000010.01111100")
                op0))
        ((typep op1 'm68k-gpregister)
         (address (op1) ((index1 amode1))
           (joinw (masque "00000010.SSMMMXXX"
                          (s (determine-width w))
                          (m amode1) (x index1))
                  op0)))))

(of-battery *assembler-prototype-m68k* :andi
            (lambda (word read-words)
              (unmasque "00000010.SSMMMXXX" word (s m x)
                (list :andi (derive-width s) (read-words 1)
                      (derive-location m x)))))

(specop subi (w op0 op1)
  (address (op1) ((index1 amode1))
    (joinw (masque "00000100.SSMMMXXX"
                   (s (determine-width w))
                   (m amode1) (x index1))
           op0)))

(of-battery *assembler-prototype-m68k* :subi
            (lambda (word read-words)
              (unmasque "00000100.SSMMMXXX" word (s m x)
                (list :subi (derive-width s) (read-words 1)
                      (derive-location m x)))))

(specop addi (w op0 op1)
  (address (op1) ((index1 amode1))
    (joinw (masque "00000110.SSMMMXXX"
                   (s (determine-width w))
                   (m amode1) (x index1))
           op0)))

(of-battery *assembler-prototype-m68k* :addi
            (lambda (word read-words)
              (unmasque "00000110.SSMMMXXX" word (s m x)
                (list :addi (derive-width s) (read-words 1)
                      (derive-location m x)))))

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

(of-battery *assembler-prototype-m68k* :eori
            (lambda (word read-words)
              (unmasque "00001010.SSMMMXXX" word (s m x)
                (list :eori (derive-width s) (read-words 1)
                      (derive-location m x)))))

(specop cmpi (w op0 op1)
  (address (op1) ((index1 amode1))
    (joinw (masque "00001100.SSMMMXXX"
                   (s (determine-width w))
                   (m amode1) (x index1))
           op0)))

(of-battery *assembler-prototype-m68k* :cmpi
            (lambda (word read-words)
              (unmasque "00001010.SSMMMXXX" word (s m x)
                (list :cmpi (derive-width s) (read-words 1)
                      (derive-location m x)))))

(specop btst (op0 op1)
  (if (numberp op0)
      (address (op1) ((index1 amode1))
        (joinw (masque "00001000.00MMMXXX"
                       (m amode1) (x index1))
               op0))
      (address (op0 op1) ((index0) (index1 amode1))
        (joinw (masque "0000DDD1.00MMMXXX"
                       (d index0) (m amode1) (x index1))))))

(of-battery *assembler-prototype-m68k* :btst-n
            (lambda (word read-words)
              (unmasque "00001000.00MMMXXX" word (m x)
                (list :btst (read-words 1)
                      (derive-location m x)))))

(of-battery *assembler-prototype-m68k* :btst-r
            (lambda (word read-words)
              (unmasque "0000DDD1.00MMMXXX" word (d m x)
                (list :btst (derive-width *** s) (read-words 1)
                      (derive-location m x)))))

(specop bchg (op0 op1)
  (if (numberp op0)
      (address (op1) ((index1 amode1))
        (joinw (masque "00001000.01MMMXXX"
                       (m amode1) (x index1))
               op0))
      (address (op0 op1) ((index0) (index1 amode1))
        (joinw (masque "0000DDD1.01MMMXXX"
                       (d index0) (m amode1) (x index1))))))

(specop bclr (op0 op1)
  (if (numberp op0)
      (address (op1) ((index1 amode1))
        (joinw (masque "00001000.10MMMXXX"
                       (m amode1) (x index1))
               op0))
      (address (op0 op1) ((index0) (index1 amode1))
        (joinw (masque "0000DDD1.10MMMXXX"
                       (d index0) (m amode1) (x index1))))))

(specop bset (op0 op1)
  (if (numberp op0)
      (address (op1) ((index1 amode1))
        (joinw (masque "00001000.11MMMXXX"
                       (m amode1) (x index1))
               op0))
      (address (op0 op1) ((index0) (index1 amode1))
        (joinw (masque "0000DDD1.11MMMXXX"
                       (d index0) (m amode1) (x index1))))))

(specop movep (w op0 op1)
  (address (op0 op1) ((index0) (index1)) ;; FIX TYPE
    (joinw (masque "0000RRR1.DS001XXX"
                   (r (if (typep op0 'm68k-gpregister) index0 index1))
                   (d (if (typep op0 'm68k-gpregister) 1      0))
                   (s (determine-width-bit w))
                   (x (if (typep op0 'm68k-gpregister) index1 index0)))
           op0)))

(specop movea (w op0 op1)
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "00SSRRR0.10MMMXXX"
                   (s (determine-width w t))
                   (r index1) (m amode0) (x index0)))))

(specop move (w op0 op1)
  (cond ((or (and (eq op0 :usp)
                  (typep  op1 'm68k-mas))
             (and (eq op1 :usp)
                  (typep  op0 'm68k-mas)))
         (masque "01001110.0110DAAA"
                 (d (if (eq op0 :usp) 0 1))
                 (a (reg-index (if (typep op1 'm68k-spregister) op0 op1))))) ;; ***
        ((eq op0 :sr) ;; move from SR
         (address (op1) ((index1 amode1))
           (masque "01000000.11MMMXXX"
                   (m amode1) (x index1))))
        ((eq op1 :sr) ;; move to SR
         (address (op0) ((index0 amode0))
           (masque "01000110.11MMMXXX"
                   (m amode0) (x index0))))
        ((typep op0 'm68k-gpregister)
         (masque "00SSYYYA.AAMMMXXX"
                 (s (determine-width w t))
                 (y (reg-index op1))
                 (a (determine-amode op1))
                 (m (determine-amode op0))
                 (x (reg-index op0))))))

;; move from and to...

(specop negx (w op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01000000.SSMMMXXX"
                   (s (determine-width w))
                   (m amode0) (x index0)))))

(specop clr (w op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01000010.SSMMMXXX"
                   (s (determine-width w))
                   (m amode0) (x index0)))))

(specop neg (w op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01000100.SSMMMXXX"
                   (s (determine-width w))
                   (m amode0) (x index0)))))

(specop not (w op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01000110.SSMMMXXX"
                   (s (determine-width w))
                   (m amode0) (x index0)))))

(specop ext (w op0)
  (address (op0) ((index0))
    (joinw (masque "01001000.1S000XXX"
                   (s (determine-width-bit w))
                   (x index0)))))

(specop nbcd (op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01001000.00MMMXXX"
                   (m amode0) (x index0)))))

(specop swap (op0)
  (address (op0) ((index0))
    (joinw (masque "01001000.01000XXX"
                   (x index0)))))
  
(specop pea (op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01001000.01MMMXXX"
                   (m amode0) (x index0)))))

(specop illegal ()
  (joinw (masque "01001010.11111100")))

(specop tas (op0)
  (joinw (masque "01001010.11MMMXXX"
                 (m (determine-amode   op0))
                 (x (base-or-reg-index op0)))))

(specop tst (w op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01001010.SSMMMXXX"
                   (s (determine-width w))
                   (m amode0) (x index0)))))

(specop trap (vector)
  (joinw (masque "01001110.0100VVVV"
                 (v vector))))

(specop link (op0 op1)
  (address (op0) ((index0))
    (joinw (masque "01001110.01010AAA"
                   (a index0))
           op1)))

(specop unlk (op0)
  (address (op0) ((index0))
    (joinw (masque "01001110.01011AAA"
                   (a index0)))))

(specop reset ()
  (joinw (masque "01001110.01110000")))

(specop nop ()
  (joinw (masque "01001110.01110001")))

(specop stop (op0)
  (joinw (masque "01001110.01110001")
         op0))

(specop rte ()
  (joinw (masque "01001110.01110011")))

(specop rts ()
  (joinw (masque "01001110.01110101")))

(specop trapv ()
  (joinw (masque "01001110.01110110")))

(specop rtr ()
  (joinw (masque "01001110.01110111")))

(specop jsr (op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01001110.10MMMXXX"
                   (m amode0) (x index0)))))
  
(specop jmp (op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01001110.11MMMXXX"
                   (m amode0) (x index0)))))

(specop movem (op0)
  (address (op0) ((index0 amode0))
    (joinw (masque "01001D00.1SMMMXXX"
                   ;; direction ?? how does it work
                   ;; !!! needs mask for registers to move
                   (s (determine-width-bit w))
                   (m amode0) (x index0)))))

(specop lea (op0 op1)
  (address (op0) ((index0 amode0))
    (joinw (masque "0100AAA1.11MMMXXX" ;; !!! add memory address
                   ;; memory address
                   (m amode0) (x index0)))))

(specop chk (op0 op1)
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "0100DDD1.10MMMXXX"
                   ;; memory address
                   (d index1) (m amode0) (x index0)))))

(specop addq (w op0 op1)
  (address (op1) ((index1 amode1))
    (joinw (masque "0101DDD0.SSMMMXXX"
                   (d op0) (s (determine-width w))
                   (m amode1) (x index1)))))

#|
(lambda (word)
  (unmasque "0101DDD0.SSMMMXXX" word (d s m x)
            (list :addq (case s (0 :b) (1 :w) (2 :l))
                  d (build-m68k-mas m x))))

|#


(specop subq (op0 op1)
  (address (op1) ((index1 amode1))
    (joinw (masque "0101DDD1.SSMMMXXX"
                   (d op0) (s (determine-width w))
                   (m amode1) (x index1)))))

(specop s (op0)
  ((:combine condition :appending :by-index
    :t :f :hi :ls :cc :cs :ne :eq :vc :vs :pl :mi :ge :lt :gt :le))
  (address (op0) ((index0 amode0))
    (joinw (masque "0101CCCC.11MMMXXX"
                   (c condition) (m amode0) (x index0)))))

(specop db (op0 op1)
  ((:combine condition :appending :by-index
    :t :f :hi :ls :cc :cs :ne :eq :vc :vs :pl :mi :ge :lt :gt :le))
  (address (op0) ((index0))
    (joinw (masque "0101CCCC.11001XXX"
                   (c condition) (x index0))
           op1)))

(specop bra (op0)
  (if (zerop (ash op0 -8))
      (joinw (masque "01100000.DDDDDDDD"
                     (d op0)))
      (joinw (masque "01100000.00000000") ;; case for 16-bit displacement
             op0)))

(specop bsr (op0)
  (if (zerop (ash op0 -8))
      (joinw (masque "01100001.DDDDDDDD"
                     (d op0)))
      (joinw (masque "01100001.00000000") ;; case for 16-bit displacement
             op0)))

(specop b (op0)
  ((:combine condition :appending :by-index
    :t :f :hi :ls :cc :cs :ne :eq :vc :vs :pl :mi :ge :lt :gt :le))
  (joinw (masque "0101CCCC.DDDDDDDD"
                 (c condition) (d op0)))) ;;; ?? join extended D-value?

(specop moveq (op0 op1) ;; ??? account for ops
  (address (op1) ((index1))
    (joinw (masque "0111AAA0.DDDDDDDD"
                   (a index1) (d op0)))))

(specop divu (op0 op1)
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "1000AAA0.11MMMXXX"
                   (a index1) (m amode0) (x index0)))))

(specop divs (op0 op1)
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "1000AAA1.11MMMXXX"
                   (a index1) (m amode0) (x index0)))))

(specop sbcd (op0 op1)
  ;; CHECK FOR D or -A
  (address (op0 op1) ((index0) (index1))
    (joinw (masque "1000AAA1.0000MXXX"
                   (a index1)
                   (m (if (typep op1 'm68k-mas) 1 0))
                   (x index0)))))

(specop or (w op0 op1)
  (address (op0 op1) ((index0 amode0) (index1 amode1))
    (joinw (masque "1000AAAD.SSMMMXXX"
                   (a (if (typep op1 'm68k-mas) index0 index1))
                   (d (if (typep op1 'm68k-mas) 1      0))
                   (s (determine-width w))
                   (m (if (typep op1 'm68k-mas) amode1 amode0))
                   (x (if (typep op1 'm68k-mas) index1 index0))))))

(specop sub (w op0 op1)
  (address (op0 op1) ((index0 amode0) (index1 amode1))
    (joinw (masque "1001AAAD.SSMMMXXX"
                   (a (if (typep op1 'm68k-mas) index0 index1))
                   (d (if (typep op1 'm68k-mas) 1      0))
                   (s (determine-width w))
                   (m (if (typep op1 'm68k-mas) amode1 amode0))
                   (x (if (typep op1 'm68k-mas) index1 index0))))))

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
  (joinw (masque "1100XXX1.MM00NYYY" ;; ***
                 (x (reg-index op1))
                 (m (determine-amode op1))
                 (n (determine-amode op0))
                 (y (base-or-reg-index op0)))))

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

