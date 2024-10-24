;;;; ops.lisp

(in-package #:specops.m68k)

(specop ori (w op0 op1)
  (cond ((and (eq op1 :ccr) (match-types op0  integer))
         (joinw (masque "00000000.00111100")
                (imm op0)))
        ((and (eq op1 :sr) (match-types op0  integer))
         (joinw (masque "00000000.01111100")
                (imm op0)))
        ((match-types op0 op1  integer location)
         (address (op1) ((index1 amode1))
           (joinw (masque "00000000.SSMMMXXX"
                          (s (determine-width w)) (m amode1) (x index1))
                  (imm op0))))
        (t (error "Invalid operands for ORI."))))

(readop ori (word read)
  (unmasque "00000000.SSMMMXXX" word (s m x)
    (list :ori (derive-width s) (funcall read 1)
          (derive-location m x))))

(specop andi (w op0 op1)
  (cond ((and (eq op1 :ccr) (match-types op0  integer))
         (joinw (masque "00000010.00111100")
                (imm op0)))
        ((and (eq op1 :sr) (match-types op0  integer))
         (joinw (masque "00000010.01111100")
                (imm op0)))
        ((match-types op0 op1  integer location)
         (address (op1) ((index1 amode1))
           (joinw (masque "00000010.SSMMMXXX"
                          (s (determine-width w)) (m amode1) (x index1))
                  (imm op0))))
        (t (error "Invalid operands for ANDI."))))

(readop (masque "00000010.00111100") (read)
  (list :andi (funcall read 1) :ccr))

(readop (masque "00000010.01111100") (read)
  (list :andi (funcall read 1) :sr))

(readop andi (word read)
  (unmasque "00000010.SSMMMXXX" word (s m x)
    (list :andi (derive-width s) (funcall read 1)
          (derive-location m x))))

(specop subi (w op0 op1)
  (assert (match-types op0 op1  integer location)
          (op0 op1)
          "SUBI operands ~a and ~a must be an immediate integer fitting within 32 bits and a location.")
  (address (op1) ((index1 amode1))
    (joinw (masque "00000100.SSMMMXXX"
                   (s (determine-width w)) (m amode1) (x index1))
           (imm op0))))

(readop subi (word read)
  (unmasque "00000100.SSMMMXXX" word (s m x)
    (list :subi (derive-width s) (funcall read 1)
          (derive-location m x))))

(specop addi (w op0 op1)
  (assert (match-types op0 op1  integer location)
          (op0 op1)
          "ADDI operands ~a and ~a must be an immediate integer fitting within 32 bits and a location.")
  (address (op1) ((index1 amode1))
    (joinw (masque "00000110.SSMMMXXX"
                   (s (determine-width w)) (m amode1) (x index1))
           (imm op0))))

(readop addi (word read)
  (unmasque "00000110.SSMMMXXX" word (s m x)
    (list :addi (derive-width s) (funcall read 1)
          (derive-location m x))))

(specop eori (w op0 op1)
  (cond ((and (eq op1 :ccr) (match-types op0  integer))
         (joinw (masque "00001010.00111100")
                (imm op0)))
        ((and (eq op1 :sr) (match-types op0  integer))
         (joinw (masque "00001010.01111100")
                (imm op0)))
        ((match-types op0 op1  integer location)
         (address (op1) ((index1 amode1))
           (joinw (masque "00001010.SSMMMXXX"
                          (s (determine-width w)) (m amode1) (x index1))
                  (imm op0))))
        (t (error "Invalid operands for EORI."))))

(readop (masque "00001010.00111100") (read)
  (list :eori (funcall read 1) :ccr))

(readop (masque "00001010.01111100") (read)
  (list :eori (funcall read 1) :sr))

(readop eori (word read)
  (unmasque "00001010.SSMMMXXX" word (s m x)
    (list :eori (derive-width s) (funcall read 1)
          (derive-location m x))))

(specop cmpi (w op0 op1)
  (assert (match-types op0 op1  integer location)
          (op0 op1)
          "CMPI operands ~a and ~a must be an immediate integer fitting within 32 bits and a location.")
  (address (op1) ((index1 amode1))
    (joinw (masque "00001100.SSMMMXXX"
                   (s (determine-width w)) (m amode1) (x index1))
           (imm op0))))

(readop cmpi (word read)
  (unmasque "00001010.SSMMMXXX" word (s m x)
    (list :cmpi (derive-width s) (funcall read 1)
          (derive-location m x))))

(specop btst (op0 op1)
  (if (numberp op0)
      (address (op1) ((index1 amode1))
        (joinw (masque "00001000.00MMMXXX"
                       (m amode1) (x index1))
               (imm op0)))
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
               (imm op0)))
      (address (op0 op1) ((index0) (index1 amode1))
        (joinw (masque "0000DDD1.10MMMXXX"
                       (d index0) (m amode1) (x index1))))))

(readop bclr-n (word read)
  (unmasque "00001000.10MMMXXX" word (m x)
    (list :bclr (funcall read 1) (derive-location m x))))

(readop bclr-r (word read)
  (unmasque "0000DDD1.10MMMXXX" word (d m x)
    (list :bclr d (derive-location m x))))

(specop bset (op0 op1)
  (if (numberp op0)
      (address (op1) ((index1 amode1))
        (joinw (masque "00001000.11MMMXXX"
                       (m amode1) (x index1))
               (imm op0)))
      (address (op0 op1) ((index0) (index1 amode1))
        (joinw (masque "0000DDD1.11MMMXXX"
                       (d index0) (m amode1) (x index1))))))

(readop bset-n (word read)
  (unmasque "00001000.11MMMXXX" word (m x)
    (list :bset (funcall read 1) (derive-location m x))))

(readop bset-r (word read)
  (unmasque "0000DDD1.11MMMXXX" word (d m x)
    (list :bset d (derive-location m x))))

(specop movep (w op0 op1)
  (let ((to-reg (position op1 (getf *m68k-layout* :gpr))))
    (multiple-value-bind (dat add)
        (if to-reg (values op1 op0) (values op0 op1))
      (assert (and (mas-base add) (mas-displ add)) (add)
              "Address operands of MOVEP must be of base/displacement type, not ~a.")
      (address (dat add) ((index-d) (index-a)) ;; FIX TYPE
        (joinw (masque "0000RRR1.DS001XXX"
                       (r index-d) (d (if to-reg 0 1))
                       (s (determine-width-bit w)) (x index-a))
               (mas-displ add))))))

(readop movep (word read)
  (unmasque "0000RRR1.DS001XXX" word (r d s x)
    (append (list :movep (derive-width-bit s))
            (funcall (if (zerop d) #'reverse #'identity)
                     (list (derive-location #b101 nil :base x
                                                      :displacement (funcall read 1))
                           (derive-location     0 r))))))

(specop movea (w op0 op1)
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "00SSRRR0.10MMMXXX"
                   (s (determine-width w t))
                   (r index1) (m amode0) (x index0)))))

(readop movea (word read)
  (unmasque "0000DDD1.11MMMXXX" word (d m x)
    (list :movea (derive-location m x) d)))

(specop move (w op0 op1)
  (cond ((eq op1 :ccr) ;; move to CCR
         (assert (eq w :w) (op1)
          "MOVEing to ~a can only be done at width W.")
         (address (op0) ((index0 amode0))
           (masque "01000100.11MMMXXX"
                   (m amode0) (x index0))))
        ((eq op0 :sr) ;; move from SR
         (assert (eq w :w) (op1)
          "MOVEing to ~a can only be done at width W.")
         (address (op1) ((index1 amode1))
           (masque "01000000.11MMMXXX"
                   (m amode1) (x index1))))
        ((eq op1 :sr) ;; move to SR
         (assert (eq w :w) (op1)
          "MOVEing to ~a can only be done at width W.")
         (address (op0) ((index0 amode0))
           (masque "01000110.11MMMXXX"
                   (m amode0) (x index0))))
        ((or (and (eq op0 :usp)
                  (typep  op1 'm68k-mas))
             (and (eq op1 :usp)
                  (typep  op0 'm68k-mas)))
         (assert (eq w :l) (op1)
          "MOVEing to ~a can only be done at width L.")
         (masque "01001110.0110DAAA"
                 (d (if (eq op0 :usp) 0 1))
                 (a (reg-index (if (eq op1 :usp) op0 op1)))))
        ((position op0 (getf *m68k-layout* :gpr))
         (address (op0 op1) ((index0 amode0) (index1 amode1))
           (masque "00SSYYYA.AAMMMXXX"
                   (s (determine-width w t))
                   (y index1) (a amode1) (m amode0) (x index0))))))

(readop move-to-ccr (word read)
  (unmasque "01000100.11MMMXXX" word (m x)
    (list :move :w (derive-location m x) :ccr)))

(readop move-from-sr (word read)
  (unmasque "01000000.11MMMXXX" word (m x)
    (list :move :w :sr (derive-location m x))))

(readop move-to-sr (word read)
  (unmasque "01000110.11MMMXXX" word (m x)
    (list :move :w (derive-location m x) :sr)))

(readop move-usp (word read)
  (unmasque "01001110.0110DAAA" word (d a)
    (append '(:move :l)
            (if (zerop d) (list (derive-location 1 a) :usp)
                (list :usp (derive-location 1 a))))))

(readop move (word read)
  (unmasque "00SSYYYA.AAMMMXXX" word (s y a m x)
    (list :move (derive-width s) (derive-location m x)
          (derive-location a y))))

(specop negx (w op0)
  (assert (match-types op0  location)
          (op0)
          "NEGX operand ~a must be a location.")
  (address (op0) ((index0 amode0))
    (joinw (masque "01000000.SSMMMXXX"
                   (s (determine-width w))
                   (m amode0) (x index0)))))

(readop negx (word read)
  (unmasque "01000000.SSMMMXXX" word (s m x)
    (list :negx (derive-width s) (derive-location m x))))

(specop clr (w op0)
  (assert (match-types op0  location)
          (op0)
          "CLR operand ~a must be a location.")
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
  (assert (and (position w #(:w :l))
               (match-types op0  gpr))
          (op0)
          "EXT operand must be a general-purpose register and its output width must be W or L.")
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
  (unmasque "01001000.00MMMXXX" word (m x)
    (list :nbcd (derive-location m x))))

(specop swap (op0)
  (address (op0) ((index0))
    (joinw (masque "01001000.01000XXX"
                   (x index0)))))

(readop swap (word read)
  (unmasque "01001000.01000XXX" word (x)
    (list :swap (derive-location 0 x))))
  
(specop pea (op0)
  (assert (match-types op0  location)
          (op0)
          "PEA operand ~a must be a location.")
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
           (imm op1))))

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
  (joinw (masque "01001110.01110010")
         (imm op0)))

(readop (masque "01001110.01110010") (read)
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

(specop movem (w op0 op1)
  (assert (or (match-types op0 op1  list location)
              (match-types op0 op1  location list))
          (op0 op1)
          "LEA operands ~a and ~a must be a location and an address register.")
  (let ((to-reg (listp op1)))
    ;; *** CONTROL FOR INPUT MODES
    (multiple-value-bind (regs addr)
        (if to-reg (values op1 op0) (values op0 op1))
      (address (addr) ((index-a amode-a))
        (joinw (masque "01001D00.1SMMMXXX"
                       (d (if to-reg 1 0))
                       (s (determine-width-bit w))
                       (m amode-a) (x index-a))
               (let ((encoding 0)
                     (indices (if (= #b100 amode-a)
                                  #(:d0 :d1 :d2 :d3 :d4 :d5 :d6 :d7
                                    :a0 :a1 :a2 :a3 :a4 :a5 :a6 :a7)
                                  #(:a7 :a6 :a5 :a4 :a3 :a2 :a1 :a0
                                    :d7 :d6 :d5 :d4 :d3 :d2 :d1 :d0))))
                 (loop :for r :in regs :for ix :from 0
                       :do (let ((pos (position r indices)))
                             (if pos (incf encoding (ash 1 (position r indices)))
                                 (error "Invalid entry in register list argument to MOVEM - all entries must be valid general-purpose or address registers."))))
                 encoding))))))

(flet ((mask-to-list (is-predec mask)
         (let ((collected))
           (loop :for i :across (if is-predec #(:d0 :d1 :d2 :d3 :d4 :d5 :d6 :d7
                                                :a0 :a1 :a2 :a3 :a4 :a5 :a6 :a7)
                                    #(:a7 :a6 :a5 :a4 :a3 :a2 :a1 :a0
                                      :d7 :d6 :d5 :d4 :d3 :d2 :d1 :d0))
                 :for ix :from 0 :do (unless (zerop (logand mask (ash 1 ix)))
                                       (push i collected)))
           (list 'quote collected))))
  (readop movem (word read)
    (unmasque "01001D00.1SMMMXXX" word (d s m x)
      (append (list :movem (derive-width-bit s))
              (funcall (if (zerop d) #'identity #'reverse)
                       (list (mask-to-list (= #b100 m) (funcall read 1))
                             (derive-location m x)))))))


(specop lea (op0 op1)
  (assert (match-types op0 op1  location gpr)
          (op0 op1)
          "LEA operands ~a and ~a must be a location and an address register.")
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "0100AAA1.11MMMXXX"
                   (a index1) (m amode0) (x index0)))))

(readop lea (word read)
  (unmasque "0100AAA1.11MMMXXX" word (a m x)
    (list :lea (derive-location m x) (derive-location 1 a))))

(specop chk (op0 op1)
  (assert (match-types op0 op1  location gpr)
          (op0 op1)
          "CHK operands ~a and ~a must be a location and a general-purpose register.")
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
  (assert (match-types op0 op1  integer location)
          (op0 op1)
          "ADDQ operands ~a and ~a must be an immediate integer (range 1-8) and a location.")
  (address (op1) ((index1 amode1))
    (joinw (masque "0101NNN0.SSMMMXXX"
                   (n (logand #b111 op0)) (s (determine-width w))
                   (m amode1) (x index1)))))

(readop addq (word read)
  (unmasque "0101NNN0.SSMMMXXX" word (n s m x)
    (list :addq (derive-width s) (if (zerop n) 8 n) (derive-location m x))))

(specop subq (w op0 op1)
  (assert (and (match-types op0 op1  integer location)
               (zerop (logand #b111 op0)))
          (op0 op1)
          "SUBQ operands ~a and ~a must be an immediate integer (range 1-8) and a location.")
  (address (op1) ((index1 amode1))
    (joinw (masque "0101NNN1.SSMMMXXX"
                   (n (logand #b111 op0)) (s (determine-width w))
                   (m amode1) (x index1)))))

(readop subq (word read)
  (unmasque "0101NNN1.SSMMMXXX" word (n s m x)
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
           (imm op1))))

(readop db (word read)
  (unmasque "0101CCCC.11001XXX" word (c x)
    (list (nth c '(:dbt  :dbf  :dbhi :dbls :dbcc :dbcs :dbne :dbeq
                   :dbvc :dbvs :dbpl :dbmi :dbge :dblt :dbgt :dble))
          (derive-location 0 x)
          (funcall read 1))))

(specop bra (op0)
  (assert (or (symbolp op0) (integerp op0)) (op0)
          "BRA can only take an integer or location tag as operand.")
  (if (and (integerp op0) (zerop (ash op0 -8)))
      (joinw (masque "01100000.DDDDDDDD"
                     (d op0)))
      (joinw (masque "01100000.00000000") ;; case for 16-bit displacement
             (of-program :label 16 16 op0))))

(readop bra (word read)
  (unmasque "01100000.DDDDDDDD" word (d)
    (list :bra (if (not (zerop d))
                   d (funcall read 1)))))

(specop bsr (op0)
  (assert (or (symbolp op0) (integerp op0)) (op0)
          "BSR can only take an integer or location tag as operand.")
  (if (and (integerp op0) (ash op0 -8))
      (joinw (masque "01100001.DDDDDDDD"
                     (d op0)))
      (joinw (masque "01100001.00000000") ;; case for 16-bit displacement
             (of-program :label 16 16 op0))))

(readop bsr (word read)
  (unmasque "01100000.DDDDDDDD" word (d)
    (list :bsr (if (not (zerop d))
                   d (funcall read 1)))))

(specop b (op0)
  ((:combine condition :appending :by-index
    :t :f :hi :ls :cc :cs :ne :eq :vc :vs :pl :mi :ge :lt :gt :le))
  (joinw (masque "0101CCCC.NNNNNNNN"
                 (c condition) (n (if (zerop (ash op0 -8)) (of-program :label 16 16 op0) 0)))
         (if (zerop (ash op0 -8)) nil (of-program :label 16 16 op0))))

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
    (list :sub (derive-width s) (if (zerop d) (derive-location 0 a) (derive-location m x))
          (if (zerop d) (derive-location m x) (derive-location 0 a)))))

(specop subx (w op0 op1)
  (assert (or (match-types op0 op1  gpr gpr)
              (match-types op0 op1  mas-predecr mas-predecr))
           (op0 op1)
           "SUBX operands ~a and ~a must both be data registers or post-decrementing address registers.")
  (address (op0 op1) ((index0) (index1))
    (joinw (masque "1001YYY1.SS00MXXX"
                   (y index1) (s (determine-width w))
                   (m (if (typep op1 'm68k-mas) 1 0))
                   (x index0)))))

(readop subx (word read)
  (unmasque "1001YYY1.SS00MXXX" word (y s m x)
    (list :subx (derive-width s) (derive-location (* m #b100) x)
          (derive-location (* m #b100) y))))

(specop suba (w op0 op1)
  (assert (match-types op0 op1  location adr)
          (op1)
          "SUBA operand 1 must be an address register.")
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "1001AAAS.11MMMXXX"
                   (a index1) (s (determine-width-bit w))
                   (m amode0) (x index0)))))

(readop suba (word read)
  (unmasque "1001AAAS.11MMMXXX" word (a s m x)
    (list :suba (derive-width-bit s) (derive-location 1 a)
          (derive-location m x))))

(specop eor (w op0 op1)
  (assert (match-types op0 op1  gpr location)
          (op1)
          "EOR operands must be a GPR and a location.")
  (address (op0 op1) ((index0) (index1 amode1))
    (joinw (masque "1011AAA1.SSMMMXXX"
                   (a index0) (s (determine-width w))
                   (m amode1) (x index1)))))

(readop eor (word read)
  (unmasque "1011AAA1.SSMMMXXX" word (a s m x)
    (list :eor (derive-width s) (derive-location 0 a)
          (derive-location m x))))

(specop cmpm (w op0 op1)
  (assert (match-types op0 op1  mas-postinc mas-postinc)
          (op1)
          "CMPM operands must be postincrement-addressed locations.")
  (address (op0 op1) ((index0) (index1))
    (joinw (masque "1011AAA1.SS001XXX"
                   (a index1) (s (determine-width w))
                   (x index0)))))

(readop cmpm (word read)
  (unmasque "1011AAA1.SS001XXX" word (a s x)
    (list :cmpm (derive-width s) (derive-location #b011 x)
          (derive-location #b011 a))))

(specop cmp (w op0 op1)
  (assert (match-types op0 op1  location gpr)
          (op1)
          "CMP operand 1 must be a data register.")
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "1011DDD0.SSMMMXXX"
                   (d index1) (s (determine-width w))
                   (m amode0) (x index0)))))

(readop cmp (word read)
  (unmasque "1011DDD0.SSMMMXXX" word (d s m x)
    (list :cmp (derive-width s) (derive-location m x)
          (derive-location 0 d))))

(specop cmpa (w op0 op1)
  (assert (storage-type-p :adr op1) (op1)
          "CMPA operand 1 must be an address register.")
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "1011AAAS.11MMMXXX"
                   (a index1) (s (determine-width-bit w))
                   (m amode0) (x index0)))))

(readop cmpa (word read)
  (unmasque "1011AAAS.11MMMXXX" word (a s m x)
    (list :cmpa (derive-width-bit s) (derive-location m x)
          (derive-location 1 a))))

(specop mulu (op0 op1)
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "1100DDD0.11MMMXXX"
                   (d index1) (m amode0) (x index0)))))

(readop mulu (word read)
  (unmasque "1100DDD0.11MMMXXX" word (d m x)
    (list :mulu (derive-location m x) (derive-location 0 d))))

(specop muls (op0 op1)
  (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "1100DDD1.11MMMXXX"
                   (d index1) (m amode0) (x index0)))))

(readop mulu (word read)
  (unmasque "1100DDD1.11MMMXXX" word (d m x)
    (list :muls (derive-location m x) (derive-location 0 d))))

(specop abcd (op0 op1)
  (assert (or (match-types op0 op1  gpr gpr)
              (match-types op0 op1  mas-predecr mas-predecr))
           (op0 op1)
           "ABCD operands ~a and ~a must both be data registers or post-decrementing address registers.")
  (address (op0 op1) ((index0) (index1))
    (joinw (masque "1100XXX1.0000MYYY"
                   (x index1)
                   (m (if (typep op1 'm68k-mas) 1 0))
                   (y index0)))))

(readop abcd (word read) ;; uses either 0 data register mode or 4 predecrement addressing mode
  (unmasque "1100XXX1.0000MYYY" word (x m y)
    (list :abcd (derive-location (* m #b100) y)
          (derive-location (* m #b100) x))))

(specop exg (op0 op1)
  (assert (and (or (typep op0 'gpr) (typep op0 'adr))
               (or (typep op1 'gpr) (typep op1 'adr)))
          (op1)
          "EXG operands must be general-purpose or address registers.")
  (address (op0 op1) ((index0) (index1))
    (joinw (masque "1100XXX1.MMMMMYYY"
                   (x index0)
                   (m (if (storage-type-p :gpr op0)
                          (if (storage-type-p :gpr op1)
                              #b01000 #b10001)
                          #b01001))
                   (y index1)))))

(readop exg (word read)
  (unmasque "1100XXX1.MMMMMYYY" word (x m y)
    (list :exg (derive-location (signum (logand m #b10000)) x)
          (derive-location (logand m 1) y))))

(specop and (w op0 op1)
  (assert (or (match-types op0 op1  location gpr)
              (match-types op0 op1  gpr location))
           (op0 op1)
          "AND operands ~a and ~a must both be data registers or post-decrementing address registers.")
  (address (op0 op1) ((index0 amode0) (index1 amode1))
    (joinw (masque "1100AAAD.SSMMMXXX"
                   (a (if (typep op1 'm68k-mas) index0 index1))
                   (d (if (typep op1 'm68k-mas) 1      0))
                   (s (determine-width w))
                   (m (if (typep op1 'm68k-mas) amode1 amode0))
                   (x (if (typep op1 'm68k-mas) index1 index0))))))

(readop and (word read)
  (unmasque "1100AAAD.SSMMMXXX" word (a d s m x)
    (list :and (derive-width s) (if (zerop d) (derive-location 0 a) (derive-location m x))
          (if (zerop d) (derive-location m x) (derive-location 0 a)))))

(specop add (w op0 op1)
  (assert (or (match-types op0 op1  location gpr)
              (match-types op0 op1  gpr location))
           (op0 op1)
          "ADD operands ~a and ~a must both be data registers or post-decrementing address registers.")
  (address (op0 op1) ((index0 amode0) (index1 amode1))
    (joinw (masque "1101AAAD.SSMMMXXX"
                   (a (if (typep op1 'm68k-mas) index0 index1))
                   (d (if (typep op1 'm68k-mas) 1      0))
                   (s (determine-width w))
                   (m (if (typep op1 'm68k-mas) amode1 amode0))
                   (x (if (typep op1 'm68k-mas) index1 index0))))))

(readop add (word read)
  (unmasque "1101AAAD.SSMMMXXX" word (a d s m x)
    (list :add (derive-width s) (if (zerop d) (derive-location 0 a) (derive-location m x))
          (if (zerop d) (derive-location m x) (derive-location 0 a)))))

(specop addx (w op0 op1)
  (assert (or (match-types op0 op1  gpr gpr)
              (match-types op0 op1  mas-predecr mas-predecr))
           (op0 op1)
           "ADDX operands ~a and ~a must both be data registers or post-decrementing address registers.")
  (address (op0 op1) ((index0) (index1))
    (joinw (masque "1101XXX1.SS00MYYY"
                   (x index1) (s (determine-width w))
                   (m (if (typep op1 'm68k-mas) 1 0))
                   (y index0)))))

(readop addx (word read)
  (unmasque "1101XXX1.SS00MYYY" word (x s m y)
    (list :addx (derive-width s) (derive-location (* m #b011) x)
          (derive-location (* m #b011) y))))

(specop adda (w op0 op1)
  (assert (match-types op0 op1 location adr)
          (op0 op1)
          "ADDA operands ~a and ~a must be a location and and address register.")
    (address (op0 op1) ((index0 amode0) (index1))
    (joinw (masque "1101AAAS.11MMMXXX"
                   (a index1) (s (determine-width-bit w))
                   (m amode0) (x index0)))))

(readop adda (word read)
  (unmasque "1101AAAS.11MMMXXX" word (a s m x)
    (list :adda (derive-width-bit s) (derive-location m x)
          (derive-location 1 a))))

;; note: the following shift and rotate instructions encode immediate degree values as bitfields
;; with a value from  1-7 if the number is the same,but encoded as 0 if the number is 8,
;; amounting to (logand #b111 n)

(specop as (w op0 &optional op1)
  ((:combine direction :appending :by-index :r :l))
  (assert (or (match-types op0 op1  gpr      gpr)
              (match-types op0 op1  integer  gpr)
              (match-types op0 op1  location null))
           (op0 op1)
           "ASl/r operands ~a and ~a must both be a pair of data registers, an integer and a data register or a location.")
  (if op1 (address (op0 op1) ((index0) (index1))
            (joinw (masque "1110RRRD.SSM00XXX"
                           (r (if (numberp op0) (logand #b111 op0) index0))
                           (d direction) (s (determine-width w))
                           (m (if (numberp op0) 0 1)) (x index1))))
      (if (not (eq :w w))
          (error "Memory shifts can only be done at the word size.")
          (address (op0) ((index0 amode0))
            (joinw (masque "1110000D.11MMMXXX"
                           (d direction) (m amode0) (x index0)))))))

(readop as-reg (word read)
  (unmasque "1110RRRD.SSM00XXX" word (r d s m x)
    (list (if (zerop d) :asr :asl)
          (derive-width s)
          (if (zerop m) (if (zerop r) 8 r)
              (derive-location 0 r))
          (derive-location 0 x))))

(readop as-mem (word read)
  (unmasque "1110000D.11MMMXXX" word (d m x)
    (list (if (zerop d) :asr :asl)
          :w (derive-location m x))))

(specop ls (w op0 &optional op1)
  ((:combine direction :appending :by-index :r :l))
  (assert (or (match-types op0 op1  gpr      gpr)
              (match-types op0 op1  integer  gpr)
              (match-types op0 op1  location null))
           (op0 op1)
           "LSl/r operands ~a and ~a must both be a pair of data registers, an integer and a data register or a location.")
  (if op1 (address (op0 op1) ((index0) (index1))
            (joinw (masque "1110RRRD.SSM01XXX"
                           (r (if (numberp op0) (logand #b111 op0) index0))
                           (d direction) (s (determine-width w))
                           (m (if (numberp op0) 0 1)) (x index1))))
      (if (not (eq :w w))
          (error "Memory shifts can only be done at the word size.")
          (address (op0) ((index0 amode0))
            (joinw (masque "1110001D.11MMMXXX"
                           (d direction) (m amode0) (x index0)))))))

(readop ls-reg (word read)
  (unmasque "1110RRRD.SSM01XXX" word (r d s m x)
    (list (if (zerop d) :lsr :lsl)
          (derive-width s)
          (if (zerop m) (if (zerop r) 8 r)
              (derive-location 0 r))
          (derive-location 0 x))))

(readop ls-mem (word read)
  (unmasque "1110001D.11MMMXXX" word (d m x)
    (list (if (zerop d) :lsr :lsl)
          :w (derive-location m x))))

(specop rox (w op0 &optional op1)
  ((:combine direction :appending :by-index :r :l))
  (if op1 (address (op0 op1) ((index0) (index1))
            (joinw (masque "1110RRRD.SSM10XXX"
                           (r (if (numberp op0) (logand #b111 op0) index0))
                           (d direction) (s (determine-width w))
                           (m (if (numberp op0) 0 1)) (x index1))))
      (if (not (eq :w w))
          (error "Memory rotations can only be done at the word size.")
          (address (op0) ((index0 amode0))
            (joinw (masque "1110010D.11MMMXXX"
                           (d direction) (m amode0) (x index0)))))))

(readop rox-reg (word read)
  (unmasque "1110RRRD.SSM01XXX" word (r d s m x)
    (list (if (zerop d) :roxr :roxl)
          (derive-width s)
          (if (zerop m) (if (zerop r) 8 r)
              (derive-location 0 r))
          (derive-location 0 x))))

(readop rox-mem (word read)
  (unmasque "1110001D.11MMMXXX" word (d m x)
    (list (if (zerop d) :roxr :roxl)
          :w (derive-location m x))))

(specop ro (w op0 &optional op1)
  ((:combine direction :appending :by-index :r :l))
  (if op1 (address (op0 op1) ((index0) (index1))
            (joinw (masque "1110RRRD.SSM11XXX"
                           (r (if (numberp op0) (logand #b111 op0) index0))
                           (d direction) (s (determine-width w))
                           (m (if (numberp op0) 0 1)) (x index1))))
      (if (not (eq :w w))
          (error "Memory rotations can only be done at the word size.")
          (address (op0) ((index0 amode0))
            (joinw (masque "1110011D.11MMMXXX"
                           (d direction) (m amode0) (x index0)))))))

(readop ro-reg (word read)
  (unmasque "1110RRRD.SSM11XXX" word (r d s m x)
    (list (if (zerop d) :ror :rol)
          (derive-width s)
          (if (zerop m) (if (zerop r) 8 r)
              (derive-location 0 r))
          (derive-location 0 x))))

(readop ro-mem (word read)
  (unmasque "1110011D.11MMMXXX" word (d m x)
    (list (if (zerop d) :ror :rol)
          :w (derive-location m x))))
