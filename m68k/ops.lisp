;;;; ops.lisp

(in-package #:specops.m68k)

(specop-68k ori (w op0 op1)
  (case (reg-name op1)
    (:ccr (determine ((w (width :b)) (op0 (imm :width 8)) (op1 (reg-fixed :ccr))) (nil im0 nil)
            (joinw (masque "00000000.00111100")
                   im0)))
    (:sr  (determine ((w (width :w)) (op0 (imm :width 16)) (op1 (reg-fixed :sr))) (nil im0 nil)
            (joinw (masque "00000000.01111100")
                   im0)))
    (t    (determine ((w width) (op0 (imm :width 32)) (op1 gpr mas-all-but-pc)) (enw im0 (ix1 ad1))
            (if (match-imm-width nil w im0)
                (joinw (masque "00000000.SSMMMXXX"
                               (s enw) (m ad1) (x ix1))
                       (encode-extension-word w op1)
                       im0)
                (error "ORI operand immediate value ~a is not of width ~a." im0 (wspec-name w)))))))

(readop-68k (masque "00000000.00111100") (of-program)
  (list :ori :b (funcall of-program :read 1) :ccr))

(readop-68k (masque "00000000.01111100") (of-program)
  (list :ori :w (funcall of-program :read 1) :sr))

(readop-68k ori (word of-program)
  (unmasque "00000000.SSMMMXXX" word (s m x)
    (list :ori (derive-width s) (funcall of-program :read 1) (derive-location m x :pa of-program))))

(specop-68k andi (w op0 op1)
  (case (reg-name op1)
    (:ccr (determine ((w (width :b)) (op0 (imm :width 8)) (op1 (reg-fixed :ccr)))
              (nil im0 nil)
            (joinw (masque "00000010.00111100")
                   im0)))
    (:sr  (determine ((w (width :w)) (op0 (imm :width 16)) (op1 (reg-fixed :sr)))
              (nil im0)
            (joinw (masque "00000010.01111100")
                   im0)))
    (t    (determine ((w width) (op0 (imm :width 32)) (op1 gpr mas-all-but-pc)) (enw im0 (ix1 ad1))
            (if (match-imm-width nil w im0)
                (joinw (masque "00000010.SSMMMXXX"
                               (s enw) (m ad1) (x ix1))
                       (encode-extension-word w op1)
                       im0)
                (error "ANDI operand immediate value ~a is not of width ~a." im0 (wspec-name w)))))))

(readop-68k (masque "00000010.00111100") (of-program)
  (list :andi (funcall of-program :read 1) :ccr))

(readop-68k (masque "00000010.01111100") (of-program)
  (list :andi (funcall of-program :read 1) :sr))

(readop-68k andi (word of-program)
  (unmasque "00000010.SSMMMXXX" word (s m x)
    (list :andi (derive-width s) (funcall of-program :read 1) (derive-location m x :pa of-program))))

(specop-68k subi (w op0 op1)
  ;; (assert (match-types op0 op1  integer location)
  ;;         (op0 op1)
  ;;         "SUBI operands ~a and ~a must be an immediate integer fitting within 32 bits and a location.")
  ;; (address (op1) ((index1 amode1))
  (determine ((w width) (op0 (imm :width 32)) (op1 gpr mas-all-but-pc)) (enw im0 (ix1 ad1))
    (if (match-imm-width nil w im0)
        (joinw (masque "00000100.SSMMMXXX"
                       (s enw) (m ad1) (x ix1))
               (encode-extension-word w op1)
               im0)
        (error "SUBI operand immediate value ~a is not of width ~a." im0 (wspec-name w)))))

(readop-68k subi (word of-program)
  (unmasque "00000100.SSMMMXXX" word (s m x)
    (list :subi (derive-width s) (funcall of-program :read 1)
          (derive-location m x :pa of-program))))

(specop-68k addi (w op0 op1)
  ;; (assert (match-types op0 op1  integer location)
  ;;         (op0 op1)
  ;;         "ADDI operands ~a and ~a must be an immediate integer fitting within 32 bits and a location.")
  ;; (address (op1) ((index1 amode1))
  (determine ((w width) (op0 (imm :width 32)) (op1 gpr mas-all-but-pc)) (enw im0 (ix1 ad1))
    (if (match-imm-width nil w im0)
        (joinw (masque "00000110.SSMMMXXX"
                       (s enw) (m ad1) (x ix1))
               (encode-extension-word w op1)
               im0)
        (error "ADDI operand immediate value ~a is not of width ~a." im0 (wspec-name w)))))

(readop-68k addi (word of-program)
  (unmasque "00000110.SSMMMXXX" word (s m x)
    (list :addi (derive-width s) (funcall of-program :read 1) (derive-location m x :pa of-program))))

(specop-68k eori (w op0 op1)
  (case (reg-name op1)
    (:ccr (determine ((w (width :b)) (op0 (imm :width 8)) (op1 (reg-fixed :ccr))) (nil im0)
            (joinw (masque "00001010.00111100")
                   im0)))
    (:sr  (determine ((w (width :w)) (op0 (imm :width 16)) (op1 (reg-fixed :sr))) (nil im0)
            (joinw (masque "00001010.01111100")
                   im0)))
    (t    (determine ((w width) (op0 (imm :width 32)) (op1 gpr mas-all-but-pc)) (enw im0 (ix1 ad1))
            (if (match-imm-width nil w im0)
                (joinw (masque "00001010.SSMMMXXX"
                               (s enw) (m ad1) (x ix1))
                       (encode-extension-word w op1)
                       im0)
                (error "EORI operand immediate value ~a is not of width ~a." im0 (wspec-name w)))))))

(readop-68k (masque "00001010.00111100") (of-program)
  (list :eori :b (funcall of-program :read 1) :ccr))

(readop-68k (masque "00001010.01111100") (of-program)
  (list :eori :w (funcall of-program :read 1) :sr))

(readop-68k eori (word of-program)
  (unmasque "00001010.SSMMMXXX" word (s m x)
    (list :eori (derive-width s) (funcall of-program :read 1) (derive-location m x :pa of-program))))

(specop-68k cmpi (w op0 op1)
  (determine ((w width) (op0 (imm :width 32)) (op1 gpr mas-all-but-pc)) (enw im0 (ix1 ad1))
    (if (match-imm-width nil w im0)
        (joinw (masque "00001100.SSMMMXXX"
                       (s enw) (m ad1) (x ix1))
               (encode-extension-word w op1)
               im0)
        (error "CMPI operand immediate value ~a is not of width ~a." im0 (wspec-name w)))))

(readop-68k cmpi (word of-program)
  (unmasque "00001010.SSMMMXXX" word (s m x)
    (list :cmpi (derive-width s) (funcall of-program :read 1) (derive-location m x :pa of-program))))

(specop-68k btst (op0 op1)
  (determine ((op0 gpr (imm :width 8)) (op1 gpr (idata 32) mas-all)) (gi0 (ix1 ad1 word))
    (if (gpr-p op0)
        (joinw (masque "0000DDD1.00MMMXXX"
                       (d gi0) (m ad1) (x ix1))
               (or word (encode-extension-word :l op1)))
        (if (imm-value op1)
            (error "BTST may not use an immediate value as both its first and second operands.")
            (joinw (masque "00001000.00MMMXXX"
                           (m ad1) (x ix1))
                   gi0 (or word (encode-extension-word :w op1)))))))

;; (specop-68k btst (op0 op1)
;;   (if (numberp op0)
;;       (address (op1) ((index1 amode1))
;;         (joinw (masque "00001000.00MMMXXX"
;;                        (m amode1) (x index1))
;;                (imm op0)))
;;       (address (op0 op1) ((index0) (index1 amode1))
;;         (joinw (masque "0000DDD1.00MMMXXX"
;;                        (d index0) (m amode1) (x index1))))))

(readop-68k btst-n (word of-program)
  (unmasque "00001000.00MMMXXX" word (m x)
    (list :btst (funcall of-program :read 1) (derive-location m x :pa of-program))))

(readop-68k btst-r (word of-program)
  (unmasque "0000DDD1.00MMMXXX" word (d m x)
    (list :btst d (derive-location m x :pa of-program))))

(specop-68k bchg (op0 op1)
  (determine ((op0 gpr (imm :width 8)) (op1 gpr mas-all-but-pc)) (bn0 (ix1 ad1 word1))
    (if (numberp op0)
        (joinw (masque "00001000.01MMMXXX"
                       (m ad1) (x ix1))
               (or word1 (encode-extension-word 0 op1))
               bn0)
        (joinw (masque "0000DDD1.01MMMXXX"
                       (d bn0) (m ad1) (x ix1))
               (or word1 (encode-extension-word 0 op1))))))

(readop-68k bchg-n (word of-program)
  (unmasque "00001000.01MMMXXX" word (m x)
    (list :bchg (funcall of-program :read 1) (derive-location m x :pa of-program))))

(readop-68k bchg-r (word of-program)
  (unmasque "0000DDD1.01MMMXXX" word (d m x)
    (list :bchg d (derive-location m x :pa of-program))))

(specop-68k bclr (op0 op1)
  (determine ((op0 gpr (imm :width 8)) (op1 gpr mas-all-but-pc)) (bn0 (ix1 ad1 word1))
    (if (numberp op0)
        (joinw (masque "00001000.10MMMXXX"
                       (m ad1) (x ix1))
               (encode-extension-word 0 op1)
               bn0)
        (joinw (masque "0000DDD1.10MMMXXX"
                       (d bn0) (m ad1) (x ix1))
               (or word1 (encode-extension-word 0 op1))))))

(readop-68k bclr-n (word of-program)
  (unmasque "00001000.10MMMXXX" word (m x)
    (list :bclr (funcall of-program :read 1) (derive-location m x :pa of-program))))

(readop-68k bclr-r (word of-program)
  (unmasque "0000DDD1.10MMMXXX" word (d m x)
    (list :bclr d (derive-location m x :pa of-program))))

(specop-68k bset (op0 op1)
  (determine ((op0 gpr (imm :width 8)) (op1 gpr mas-all-but-pc)) (bn0 (ix1 ad1 word1))
    (if (numberp op0)
        (joinw (masque "00001000.11MMMXXX"
                       (m ad1) (x ix1))
               (encode-extension-word 0 op1)
               bn0 word1)
        (joinw (masque "0000DDD1.11MMMXXX"
                       (d bn0) (m ad1) (x ix1))
               (or word1 (encode-extension-word 0 op1))))))

(readop-68k bset-n (word of-program)
  (unmasque "00001000.11MMMXXX" word (m x)
    (list :bset (funcall of-program :read 1) (derive-location m x :pa of-program))))

(readop-68k bset-r (word of-program)
  (unmasque "0000DDD1.11MMMXXX" word (d m x)
    (list :bset d (derive-location m x :pa of-program))))

(specop-68k movep (w op0 op1)
  (determine ((w width-bit) (op0 gpr (mas mas-disp)) (op1 gpr (mas mas-disp)))
      (enw (ix0 _ word0) (ix1 ad1 word1))
    (joinw (masque "0000RRR1.DS001XXX" ;; ad1 as zero indicates that data is moved to a register
                   (r (if (zerop ad1) ix1 ix0)) (d (if (zerop ad1) 0 1))
                   (s enw) (x (if (zerop ad1) ix0 ix1)))
           (or word0 word1))))

;; (specop-68k movep (w op0 op1)
;;   (let ((to-reg (position op1 (getf *m68k-layout* :gpr))))
;;     (multiple-value-bind (dat add)
;;         (if to-reg (values op1 op0) (values op0 op1))
;;       (assert (and (mas-base add) (mas-displ add)) (add)
;;               "Address operands of MOVEP must be of base/displacement type, not ~a.")
;;       (address (dat add) ((index-d) (index-a)) ;; FIX TYPE
;;         (joinw (masque "0000RRR1.DS001XXX"
;;                        (r index-d) (d (if to-reg 0 1))
;;                        (s (determine-width-bit w)) (x index-a))
;;                (mas-displ add))))))

(readop-68k movep (word of-program)
  (unmasque "0000RRR1.DS001XXX" word (r d s x)
    (append (list :movep (derive-width-bit s))
            (funcall (if (zerop d) #'reverse #'identity)
                     (list (derive-location #b101 nil :base x
                                                      :displacement (funcall of-program :read 1))
                           (derive-location 0 r))))))

(specop-68k movea (w op0 op1)
  (determine ((w (width :w :l)) (op0 gpr adr mas-all (idata 32)) (op1 adr)) (enw (ix0 ad0 word) ix1)
    (if (or (not word) (match-imm-width ad0 w word))
        (joinw (masque "00SSRRR0.01MMMXXX"
                       (s enw) (r ix1) (m ad0) (x ix0))
               (or word (encode-extension-word w op0)))
        (error "MOVEA operand immediate value ~a is not of width ~a." word (wspec-name w)))))

(readop-68k movea (word of-program)
  (unmasque "0000DDD1.11MMMXXX" word (d m x)
    (list :movea (derive-location m x :pa of-program) d)))

(specop-68k move (w op0 op1)
  (case (reg-name op1)
    (:ccr (determine ((w (width :w)) (op0 gpr mas-all (idata 16)) (op1 (reg-fixed :ccr))) (nil (ix0 ad0 word))
            (joinw (masque "01000100.11MMMXXX" ;; move to CCR
                           (m ad0) (x ix0))
                   (or word (encode-extension-word w op0)))))
    (:sr  (determine ((w (width :w)) (op0 gpr mas-all (idata 16)) (op1 (reg-fixed :sr))) (nil (ix0 ad0 word))
            (joinw (masque "01000110.11MMMXXX" ;; move to SR
                           (m ad0) (x ix0))
                   (or word (encode-extension-word w op0)))))
    (t    (cond ((eq :sr (reg-name op0)) ;; move from SR
                 (determine ((w (width :w)) (op0 (reg-fixed :sr)) (op1 gpr mas-all-but-pc))
                     (nil nil (ix1 ad1 word))
                   (joinw (masque "01000000.11MMMXXX"
                                  (m ad1) (x ix1))
                          word)))
                ((or (eq :usp (reg-name op0)) (eq :usp (reg-name op1))) ;; move to/from USP
                 (determine ((w (width :l)) (op0 adr (reg-fixed :usp)) (op1 adr (reg-fixed :usp)))
                     (nil ix0 ix1)
                   (masque "01001110.0110DAAA"
                           (d (if (eq :usp (reg-name op0)) 0   1))
                           (a (if (eq :usp (reg-name op0)) ix1 ix0)))))
                (t (determine ((w width-prefix) (op0 gpr adr mas-all (idata 32)) (op1 gpr mas-all-but-pc))
                       (enw (ix0 ad0 word0) (ix1 ad1 word1))
                     (if (or (not word0) (match-imm-width ad0 w word0))
                         (joinw (masque "00SSYYYA.AAMMMXXX"
                                        (s enw) (y ix1) (a ad1) (m ad0) (x ix0))
                                (or word0 word1))
                         (error "MOVE operand immediate value ~a is not of width ~a."
                                word0 (wspec-name w)))))))))

(readop-68k move-to-ccr (word of-program)
  (unmasque "01000100.11MMMXXX" word (m x)
    (list :move :w (derive-location m x :pa of-program) :ccr)))

(readop-68k move-from-sr (word of-program)
  (unmasque "01000000.11MMMXXX" word (m x)
    (list :move :w :sr (derive-location m x :pa of-program))))

(readop-68k move-to-sr (word of-program)
  (unmasque "01000110.11MMMXXX" word (m x)
    (list :move :w (derive-location m x :pa of-program) :sr)))

(readop-68k move-usp (word of-program)
  (unmasque "01001110.0110DAAA" word (d a)
    (append '(:move :l)
            (if (zerop d) (list (derive-location 1 a) :usp)
                (list :usp (derive-location 1 a))))))

(readop-68k move (word of-program)
  (unmasque "00SSYYYA.AAMMMXXX" word (s y a m x)
    (list :move (derive-width s) (derive-location m x :pa of-program)
          (derive-location a y :pa of-program))))

(specop-68k negx (w op0)
  (determine ((w width) (op0 gpr mas-all-but-pc)) (enw (ix0 ad0 word))
    (joinw (masque "01000000.SSMMMXXX"
                   (s enw) (m ad0) (x ix0))
           (or word (encode-extension-word w op0)))))

(readop-68k negx (word of-program)
  (unmasque "01000000.SSMMMXXX" word (s m x)
    (list :negx (derive-width s) (derive-location m x :pa of-program))))

(specop-68k clr (w op0)
  (determine ((w width) (op0 gpr mas-all-but-pc)) (enw (ix0 ad0 word))
    (joinw (masque "01000010.SSMMMXXX"
                   (s enw) (m ad0) (x ix0))
           (or word (encode-extension-word w op0)))))

(readop-68k clr (word of-program)
  (unmasque "01000010.SSMMMXXX" word (s m x)
    (list :clr (derive-width s) (derive-location m x :pa of-program))))

(specop-68k neg (w op0)
  (determine ((w width) (op0 gpr mas-all-but-pc)) (enw (ix0 ad0 word))
    (joinw (masque "01000100.SSMMMXXX"
                   (s enw) (m ad0) (x ix0))
           (or word (encode-extension-word w op0)))))

(readop-68k neg (word of-program)
  (unmasque "01000100.SSMMMXXX" word (s m x)
    (list :neg (derive-width s) (derive-location m x :pa of-program))))

(specop-68k not (w op0)
  (determine ((w width) (op0 gpr (mas mas-simple mas-postinc mas-predecr mas-disp
                                      mas-bi+disp mas-abs-w mas-abs-l)))
      (enw (ix0 ad0 word))
    (joinw (masque "01000110.SSMMMXXX"
                   (s enw) (m ad0) (x ix0))
           (or word (encode-extension-word w op0)))))

(readop-68k not (word of-program)
  (unmasque "01000110.SSMMMXXX" word (s m x)
    (list :not (derive-width s) (derive-location m x :pa of-program))))

(specop-68k ext (w op0)
  (determine ((w width-bit) (op0 gpr))
      (enw ix0)
    (joinw (masque "01001000.1S000XXX"
                   (s enw) (x ix0)))))

(readop-68k ext (word of-program)
  (unmasque "01001000.1S000XXX" word (s x)
    (list :ext (derive-width-bit s) (derive-location 0 x))))

(specop-68k nbcd (op0)
  (determine ((op0 gpr mas-all (idata 8))) ((ix0 ad0 word))
    (joinw (masque "01001000.00MMMXXX"
                   (m ad0) (x ix0))
           (or word (encode-extension-word 0 op0)))))

(readop-68k nbcd (word of-program)
  (unmasque "01001000.00MMMXXX" word (m x)
    (list :nbcd (derive-location m x :pa of-program))))

(specop-68k swap (op0)
  (determine ((op0 gpr)) (ix0)
    (joinw (masque "01001000.01000XXX"
                   (x ix0)))))

(readop-68k swap (word of-program)
  (unmasque "01001000.01000XXX" word (x)
    (list :swap (derive-location 0 x))))

(specop-68k pea (op0)
  (determine ((op0 (mas mas-simple mas-disp mas-bi+disp mas-abs-w mas-abs-l mas-pc+disp mas-pci+disp)))
      ((ix0 ad0 word))
    (joinw (masque "01001000.01MMMXXX"
                   (m ad0) (x ix0))
           (or word (encode-extension-word :l op0)))))

(readop-68k pea (word of-program)
  (unmasque "01001000.01MMMXXX" word (m x)
    (list :pea (derive-location m x :pa of-program))))

(specop-68k illegal ()
  (joinw (masque "01001010.11111100")))

(readop-68k (masque "01001010.11111100") (of-program)
  (list :illegal))

(specop-68k tas (op0)
  (determine ((op0 gpr mas-all-but-pc)) ((ix0 ad0 word))
    (joinw (masque "01001010.11MMMXXX"
                   (m ad0) (x ix0))
           (or word (encode-extension-word :b op0)))))

(readop-68k tas (word of-program)
  (unmasque "01001010.11MMMXXX" word (m x)
    (list :tas (derive-location m x :pa of-program))))

(specop-68k tst (w op0)
  (determine ((w width) (op0 gpr mas-all-but-pc))
      (enw (ix0 ad0 word))
    (joinw (masque "01001010.SSMMMXXX"
                   (s enw) (m ad0) (x ix0))
           (or word (encode-extension-word :w op0)))))

(readop-68k tst (word of-program)
  (unmasque "01001010.SSMMMXXX" word (s m x)
    (list :tst (derive-width s) (derive-location m x :pa of-program))))

(specop-68k trap (op0)
  (determine ((op0 vector)) (env)
    (joinw (masque "01001110.0100VVVV"
                   (v env)))))

(readop-68k trap (word of-program)
  (unmasque "01001110.0100VVVV" word (v)
    (list :trap v)))

(specop-68k link (op0 op1)
  (determine ((op0 adr) (op1 (imm :width 32))) (ix0 im1)
    (joinw (masque "01001110.01010AAA"
                   (a ix0))
           im1)))

(readop-68k link (word of-program)
  (unmasque "01001110.01010AAA" word (a)
    (list :link (derive-location 1 a) (funcall of-program :read 1))))

(specop-68k unlk (op0)
  (determine ((op0 adr)) (ix0)
    (joinw (masque "01001110.01011AAA"
                   (a ix0)))))

(readop-68k unlk (word of-program)
  (unmasque "01001110.01011AAA" word (a)
    (list :unlk (derive-location 1 a))))

(specop-68k reset ()
  (joinw (masque "01001110.01110000")))

(readop-68k (masque "01001110.01110000") (of-program)
  (list :reset))

(specop-68k nop ()
  (joinw (masque "01001110.01110001")))

(readop-68k (masque "01001110.01110001") (of-program)
  (list :nop))

(specop-68k stop (op0)
  (determine ((op0 (imm :width 32))) (im0)
    (joinw (masque "01001110.01110010")
           im0)))

(readop-68k (masque "01001110.01110010") (of-program)
  (list :stop (funcall of-program :read 1)))

(specop-68k rte ()
  (joinw (masque "01001110.01110011")))

(readop-68k (masque "01001110.01110011") (of-program)
  (list :rte))

(specop-68k rts ()
  (joinw (masque "01001110.01110101")))

(readop-68k (masque "01001110.01110101") (of-program)
  (list :rts))

(specop-68k trapv ()
  (joinw (masque "01001110.01110110")))

(readop-68k (masque "01001110.01110110") (of-program)
  (list :trapv))

(specop-68k rtr ()
  (joinw (masque "01001110.01110111")))

(readop-68k (masque "01001110.01110111") (of-program)
  (list :rtr))

(specop-68k jsr (op0)
  (determine ((op0 (mas mas-simple mas-disp mas-bi+disp mas-abs-w mas-abs-l mas-pc+disp mas-pci+disp)))
      ((ix0 ad0 word))
    (joinw (masque "01001110.10MMMXXX"
                   (m ad0) (x ix0))
           (or word (encode-extension-word 0 op0)))))

(readop-68k jsr (word of-program)
  (unmasque "01001110.10MMMXXX" word (m x)
    (list :jsr (derive-location m x :pa of-program))))

(specop-68k jmp (op0)
  (determine ((op0 (mas mas-simple mas-disp mas-bi+disp mas-abs-w mas-abs-l mas-pc+disp mas-pci+disp)))
      ((ix0 ad0 word))
    (joinw (masque "01001110.11MMMXXX"
                   (m ad0) (x ix0))
           (or word (encode-extension-word 0 op0)))))

(readop-68k jmp (word of-program)
  (unmasque "01001110.11MMMXXX" word (m x)
    (list :jmp (derive-location m x :pa of-program))))

;; (specop-68k movem (w op0 op1)
;;   (determine ((w width-bit)
;;               (op0 reg-list (mas mas-simple mas-postinc mas-disp mas-bi+disp mas-abs-w mas-abs-l))
;;               (op1 reg-list (mas mas-simple mas-predecr mas-disp mas-bi+disp mas-abs-w mas-abs-l)))
;;       (enw (ix0 ad0 word0) (ix1 ad1 word1))
;;       (let ((to-reg (typep op0 'm68k-mas))
;;             (regs (if to-reg ix1 ix0))
;;             (addr (or ad0 ad1)))
;;         (joinw (masque "01001D00.1SMMMXXX"
;;                        (d (if to-reg 1 0)) (s enw) (m addr) (x (or ix0 ix1))) 
;;                (let ((encoding 0)
;;                      (indices (if (= #b100 addr)
;;                                   #(:d0 :d1 :d2 :d3 :d4 :d5 :d6 :d7
;;                                     :a0 :a1 :a2 :a3 :a4 :a5 :a6 :a7)
;;                                   #(:a7 :a6 :a5 :a4 :a3 :a2 :a1 :a0
;;                                     :d7 :d6 :d5 :d4 :d3 :d2 :d1 :d0))))
;;                  (loop :for r :in regs :for ix :from 0
;;                        :do (let ((pos (position r indices)))
;;                              (if pos (incf encoding (ash 1 pos))
;;                                  (error "Invalid entry in register list argument to MOVEM - all entries must be valid general-purpose or address registers."))))
;;                  encoding)
;;                (or word0 word1)))))

(flet ((mask-to-list (is-predec mask)
         (let ((collected))
           (loop :for i :across (if is-predec #(:d0 :d1 :d2 :d3 :d4 :d5 :d6 :d7
                                                :a0 :a1 :a2 :a3 :a4 :a5 :a6 :a7)
                                    #(:a7 :a6 :a5 :a4 :a3 :a2 :a1 :a0
                                      :d7 :d6 :d5 :d4 :d3 :d2 :d1 :d0))
                 :for ix :from 0 :do (unless (zerop (logand mask (ash 1 ix)))
                                       (push i collected)))
           (list 'quote collected))))
  (readop-68k movem (word of-program)
    (unmasque "01001D00.1SMMMXXX" word (d s m x)
      (append (list :movem (derive-width-bit s))
              (funcall (if (zerop d) #'identity #'reverse)
                       (list (mask-to-list (= #b100 m) (funcall of-program :read 1))
                             (derive-location m x :pa of-program)))))))


(specop-68k lea (op0 op1)
  (determine ((op0 (mas mas-simple mas-disp mas-bi+disp mas-abs-w mas-abs-l mas-pc+disp mas-pci+disp))
              (op1 adr))
      ((ix0 ad0 word) ix1)
    (joinw (masque "0100AAA1.11MMMXXX"
                   (a ix1) (m ad0) (x ix0))
           (or word (encode-extension-word 0 op0)))))

(readop-68k lea (word of-program)
  (unmasque "0100AAA1.11MMMXXX" word (a m x)
    (list :lea (derive-location m x :pa of-program) (derive-location 1 a))))

(specop-68k chk (op0 op1)
  (determine ((op0 gpr mas-all (idata 16)) (op1 gpr)) ((ix0 ad0 word) ix1)
    (joinw (masque "0100DDD1.10MMMXXX"
                   (d ix1) (m ad0) (x ix0)) ;; memory address
           (or word (encode-extension-word 0 op0)))))

(readop-68k chk (word of-program)
  (unmasque "0100DDD1.10MMMXXX" word (d m x)
    (list :chk (derive-width d) (derive-location m x :pa of-program) (derive-location 1 d))))

;; note: the immediate value is encoded in the bitfield as 1-7 if the number is the same,
;; but encoded as 0 if the number is 8, amounting to (logand #b111 n)
(specop-68k addq (w op0 op1)
  (determine ((w width) (op0 (imm :range 1 8)) (op1 gpr adr mas-all-but-pc)) (enw im0 (ix1 ad1 word))
    (joinw (masque "0101NNN0.SSMMMXXX"
                   (n (logand #b111 im0)) (s enw) (m ad1) (x ix1))
           (or word (encode-extension-word w op1)))))

(readop-68k addq (word of-program)
  (unmasque "0101NNN0.SSMMMXXX" word (n s m x)
    (list :addq (derive-width s) (if (zerop n) 8 n) (derive-location m x :pa of-program))))

(specop-68k subq (w op0 op1)
  (determine ((w width) (op0 (imm :range 1 8)) (op1 gpr adr mas-all-but-pc)) (enw im0 (ix1 ad1 word))
    (joinw (masque "0101NNN1.SSMMMXXX"
                   (n (logand #b111 im0)) (s enw) (m ad1) (x ix1))
           (or word (encode-extension-word w op1)))))

(readop-68k subq (word of-program)
  (unmasque "0101NNN1.SSMMMXXX" word (n s m x)
    (list :subq (derive-width s) (if (zerop n) 8 n) (derive-location m x :pa of-program))))

(specop-68k s (op0)
  ((:combine condition :appending :by-index
    :t :f :hi :ls :cc :cs :ne :eq :vc :vs :pl :mi :ge :lt :gt :le))
  (determine ((op0 gpr mas-all-but-pc)) ((ix0 ad0 word))
    (joinw (masque "0101CCCC.11MMMXXX"
                   (c condition) (m ad0) (x ix0))
           (or word (encode-extension-word 0 op0)))))

(readop-68k s (word of-program)
  (unmasque "0101CCCC.11MMMXXX" word (c m x)
    (list (nth c '(:st :sf :shi :sls :scc :scs :sne :seq :svc :svs :spl :smi :sge :slt :sgt :sle))
          (derive-location m x :pa of-program))))

(specop-68k db (op0 op1) ;; TODO : ENABLE PROGRAM API
  ((:combine condition :appending :by-index
    :t :f :hi :ls :cc :cs :ne :eq :vc :vs :pl :mi :ge :lt :gt :le))
  (determine ((op0 gpr) (op1 (imm :width 16))) (ix0 im1)
    (joinw (masque "0101CCCC.11001XXX"
                   (c condition) (x ix0))
           (imm-value im1))))

(readop-68k db (word of-program)
  (unmasque "0101CCCC.11001XXX" word (c x)
    (list (nth c '(:dbt  :dbf  :dbhi :dbls :dbcc :dbcs :dbne :dbeq
                   :dbvc :dbvs :dbpl :dbmi :dbge :dblt :dbgt :dble))
          (derive-location 0 x)
          (funcall of-program :read 1))))

(specop-68k bra (op0)
  ;; (assert (or (symbolp op0) (integerp op0)) (op0)
  ;;         "BRA can only take an integer or location tag as operand.")
  (determine ((op0 label)) (ix0)
    (if (and (integerp ix0) (zerop (ash ix0 -8)))
        (joinw (masque "01100000.DDDDDDDD"
                       (d ix0)))
        (joinw (masque "01100000.00000000") ;; case for 16-bit displacement
               (of-program :label 16 16 ix0)))))

(readop-68k bra (word of-program)
  (unmasque "01100000.DDDDDDDD" word (d)
    (list :bra (if (not (zerop d)) d (funcall of-program :read 1)))))

(specop-68k bsr (op0)
  ;; (assert (or (symbolp op0) (integerp op0)) (op0)
  ;;         "BSR can only take an integer or location tag as operand.")
  (determine ((op0 label)) (ix0)
    (if (and (integerp ix0) (ash ix0 -8))
        (joinw (masque "01100001.DDDDDDDD"
                       (d ix0)))
        (joinw (masque "01100001.00000000") ;; case for 16-bit displacement
               (of-program :label 16 16 ix0)))))

(readop-68k bsr (word of-program)
  (unmasque "01100000.DDDDDDDD" word (d)
    (list :bsr (if (not (zerop d)) d (funcall of-program :read 1)))))

(specop-68k b (op0)
  ((:combine condition :appending :by-index
    :t :f :hi :ls :cc :cs :ne :eq :vc :vs :pl :mi :ge :lt :gt :le))
  (determine ((op0 label)) (lb0)
    (let ((offset (of-program :label 16 16 lb0)))
      (joinw (masque "0101CCCC.NNNNNNNN"
                     (c condition) (n (if (zerop (ash offset -8)) offset 0)))
             (if (zerop (ash offset -8)) nil offset)))))

(readop-68k b (word of-program)
  (unmasque "0101CCCC.NNNNNNNN" word (c n)
    (list (nth c '(:bt :bf :bhi :bls :bcc :bcs :bne :beq :bvc :bvs :bpl :bmi :bge :blt :bgt :ble))
          (if (not (zerop n)) n (funcall of-program :read 1)))))

(specop-68k moveq (op0 op1)
  (determine ((op0 (imm :width 8)) (op1 gpr)) (im0 ix1)
    (joinw (masque "0111DDD0.NNNNNNNN"
                   (d ix1) (n im0)))))

(readop-68k moveq (word of-program)
  (unmasque "0111DDD0.NNNNNNNN" word (d n)
    (list :moveq n (derive-location 0 d))))

(specop-68k divu (op0 op1)
  (determine ((op0 gpr mas-all (idata 16)) (op1 gpr)) ((ix0 ad0 word) ix1)
    (joinw (masque "1000AAA0.11MMMXXX"
                   (a ix1) (m ad0) (x ix0))
           (or word (encode-extension-word 0 op0)))))

(readop-68k divu (word of-program)
  (unmasque "1000AAA0.11MMMXXX" word (a m x)
    (list :divu (derive-location m x :pa of-program) (derive-location 0 a))))

(specop-68k divs (op0 op1)
  (determine ((op0 gpr mas-all (idata 16)) (op1 gpr)) ((ix0 ad0 word) ix1)
    (joinw (masque "1000AAA1.11MMMXXX"
                   (a ix1) (m ad0) (x ix0))
           (or word (encode-extension-word 0 op0)))))

(readop-68k divs (word of-program)
  (unmasque "1000AAA1.11MMMXXX" word (a m x)
    (list :divs (derive-location m x :pa of-program) (derive-location 0 a))))

(specop-68k sbcd (op0 op1)
  (determine ((op0 gpr (mas mas-predecr)) (op1 gpr (mas mas-predecr))) (ix0 ix1)
    (assert (or (match-types op0 op1  gpr gpr)
                (match-types op0 op1  mas-predecr mas-predecr))
            () "SBCD operands must either both be data registers or pre-decrementing memory accesses.")
    (joinw (masque "1000AAA1.0000MXXX"
                   (a ix1) (m (if (mas-predecr-p op1) 1 0)) (x ix0)))))

(readop-68k sbcd (word of-program)
  (unmasque "1000AAA1.0000MXXX" word (a m x)
    (list :sbcd (derive-location (* m #b100) x) (derive-location (* m #b100) a))))

(specop-68k or (w op0 op1)
  (determine ((w width) (op0 gpr mas-all (idata 32)) (op1 gpr mas-all-but-pc)) (enw (ix0 ad0 word) (ix1 ad1))
    (assert (or (and (gpr-p op0) (typep op1 'mas-m68k))
                (gpr-p op1))
            () "If the source operand of OP is a general purpose register, the destination operand must be a memory alterable location.")
    (if (or (not word) (match-imm-width ad0 w word))
        (joinw (masque "1000AAAD.SSMMMXXX"
                       (a (if (typep op1 'mas-m68k) ix0 ix1))
                       (d (if (typep op1 'mas-m68k)   1   0))
                       (s enw)
                       (m (if (typep op1 'mas-m68k) ad1 ad0))
                       (x (if (typep op1 'mas-m68k) ix1 ix0)))
               (or word (encode-extension-word w op0))
               (encode-extension-word w op1))
        (error "OR operand immediate value ~a is not of width ~a." word (wspec-name w)))))

(readop-68k or (word of-program)
  (unmasque "1000AAAD.SSMMMXXX" word (a d s m x)
    (list :or (derive-width s) (if (zerop d) (derive-location 0 a) (derive-location m x :pa of-program))
          (if (zerop d) (derive-location m x :pa of-program) (derive-location 0 a)))))

(specop-68k sub (w op0 op1)
  (determine ((w width) (op0 gpr mas-all (idata 32)) (op1 gpr mas-all-but-pc))
      (enw (ix0 ad0 word) (ix1 ad1))
    (if (or (not word) (match-imm-width ad0 w word))
        (joinw (masque "1001AAAD.SSMMMXXX"
                       (a (if (typep op1 'mas-m68k) ix0 ix1))
                       (d (if (typep op1 'mas-m68k)   1   0))
                       (s enw)
                       (m (if (typep op1 'mas-m68k) ad1 ad0))
                       (x (if (typep op1 'mas-m68k) ix1 ix0))))
        (error "SUB operand immediate value ~a is not of width ~a." word (wspec-name w)))))

(readop-68k sub (word of-program)
  (unmasque "1001AAAD.SSMMMXXX" word (a d s m x)
    (list :sub (derive-width s) (if (zerop d) (derive-location 0 a) (derive-location m x :pa of-program))
          (if (zerop d) (derive-location m x :pa of-program) (derive-location 0 a)))))

(specop-68k subx (w op0 op1)
  (determine ((w width) (op0 gpr (mas mas-predecr)) (op1 gpr (mas mas-predecr))) (enw ix0 ix1)
    (assert (or (match-types op0 op1  gpr gpr)
                (match-types op0 op1  mas-predecr mas-predecr))
            () "SUBX operands must both be data registers or pre-decrementing address registers.")
    (joinw (masque "1001YYY1.SS00MXXX"
                   (y ix1) (s enw) (m (if (typep op1 'mas-m68k) 1 0)) (x ix0)))))

(readop-68k subx (word of-program)
  (unmasque "1001YYY1.SS00MXXX" word (y s m x)
    (list :subx (derive-width s) (derive-location (* m #b100) x)
          (derive-location (* m #b100) y))))

(specop-68k suba (w op0 op1)
  (determine ((w (width :w :l)) (op0 gpr adr mas-all (idata 32)) (op1 adr)) (enw (ix0 ad0 word) ix1)
    (if (or (not word) (match-imm-width ad0 w word))
        (joinw (masque "1001AAAS.11MMMXXX"
                       (a ix1) (s enw) (m ad0) (x ix0))
               (or word (encode-extension-word w op0)))
        (error "SUBA operand immediate value ~a is not of width ~a." word (wspec-name w)))))

(readop-68k suba (word of-program)
  (unmasque "1001AAAS.11MMMXXX" word (a s m x)
    (list :suba (derive-width-bit s) (derive-location 1 a) (derive-location m x :pa of-program))))

(specop-68k eor (w op0 op1)
  (determine ((w width) (op0 gpr) (op1 gpr mas-all-but-pc)) (enw ix0 (ix1 ad1 word))
    (joinw (masque "1011AAA1.SSMMMXXX"
                   (a ix0) (s enw) (m ad1) (x ix1))
           (or word (encode-extension-word w op1)))))

(readop-68k eor (word of-program)
  (unmasque "1011AAA1.SSMMMXXX" word (a s m x)
    (list :eor (derive-width s) (derive-location 0 a) (derive-location m x :pa of-program))))

(specop-68k cmpm (w op0 op1)
  (determine ((w width) (op0 (mas mas-postinc)) (op1 (mas mas-postinc))) (enw ix0 ix1)
    (joinw (masque "1011AAA1.SS001XXX"
                   (a ix1) (s enw) (x ix0)))))

(readop-68k cmpm (word of-program)
  (unmasque "1011AAA1.SS001XXX" word (a s x)
    (list :cmpm (derive-width s) (derive-location #b011 x) (derive-location #b011 a))))

(specop-68k cmp (w op0 op1)
  (determine ((w width) (op0 gpr adr mas-all (idata 32)) (op1 gpr)) (enw (ix0 ad0 word) ix1)
    (if (or (not word) (match-imm-width ad0 w word))
        (joinw (masque "1011DDD0.SSMMMXXX"
                       (d ix1) (s enw) (m ad0) (x ix0))
               (or word (encode-extension-word w op0)))
        (error "CMP operand immediate value ~a is not of width ~a." word (wspec-name w)))))

(readop-68k cmp (word of-program)
  (unmasque "1011DDD0.SSMMMXXX" word (d s m x)
    (list :cmp (derive-width s) (derive-location m x :pa of-program) (derive-location 0 d))))

(specop-68k cmpa (w op0 op1)
  (determine ((w width) (op0 gpr adr mas-all (idata 32)) (op1 adr)) (enw (ix0 ad0 word) ix1)
    (if (or (not word) (match-imm-width ad0 w word))
        (joinw (masque "1011AAAS.11MMMXXX"
                       (a ix1) (s enw) (m ad0) (x ix0))
               (or word (encode-extension-word w op0)))
        (error "CMPA operand immediate value ~a is not of width ~a." word (wspec-name w)))))

(readop-68k cmpa (word of-program)
  (unmasque "1011AAAS.11MMMXXX" word (a s m x)
    (list :cmpa (derive-width-bit s) (derive-location m x :pa of-program) (derive-location 1 a))))

(specop-68k mulu (op0 op1)
  (determine ((op0 gpr mas-all (idata 16)) (op1 gpr)) ((ix0 ad0 word) ix1)
    (joinw (masque "1100DDD0.11MMMXXX"
                   (d ix1) (m ad0) (x ix0))
           (or word (encode-extension-word 0 op0)))))

(readop-68k mulu (word of-program)
  (unmasque "1100DDD0.11MMMXXX" word (d m x)
    (list :mulu (derive-location m x :pa of-program) (derive-location 0 d))))

(specop-68k muls (op0 op1)
  (determine ((op0 gpr mas-all (idata 16)) (op1 gpr)) ((ix0 ad0 word) ix1)
    (joinw (masque "1100DDD1.11MMMXXX"
                   (d ix1) (m ad0) (x ix0))
           (or word (encode-extension-word 0 op0)))))

(readop-68k muls (word of-program)
  (unmasque "1100DDD1.11MMMXXX" word (d m x)
    (list :muls (derive-location m x :pa of-program) (derive-location 0 d))))

(specop-68k abcd (op0 op1)
  (determine ((op0 gpr (mas mas-predecr)) (op1 gpr (mas mas-predecr))) (ix0 ix1)
    (assert (or (match-types op0 op1  gpr gpr)
                (match-types op0 op1  mas-predecr mas-predecr))
            () "ABCD operands must both be data registers or pre-decrementing address registers.")
    (joinw (masque "1100XXX1.0000MYYY"
                   (x ix1) (m (if (typep op1 'mas-m68k) 1 0)) (y ix0)))))

(readop-68k abcd (word of-program) ;; uses either 0 data register mode or 4 predecrement addressing mode
  (unmasque "1100XXX1.0000MYYY" word (x m y)
    (list :abcd (derive-location (* m #b100) y) (derive-location (* m #b100) x))))

(specop-68k exg (op0 op1)
  (determine ((op0 gpr adr) (op1 gpr adr)) (ix0 ix1)
    (assert (not (and (typep op0 'adr) (typep op1 'gpr)))
            () "EXG may not take an address register as source and a general purpose register as destination.")
    (joinw (masque "1100XXX1.MMMMMYYY"
                   (x ix0) (m (if (gpr-p op0) (if (gpr-p op1) #b01000 #b10001) #b01001)) (y ix1)))))

(readop-68k exg (word of-program)
  (unmasque "1100XXX1.MMMMMYYY" word (x m y)
    (list :exg (derive-location (signum (logand m #b10000)) x)
          (derive-location (logand m 1) y))))

(specop-68k and (w op0 op1)
  (determine ((w width) (op0 gpr mas-all (idata 32)) (op1 gpr mas-all-but-pc)) (enw (ix0 ad0 word) (ix1 ad1))
    (assert (or (and (gpr-p op0) (typep op1 'mas-m68k))
                (gpr-p op1))
            () "If the source operand of AND is a general purpose register, the destination operand must be a memory alterable location.")
    (if (or (not word) (match-imm-width ad0 w word))
        (joinw (masque "1100AAAD.SSMMMXXX"
                       (a (if (typep op1 'mas-m68k) ix0 ix1))
                       (d (if (typep op1 'mas-m68k)   1   0))
                       (s enw)
                       (m (if (typep op1 'mas-m68k) ad1 ad0))
                       (x (if (typep op1 'mas-m68k) ix1 ix0)))
               (or word (encode-extension-word w op0))
               (encode-extension-word w op1))
        (error "AND operand immediate value ~a is not of width ~a." word (wspec-name w)))))

(readop-68k and (word of-program)
  (unmasque "1100AAAD.SSMMMXXX" word (a d s m x)
    (list :and (derive-width s) (if (zerop d) (derive-location 0 a) (derive-location m x :pa of-program))
          (if (zerop d) (derive-location m x :pa of-program) (derive-location 0 a)))))

(specop-68k add (w op0 op1)
  (determine ((w width) (op0 gpr mas-all (idata 32)) (op1 gpr mas-all-but-pc)) (enw (ix0 ad0 word) (ix1 ad1))
    (assert (or (and (gpr-p op0) (typep op1 'mas-m68k))
                (gpr-p op1))
            () "If the source operand of AND is a general purpose register, the destination operand must be a memory alterable location.")
    (if (or (not word) (match-imm-width ad0 w word))
        (joinw (masque "1101AAAD.SSMMMXXX"
                       (a (if (typep op1 'mas-m68k) ix0 ix1))
                       (d (if (typep op1 'mas-m68k)   1   0))
                       (s enw)
                       (m (if (typep op1 'mas-m68k) ad1 ad0))
                       (x (if (typep op1 'mas-m68k) ix1 ix0)))
               (or word (encode-extension-word w op0))
               (encode-extension-word w op1))
        (error "ADD operand immediate value ~a is not of width ~a." word (wspec-name w)))))

(readop-68k add (word of-program)
  (unmasque "1101AAAD.SSMMMXXX" word (a d s m x)
    (list :add (derive-width s) (if (zerop d) (derive-location 0 a) (derive-location m x :pa of-program))
          (if (zerop d) (derive-location m x :pa of-program) (derive-location 0 a)))))

(specop-68k addx (w op0 op1)
  (determine ((w width) (op0 gpr (mas mas-predecr)) (op1 gpr (mas mas-predecr))) (enw ix0 ix1)
    (assert (or (match-types op0 op1  gpr gpr)
                (match-types op0 op1  mas-predecr mas-predecr))
            () "ADDX operands must both be data registers or pre-decrementing address registers.")
    (joinw (masque "1101XXX1.SS00MYYY"
                   (x ix1) (s enw) (m (if (typep op1 'mas-m68k) 1 0)) (y ix0)))))

(readop-68k addx (word of-program)
  (unmasque "1101XXX1.SS00MYYY" word (x s m y)
    (list :addx (derive-width s) (derive-location (* m #b011) x)
          (derive-location (* m #b011) y))))

(specop-68k adda (w op0 op1)
  (determine ((w (width :w :l)) (op0 gpr adr mas-all (idata 32)) (op1 adr)) (enw (ix0 ad0 word) ix1)
    (if (or (not word) (match-imm-width ad0 w word))
        (joinw (masque "1101AAAS.11MMMXXX"
                       (a ix1) (s enw) (m ad0) (x ix0))
               (or word (encode-extension-word w op0)))
        (error "ADDA operand immediate value ~a is not of width ~a." word (wspec-name w)))))

(readop-68k adda (word of-program)
  (unmasque "1101AAAS.11MMMXXX" word (a s m x)
    (list :adda (derive-width-bit s) (derive-location m x :pa of-program) (derive-location 1 a))))

;; note: the following shift and rotate instructions encode immediate degree values as bitfields
;; with a value from  1-7 if the number is the same,but encoded as 0 if the number is 8,
;; amounting to (logand #b111 n)

(specop-68k as (w op0 &optional op1)
  ((:combine direction :appending :by-index :r :l))
  (determine ((w width) (op0 gpr mas-all-but-pc (imm :range 1 8)) (op1 gpr nil))
      (enw (ix0 ad0 word) ix1)
    (if op1 (if (or (match-types op0 op1  gpr     gpr)
                    (match-types op0 op1  integer gpr))
                (masque "1110RRRD.SSM00XXX"
                        (r (if ad0 ix0 (logand #b111 ix0)))
                        (d direction) (s enw) (m (if ad0 1 0)) (x ix1))
                (error "When called on two operands, ASL/R takes either two general-pupose registers or an integer between 1 and 8 and a general purpose register."))
        (if (eq :w w)
            (joinw (masque "1110000D.11MMMXXX"
                           (d direction) (m ad0) (x ix0))
                   (or word (encode-extension-word w op0)))
            (error "Memory shifts can only be done at the word size.")))))

;; (specop-68k as (w op0 &optional op1)
;;   ((:combine direction :appending :by-index :r :l))
;;   (assert (or (match-types op0 op1  gpr      gpr)
;;               (match-types op0 op1  integer  gpr)
;;               (match-types op0 op1  location null))
;;            (op0 op1)
;;            "ASl/r operands ~a and ~a must both be a pair of data registers, an integer and a data register or a location.")
;;   (if op1 (address (op0 op1) ((index0) (index1))
;;             (joinw (masque "1110RRRD.SSM00XXX"
;;                            (r (if (numberp op0) (logand #b111 op0) index0))
;;                            (d direction) (s (determine-width w))
;;                            (m (if (numberp op0) 0 1)) (x index1))))
;;       (if (not (eq :w w))
;;           (error "Memory shifts can only be done at the word size.")
;;           (address (op0) ((index0 amode0))
;;             (joinw (masque "1110000D.11MMMXXX"
;;                            (d direction) (m amode0) (x index0)))))))

(readop-68k as-reg (word of-program)
  (unmasque "1110RRRD.SSM00XXX" word (r d s m x)
    (list (if (zerop d) :asr :asl)
          (derive-width s)
          (if (zerop m) (if (zerop r) 8 r)
              (derive-location 0 r))
          (derive-location 0 x))))

(readop-68k as-mem (word of-program)
  (unmasque "1110000D.11MMMXXX" word (d m x)
    (list (if (zerop d) :asr :asl) :w (derive-location m x :pa of-program))))

;; (specop-68k ls (w op0 &optional op1)
;;   ((:combine direction :appending :by-index :r :l))
;;   (assert (or (match-types op0 op1  gpr      gpr)
;;               (match-types op0 op1  integer  gpr)
;;               (match-types op0 op1  location null))
;;            (op0 op1)
;;            "LSl/r operands ~a and ~a must both be a pair of data registers, an integer and a data register or a location.")
;;   (if op1 (address (op0 op1) ((index0) (index1))
;;             (joinw (masque "1110RRRD.SSM01XXX"
;;                            (r (if (numberp op0) (logand #b111 op0) index0))
;;                            (d direction) (s (determine-width w))
;;                            (m (if (numberp op0) 0 1)) (x index1))))
;;       (if (not (eq :w w))
;;           (error "Memory shifts can only be done at the word size.")
;;           (address (op0) ((index0 amode0))
;;             (joinw (masque "1110001D.11MMMXXX"
;;                            (d direction) (m amode0) (x index0)))))))

(specop-68k ls (w op0 &optional op1)
  ((:combine direction :appending :by-index :r :l))
  (determine ((w width) (op0 gpr mas-all-but-pc (imm :range 1 8)) (op1 gpr nil))
      (enw (ix0 ad0 word) ix1)
    (if op1 (if (or (match-types op0 op1  gpr     gpr)
                    (match-types op0 op1  integer gpr))
                (masque "1110RRRD.SSM01XXX"
                        (r (if ad0 ix0 (logand #b111 ix0)))
                        (d direction) (s enw) (m (if ad0 1 0)) (x ix1))
                (error "When called on two operands, LSL/R takes either two general-pupose registers or an integer between 1 and 8 and a general purpose register."))
        (if (eq :w w)
            (joinw (masque "1110001D.11MMMXXX"
                           (d direction) (m ad0) (x ix0))
                   (or word (encode-extension-word w op0)))
            (error "Memory shifts can only be done at the word size.")))))

(readop-68k ls-reg (word of-program)
  (unmasque "1110RRRD.SSM01XXX" word (r d s m x)
    (list (if (zerop d) :lsr :lsl)
          (derive-width s)
          (if (zerop m) (if (zerop r) 8 r)
              (derive-location 0 r))
          (derive-location 0 x))))

(readop-68k ls-mem (word of-program)
  (unmasque "1110001D.11MMMXXX" word (d m x)
    (list (if (zerop d) :lsr :lsl) :w (derive-location m x :pa of-program))))

;; (specop-68k rox (w op0 &optional op1)
;;   ((:combine direction :appending :by-index :r :l))
;;   (if op1 (address (op0 op1) ((index0) (index1))
;;             (joinw (masque "1110RRRD.SSM10XXX"
;;                            (r (if (numberp op0) (logand #b111 op0) index0))
;;                            (d direction) (s (determine-width w))
;;                            (m (if (numberp op0) 0 1)) (x index1))))
;;       (if (not (eq :w w))
;;           (error "Memory rotations can only be done at the word size.")
;;           (address (op0) ((index0 amode0))
;;             (joinw (masque "1110010D.11MMMXXX"
;;                            (d direction) (m amode0) (x index0)))))))

(specop-68k rox (w op0 &optional op1)
  ((:combine direction :appending :by-index :r :l))
  (determine ((w width) (op0 gpr mas-all-but-pc (imm :range 1 8)) (op1 gpr nil)) (enw (ix0 ad0 word) ix1)
    (if op1 (if (or (match-types op0 op1  gpr     gpr)
                    (match-types op0 op1  integer gpr))
                (masque "1110RRRD.SSM10XXX"
                        (r (if ad0 ix0 (logand #b111 ix0)))
                        (d direction) (s enw) (m (if ad0 1 0)) (x ix1))
                (error "When called on two operands, ROXL/R takes either two general-pupose registers or an integer between 1 and 8 and a general purpose register."))
        (if (eq :w w)
            (joinw (masque "1110010D.11MMMXXX"
                           (d direction) (m ad0) (x ix0))
                   (or word (encode-extension-word w op0)))
            (error "Memory rotations can only be done at the word size.")))))

(readop-68k rox-reg (word of-program)
  (unmasque "1110RRRD.SSM01XXX" word (r d s m x)
    (list (if (zerop d) :roxr :roxl)
          (derive-width s)
          (if (zerop m) (if (zerop r) 8 r)
              (derive-location 0 r))
          (derive-location 0 x))))

(readop-68k rox-mem (word of-program)
  (unmasque "1110001D.11MMMXXX" word (d m x)
    (list (if (zerop d) :roxr :roxl) :w (derive-location m x :pa of-program))))

(specop-68k ro (w op0 &optional op1)
  ((:combine direction :appending :by-index :r :l))
  (determine ((w width) (op0 gpr mas-all-but-pc (imm :range 1 8)) (op1 gpr nil)) (enw (ix0 ad0 word) ix1)
    (if op1 (if (or (match-types op0 op1  gpr     gpr)
                    (match-types op0 op1  integer gpr))
                (masque "1110RRRD.SSM11XXX"
                        (r (if ad0 ix0 (logand #b111 ix0)))
                        (d direction) (s enw) (m (if ad0 1 0)) (x ix1))
                (error "When called on two operands, ROL/R takes either two general-pupose registers or an integer between 1 and 8 and a general purpose register."))
        (if (eq :w w)
            (joinw (masque "1110011D.11MMMXXX"
                           (d direction) (m ad0) (x ix0))
                   (or word (encode-extension-word w op0)))
            (error "Memory rotations can only be done at the word size.")))))

(readop-68k ro-reg (word of-program)
  (unmasque "1110RRRD.SSM11XXX" word (r d s m x)
    (list (if (zerop d) :ror :rol)
          (derive-width s)
          (if (zerop m) (if (zerop r) 8 r)
              (derive-location 0 r))
          (derive-location 0 x))))

(readop-68k ro-mem (word of-program)
  (unmasque "1110011D.11MMMXXX" word (d m x)
    (list (if (zerop d) :ror :rol) :w (derive-location m x :pa of-program))))
