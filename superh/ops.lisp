;;;; ops.lisp

(in-package #:specops.superh)

(specops-sh mov (w op0 op1)
    ((:type-matcher . matching-types)
     (:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (determine ((w width) (op0 gpr (imm 8) (mas mas-simple mas-postinc mas-predecr mas-b+rzero
                                              mas-disp4 mas-disp12 mas-pc+disp mas-gb+disp))
              (op1 gpr (mas mas-postinc mas-predecr mas-b+rzero
                            mas-disp4 mas-disp12 mas-gb+disp))))
  (cond ((matching-types :sh2a) ;; the SH2A-specific MOV instructions
         (cond ((match-types op0 op1  gpr mas-disp12)
                (case w
                  (:b (masque "0011NNNN.MMMM0001.0000DDDD.DDDDDDDD" ;; mov.b Rm,@(disp12,Rn)
                              (n (rix (mas-base op1) :gp)) (m (rix op0 :gp)) (d (mas-displ op1))))
                  (:w (masque "0011NNNN.MMMM0001.0001DDDD.DDDDDDDD" ;; mov.w Rm,@(disp12,Rn)
                              (n (rix (mas-base op1) :gp)) (m (rix op0 :gp)) (d (mas-displ op1))))
                  (:l (masque "0011NNNN.MMMM0001.0010DDDD.DDDDDDDD" ;; mov.l Rm,@(disp12,Rn)
                              (n (rix (mas-base op1) :gp)) (m (rix op0 :gp)) (d (mas-displ op1))))))
               ((match-types op0 op1  mas-disp12 gpr)
                (case w
                  (:b (masque "0011NNNN.MMMM0001.0100DDDD.DDDDDDDD" ;; mov.b @(disp12,Rm),Rn
                              (n (rix (mas-base op1) :gp)) (m (rix op0 :gp)) (d (mas-displ op0))))
                  (:w (masque "0011NNNN.MMMM0001.0101DDDD.DDDDDDDD" ;; mov.w @(disp12,Rm),Rn
                              (n (rix (mas-base op1) :gp)) (m (rix op0 :gp)) (d (mas-displ op0))))
                  (:l (masque "0011NNNN.MMMM0001.0110DDDD.DDDDDDDD" ;; mov.l @(disp12,Rm),Rn
                              (n (rix (mas-base op1) :gp)) (m (rix op0 :gp)) (d (mas-displ op0))))))))
        ((match-types op0 op1  gpr gpr)
         (assert (eq w :l) (op0)
                 "MOVing data between registers can only be done at width L (32 bits).")
         (masque "0110NNNN.MMMM0011" ;; mov Rm,Rn
                 (n (rix op1 :gp)) (m (rix op0 :gp))))
        ((match-types op0 op1  integer gpr)
         (assert (eq w :b) (op0)
                 "MOVing immediate data to a register can only be done at width B (8 bits).")
         (masque "1110NNNN.IIIIIIII" ;; mov #imm,Rn
                 (n (rix op1 :gp)) (i op0)))
        ((match-types op0 op1  mas-pc+disp gpr)
         (case w
           (:w (masque "1001NNNN.DDDDDDDD" ;; mov.w @(disp,PC),Rn
                       (n (rix op1 :gp)) (d (mas-displ op0))))
           (:l (masque "1101NNNN.DDDDDDDD" ;; mov.l @(disp,PC),Rn
                       (n (rix op1 :gp)) (d (mas-displ op0))))))
        ((match-types op0 op1  mas-simple gpr)
         (case w
           (:b (masque "0110NNNN.MMMM0000" ;; mov.b @Rm,Rn
                       (n (rix op1 :gp)) (m (rix (mas-base op0) :gp))))
           (:w (masque "0110NNNN.MMMM0001" ;; mov.w @Rm,Rn
                       (n (rix op1 :gp)) (m (rix (mas-base op0) :gp))))
           (:l (masque "0110NNNN.MMMM0010" ;; mov.l @Rm,Rn
                       (n (rix op1 :gp)) (m (rix (mas-base op0) :gp))))))
        ((match-types op0 op1  gpr mas-simple)
         (case w
           (:b (masque "0010NNNN.MMMM0000" ;; mov.b Rm,@Rn
                       (n (rix (mas-base op1) :gp)) (m (rix op0 :gp))))
           (:w (masque "0010NNNN.MMMM0001" ;; mov.w Rm,@Rn
                       (n (rix (mas-base op1) :gp)) (m (rix op0 :gp))))
           (:l (masque "0010NNNN.MMMM0010" ;; mov.l Rm,@Rn
                       (n (rix (mas-base op1) :gp)) (m (rix op0 :gp))))))
        ((match-types op0 op1  mas-postinc gpr)
         (case w
           (:b (masque "0110NNNN.MMMM0100" ;; mov.b @Rm+,Rn
                       (n (rix op1 :gp)) (m (rix (mas-base op0) :gp))))
           (:w (masque "0110NNNN.MMMM0101" ;; mov.w @Rm+,Rn
                       (n (rix op1 :gp)) (m (rix (mas-base op0) :gp))))
           (:l (masque "0110NNNN.MMMM0101" ;; mov.l @Rm+,Rn
                       (n (rix op1 :gp)) (m (rix (mas-base op0) :gp))))))
        ((match-types op0 op1  gpr mas-predecr)
         (case w
           (:b (masque "0010NNNN.MMMM0100" ;; mov.b Rm,@-Rn
                       (n (rix (mas-base op1) :gp)) (m (rix op0 :gp))))
           (:w (masque "0010NNNN.MMMM0101" ;; mov.w Rm,@-Rn
                       (n (rix (mas-base op1) :gp)) (m (rix op0 :gp))))
           (:l (masque "0010NNNN.MMMM0110" ;; mov.l Rm,@-Rn
                       (n (rix (mas-base op1) :gp)) (m (rix op0 :gp))))))
        
        ((and (eq :r0 op1) (match-types op1  mas-predecr))
         (case w
           (:b (masque "0100MMMM.11001011" ;; mov.b @-Rm,R0
                       (m (rix (mas-base op0) :gp))))
           (:w (masque "0100MMMM.11011011" ;; mov.w @-Rm,R0
                       (m (rix (mas-base op0) :gp))))
           (:l (masque "0100MMMM.11101011" ;; mov.l @-Rm,R0
                       (m (rix (mas-base op0) :gp))))))
        ((and (eq :r0 op0) (match-types op1  mas-postinc))
         (case w
           (:b (masque "0100NNNN.10001011" ;; mov.b R0,@Rn+
                       (n (rix (mas-base op1) :gp))))
           (:w (masque "0100NNNN.10011011" ;; mov.w R0,@Rn+
                       (n (rix (mas-base op0) :gp))))
           (:l (masque "0100NNNN.10101011" ;; mov.l R0,@Rn+
                       (n (rix (mas-base op0) :gp))))))
        
        ((and (eq :r0 op1) (match-types op0  mas-disp4))
         (case w
           (:b (masque "10000100.MMMMDDDD" ;; mov.b @(disp,Rm),R0
                       (m (rix (mas-base op0) :gp)) (d (mas-displ op0))))
           (:w (masque "10000101.MMMMDDDD" ;; mov.w @(disp,Rm),R0
                       (m (rix (mas-base op0) :gp)) (d (mas-displ op0))))))
        ((match-types op0 op1  mas-disp4 gpr)
         (case w
           (:l (masque "0101NNNN.MMMMDDDD" ;; mov.l @(disp,Rm),Rn
                       (n (rix op1 :gp)) (m (rix (mas-base op0) :gp)) (d (mas-displ op0))))))
        ((and (eq :r0 op0) (match-types op1  mas-disp4))
         (case w
           (:b (masque "10000000.NNNNDDDD" ;; mov.b R0,@(disp,Rn)
                       (n (rix (mas-base op1) :gp)) (d (mas-displ op1))))
           (:w (masque "10000001.NNNNDDDD" ;; mov.w R0,@(disp,Rn)
                       (n (rix (mas-base op1) :gp)) (d (mas-displ op1))))))
        ((match-types op0 op1  gpr mas-disp4)
         (case w
           (:l (masque "0001NNNN.MMMMDDDD" ;; mov.l Rm,@(disp,Rn)
                       (n (rix (mas-base op1) :gp)) (m (rix op0 :gp)) (d (mas-displ op1))))))
        
        ((match-types op0 op1  mas-b+rzero gpr)
         (case w
           (:b (masque "0000NNNN.MMMM1100" ;; mov.b @(R0,Rm),Rn
                       (n (rix op1 :gp)) (m (rix (mas-base op0) :gp))))
           (:w (masque "0000NNNN.MMMM1101" ;; mov.w @(R0,Rm),Rn
                       (n (rix op1 :gp)) (m (rix (mas-base op0) :gp))))
           (:l (masque "0000NNNN.MMMM1110" ;; mov.l @(R0,Rm),Rn
                       (n (rix op1 :gp)) (m (rix (mas-base op0) :gp))))))
        ((match-types op0 op1  gpr mas-b+rzero)
         (case w
           (:b (masque "0000NNNN.MMMM0100" ;; mov.b Rm,@(R0,Rn)
                       (n (rix (mas-base op1) :gp)) (m (rix op0 :gp))))
           (:w (masque "0000NNNN.MMMM0101" ;; mov.w Rm,@(R0,Rn)
                       (n (rix (mas-base op1) :gp)) (m (rix op0 :gp))))
           (:l (masque "0000NNNN.MMMM0110" ;; mov.l Rm,@(R0,Rn)
                       (n (rix (mas-base op1) :gp)) (m (rix op0 :gp))))))
        ((and (eq :r0 op1) (match-types op0  mas-gb+disp))
         (case w
           (:b (masque "11000100.DDDDDDDD" ;; mov.b @(disp,GBR),R0
                       (d (mas-displ op0))))
           (:w (masque "11000101.DDDDDDDD" ;; mov.w @(disp,GBR),R0
                       (d (mas-displ op0))))
           (:l (masque "11000110.DDDDDDDD" ;; mov.l @(disp,GBR),R0
                       (d (mas-displ op0))))))
        ((and (eq :r0 op0) (match-types op1  mas-gb+disp))
         (case w
           (:b (masque "11000000.DDDDDDDD" ;; mov.b R0,@(disp,GBR)
                       (d (mas-displ op1))))
           (:w (masque "11000001.DDDDDDDD" ;; mov.w R0,@(disp,GBR)
                       (d (mas-displ op1))))
           (:l (masque "11000010.DDDDDDDD" ;; mov.l R0,@(disp,GBR)
                       (d (mas-displ op1))))))))

(readops-sh mov.l.r-r (word read) ;; mov Rm,Rn
  (unmasque "0110NNNN.MMMM0011" word (n m)
    (list :mov :l (drv-gpr m) (drv-gpr n))))

(readops-sh mov.l.i-r (word read) ;; mov #imm,Rn
  (unmasque "1110NNNN.IIIIIIII" word (n i)
    (list :mov :b i (drv-gpr n))))

(readops-sh mov.w.dspc-r (word read) ;; mov.w @(disp,PC),Rn
  (unmasque "1001NNNN.DDDDDDDD" word (n d)
    (list :mov :w (list '@pc d) (drv-gpr n))))

(readops-sh mov.l.dspc-r (word read) ;; mov.l @(disp,PC),Rn
  (unmasque "1101NNNN.DDDDDDDD" word (n d)
    (list :mov :l (list '@pc d) (drv-gpr n))))

(readops-sh mov.b.@-r (word read) ;; mov.b @Rm,Rn
  (unmasque "0110NNNN.MMMM0000" word (n m)
    (list :mov :b (list '@ (drv-gpr m)) (drv-gpr n))))

(readops-sh mov.w.@-r (word read) ;; mov.w @Rm,Rn
  (unmasque "0110NNNN.MMMM0001" word (n m)
    (list :mov :w (list '@ (drv-gpr m)) (drv-gpr n))))

(readops-sh mov.l.@-r (word read) ;; mov.l @Rm,Rn
  (unmasque "0110NNNN.MMMM0010" word (n m)
    (list :mov :l (list '@ (drv-gpr m)) (drv-gpr n))))

(readops-sh mov.b.r-@ (word read) ;; mov.b Rm,@Rn
  (unmasque "0010NNNN.MMMM0000" word (n m)
    (list :mov :b (drv-gpr m) (list '@ (drv-gpr n)))))

(readops-sh mov.w.r-@ (word read) ;; mov.w Rm,@Rn
  (unmasque "0010NNNN.MMMM0001" word (n m)
    (list :mov :w (drv-gpr m) (list '@ (drv-gpr n)))))

(readops-sh mov.l.r-@ (word read) ;; mov.l Rm,@Rn
  (unmasque "0010NNNN.MMMM0010" word (n m)
    (list :mov :l (drv-gpr m) (list '@ (drv-gpr n)))))

(readops-sh mov.b.+-r (word read) ;; mov.b @Rm+,Rn
  (unmasque "0110NNNN.MMMM0100" word (n m)
    (list :mov :b (list '@+ (drv-gpr m)) (drv-gpr n))))

(readops-sh mov.w.+-r (word read) ;; mov.w @Rm+,Rn
  (unmasque "0110NNNN.MMMM0101" word (n m)
    (list :mov :w (list '@+ (drv-gpr m)) (drv-gpr n))))

(readops-sh mov.l.+-r (word read) ;; mov.l @Rm+,Rn
  (unmasque "0110NNNN.MMMM0110" word (n m)
    (list :mov :l (list '@+ (drv-gpr m)) (drv-gpr n))))

(readops-sh mov.b.r-- (word read) ;; mov.b Rm,@-Rn
  (unmasque "0010NNNN.MMMM0100" word (n m)
    (list :mov :b (drv-gpr m) (list '-@ (drv-gpr n)))))

(readops-sh mov.w.r-- (word read) ;; mov.w Rm,@-Rn
  (unmasque "0010NNNN.MMMM0101" word (n m)
    (list :mov :w (drv-gpr m) (list '-@ (drv-gpr n)))))

(readops-sh mov.l.r-- (word read) ;; mov.l Rm,@-Rn
  (unmasque "0010NNNN.MMMM0110" word (n m)
    (list :mov :l (drv-gpr m) (list '-@ (drv-gpr n)))))

(readops-sh mov.b.--0 (word read) ;; mov.b @-Rm,R0
  (unmasque "0100MMMM.11001011" word (m)
    (list :mov :b (list '-@ (drv-gpr m)) :r0)))

(readops-sh mov.w.--0 (word read) ;; mov.w @-Rm,R0
  (unmasque "0100MMMM.11011011" word (m)
    (list :mov :w (list '-@ (drv-gpr m)) :r0)))

(readops-sh mov.l.--0 (word read) ;; mov.l @-Rm,R0
  (unmasque "0100MMMM.11101011" word (m)
    (list :mov :l (list '-@ (drv-gpr m)) :r0)))

(readops-sh mov.b.0-+ (word read) ;; mov.b R0,@Rn+
  (unmasque "0100NNNN.10001011" word (n)
    (list :mov :b :r0 (list '@+ (drv-gpr n)))))

(readops-sh mov.w.0-+ (word read) ;; mov.w R0,@Rn+
  (unmasque "0100NNNN.10011011" word (n)
    (list :mov.w :r0 (list '@+ (drv-gpr n)))))

(readops-sh mov.l.0-+ (word read) ;; mov.l R0,@Rn+
  (unmasque "0100NNNN.10101011" word (n)
    (list :mov :l :r0 (list '@+ (drv-gpr n)))))

(readops-sh mov.b.dr-0 (word read) ;; mov.b @(disp,Rm),R0
  (unmasque "10000100.MMMMDDDD" word (m d)
    (list :mov :b (list '@> (drv-gpr m) d) :r0)))

(readops-sh mov.w.dr-0 (word read) ;; mov.w @(disp,Rm),R0
  (unmasque "10000101.MMMMDDDD" word (m d)
    (list :mov :w (list '@> (drv-gpr m) d) :r0)))

(readops-sh mov.l.dr-r (word read) ;; mov.l @(disp,Rm),Rn
  (unmasque "0101NNNN.MMMMDDDD" word (n m d)
    (list :mov :l (list '@> (drv-gpr m) d) (drv-gpr n))))

(readops-sh mov.b.r-dr (word read) ;; mov.b R0,@(disp,Rn)
  (unmasque "10000000.NNNNDDDD" word (n d)
    (list :mov :b :r0 (list '@> (drv-gpr n) d))))

(readops-sh mov.w.r-dr (word read) ;; mov.w R0,@(disp,Rn)
  (unmasque "10000001.NNNNDDDD" word (n d)
    (list :mov :w :r0 (list '@> (drv-gpr n) d))))

(readops-sh mov.l.r-dr (word read) ;; mov.l Rm,@(disp,Rn)
  (unmasque "0001NNNN.MMMMDDDD" word (n m d)
    (list :mov :l (drv-gpr m) (list '@> (drv-gpr n) d))))

(readops-sh mov.b.0r-r (word read) ;; mov.b @(R0,Rm),Rn
  (unmasque "0000NNNN.MMMM1100" word (n m)
    (list :mov :b (list '@0 (drv-gpr m)) (drv-gpr n))))

(readops-sh mov.w.0r-r (word read) ;; mov.w @(R0,Rm),Rn
  (unmasque "0000NNNN.MMMM1101" word (n m)
    (list :mov :w (list '@0 (drv-gpr m)) (drv-gpr n))))

(readops-sh mov.l.0r-r (word read) ;; mov.l @(R0,Rm),Rn
  (unmasque "0000NNNN.MMMM1110" word (n m)
    (list :mov :l (list '@0 (drv-gpr m)) (drv-gpr n))))

(readops-sh mov.b.r-0r (word read) ;; mov.b Rm,@(R0,Rn)
  (unmasque "0000NNNN.MMMM0100" word (n m)
    (list :mov :b (drv-gpr m) (list '@0 (drv-gpr n)))))

(readops-sh mov.w.r-0r (word read) ;; mov.w Rm,@(R0,Rn)
  (unmasque "0000NNNN.MMMM0101" word (n m)
    (list :mov :w (drv-gpr m) (list '@0 (drv-gpr n)))))

(readops-sh mov.l.r-0r (word read) ;; mov.l Rm,@(R0,Rn)
  (unmasque "0000NNNN.MMMM0110" word (n m)
    (list :mov :l (drv-gpr m) (list '@0 (drv-gpr n)))))

(readops-sh mov.b.dg-0 (word read) ;; mov.b @(disp,GBR),R0
  (unmasque "11000100.DDDDDDDD" word (d)
    (list :mov :b (list '@0 d) :r0)))

(readops-sh mov.w.dg-0 (word read) ;; mov.w @(disp,GBR),R0
  (unmasque "11000101.DDDDDDDD" word (d)
    (list :mov :w (list '@0 d) :r0)))

(readops-sh mov.l.dg-0 (word read) ;; mov.l @(disp,GBR),R0
  (unmasque "11000110.DDDDDDDD" word (d)
    (list :mov :l (list '@0 d) :r0)))

(readops-sh mov.b.0-dg (word read) ;; mov.b R0,@(disp,GBR)
  (unmasque "11000000.DDDDDDDD" word (d)
    (list :mov :b :r0 (list '@0 d))))

(readops-sh mov.w.0-dg (word read) ;; mov.w R0,@(disp,GBR)
  (unmasque "11000001.DDDDDDDD" word (d)
    (list :mov :w :r0 (list '@0 d))))

(readops-sh mov.l.0-dg (word read) ;; mov.l R0,@(disp,GBR)
  (unmasque "11000010.DDDDDDDD" word (d)
    (list :mov :l :r0 (list '@0 d))))

(readops-sh mov.b.r-d12 (word read) ;; mov.b Rm,@(disp12,Rn)
  (unmasque "0011NNNN.MMMM0001.0000DDDD.DDDDDDDD" word (n m d)
    (list :mov :b (drv-gpr m) (list '@> (drv-gpr n) d))))

(readops-sh mov.w.r-d12 (word read) ;; mov.w Rm,@(disp12,Rn)
  (unmasque "0011NNNN.MMMM0001.0001DDDD.DDDDDDDD" word (n m d)
    (list :mov :w (drv-gpr m) (list '@> (drv-gpr n) d))))

(readops-sh mov.l.r-d12 (word read) ;; mov.l Rm,@(disp12,Rn)
  (unmasque "0011NNNN.MMMM0001.0010DDDD.DDDDDDDD" word (n m d)
    (list :mov :l (drv-gpr m) (list '@> (drv-gpr n) d))))

(readops-sh mov.b.d12-r (word read) ;; mov.b @(disp12,Rm),Rn
  (unmasque "0011NNNN.MMMM0001.0100DDDD.DDDDDDDD" word (n m d)
    (list :mov :b (list '@> (drv-gpr m) d) (drv-gpr n))))

(readops-sh mov.w.d12-r (word read) ;; mov.w @(disp12,Rm),Rn
  (unmasque "0011NNNN.MMMM0001.0101DDDD.DDDDDDDD" word (n m d)
    (list :mov :w (list '@> (drv-gpr m) d) (drv-gpr n))))

(readops-sh mov.l.d12-r (word read) ;; mov.l @(disp12,Rm),Rn
  (unmasque "0011NNNN.MMMM0001.0110DDDD.DDDDDDDD" word (n m d)
    (list :mov :l (list '@> (drv-gpr m) d) (drv-gpr n))))

;; (specops-sh movi20 (op0 op1)
;;   ((:for-types :sh2a))
;;   (assert (match-types op0 op1  integer gpr) (op0 op1)
;;           "MOVI20 may take only an immediate integer value and a general-purpose register as its operands.")
;;   (masque "0000NNNN.HHHH0000.LLLLLLLL.LLLLLLLL" ;; movi20 #imm20,Rn
;;           (n (rix op1 :gp)) (h (rs16 op0)) (l (lo16 op0))))

(specops-sh movi20 (op0 op1)
  ((:for-types :sh2a))
  (determine ((op0 (imm 20)) (op1 gpr)) (im0 ix1)
    (masque "0000NNNN.HHHH0000.LLLLLLLL.LLLLLLLL" ;; movi20 #imm20,Rn
            (n ix1) (h (rs16 im0)) (l (lo16 im0)))))

(readops-sh movi20 (word read) ;; movi20 #imm20,Rn
  (unmasque "0000NNNN.HHHH0000.LLLLLLLL.LLLLLLLL" word (n h l)
    (list :movi20 (+ l (ash h 16)) (drv-gpr n))))

;; (specops-sh movi20s (op0 op1)
;;   ((:for-types :sh2a))
;;   (assert (match-types op0 op1  integer gpr) (op0 op1)
;;           "MOVI20S may take only an immediate integer value and a general-purpose register as its operands.")
;;   (masque "0000NNNN.HHHH0001.LLLLLLLL.LLLLLLLL" ;; movi20s #imm20,Rn
;;           (n (rix op1 :gp)) (h (rs16 op0)) (l (lo16 op0))))

(specops-sh movi20s (op0 op1)
  ((:for-types :sh2a))
  (determine ((op0 (imm 20)) (op1 gpr)) (im0 ix1)
      (masque "0000NNNN.HHHH0001.LLLLLLLL.LLLLLLLL" ;; movi20s #imm20,Rn
              (n ix1) (h (rs16 im0)) (l (lo16 im0)))))

(readops-sh movi20s (word read) ;; movi20s #imm20,Rn
  (unmasque "0000NNNN.HHHH0001.LLLLLLLL.LLLLLLLL" word (n h l)
    (list :movi20s (+ l (ash h 16)) (drv-gpr n))))

;; (specops-sh mova (op0 op1)
;;   ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
;;   (assert (and (eq op1 :r0) (match-types op0  mas-pc+disp)) (op0)
;;           "MOVA may take only a displacement+PC memory address and R0 as its operands.")
;;   (masque "11000111.DDDDDDDD" ;; mova @(disp,PC),R0
;;           (d (mas-displ op0))))

(specops-sh mova (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (determine ((op0 (mas mas-pc+disp)) (op1 (reg-fixed :r0))) (ds0)
    (masque "11000111.DDDDDDDD" ;; mova @(disp,PC),R0
            (d ds0))))

(readops-sh mova (word read)
  (unmasque "11000111.DDDDDDDD" word (d)
    (list :mova (list '@pc d) :r0)))

;; (specops-sh movu (w op0 op1)
;;   ((:for-types :sh2a))
;;   (assert (match-types op0 op1  mas-disp gpr) (op0)
;;           "MOVU may take only a base+displacement memory address and a general-purpose register as its operands.")
;;   (case w
;;     (:b (masque "0011NNNN.MMMM0001.1000DDDD.DDDDDDDD" ;; movu.b @(disp12,Rm),Rn
;;                 (n (rix op1 :gp)) (m (rix (mas-base op0) :gp)) (d (mas-displ op0))))
;;     (:w (masque "0011NNNN.MMMM0001.1001DDDD.DDDDDDDD" ;; movu.w @(disp12,Rm),Rn
;;                 (n (rix op1 :gp)) (m (rix (mas-base op0) :gp)) (d (mas-displ op0))))))

(specops-sh movu (w op0 op1)
  ((:for-types :sh2a))
  (determine ((w (width :b :w)) (op0 mas-disp12) (op1 gpr)) (nil (ix0 ds0) ix1)
    (case w
      (:b (masque "0011NNNN.MMMM0001.1000DDDD.DDDDDDDD" ;; movu.b @(disp12,Rm),Rn
                  (n ix1) (m ix0) (d ds0)))
      (:w (masque "0011NNNN.MMMM0001.1001DDDD.DDDDDDDD" ;; movu.w @(disp12,Rm),Rn
                  (n ix1) (m ix0) (d ds0))))))

(readops-sh movu-b (word read) ;; movu.b @(disp12,Rm),Rn
  (unmasque "0011NNNN.MMMM0001.1000DDDD.DDDDDDDD" word (n m d)
    (list :movu :b (list '@> (drv-gpr m) d) (drv-gpr n))))

(readops-sh movu-w (word read) ;; movu.w @(disp12,Rm),Rn
  (unmasque "0011NNNN.MMMM0001.1001DDDD.DDDDDDDD" word (n m d)
    (list :movu :w (list '@> (drv-gpr m) d) (drv-gpr n))))

;; (specops-sh movco.l (op0 op1)
;;   ((:for-types :sh4a))
;;   (if (and (eq op0 :r0) (typep op1 'mas-simple))
;;       (masque "0000NNNN.01110011" ;; movco.l R0,@Rn
;;               (n (rix (mas-base op1) :gp)))
;;       (error "MOVCO.L takes only R0 and a displacement+PC memory address as its operands.")))

(specops-sh movco.l (op0 op1)
  ((:for-types :sh4a))
  (determine ((op0 (reg-fixed :r0)) (op1 (mas mas-simple))) (nil ix1)
    (if (and (eq op0 :r0) (typep op1 'mas-simple))
        (masque "0000NNNN.01110011" ;; movco.l R0,@Rn
                (n ix1))
        (error "MOVCO.L takes only R0 and a displacement+PC memory address as its operands."))))

(readops-sh movco.l (word read)
  (unmasque "0000NNNN.01110011" word (n)
    (list :movco.l :r0 (list '@ (drv-gpr n)))))

;; (specops-sh movli.l (op0 op1)
;;   ((:for-types :sh4a))
;;   (if (and (eq op1 :r0) (match-types op0  mas-simple))
;;       (masque "0000MMMM.01100011" ;; movli.l @Rm,R0
;;               (m (rix (mas-base op0) :gp)))
;;       (error "MOVLI.L takes only a simple memory address and R0 as its operands.")))

(specops-sh movli.l (op0 op1)
  ((:for-types :sh4a))
  (determine ((op0 (mas mas-simple)) (op1 (reg-fixed :r0))) (ix0)
    (if (and (eq op1 :r0) (match-types op0  mas-simple))
        (masque "0000MMMM.01100011" ;; movli.l @Rm,R0
                (m ix0))
        (error "MOVLI.L takes only a simple memory address and R0 as its operands."))))

(readops-sh movli.l (word read) ;; movli.l @Rm,R0
  (unmasque "0000MMMM.01100011" word (m)
    (list :movli.l (list '@ (drv-gpr m)) :r0)))

;; (specops-sh movua.l (op0 op1)
;;   ((:for-types :sh4a))
;;   (cond ((and (eq op1 :r0) (match-types op0  mas-simple))
;;          (masque "0100MMMM.10101001" ;; movua.l @Rm,R0
;;                  (m (rix (mas-base op0) :gp))))
;;         ((and (eq op1 :r0) (match-types op0  mas-postinc))
;;          (masque "0100MMMM.11101001" ;; movua.l @Rm+,R0
;;                  (m (rix (mas-base op0) :gp))))
;;         (t (error "MOVUA.L takes only either a simple memory address or postincrementing address and R0 as its operands."))))

(specops-sh movua.l (op0 op1)
  ((:for-types :sh4a))
  (determine ((op0 (mas mas-simple mas-postinc)) (op1 (reg-fixed :r0))) (ix0)
    (cond ((and (eq op1 :r0) (match-types op0  mas-simple))
           (masque "0100MMMM.10101001" ;; movua.l @Rm,R0
                   (m ix0)))
          ((and (eq op1 :r0) (match-types op0  mas-postinc))
           (masque "0100MMMM.11101001" ;; movua.l @Rm+,R0
                   (m ix0))))))

(readops-sh movua.l.@-0 (word read) ;; movua.l @Rm,R0
  (unmasque "0100MMMM.10101001" word (m)
    (list :movua.l (list '@ (drv-gpr m)) :r0)))

(readops-sh movua.l.+-0 (word read) ;; movua.l @Rm+,R0
  (unmasque "0100MMMM.11101001" word (m)
    (list :movua (list '@+ (drv-gpr m)) :r0)))

;; (specops-sh movml.l (op0 op1)
;;   ((:for-types :sh2a))
;;   (cond ((and (match-types op0 op1  gpr mas-predecr) (eq (mas-displ op1) :r15))
;;          (masque "0100MMMM.11110001" ;; movml.l Rm,@-R15
;;                  (m (rix op0 :gp))))
;;         ((and (match-types op0 op1  mas-postinc gpr) (eq (mas-displ op0) :r15))
;;          (masque "0100NNNN.11110101" ;; movml.l @R15+,Rn
;;                  (n (rix op1 :gp))))
;;         (t (error "MOVML takes only a general-purpose register and pre-decrementing memory address with R15 base as its operands."))))

(specops-sh movml.l (op0 op1)
  ((:for-types :sh2a))
  (determine ((op0 gpr (mas mas-postinc-r15)) (op1 gpr (mas mas-predecr-r15))) (ix0 ix1)
    (cond ((and (match-types op0 op1  gpr mas-predecr) (eq (mas-base op1) :r15))
           (masque "0100MMMM.11110001" ;; movml.l Rm,@-R15
                   (m ix0)))
          ((and (match-types op0 op1  mas-postinc gpr) (eq (mas-base op0) :r15))
           (masque "0100NNNN.11110101" ;; movml.l @R15+,Rn
                   (n ix1))))))

(readops-sh movml.l.r--15 (word read) ;; movml.l Rm,@-R15
  (unmasque "0100MMMM.11110001" word (m)
    (list :movml.l (drv-gpr m) '(-@ :r15))))

(readops-sh movml.l.+15-r (word read) ;; movml.l @R15+,Rn
  (unmasque "0100NNNN.11110101" word (n)
    (list :movml.l '(@+ :r15) (drv-gpr n))))

;; (specops-sh movmu.l (op0 op1)
;;   ((:for-types :sh2a))
;;   (cond ((match-types op0 op1  gpr mas-predecr)
;;          (assert (eq (mas-displ op1) :r15)
;;                  () "MOVMU.L may only use a memory access with register R15 as its base.")
;;          (masque "0100MMMM.11110000" ;; movmu.l Rm,@-R15
;;                  (m (rix op0 :gp))))
;;         ((match-types op0 op1  mas-postinc gpr)
;;          (assert (eq (mas-displ op1) :r15)
;;                  () "MOVMU.L may only use a memory access with register R15 as its base.")
;;          (masque "0100NNNN.11110100" ;; movmu.l @R15+,Rn
;;                  (n (rix op1 :gp))))))

(specops-sh movmu.l (op0 op1)
  ((:for-types :sh2a))
  (determine ((op0 gpr (mas mas-postinc-r15)) (op1 gpr (mas mas-predecr-r15))) (ix0 ix1)
    (cond ((match-types op0 op1  gpr mas-predecr)
           (masque "0100MMMM.11110000" ;; movmu.l Rm,@-R15
                   (m ix0)))
          ((match-types op0 op1  mas-postinc gpr)
           (masque "0100NNNN.11110100" ;; movmu.l @R15+,Rn
                   (n ix1))))))

(readops-sh movmu.l.r--15 (word read) ;; movmu.l Rm,@-R15
  (unmasque "0100MMMM.11110000" word (m)
    (list :movmu.l (drv-gpr m) '(-@ :r15))))

(readops-sh movmu.l.+15-r (word read) ;; movmu.l @R15+,Rn
  (unmasque "0100NNNN.11110100" word (n)
    (list :movmu.l '(@+ :r15) (drv-gpr n))))

;; (specops-sh movrt (op0)
;;   ((:for-types :sh2a))
;;   (assert (typep op0 'gpr) (op0)
;;           "MOVRT may take only a general-purpose register as its operand.")
;;   (masque "0000NNNN.00111001" ;; movrt Rn
;;           (n (rix op0 :gp))))

(specops-sh movrt (op0)
  ((:for-types :sh2a))
  (determine ((op0 gpr)) (ix0)
    (masque "0000NNNN.00111001" ;; movrt Rn
            (n ix0))))

(readops-sh movrt (word read) ;; movrt Rn
  (unmasque "0000NNNN.00111001" word (n)
    (list :movrt (drv-gpr n))))

;; (specops-sh movt (op0)
;;   ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
;;   (assert (match-types op0  gpr) (op0)
;;           "MOVT may take only a general-purpose register as its operand.")
;;   (masque "0000NNNN.00101001" ;; movt Rn
;;           (n (rix op0 :gp))))

;;; AAA

(specops-sh movt (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (determine ((op0 gpr)) (ix0)
    (masque "0000NNNN.00101001" ;; movt Rn
            (n ix0))))

(readops-sh movt (word read) ;; movt Rn
  (unmasque "0000NNNN.00101001" word (n)
    (list :movt (drv-gpr n))))

;;; *** MOVE STOPS

(specops-sh nott ()
  ((:for-types :sh2a))
  (masque "00000000.01101000")) ;; nott

(readops-sh (masque "00000000.01101000") (read) 
         (list :nott))

(specops-sh swap (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (case w
    (:b (masque "0110NNNN.MMMM1000" ;; swap.b Rm,Rn
                (n (rix op1 :gp)) (m (rix op0 :gp))))
    (:w (masque "0110NNNN.MMMM1001" ;; swap.w Rm,Rn
                (n (rix op1 :gp)) (m (rix op0 :gp))))))

(readops-sh swap.b (word read) ;; swap.b Rm,Rn
  (unmasque "0110NNNN.MMMM1000" word (n m)
    (list :swap :b (drv-gpr m) (drv-gpr n))))

(readops-sh swap.w (word read) ;; swap.b Rm,Rn
  (unmasque "0110NNNN.MMMM1001" word (n m)
    (list :swap :w (drv-gpr m) (drv-gpr n))))

(specops-sh xtrct (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0010NNNN.MMMM1101" ;; xtrct Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh xtrct (word read) ;; xtrct Rm,Rn
  (unmasque "0010NNNN.MMMM1101" word (n m)
    (list :xtrct (drv-gpr m) (drv-gpr n))))

(specops-sh band.b (op0 op1)
  ((:for-types :sh2a))
  (if (match-types op0 op1  integer mas-disp12)
      (masque "0011NNNN.0III1001.0100DDDD.DDDDDDDD" ;; band.b #imm3,@disp12,Rn
              (n (rix (mas-base op1) :gp)) (i op0) (d (mas-displ op1)))
      (error "BAND.B can only take an immediate value and base+displacement memory access as operands.")))

(readops-sh bandnot.b (word read)
  (unmasque "0011NNNN.0III1001.0100DDDD.DDDDDDDD" word (n i d) 
    (list :band.b i (list '@> (drv-gpr n) d))))

(specops-sh bandnot.b (op0 op1)
  ((:for-types :sh2a))
  (if (match-types op0 op1  integer mas-disp12)
      (masque "0011NNNN.0III1001.1100DDDD.DDDDDDDD" ;; bandnot.b #imm3,@(disp12,Rn)
              (n (rix (mas-base op1) :gp)) (i op0) (d (mas-displ op1)))
      (error "BANDNOT.B can only take an immediate value and base+displacement memory access as operands.")))

(readops-sh bandnot.b (word read)
  (unmasque "0011NNNN.0III1001.1100DDDD.DDDDDDDD" word (n i d) 
    (list :bandnot.b i (list '@> (drv-gpr n) d))))

(specops-sh bclr.b (op0 op1)
  ((:for-types :sh2a))
  (if (match-types op0 op1  integer mas-disp12)
      (masque "0011NNNN.0III1001.0000DDDD.DDDDDDDD" ;; bclr.b #imm3,@(disp12,Rn)
              (n (rix (mas-base op1) :gp)) (i op0) (d (mas-displ op1)))
      (error "BCLR.B can only be called on a 3-bit integer value and a displaced memory access.")))

(readops-sh bclr.b (word read) ;; bclr.b #imm3,@(disp12,Rn)
  (unmasque "0011NNNN.0III1001.0000DDDD.DDDDDDDD" word (n i d)
    (list :bclr.b i (list '@> (drv-gpr n) d))))

(specops-sh bclr (op0 op1)
  ((:for-types :sh2a))
  (if (match-types op0 op1  integer gpr)
      (masque "10000110.NNNN0III" ;; bclr #imm3,Rn
              (n (rix op1 :gp)) (i op0))
      (error "Invalid operands passed to BCLR.")))

(readops-sh bclr (word read) ;; bclr #imm3,Rn
  (unmasque "10000110.NNNN0III" word (n i)
    (list :bclr i (drv-gpr n))))

(specops-sh bld.b (op0 op1)
  ((:for-types :sh2a))
  (if (match-types op0 op1  integer mas-disp12)
      (masque "0011NNNN.0III1001.0011DDDD.DDDDDDDD" ;; bld.b #imm3,@(disp12,Rn)
              (n (rix (mas-base op1) :gp)) (i op0) (d (mas-displ op1)))
      (error "Invalid operands passed to BLD.B.")))

(readops-sh bld.b (word read) ;; bld.b #imm3,@(disp12,Rn)
  (unmasque "0011NNNN.0III1001.0011DDDD.DDDDDDDD" word (n i d)
    (list :bld.b i (list '@> (drv-gpr n) d))))

(specops-sh bld (op0 op1)
  ((:for-types :sh2a)) 
  (if (and (match-types op0 op1  integer gpr) (zerop (ash op0 -3)))
      (masque "10000111.NNNN1III" ;; bld #imm3,Rn
              (n (rix op1 :gp)) (i op0))
      (error "Invalid operands passed to BLD.")))

(readops-sh bld (word read) ;; bld #imm3,Rn
  (unmasque "10000111.NNNN1III" word (n i)
    (list :bld i (drv-gpr n))))

(specops-sh bldnot.b (op0 op1)
  ((:for-types :sh2a))
  (if (match-types op0 op1  gpr mas-disp12)
      (masque "0011NNNN.0III1001.1011DDDD.DDDDDDDD" ;; bldnot.b #imm3,@(disp12,Rn)
              (n (rix (mas-base op1) :gp)) (i op0) (d (mas-displ op1)))
      (error "Invalid operands passed to BLDNOT.")))

(readops-sh bldnot.b (word read) ;; bldnot.b #imm3,@(disp12,Rn)
  (unmasque "0011NNNN.0III1001.1011DDDD.DDDDDDDD" word (n i d)
    (list :bldnot.b i (list '@> (drv-gpr n) d))))

(specops-sh bor.b (op0 op1)
  ((:for-types :sh2a))
  (if (match-types op0 op1  integer mas-disp12)
      (masque "0011NNNN.0III1001.0101DDDD.DDDDDDDD" ;; bor.b #imm3,@(disp12,Rn)
              (n (rix (mas-base op1) :gp)) (i op0) (d (mas-displ op1)))
      (error "BOR can only be called at width B with an immediate value and base+displacement memory access as its operands.")))

(readops-sh bor.b (word read) ;; bor.b #imm3,@(disp12,Rn)
  (unmasque "0011NNNN.0III1001.0101DDDD.DDDDDDDD" word (n i d)
    (list :bor.b i (list '@> (drv-gpr n) d))))

(specops-sh bornot.b (op0 op1)
  ((:for-types :sh2a))
  (if (match-types op0 op1  integer mas-disp12)
      (masque "0011NNNN.0III1001.1101DDDD.DDDDDDDD" ;; bornot.b #imm3,@(disp12,Rn)
              (n (rix (mas-base op1) :gp)) (i op0) (d (mas-displ op1)))
      (error "BORNOT can only be called at width B with an immediate value and base+displacement memory access as its operands.")))

(readops-sh bornot.b (word read) ;; bornot.b #imm3,@(disp12,Rn)
  (unmasque "0011NNNN.0III1001.0101DDDD.DDDDDDDD" word (n i d)
    (list :bornot.b i (list '@> (drv-gpr n) d))))

(specops-sh bset.b (op0 op1)
  ((:for-types :sh2a))
  ;; (determine ((op0 (imm :width 3)) (op1 mas-disp)))
  (if (match-types op0 op1  integer mas-disp12)
      (masque "0011NNNN.0III1001.0001DDDD.DDDDDDDD" ;; bset.b #imm3,@(disp12,Rn)
              (n (rix (mas-base op1) :gp)) (i op0) (d (mas-displ op1)))
      (error "BSET can only be called with an immediate value and base+displacement memory access as its operands at width B.")))

(readops-sh bset.b (word read) ;; bset.b #imm3,@(disp12,Rn)
  (unmasque "0011NNNN.0III1001.0001DDDD.DDDDDDDD" word (n i d)
    (list :bset.b i (list '@> (drv-gpr n) d))))

(specops-sh bset (op0 op1)
  ((:for-types :sh2a))
  (if (match-types op0 op1  integer gpr)
      (masque "10000110.NNNN1III" ;; bset #imm3,Rn
              (n (rix op1 :gp)) (i op0))
      (error "Invalid operands passed to BSET.")))

(readops-sh bset (word read) ;; bset #imm3,Rn
  (unmasque "10000110.NNNN1III" word (n i)
    (list :bset i (drv-gpr n))))

(specops-sh bst.b (op0 op1)
  ((:for-types :sh2a))
  (if (match-types op0 op1  integer mas-disp12)
      (masque "0011NNNN.0III1001.0010DDDD.DDDDDDDD" ;; bst.b #imm3,@(disp12,Rn)
              (n (rix (mas-base op1) :gp)) (i op0) (d (mas-displ op1)))
      (error "BST can only be called with an immediate value and base+displacement memory access as its operands at width B.")))

(readops-sh bst.b (word read) ;; bst.b #imm3,@(disp12,Rn)
  (unmasque "0011NNNN.0III1001.0010DDDD.DDDDDDDD" word (n i d)
    (list :bst.b i (list '@> (drv-gpr n) d))))

(specops-sh bst (op0 op1)
  ((:for-types :sh2a))
  (if (match-types op0 op1  integer gpr)
      (masque "10000111.NNNN0III" ;; bst #imm3,Rn
              (n (rix op1 :gp)) (i op0))
      (error "Invalid operands passed to BSET.")))

(readops-sh bst (word read) ;; bst #imm3,Rn
  (unmasque "10000111.NNNN0III" word (n i)
    (list :bst i (drv-gpr n))))

(specops-sh bxor.b (op0 op1)
  ((:for-types :sh2a))
  (if (match-types op0 op1  gpr mas-disp12)
      (masque "0011NNNN.0III1001.0110DDDD.DDDDDDDD" ;; bxor.b #imm3,@(disp12,Rn)
              (n (rix (mas-base op1) :gp)) (i op0) (d (mas-displ op1)))
      (error "Invalid operands passed to BXOR.")))

(readops-sh bxor.b (word read) ;; bxor.b #imm3,@(disp12,Rn)
  (unmasque "0011NNNN.0III1001.0110DDDD.DDDDDDDD" word (n i d)
    (list :bxor.b i (list '@> (drv-gpr n) d))))

;; *** BITS END ***

(specops-sh add (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (cond ((match-types op0 op1  gpr gpr)
         (masque "0011NNNN.MMMM1100" ;; add Rm,Rn
                 (n (rix op1 :gp)) (m (rix op0 :gp))))
        ((match-types op0 op1  integer gpr)
         (masque "0111NNNN.IIIIIIII" ;; add #imm,Rn
                 (n (rix op1 :gp)) (i op0)))))

(readops-sh add.r-r (word read) ;; add Rm,Rn
  (unmasque "0011NNNN.MMMM1100" word (n m)
    (list :add (drv-gpr m) (drv-gpr n))))

(readops-sh add.i-r (word read) ;; add #imm,Rn
  (unmasque "0111NNNN.IIIIIIII" word (n i)
    (list :add i (drv-gpr n))))

(specops-sh addc (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0011NNNN.MMMM1110" ;; addc Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh addc (word read) ;; addc Rm,Rn
  (unmasque "0011NNNN.MMMM1110" word (n m)
    (list :addc (drv-gpr m) (drv-gpr n))))

(specops-sh addv (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0011NNNN.MMMM1111" ;; addv Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh addv (word read)
  (unmasque "0011NNNN.MMMM1111" word (n m)
    (list :addv (drv-gpr m) (drv-gpr n))))

(specops-sh cmp/eq (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (cond ((and (eq op1 :r0) (typep op0 'integer))
         (masque "10001000.IIIIIIII" ;; cmp/eq #imm,R0
                 (i op0)))
        ((match-types op0 op1  gpr gpr)
         (masque "0011NNNN.MMMM0000" ;; cmp/eq Rm,Rn
                 (n (rix op1 :gp)) (m (rix op0 :gp))))))

(readops-sh cmp/eq.i-0 (word read) ;; cmp/eq #imm,R0
  (unmasque "10001000.IIIIIIII" word (i)
    (list :cmp/eq i)))

(readops-sh cmp/eq.r-r (word read) ;; cmp/eq Rm,Rn
  (unmasque "0011NNNN.MMMM0000" word (n m)
    (list :cmp/eq (drv-gpr m) (drv-gpr n))))

(specops-sh cmp/hs (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0011NNNN.MMMM0010" ;; cmp/hs Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh cmp/hs (word read) ;; cmp/hs Rm,Rn
  (unmasque "0011NNNN.MMMM0010" word (n m)
    (list :cmp/hs (drv-gpr m) (drv-gpr n))))

(specops-sh cmp/ge (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0011NNNN.MMMM0011" ;; cmp/ge Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh cmp/ge (word read) ;; cmp/ge Rm,Rn
  (unmasque "0011NNNN.MMMM0011" word (n m)
    (list :cmp/ge (drv-gpr m) (drv-gpr n))))

(specops-sh cmp/hi (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0011NNNN.MMMM0110" ;; cmp/hi Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh cmp/hi (word read) ;; cmp/hi Rm,Rn
  (unmasque "0011NNNN.MMMM0110" word (n m)
    (list :cmp/hi (drv-gpr m) (drv-gpr n))))

(specops-sh cmp/gt (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0011NNNN.MMMM0111" ;; cmp/gt Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh cmp/gt (word read)
  (unmasque "0011NNNN.MMMM0111" word (n m)
    (list :cmp/gt (drv-gpr m) (drv-gpr n))))

(specops-sh cmp/pl (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0100NNNN.00010101" ;; cmp/pl Rn
          (n (rix op0 :gp))))

(readops-sh cmp/pl (word read) ;; cmp/pl Rn
  (unmasque "0100NNNN.00010101" word (n)
    (list :cmp/pl (drv-gpr n))))

(specops-sh cmp/pz (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0100NNNN.00010001" ;; cmp/pz Rn
          (n (rix op0 :gp))))

(readops-sh cmp/pz (word read) ;; cmp/pz Rn
  (unmasque "0100NNNN.00010001" word (n)
    (list :cmp/pz (drv-gpr n))))

(specops-sh cmp/str (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0010NNNN.MMMM1100" ;; cmp/str Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh cmp/str (word read) ;; cmp/str Rm,Rn
  (unmasque "0010NNNN.MMMM1100" word (n m)
    (list :cmp/str (drv-gpr m) (drv-gpr n))))

(specops-sh clips (w op0)
  ((:for-types :sh2a))
  (assert (match-types op0  gpr) (op0)
          "CLIPS may take only a general-purpose register as its operand.")
  (case w
    (:b (masque "0100NNNN.10010001" ;; clips.b Rn
                (n (rix op0 :gp))))
    (:w (masque "0100NNNN.10010101" ;; clips.w Rn
                (n (rix op0 :gp))))
    (t  (error "CLIPS may only operate at widths B or W."))))

(readops-sh clips-b (word read)
  (unmasque "0100NNNN.10010001" word (n)
    (list :clips :b (drv-gpr n))))

(readops-sh clips-w (word read)
  (unmasque "0100NNNN.10010101" word (n)
    (list :clips :w (drv-gpr n))))

(specops-sh clipu (w op0)
  ((:for-types :sh2a))
  (assert (match-types op0  gpr) (op0)
          "CLIPU may take only a general-purpose register as its operand.")
  (case w
    (:b (masque "0100NNNN.10000001" ;; clipu.b Rn
                (n (rix op0 :gp))))
    (:w (masque "0100NNNN.10000101" ;; clipu.w Rn
                (n (rix op0 :gp))))
    (t  (error "CLIPU may only operate at widths B or W."))))

(readops-sh clipu-b (word read) ;; clipu.b Rn
  (unmasque "0100NNNN.10000001" word (n)
    (list :clipu :b (drv-gpr n))))

(readops-sh clipu-w (word read) ;; clipu.w Rn
  (unmasque "0100NNNN.10000101" word (n)
    (list :clipu :w (drv-gpr n))))

(specops-sh div0s (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "DIV0S may take only general-purpose registers as its operands.")
  (masque "0010NNNN.MMMM0111" ;; div0s Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh div0s (word read) ;; div0s Rm,Rn
  (unmasque "0010NNNN.MMMM0111" word (m n)
    (list :div0s (drv-gpr m) (drv-gpr n))))

(specops-sh div0u ()
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "00000000.00011001")) ;; div0u

(readops-sh (masque "00000000.00011001") (read) ;; div0u
  (list :div0u))

(specops-sh div1 (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "DIV1 may take only general-purpose registers as its operands.")
  (masque "0011NNNN.MMMM0100" ;; div1 Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh div1 (word read) ;; div1 Rm,Rn
  (unmasque "0011NNNN.MMMM0100" word (n m)
    (list :div1 (drv-gpr m) (drv-gpr n))))

(specops-sh divs (op0 op1)
  ((:for-types :sh2a))
  (assert (and (eq :r0 op0) (match-types op1  gpr)) (op0 op1)
          "DIVS may take only R0 and a general-purpose register as operands.")
  (masque "0100NNNN.10010100" ;; divs R0,Rn
            (n (rix op1 :gp))))

(readops-sh divs (word read) ;; divs R0,Rn
  (unmasque "0100NNNN.10010100" word (n)
    (list :divs :r0 (drv-gpr n))))

(specops-sh divu (op0 op1)
  ((:for-types :sh2a))
  (assert (and (eq :r0 op0) (match-types op1  gpr)) (op0 op1)
          "DIVU may take only R0 and a general-purpose register as operands.")
  (masque "0100NNNN.10000100" ;; divu R0,Rn
            (n (rix op1 :gp))))

(readops-sh divu (word read) ;; divu R0,Rn
  (unmasque "0100NNNN.10000100" word (n)
    (list :divu :r0 (drv-gpr n))))

(specops-sh dmuls.l (op0 op1)
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (if (match-types op0 op1  gpr gpr)
      (masque "0011NNNN.MMMM1101" ;; dmuls.l Rm,Rn
              (n (rix op1 :gp)) (m (rix op0 :gp)))
      (error "DMULS.L may take only general-purpose registers as its operands.")))

(readops-sh dmuls.l (word read) ;; dmuls.l Rm,Rn
  (unmasque "0011NNNN.MMMM1101" word (n m)
    (list :dmuls.l (drv-gpr m) (drv-gpr n))))

(specops-sh dmulu.l (op0 op1)
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (if (match-types op0 op1  gpr gpr)
      (masque "0011NNNN.MMMM0101" ;; dmulu.l Rm,Rn
              (n (rix op1 :gp)) (m (rix op0 :gp)))
      (error "DMULU works only at width L and may take only general-purpose registers as its operands.")))

(readops-sh dmulu.l (word read)
  (unmasque "0011NNNN.MMMM0101" word (n m)
    (list :dmulu.l (drv-gpr m) (drv-gpr n))))

(specops-sh dt (op0)
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (typep op0 'gpr) (op0)
          "DT may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00010000" ;; dt Rn
          (n (rix op0 :gp))))

(readops-sh dt (word read) ;; dt Rn
  (unmasque "0100NNNN.00010000" word (n)
    (list :dt (drv-gpr n))))

(specops-sh exts (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "EXTS may take only general-purpose registers as its operands.")
  (case w
    (:b (masque "0110NNNN.MMMM1110" ;; exts.b Rm,Rn
                (n (rix op1 :gp)) (m (rix op0 :gp))))
    (:w (masque "0110NNNN.MMMM1111" ;; exts.w Rm,Rn
                (n (rix op1 :gp)) (m (rix op0 :gp))))
    (t  (error "EXTS may only operate at widths B or W."))))

(readops-sh exts-b (word read)
  (unmasque "0110NNNN.MMMM1110" word (n m)
    (list :exts :b (drv-gpr m) (drv-gpr n))))

(readops-sh exts-w (word read)
  (unmasque "0110NNNN.MMMM1111" word (n m)
    (list :exts :w (drv-gpr m) (drv-gpr n))))

(specops-sh extu (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "EXTU may take only general-purpose registers as its operands.")
  (case w
    (:b (masque "0110NNNN.MMMM1100" ;; extu.b Rm,Rn
                (n (rix op1 :gp)) (m (rix op0 :gp))))
    (:w (masque "0110NNNN.MMMM1101" ;; extu.w Rm,Rn
                (n (rix op1 :gp)) (m (rix op0 :gp))))
    (t  (error "EXTS may only operate at widths B or W."))))

(readops-sh extu-b (word read) ;; extu.b Rm,Rn
  (unmasque "0110NNNN.MMMM1100" word (n m)
    (list :extu :b (drv-gpr m) (drv-gpr n))))

(readops-sh extu-w (word read) ;; extu.w Rm,Rn
  (unmasque "0110NNNN.MMMM1101" word (n m)
    (list :extu :w (drv-gpr m) (drv-gpr n))))

(specops-sh mac (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  mas-postinc mas-postinc) (op0 op1)
          "MAC may take only post-incrementing memory access points as its operands.")
  (case w
    (:l (masque "0000NNNN.MMMM1111" ;; mac.l @Rm+,@Rn+
                (n (rix op1 :gp)) (m (rix op0 :gp))))
    (:w (masque "0100NNNN.MMMM1111" ;; mac.w @Rm+,@Rn+
                (n (rix op1 :gp)) (m (rix op0 :gp))))
    (t  (error "MAC may only operate at widths L or W."))))

(readops-sh mac-l (word read) ;; mac.l @Rm+,@Rn+
  (unmasque "0000NNNN.MMMM1111" word (n m)
    (list :mac :l (drv-gpr m) (drv-gpr n))))

(readops-sh mac-w (word read) ;; mac.w @Rm+,@Rn+
  (unmasque "0100NNNN.MMMM1111" word (n m)
    (list :mac :w (drv-gpr m) (drv-gpr n))))

(specops-sh mul.l (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (if (match-types op0 op1  gpr gpr)
      (masque "0000NNNN.MMMM0111" ;; mul.l Rm,Rn
              (n (rix op1 :gp)) (m (rix op0 :gp)))
      (error "MUL.L may take only general-purpose registers as its operands.")))

(readops-sh mul.l (word read) ;; mul.l Rm,Rn
  (unmasque "0000NNNN.MMMM0111" word (n m)
    (list :mul :l (drv-gpr m) (drv-gpr n))))

(specops-sh mulr (op0 op1)
  ((:for-types :sh2a))
  (assert (and (eq :r0 op0) (match-types op1  gpr)) (op0 op1)
          "MULR may take only R0 and a general-purpose register as operands.")
  (masque "0100NNNN.10000000" ;; mulr R0,Rn
          (n (rix op1 :gp))))

(readops-sh mulr (word read) ;; mulr R0,Rn
  (unmasque "0100NNNN.10000000" word (n)
    (list :mulr (drv-gpr n))))

(specops-sh muls.w (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (if (match-types op0 op1  gpr gpr)
      (masque "0010NNNN.MMMM1111" ;; muls.w Rm,Rn
              (n (rix op1 :gp)) (m (rix op0 :gp)))
      (error "MULS.W may take only general-purpose registers as its operands.")))

(readops-sh muls.w (word read) ;; muls.w Rm,Rn
  (unmasque "0010NNNN.MMMM1111" word (n m)
    (list :muls :w (drv-gpr m) (drv-gpr n))))

(specops-sh mulu.w (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (if (match-types op0 op1  gpr gpr)
      (masque "0010NNNN.MMMM1110" ;; mulu.w Rm,Rn
              (n (rix op1 :gp)) (m (rix op0 :gp)))
      (error "MULS may take only general-purpose registers as its operands.")))

(readops-sh mulu.w (word read) ;; mulu.w Rm,Rn
  (unmasque "0010NNNN.MMMM1110" word (n m)
    (list :mulu :w (drv-gpr m) (drv-gpr n))))

(specops-sh neg (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "NEG may take only general-purpose registers as its operands.")
  (masque "0110NNNN.MMMM1011" ;; neg Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh neg (word read) ;; neg Rm,Rn
  (unmasque "0110NNNN.MMMM1011" word (n m)
    (list :neg (drv-gpr m) (drv-gpr n))))

(specops-sh negc (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "NEGC may take only general-purpose registers as its operands.")
  (masque "0110NNNN.MMMM1010" ;; negc Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh negc (word read) ;; negc Rm,Rn
  (unmasque "0110NNNN.MMMM1010" word (n m)
    (list :negc (drv-gpr m) (drv-gpr n))))

(specops-sh sub (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "SUB may take only general-purpose registers as its operands.")
  (masque "0011NNNN.MMMM1000" ;; sub Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh sub (word read)
  (unmasque "0011NNNN.MMMM1000" word (n m)
    (list :sub (drv-gpr m) (drv-gpr n))))

(specops-sh subc (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "SUBC may take only general-purpose registers as its operands.")
  (masque "0011NNNN.MMMM1010" ;; subc Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh subc (word read) ;; subc Rm,Rn
  (unmasque "0011NNNN.MMMM1010" word (n m)
    (list :subc (drv-gpr m) (drv-gpr n))))

(specops-sh subv (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "SUBV may take only general-purpose registers as its operands.")
  (masque "0011NNNN.MMMM1011" ;; subv Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh subv (word read) ;; subv Rm,Rn
  (unmasque "0011NNNN.MMMM1011" word (n m)
    (list :subv (drv-gpr m) (drv-gpr n))))

;;; *** ARITHMETIC ENDS - LOGIC BEGINS

(specops-sh and (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (cond ((match-types op0 op1  gpr gpr)
         (masque "0010NNNN.MMMM1001" ;; and Rm,Rn
                 (n (rix op1 :gp)) (m (rix op0 :gp))))
        ((and (eq op1 :r0) (typep op0 'integer))
         (masque "11001001.IIIIIIII" ;; and #imm,R0
                 (i op0)))))

(readops-sh and.r-r (word read) ;; and Rm,Rn
  (unmasque "0010NNNN.MMMM1001" word (n m)
    (list :and (drv-gpr m) (drv-gpr n))))

(readops-sh and.i-r (word read) ;; and #imm,R0
  (unmasque "11001001.IIIIIIII" word (i)
    (list :and i :r0)))

(specops-sh and.b (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  integer mas-gb+rzro) (op0 op1)
          "ANDB may take only an immediate value and a (@gbr0) memory access point as operands.")
  (masque "11001101.IIIIIIII" ;; and.b #imm,@(R0,GBR)
          (i op0)))

(readops-sh and.b (word read) ;; and.b #imm,@(R0,GBR)
  (unmasque "11001101.IIIIIIII" word (i)
    (list :and :b i '(@gbr0))))

(specops-sh not (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "NOT may take only general-purpose registers as its operands.")
  (masque "0110NNNN.MMMM0111" ;; not Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh not (word read)
  (unmasque "0110NNNN.MMMM0111" word (n m)
    (list :not (drv-gpr m) (drv-gpr n))))

(specops-sh or (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (cond ((match-types op0 op1  gpr gpr)
         (masque "0010NNNN.MMMM1011" ;; or Rm,Rn
                 (n (rix op1 :gp)) (m (rix op0 :gp))))
        ((and (eq op1 :r0) (typep op0 'integer))
         (masque "11001011.IIIIIIII" ;; or #imm,R0
                 (i op0)))))

(readops-sh or.r-r (word read) ;; or Rm,Rn
  (unmasque "0010NNNN.MMMM1011" word (n m)
    (list :or (drv-gpr m) (drv-gpr n))))

(readops-sh or.i-r (word read) ;; or #imm,R0
  (unmasque "11001011.IIIIIIII" word (i)
    (list :or i :r0)))

(specops-sh or.b (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  integer mas-gb+rzro) ()
          "ORB may take only an immediate value and a (@gbr0) memory access point as operands.")
  (masque "11001111.IIIIIIII" ;; or.b #imm,@(R0,GBR)
          (i op0)))

(readops-sh or.b (word read) ;; or.b #imm,@(R0,GBR)
  (unmasque "11001111.IIIIIIII" word (i)
    (list :or.b i '(@gbr0))))

(specops-sh tas.b (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  mas-simple) ()
          "TAS only operates at width B and may take only a plain memory access scheme as its operand.")
  (masque "0100NNNN.00011011" ;; tas.b @Rn
          (n (rix (mas-base op0) :gp))))

(readops-sh tas.b (word read) ;; tas.b @Rn
  (unmasque "0100NNNN.00011011" word (n)
    (list :tas.b (list '@ (drv-gpr n)))))

(specops-sh tst (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (cond ((match-types op0 op1  gpr gpr)
         (masque "0010NNNN.MMMM1000" ;; tst Rm,Rn
                 (n (rix op1 :gp)) (m (rix op0 :gp))))
        ((and (eq op1 :r0) (typep op0 'integer))
         (masque "11001000.IIIIIIII" ;; tst #imm,R0
                 (i op0)))))

(readops-sh tst.r-r (word read) ;; tst Rm,Rn
  (unmasque "0010NNNN.MMMM1000" word (n m)
    (list :tst (drv-gpr m) (drv-gpr n))))

(readops-sh tst.i-r (word read) ;; tst #imm,R0
  (unmasque "11001000.IIIIIIII" word (i)
    (list :tst i :r0)))

(specops-sh tst.b (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  integer mas-gb+rzro) (op0 op1)
          "TST.B may take only an immediate value and a (@gbr0) memory access point as operands.")
  (masque "11001100.IIIIIIII" ;; tst.b #imm,@(R0,GBR)
          (i op0)))

(readops-sh tst.b (word read) ;; tst.b #imm,@(R0,GBR)
  (unmasque "11001100.IIIIIIII" word (i)
    (list :tst.b i '(@gbr0))))

(specops-sh xor (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (cond ((match-types op0 op1  gpr gpr)
         (masque "0010NNNN.MMMM1010" ;; xor Rm,Rn
                 (n (rix op1 :gp)) (m (rix op0 :gp))))
        ((and (eq op1 :r0) (typep op0 'integer))
         (masque "11001010.IIIIIIII" ;; or #imm,R0
                 (i op0)))))

(readops-sh xor.r-r (word read) ;; xor Rm,Rn
  (unmasque "0010NNNN.MMMM1010" word (n m)
    (list :xor (drv-gpr m) (drv-gpr n))))

(readops-sh xor.i-r (word read) ;; or #imm,R0
  (unmasque "11001010.IIIIIIII" word (i)
    (list :xor i :r0)))

(specops-sh xor.b (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  integer mas-gb+rzro) (op0 op1)
          "XORB may take only an immediate value and a (@gbr0) memory access point as operands.")
  (masque "11001110.IIIIIIII" ;; xor.b #imm,@(R0,GBR)
          (i op0)))

(readops-sh xor.b (word read)
  (unmasque "11001110.IIIIIIII" word (i)
    (list :xor.b i '(@gbr0))))

;;; *** LOGICAL OPS END, SHIFTS BEGIN

(specops-sh rotcl (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "ROTCL may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00100100" ;; rotcl Rn
            (n (rix op0 :gp))))

(readops-sh rotcl (word read) ;; rotcl Rn
  (unmasque "0100NNNN.00100100" word (n)
    (list :rotcl (drv-gpr n))))

(specops-sh rotcr (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "ROTCR may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00100101" ;; rotcr Rn
            (n (rix op0 :gp))))

(readops-sh rotcr (word read) ;; rotcr Rn
  (unmasque "0100NNNN.00100101" word (n)
    (list :rotcr (drv-gpr n))))

(specops-sh rotl (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "ROTL may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00000100" ;; rotl Rn
            (n (rix op0 :gp))))

(readops-sh rotl (word read) ;; rotl Rn
  (unmasque "0100NNNN.00000100" word (n)
    (list :rotl (drv-gpr n))))

(specops-sh rotr (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "ROTR may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00000101" ;; rotr Rn
            (n (rix op0 :gp))))

(readops-sh rotr (word read) ;; rotr Rn
  (unmasque "0100NNNN.00000101" word (n)
    (list :rotr (drv-gpr n))))

(specops-sh shad (op0 op1)
  ((:for-types :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "SHAD may take only general-purpose registers as its operands.")
  (masque "0100NNNN.MMMM1100" ;; shad Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh shad (word read) ;; shad Rm,Rn
  (unmasque "0100NNNN.MMMM1100" word (n m)
    (list :shad (drv-gpr m) (drv-gpr n))))

(specops-sh shal (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHAL may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00100000" ;; shal Rn
            (n (rix op0 :gp))))

(readops-sh shal (word read) ;; shal Rn
  (unmasque "0100NNNN.00100000" word (n)
    (list :shal (drv-gpr n))))

(specops-sh shar (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHAR may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00100001" ;; shal Rn
            (n (rix op0 :gp))))

(readops-sh shar (word read) ;; shal Rn
  (unmasque "0100NNNN.00100001" word (n)
    (list :shar (drv-gpr n))))

(specops-sh shld (op0 op1)
  ((:for-types :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "SHLD may take only general-purpose registers as its operands.")
  (masque "0100NNNN.MMMM1101" ;; shld Rm,Rn
          (n (rix op1 :gp)) (m (rix op0 :gp))))

(readops-sh shld (word read) ;; shld Rm,Rn
  (unmasque "0100NNNN.MMMM1101" word (n m)
    (list :shld (drv-gpr m) (drv-gpr n))))

(specops-sh shll (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHLL may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00000000" ;; shll Rn
            (n (rix op0 :gp))))

(readops-sh shll (word read) ;; shll Rn
  (unmasque "0100NNNN.00000000" word (n)
    (list :shll (drv-gpr n))))

(specops-sh shll2 (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHLL2 may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00001000" ;; shll2 Rn
          (n (rix op0 :gp))))

(readops-sh shll2 (word read) ;; shll2 Rn
  (unmasque "0100NNNN.00001000" word (n)
    (list :shll2 (drv-gpr n))))

(specops-sh shll8 (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHLL8 may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00011000" ;; shll8 Rn
          (n (rix op0 :gp))))

(readops-sh shll8 (word read) ;; shll8 Rn
  (unmasque "0100NNNN.00011000" word (n)
    (list :shll8 (drv-gpr n))))

(specops-sh shll16 (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHLL16 may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00101000" ;; shll16 Rn
          (n (rix op0 :gp))))

(readops-sh shll16 (word read) ;; shll16 Rn
  (unmasque "0100NNNN.00101000" word (n)
    (list :shll16 (drv-gpr n))))

(specops-sh shlr (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHLR may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00000001" ;; shlr Rn
          (n (rix op0 :gp))))

(readops-sh shlr (word read) ;; shlr Rn
  (unmasque "0100NNNN.00000001" word (n)
    (list :shlr (drv-gpr n))))

(specops-sh shlr2 (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHLR2 may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00001001" ;; shlr2 Rn
          (n (rix op0 :gp))))

(readops-sh shlr2 (word read) ;; shlr2 Rn
  (unmasque "0100NNNN.00001001" word (n)
    (list :shlr2 (drv-gpr n))))

(specops-sh shlr8 (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHLR8 may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00011001" ;; shlr8 Rn
          (n (rix op0 :gp))))

(readops-sh shlr8 (word read) ;; shlr8 Rn
  (unmasque "0100NNNN.00011001" word (n)
    (list :shlr8 (drv-gpr n))))

(specops-sh shlr16 (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHLR16 may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00101001" ;; shlr16 Rn
          (n (rix op0 :gp))))

(readops-sh shlr16 (word read) ;; shlr16 Rn
  (unmasque "0100NNNN.00101001" word (n)
    (list :shlr16 (drv-gpr n))))

;;; *** SHIFTS END, BRANCHES BEGIN

(specops-sh bf (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (if (symbolp op0)
      (to-tag (lambda (position index)
                (masque "10001011.DDDDDDDD"
                        (d (- position index))))
              :breadth 1 :bindings (list op0))
      (if (integerp op0)
          (masque "10001011.DDDDDDDD" ;; bf label
                  (d op0))
          (error "BF's operand must be a label or an integer."))))

(readops-sh bf (word read) ;; bf label
  (unmasque "10001011.DDDDDDDD" word (d)
    (list :bf d)))

(specops-sh bf/s (op0)
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (if (symbolp op0)
      (to-tag (lambda (position index)
                (masque "10001111.DDDDDDDD"
                        (d (- position index))))
              :breadth 1 :bindings (list op0))
      (if (integerp op0)
          (masque "10001111.DDDDDDDD" ;; bf/s label
                  (d op0))
          (error "BF/S's operand must be a label or an integer."))))

(readops-sh bf/s (word read) ;; bf/s label
  (unmasque "10001111.DDDDDDDD" word (d)
    (list :bf/s d)))

(specops-sh bt (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (if (symbolp op0)
      (to-tag (lambda (position index)
                (masque "10001001.DDDDDDDD"
                        (d (- position index))))
              :breadth 1 :bindings (list op0))
      (if (integerp op0)
          (masque "10001001.DDDDDDDD" ;; bt label
                  (d op0))
          (error "BT's operand must be a label or an integer."))))

(readops-sh bt (word read) ;; bt label
  (unmasque "10001001.DDDDDDDD" word (d)
    (list :bt d)))

(specops-sh bt/s (op0)
  ;; bt/s label
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (if (symbolp op0)
      (to-tag (lambda (position index)
                (masque "10001001.DDDDDDDD"
                        (d (- position index))))
              :breadth 1 :bindings (list op0))
      (if (integerp op0)
          (masque "10001101.DDDDDDDD" ;; bt/s label
                  (d op0))
          (error "BT/S's operand must be a label or an integer."))))

(readops-sh bt/s (word read) ;; bt/s label
  (unmasque "10001101.DDDDDDDD" word (d)
    (list :bt/s d)))

(specops-sh bra (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (if (symbolp op0)
      (to-tag (lambda (position index)
                (masque "1010DDDD.DDDDDDDD"
                        (d (- position index))))
              :breadth 1 :bindings (list op0))
      (if (integerp op0)
          (masque "1010DDDD.DDDDDDDD" ;; bra label
                  (d op0))
          (error "BRA's operand must be a label or an integer."))))

(readops-sh bra (word read) ;; bra label
  (unmasque "1010DDDD.DDDDDDDD" word (d)
    (list :bra d)))

(specops-sh braf (op0)
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "BRAF may take only a general-purpose register as its operand.")
  (masque "0000MMMM.00100011" ;; braf Rm
            (m (rix op0 :gp))))

(readops-sh braf (word read) ;; braf Rm
  (unmasque "0000MMMM.00100011" word (m)
    (list :braf (drv-gpr m))))

(specops-sh bsr (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (if (symbolp op0)
      (to-tag (lambda (position index)
                (masque "1011DDDD.DDDDDDDD"
                        (d (- position index))))
              :breadth 1 :bindings (list op0))
      (if (integerp op0)
          (masque "1011DDDD.DDDDDDDD" ;; bsr label
                  (d op0))
          (error "BSR's operand must be a label or an integer."))))

(readops-sh bsr (word read) ;; bsr label
  (unmasque "1011DDDD.DDDDDDDD" word (d)
    (list :bsr d)))

(specops-sh bsrf (op0)
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "BRSF may take only a general-purpose register as its operand.")
  (masque "0000MMMM.00000011" ;; bsrf Rm
            (m (rix op0 :gp))))

(readops-sh bsrf (word read) ;; bsrf Rm
  (unmasque "0000MMMM.00000011" word (m)
    (list :bsrf (drv-gpr m))))

(specops-sh jmp (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  mas-simple) (op0)
          "JMP may only take a plain memory access scheme as its operand.")
  (masque "0100MMMM.00101011" ;; jmp @Rm
          (m (rix (mas-base op0) :gp))))

(readops-sh jmp (word read) ;; jmp @Rm
  (unmasque "0100MMMM.00101011" word (m)
    (list :jmp (list '@ (drv-gpr m)))))

(specops-sh jsr (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  mas-simple) (op0)
          "JMP may only take a plain memory access scheme as its operand.")
  (masque "0100MMMM.00001011" ;; jsr @Rm
          (m (rix (mas-base op0) :gp))))

(readops-sh jsr (word read) ;; jsr @Rm
  (unmasque "0100MMMM.00001011" word (m)
    (list :jsr (list '@ (drv-gpr m)))))

(specops-sh jsr/n (op0)
  ((:for-types :sh2a))
  (cond ((match-types op0  mas-simple)
         (masque "0100MMMM.01001011" ;; jsr/n @Rm
                 (m (rix (mas-base op0) :gp))))
        ((match-types op0  mas-tb+dis4)
         (masque "10000011.DDDDDDDD" ;; jsr/n @@(disp8,TBR)
                 (d (mas-displ op0))))
        (t (error "JSR/N may only take a plain memory access scheme or a TBR displacement memory access as its operand."))))

(readops-sh jsr/n-@ (word read) ;; jsr/n @Rm
  (unmasque "0100MMMM.01001011" word (m)
    (list :jsr/n (list '@ (drv-gpr m)))))

(readops-sh jsr/n-@@ (word read) ;; jsr/n @@(disp8,TBR)
  (unmasque "10000011.DDDDDDDD" word (d)
    (list :jsr/n (list '@@tbr d))))

(specops-sh rts ()
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "00000000.00001011")) ;; rts

(readops-sh rts (word read) ;; rts
  (unmasque "00000000.00001011" word ()
    (list :rts)))

(specops-sh rts/n ()
  ((:for-types :sh2a))
  (masque "00000000.01101011")) ;; rts/n

(readops-sh rts/n (word read) ;; rts/n
  (unmasque "00000000.01101011" word ()
    (list :rts/n)))

(specops-sh rtv/n (op0)
  ((:for-types :sh2a))
  (assert (match-types op0  gpr) (op0)
          "RTV/N may take only a general-purpose register as its operand.")
  (masque "0000MMMM.01111011" ;; rtv/n Rm
          (m (rix op0 :gp))))

(readops-sh rtv/n (word read)
  (unmasque "0000MMMM.01111011" word (m)
    (list :rtv/n (drv-gpr m))))

;;; *** BRANCHES END, SYSTEM INSTRUCTIONS BEGIN

(specops-sh clrmac ()
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "00000000.00101000")) ;; clrmac

(readops-sh (masque "00000000.00101000") (read)
         (list :clrmac))

(specops-sh clrs ()
  ((:for-types :sh3 :sh4 :sh4a))
  (masque "00000000.01001000")) ;; clrs

(readops-sh (masque "00000000.01001000") (read)
         (list :clrs))

(specops-sh clrt ()
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "00000000.00001000")) ;; clrt

(readops-sh (masque "00000000.00001000") (read)
    (list :clrt))

(specops-sh icbi (op0)
  ((:for-types :sh4a))
  (assert (match-types op0  gpr) (op0)
          "ICBI may take only a general-purpose register as its operand.")
  (masque "0000NNNN.11100011" ;; icbi @Rn
          (n (rix op0 :gp))))

(readops-sh icbi (word read)
  (unmasque "0000NNNN.11100011" word (n)
    (list :icbi (drv-gpr n))))

(specops-sh ldbank (op0 op1)
  ((:for-types :sh2a))
  (assert (and (eq op1 :r0) (match-types op0  mas-simple)) (op0)
          "LDBANK takes only a simple memory address and R0 as its operands.")
  (masque "0100MMMM.11100101" ;; ldbank @Rm,R0
          (m (rix (mas-base op0) :gp))))

(readops-sh ldbank (word read)
  (unmasque "0100MMMM.11100101" word (m)
    (list :ldbank (list '@ (drv-gpr m)) :r0)))
    
(specops-sh ldc (op0 op1)
    ((:type-matcher . matching-types)
     (:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :dsp :privileged))
  (case op1
    (:sr
     (masque "0100MMMM.00001110" ;; ldc Rm,SR
             (m (rix op0 :gp))))
    (:tbr (assert (matching-types                           :sh2a) ()
                  "LDC Rm,TBR incompatible with this architecture type.")
     (masque "0100MMMM.01001010" ;; ldc Rm,TBR
             (m (rix op0 :gp))))
    (:gbr (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                  "LDC Rm,GBR incompatible with this architecture type.")
     (masque "0100MMMM.00011110" ;; ldc Rm,GBR
             (m (rix op0 :gp))))
    (:vbr
     (masque "0100MMMM.00101110" ;; ldc Rm,VBR
             (m (rix op0 :gp))))
    (:mod (assert (matching-types :dsp) ()
                  "LDC Rm,MOD incompatible with this architecture type.")
     (masque "0100MMMM.01011110" ;; ldc Rm,MOD
             (m (rix op0 :gp))))
    (:re  (assert (matching-types :dsp) ()
                  "LDC Rm,RE incompatible with this architecture type.")
     (masque "0100MMMM.01111110" ;; ldc Rm,RE
             (m (rix op0 :gp))))
    (:rs  (assert (matching-types :dsp) ()
                  "LDC Rm,RS incompatible with this architecture type.")
     (masque "0100MMMM.01101110" ;; ldc Rm,RS
             (m (rix op0 :gp))))
    (:sgr (assert (matching-types           :sh4a :privileged) ()
                  "LDC Rm,SGR incompatible with this architecture type.")
     (masque "0100MMMM.00111010" ;; ldc Rm,SGR
             (m (rix op0 :gp))))
    (:ssr (assert (matching-types :sh3 :sh4 :sh4a :privileged) ()
                  "LDC Rm,SSR incompatible with this architecture type.")
     (masque "0100MMMM.00111110" ;; ldc Rm,SSR
             (m (rix op0 :gp))))
    (:spc (assert (matching-types :sh3 :sh4 :sh4a :privileged) ()
                  "LDC Rm,SPC incompatible with this architecture type.")
     (masque "0100MMMM.01001110" ;; ldc Rm,SPC
             (m (rix op0 :gp))))
    (:dbr (assert (matching-types      :sh4 :sh4a :privileged) ()
                  "LDC Rm,DBR incompatible with this architecture type.")
     (masque "0100MMMM.11111010" ;; ldc Rm,DBR
             (m (rix op0 :gp))))
    (t (if (typep op1 'gprb)
           (progn (assert (matching-types :sh3 :sh4 :sh4a :privileged) ()
                          "LDC Rm,Rn_BANK is incompatible with this architecture type.")
                  (masque "0100MMMM.1NNN1110" ;; ldc Rm,Rn_BANK
                          (m (rix op0 :gp)) (n (rix op1 :gb))))
           (error "Invalid operands for LDC.")))))

(specops-sh ldc.l (op0 op1)
  ((:type-matcher . matching-types)
   (:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :dsp :privileged))
  (case op1
    (:sr
     (masque "0100MMMM.00000111" ;; ldc.l @Rm+,SR
             (m (rix (mas-base op1) :gp))))
    (:gbr (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                  "LDC.L @Rm+,GBR incompatible with this architecture type.")
     (masque "0100MMMM.00010111" ;; ldc.l @Rm+,GBR
             (m (rix (mas-base op1) :gp))))
    (:vbr
     (masque "0100MMMM.00100111" ;; ldc.l @Rm+,VBR
             (m (rix (mas-base op1) :gp))))
    (:mod (assert (matching-types :dsp) ()
                  "LDC.L @Rm+,MOD incompatible with this architecture type.")
     (masque "0100MMMM.01010111" ;; ldc.l @Rm+,MOD
             (m (rix (mas-base op1) :gp))))
    (:re  (assert (matching-types :dsp) ()
                  "LDC.L @Rm+,RE incompatible with this architecture type.")
     (masque "0100MMMM.01110111" ;; ldc.l @Rm+,RE
             (m (rix (mas-base op1) :gp))))
    (:rs  (assert (matching-types :dsp) ()
                  "LDC.L @Rm+,RS incompatible with this architecture type.")
     (masque "0100MMMM.01100111" ;; ldc.l @Rm+,RS
             (m (rix (mas-base op1) :gp))))
    (:sgr (assert (matching-types           :sh4a :privileged) ()
                  "LDC.L @Rm+,SGR incompatible with this architecture type.")
     (masque "0100MMMM.00110110" ;; ldc.l @Rm+,SGR
             (m (rix (mas-base op1) :gp))))
    (:ssr (assert (matching-types :sh3 :sh4 :sh4a :privileged) ()
                  "LDC.L @Rm+,SSR incompatible with this architecture type.")
     (masque "0100MMMM.00110111" ;; ldc.l @Rm+,SSR
             (m (rix (mas-base op1) :gp))))
    (:spc (assert (matching-types      :sh4 :sh4a :privileged) ()
                  "LDC.L @Rm+,SPC incompatible with this architecture type.")
     (masque "0100MMMM.01000111" ;; ldc.l @Rm+,SPC
             (m (rix (mas-base op1) :gp))))
    (:dbr (assert (matching-types      :sh4 :sh4a :privileged) ()
                  "LDC.L @Rm+,DBR incompatible with this architecture type.")
     (masque "0100MMMM.11110110" ;; ldc.l @Rm+,DBR
             (m (rix (mas-base op1) :gp))))
    (t (if (typep op1 'gprb)
           (progn (assert (matching-types :sh3 :sh4 :sh4a :privileged) ()
                          "LDC @Rm+,Rn_BANK is incompatible with this architecture type.")
                  (masque "0100MMMM.1NNN0111" ;; ldc.l @Rm+,Rn_BANK
                          (m (mas-base op0)) (n (rix op1 :rb))))
           (error "Invalid operands for LDC.")))))
    
(readops-sh ldc.sr (word read) ;; ldc Rm,SR
  (unmasque "0100MMMM.00001110" word (m)
    (list :ldc (drv-gpr m) :sr)))

(readops-sh ldc.l.sr (word read) ;; ldc.l @Rm+,SR
  (unmasque "0100MMMM.00000111" word (m)
    (list :ldc.l (list '@+ (drv-gpr m)) :sr)))

(readops-sh ldc.tbr (word read) ;; ldc Rm,TBR
  (unmasque "0100MMMM.01001010" word (m)
    (list :ldc (drv-gpr m) :tbr)))

(readops-sh ldc.gbr (word read) ;; ldc Rm,GBR
  (unmasque "0100MMMM.00011110" word (m)
    (list :ldc (drv-gpr m) :gbr)))

(readops-sh ldc.l.gbr (word read) ;; ldc.l @Rm+,GBR
  (unmasque "0100MMMM.00010111" word (m)
    (list :ldc.l (list '@+ (drv-gpr m)) :gbr)))

(readops-sh ldc.vbr (word read) ;; ldc Rm,VBR
  (unmasque "0100MMMM.00101110" word (m)
    (list :ldc (drv-gpr m) :vbr)))

(readops-sh ldc.l.vbr (word read) ;; ldc.l @Rm+,VBR
  (unmasque "0100MMMM.00100111" word (m)
    (list :ldc.l (list '@+ (drv-gpr m)) :vbr)))

(readops-sh ldc.mod (word read) ;; ldc Rm,MOD
  (unmasque "0100MMMM.01011110" word (m)
    (list :ldc (drv-gpr m) :mod)))

(readops-sh ldc.l.mod (word read) ;; ldc.l @Rm+,MOD
  (unmasque "0100MMMM.01010111" word (m)
    (list :ldc.l (list '@+ (drv-gpr m)) :mod)))

(readops-sh ldc.re (word read) ;; ldc Rm,RE
  (unmasque "0100MMMM.01111110" word (m)
    (list :ldc (drv-gpr m) :re)))

(readops-sh ldc.l.re (word read) ;; ldc.l @Rm+,RE
  (unmasque "0100MMMM.01110111" word (m)
    (list :ldc.l (list '@+ (drv-gpr m)) :re)))

(readops-sh ldc.rs (word read) ;; ldc Rm,RS
  (unmasque "0100MMMM.01101110" word (m)
    (list :ldc (drv-gpr m) :rs)))

(readops-sh ldc.l.rs (word read) ;; ldc.l @Rm+,RS
  (unmasque "0100MMMM.01100111" word (m)
    (list :ldc.l (list '@+ (drv-gpr m)) :rs)))

(readops-sh ldc.sgr (word read) ;; ldc Rm,SGR
  (unmasque "0100MMMM.00111010" word (m)
    (list :ldc (drv-gpr m) :sgr)))

(readops-sh ldc.l.sgr (word read) ;; ldc.l @Rm+,SGR
  (unmasque "0100MMMM.00110110" word (m)
    (list :ldc.l (list '@+ (drv-gpr m)) :sgr)))

(readops-sh ldc.ssr (word read) ;; ldc Rm,SSR
  (unmasque "0100MMMM.00111110" word (m)
    (list :ldc (drv-gpr m) :ssr)))

(readops-sh ldc.l.ssr (word read) ;; ldc.l @Rm+,SSR
  (unmasque "0100MMMM.00110111" word (m)
    (list :ldc.l (list '@+ (drv-gpr m)) :ssr)))

(readops-sh ldc.spc (word read) ;; ldc Rm,SPC
  (unmasque "0100MMMM.01001110" word (m)
    (list :ldc (drv-gpr m) :spc)))

(readops-sh ldc.l.spc (word read) ;; ldc.l @Rm+,SPC
  (unmasque "0100MMMM.01000111" word (m)
    (list :ldc.l (list '@+ (drv-gpr m)) :spc)))

(readops-sh ldc.dbr (word read) ;; ldc Rm,DBR
  (unmasque "0100MMMM.11111010" word (m)
    (list :ldc (drv-gpr m) :dbr)))

(readops-sh ldc.l.dbr (word read) ;; ldc.l @Rm+,DBR
  (unmasque "0100MMMM.11110110" word (m)
    (list :ldc.l (list '@+ (drv-gpr m)) :dbr)))

(readops-sh ldc.bank (word read) ;; ldc Rm,Rn_BANK
  (unmasque "0100MMMM.1NNN1110" word (m n)
    (list :ldc (drv-gpr m) (drv-gprb n))))

(readops-sh ldc.l.bank (word read) ;; ldc.l @Rm+,Rn_BANK
  (unmasque "0100MMMM.1NNN0111" word (m n)
    (list :ldc.l (list '@+ (drv-gpr m)) (drv-gprb n))))

(specops-sh ldre (op0)
  ((:for-types :dsp))
  (assert (match-types op0  mas-pc+disp) ()
          "LDRE takes only a PC+displacement memory access as its operand.")
  (masque "10001110.DDDDDDDD" ;; ldre @(disp,PC)
          (d (mas-displ op0))))

(readops-sh ldre (word read)
  (unmasque "10001110.DDDDDDDD" word (d)
    (list :ldre (list '@pc d))))

(specops-sh ldrs (op0)
  ((:for-types :dsp))
  (assert (match-types op0  mas-pc+disp) ()
          "LDRE takes only a PC+displacement memory access as its operand.")
  (masque "10001100.DDDDDDDD" ;; ldrs @(disp,PC)
          (d (mas-displ op0))))

(readops-sh ldrs (word read)
  (unmasque "10001100.DDDDDDDD" word (d)
    (list :ldrs (list '@pc d))))

(specops-sh lds (op0 op1)
    ((:type-matcher . matching-types)
     (:for-types :sh1 :sh2 :sh3 :sh2e :sh3e :sh4 :sh4a :sh2a :dsp :privileged))
  (case op1
    (:mach  (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                    "LDS Rm,MACH incompatible with this architecture type.")
     (masque "0100MMMM.00001010" ;; lds Rm,MACH
             (m (rix op0 :gp))))
    (:macl  (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                    "LDS Rm,MACL incompatible with this architecture type.")
     (masque "0100MMMM.00011010" ;; lds Rm,MACL
             (m (rix op0 :gp))))
    (:pr    (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                    "LDS Rm,PR incompatible with this architecture type.")
     (masque "0100MMMM.00101010" ;; lds Rm,PR
             (m (rix op0 :gp))))

    ;; floating point instructions

    (:fpscr (assert (matching-types :sh2e :sh3e :sh4 :sh4a :sh2a) ()
                    "LDS Rm,FPSCR incompatible with this architecture type.")
     (masque "0100MMMM.01101010" ;; lds Rm,FPSCR
             (m (rix op0 :gp))))
    (:fpul  (assert (matching-types :sh2e :sh3e :sh4 :sh4a :sh2a) ()
                    "LDS Rm,FPUL incompatible with this architecture type.")
     (masque "0100MMMM.01011010" ;; lds Rm,FPUL
             (m (rix op0 :gp))))

    ;; DSP instructions
    
    (:dsr   (assert (matching-types :dsp) ()
                    "LDS Rm,DSR incompatible with this architecture type.")
     (masque "0100MMMM.01101010" ;; lds Rm,DSR
             (m (rix op0 :gp))))
    (:a0    (assert (matching-types :dsp) ()
                    "LDS Rm,A0 incompatible with this architecture type.")
     (masque "0100MMMM.01110110" ;; lds Rm,A0
             (m (rix op0 :gp))))
    (:x0    (assert (matching-types :dsp) ()
                    "LDS Rm,X0 incompatible with this architecture type.")
     (masque "0100MMMM.10001010" ;; lds Rm,X0
             (m (rix op0 :gp))))
    (:x1    (assert (matching-types :dsp) ()
                    "LDS Rm,X1 incompatible with this architecture type.")
     (masque "0100MMMM.10011010" ;; lds Rm,X1
             (m (rix op0 :gp))))
    (:y0    (assert (matching-types :dsp) ()
                    "LDS Rm,Y0 incompatible with this architecture type.")
     (masque "0100MMMM.10101010" ;; lds Rm,Y0
             (m (rix op0 :gp))))
    (:y1    (assert (matching-types :dsp) ()
                    "LDS Rm,Y1 incompatible with this architecture type.")
     (masque "0100MMMM.10111010" ;; lds Rm,Y1
             (m (rix op0 :gp))))))

(specops-sh lds.l (op0 op1)
    ((:type-matcher . matching-types)
     (:for-types :sh1 :sh2 :sh3 :sh2e :sh3e :sh4 :sh4a :sh2a :dsp :privileged))
  (case op1
    (:mach  (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                    "LDS.L @Rm+,MACH incompatible with this architecture type.")
     (masque "0100MMMM.00000110" ;; lds.l @Rm+,MACH
             (m (rix (mas-base op0) :gp))))
    (:macl  (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                    "LDS.L @Rm+,MACL incompatible with this architecture type.")
     (masque "0100MMMM.00010110" ;; lds.l @Rm+,MACL
             (m (rix (mas-base op0) :gp))))
    (:pr    (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                    "LDS.L @Rm+,PR incompatible with this architecture type.")
     (masque "0100MMMM.00100110" ;; lds.l @Rm+,PR
             (m (rix (mas-base op0) :gp))))

    ;; floating point instructions

    (:fpscr (assert (matching-types :sh2e :sh3e :sh4 :sh4a :sh2a) ()
                    "LDS.L @Rm+,FPSCR incompatible with this architecture type.")
     (masque "0100MMMM.01100110" ;; lds.l @Rm+,FPSCR
             (m (rix (mas-base op0) :gp))))
    (:fpul  (assert (matching-types :sh2e :sh3e :sh4 :sh4a :sh2a) ()
                    "LDS.L @Rm+,FPUL incompatible with this architecture type.")
     (masque "0100MMMM.01010110" ;; lds.l @Rm+,FPUL
             (m (rix (mas-base op0) :gp))))

    ;; DSP instructions
    
    (:dsr   (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                    "LDS.L @Rm+,DSR incompatible with this architecture type.")
     (masque "0100MMMM.01100110" ;; lds.l @Rm+,DSR
             (m (rix (mas-base op0) :gp))))
    (:a0    (assert (matching-types :dsp) ()
                    "LDS.L @Rm+,A0 incompatible with this architecture type.")
     (masque "0100MMMM.01100110" ;; lds.l @Rm+,A0
             (m (rix (mas-base op0) :gp))))
    (:x0    (assert (matching-types :dsp) ()
                    "LDS.L @Rm+,X0 incompatible with this architecture type.")
     (masque "0100MMMM.10000110" ;; lds.l @Rm+,X0
             (m (rix (mas-base op0) :gp))))
    (:x1    (assert (matching-types :dsp) ()
                    "LDS.L @Rm+,X1 incompatible with this architecture type.")
     (masque "0100MMMM.10010110" ;; lds.l @Rm+,X1
             (m (rix (mas-base op0) :gp))))
    (:y0    (assert (matching-types :dsp) ()
                    "LDS.L @Rm+,Y0 incompatible with this architecture type.")
     (masque "0100MMMM.10100110" ;; lds.l @Rm+,Y0
             (m (rix (mas-base op0) :gp))))
    (:y1    (assert (matching-types :dsp) ()
                    "LDS.L @Rm+,Y1 incompatible with this architecture type.")
     (masque "0100MMMM.10110110" ;; lds.l @Rm+,Y1
             (m (rix (mas-base op0) :gp))))))

(readops-sh lds.mach (word read) ;; lds Rm,MACH
  (unmasque "0100MMMM.00001010" word (m)
    (list :lds (drv-gpr m) :mach)))

(readops-sh lds.l.mach (word read) ;; lds.l @Rm+,MACH
  (unmasque "0100MMMM.00000110" word (m)
    (list :lds.l (list '@+ (drv-gpr m)) :mach)))

(readops-sh lds.macl (word read) ;; lds Rm,MACL
  (unmasque "0100MMMM.00011010" word (m)
    (list :lds (drv-gpr m) :macl)))

(readops-sh lds.l.macl (word read) ;; lds.l @Rm+,MACL
  (unmasque "0100MMMM.00010110" word (m)
    (list :lds.l (list '@+ (drv-gpr m)) :macl)))

(readops-sh lds.pr (word read) ;; lds Rm,PR
  (unmasque "0100MMMM.00101010" word (m)
    (list :lds (drv-gpr m) :pr)))

(readops-sh lds.l.pr (word read) ;; lds.l @Rm+,PR
  (unmasque "0100MMMM.00100110" word (m)
    (list :lds.l (list '@+ (drv-gpr m)) :pr)))

(readops-sh lds.dsr (word read) ;; lds Rm,DSR
  (unmasque "0100MMMM.01101010" word (m)
    (list :lds (drv-gpr m) :dsr)))

(readops-sh lds.l.dsr (word read) ;; lds.l @Rm+,DSR
  (unmasque "0100MMMM.01100110" word (m)
    (list :lds.l (list '@+ (drv-gpr m)) :dsr)))

(readops-sh lds.a0 (word read) ;; lds Rm,A0
  (unmasque "0100MMMM.01110110" word (m)
    (list :lds (drv-gpr m) :a0)))

(readops-sh lds.l.a0 (word read) ;; lds.l @Rm+,A0
  (unmasque "0100MMMM.01110110" word (m)
    (list :lds.l (list '@+ (drv-gpr m)) :a0)))

(readops-sh lds.x0 (word read) ;; lds Rm,X0
  (unmasque "0100MMMM.10001010" word (m)
    (list :lds (drv-gpr m) :x0)))

(readops-sh lds.l.x0 (word read) ;; lds.l @Rm+,X0
  (unmasque "0100MMMM.10000110" word (m)
    (list :lds.l (list '@+ (drv-gpr m)) :x0)))

(readops-sh lds.x1 (word read) ;; lds Rm,X1
  (unmasque "0100MMMM.10011010" word (m)
    (list :lds (drv-gpr m) :x1)))

(readops-sh lds.l.x1 (word read) ;; lds.l @Rm+,X1
  (unmasque "0100MMMM.10010110" word (m)
    (list :lds.l (list '@+ (drv-gpr m)) :x1)))

(readops-sh lds.y0 (word read) ;; lds Rm,Y0
  (unmasque "0100MMMM.10101010" word (m)
    (list :lds (drv-gpr m) :y0)))

(readops-sh lds.l.y0 (word read) ;; lds.l @Rm+,Y0
  (unmasque "0100MMMM.10100110" word (m)
    (list :lds.l (list '@+ (drv-gpr m)) :y0)))

(readops-sh lds.y1 (word read) ;; lds Rm,Y1
  (unmasque "0100MMMM.10111010" word (m)
    (list :lds (drv-gpr m) :y1)))

(readops-sh lds.l.y1 (word read) ;; lds.l @Rm+,Y1
  (unmasque "0100MMMM.10110110" word (m)
    (list :lds.l (list '@+ (drv-gpr m)) :y1)))

(specops-sh ldtlb ()
  ((:for-types :sh3 :sh4 :sh4a :privileged))
  (masque "00000000.00111000")) ;; ldtlb

(readops-sh ldtlb (word read)
  (unmasque "00000000.00111000" word ()
    (list :ldtlb )))

(specops-sh movca.l (op0 op1)
    ((:for-types :sh4 :sh4a))
  (assert (and (eq :r0 op0) (typep op1 'mas-simple)) ()
          "MOVCA.L takes only R0 and a simple memory address as its operands.")
  (masque "0000NNNN.11000011" ;; movca.l R0,@Rn
          (n (rix (mas-base op1) :gp))))

(readops-sh movca.l (word read) ;; movca.l R0,@Rn
  (unmasque "0000NNNN.11000011" word (n)
    (list :movca.l :r0 (list '@ (drv-gpr n)))))

(specops-sh nop ()
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "00000000.00001001")) ;; nop

(readops-sh (masque "00000000.00001001") (read)
  (list :nop))

(specops-sh ocbi (op0)
  ((:for-types :sh4 :sh4a))
  (assert (match-types op0  mas-simple) ()
          "OCBI takes only a simple memory address as its operand.")
  (masque "0000NNNN.10010011" ;; ocbi @Rn
          (n (rix (mas-base op0) :gp))))

(readops-sh ocbi (word read) ;; ocbi @Rn
  (unmasque "0000NNNN.10010011" word (n)
    (list :ocbi (list '@ (drv-gpr n)))))

(specops-sh ocbp (op0)
  ((:for-types :sh4 :sh4a))
  (assert (match-types op0  mas-simple) ()
          "OCBI takes only a simple memory address as its operand.")
  (masque "0000NNNN.10100011" ;; ocbp @Rn
          (n (rix (mas-base op0) :gp))))

(readops-sh ocbp (word read)
  (unmasque "0000NNNN.10100011" word (n)
    (list :ocbp (list '@ (drv-gpr n)))))

(specops-sh ocbwb (op0)
  ((:for-types :sh4 :sh4a))
  (assert (match-types op0  mas-simple) ()
          "OCBWB takes only a simple memory address as its operand.")
  (masque "0000NNNN.10110011" ;; ocbwb @Rn
          (n (rix (mas-base op0) :gp))))

(readops-sh ocbwb (word read) ;; ocbwb @Rn
  (unmasque "0000NNNN.10110011" word (n)
    (list :ocbwb (list '@ (drv-gpr n)))))

(specops-sh pref (op0)
  ((:for-types :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  mas-simple) ()
          "PREF takes only a simple memory address as its operand.")
  (masque "0000NNNN.10000011" ;; pref @Rn
          (n (rix (mas-base op0) :gp))))

(readops-sh pref (word read)
  (unmasque "0000NNNN.10000011" word (n)
    (list :pref (list '@ (drv-gpr n)))))

(specops-sh prefi (op0)
  ((:for-types :sh4a))
  (assert (match-types op0  mas-simple) ()
          "PREFI takes only a simple memory address as its operand.")
  (masque "0000NNNN.11010011" ;; prefi @Rn
          (n (rix (mas-base op0) :gp))))

(readops-sh prefi (word read)
  (unmasque "0000NNNN.11010011" word (n)
    (list :prefi (list '@ (drv-gpr n)))))

(specops-sh resbank ()
  ((:for-types :sh2a))
  (masque "00000000.01011011")) ;; resbank

(readops-sh (masque "00000000.01011011") (read)
  (list :resbank))

(specops-sh rte ()
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :privileged))
  (masque "00000000.00101011")) ;; rte

(readops-sh (masque "00000000.00101011") (read)
  (list :rte))

(specops-sh setrc (op0)
  ((:for-types :dsp))
  (typecase op0
    (gpr     (masque "0100NNNN.00010100" ;; setrc Rn
                     (n (rix op0 :gp))))
    (integer (masque "10000010.IIIIIIII" ;; setrc #imm
                     (i op0)))
    (t (error "SETRC takes either a general-purpose register or 8-bit immediate value as its operand."))))
  
(readops-sh setrc-r (word read)
  (unmasque "0100NNNN.00010100" word (n)
    (list :setrc (drv-gpr n))))

(readops-sh setrc-i (word read)
  (unmasque "10000010.IIIIIIII" word (i)
    (list :setrc i)))

(specops-sh sets ()
  ((:for-types :sh3 :sh4 :sh4a))
  (masque "00000000.01011000")) ;; sets

(readops-sh (masque "00000000.01011000") (read)
  (list :sets))

(specops-sh sett ()
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "00000000.00011000")) ;; sett

(readops-sh (masque "00000000.00011000") (read)
  (list :sett))

(specops-sh sleep ()
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :privileged))
  (masque "00000000.00011011")) ;; sleep

(readops-sh (masque "00000000.00011011") (read)
  (list :sleep))

(specops-sh stbank (op0 op1)
  ((:for-types :sh2a))
  (assert (and (eq :r0 op0) (typep op1 'mas-simple)) ()
          "STBANK takes only R0 and a simple memory address as its operands.")
  (masque "0100NNNN.11100001" ;; stbank R0,@Rn
          (n (rix (mas-base op1) :gp))))

(readops-sh stbank (word read) ;; stbank R0,@Rn
  (unmasque "0100NNNN.11100001" word (n)
    (list :stbank :r0 (list '@ (drv-gpr n)))))

(specops-sh stc (op0 op1)
    ((:type-matcher . matching-types)
     (:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :dsp :privileged))
  (case op0
    (:sr
     (masque "0000NNNN.00000010" ;; stc SR,Rn
             (n (rix op1 :gp))))
    (:tbr (assert (matching-types                           :sh2a) ()
                  "STC Rm,TBR incompatible with this architecture type.")
     (masque "0000NNNN.01001010" ;; stc TBR,Rn
             (n (rix op1 :gp))))
    (:gbr (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                  "STC Rm,GBR incompatible with this architecture type.")
     (masque "0000NNNN.00010010" ;; stc GBR,Rn
             (n (rix op1 :gp))))
    (:vbr
     (masque "0000NNNN.00100010" ;; stc VBR,Rn
             (n (rix op1 :gp))))
    (:mod (assert (matching-types :dsp) ()
                  "STC Rm,MOD incompatible with this architecture type.")
     (masque "0000NNNN.01010010" ;; stc MOD,Rn
             (n (rix op1 :gp))))
    (:re  (assert (matching-types :dsp) ()
                  "STC Rm,RE incompatible with this architecture type.")
     (masque "0000NNNN.01110010" ;; stc RE,Rn
             (n (rix op1 :gp))))
    (:rs  (assert (matching-types :dsp) ()
                  "STC Rm,RS incompatible with this architecture type.")
     (masque "0000NNNN.01100010" ;; stc RS,Rn
             (n (rix op1 :gp))))
    (:sgr (assert (matching-types           :sh4a :privileged) ()
                  "STC Rm,SGR incompatible with this architecture type.")
     (masque "0000NNNN.00111010" ;; stc SGR,Rn
             (n (rix op1 :gp))))
    (:ssr (assert (matching-types :sh3 :sh4 :sh4a :privileged) ()
                  "STC Rm,SSR incompatible with this architecture type.")
     (masque "0000NNNN.00110010" ;; stc SSR,Rn
             (n (rix op1 :gp))))
    (:spc (assert (matching-types :sh3 :sh4 :sh4a :privileged) ()
                  "STC Rm,SPC incompatible with this architecture type.")
     (masque "0000NNNN.01000010" ;; stc SPC,Rn
             (n (rix op1 :gp))))
    (:dbr (assert (matching-types      :sh4 :sh4a :privileged) ()
                  "STC Rm,DBR incompatible with this architecture type.")
     (masque "0000NNNN.11111010" ;; stc DBR,Rn
             (n (rix op1 :gp))))
    (t (if (typep op1 'gprb)
           (progn (assert (matching-types :sh3 :sh4 :sh4a :privileged) ()
                          "STC Rm,Rn_BANK is incompatible with this architecture type.")
                  (masque "0000NNNN.1MMM0010" ;; stc Rm_BANK,Rn
                          (n (rix op1 :gp)) (m (rix op0 :rb))))
           (error "Invalid operands for STC.")))))

(specops-sh stc.l (op0 op1)
    ((:type-matcher . matching-types)
     (:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :dsp :privileged))
  (case op0
    (:sr
     (masque "0100NNNN.00000011" ;; stc.l SR,@-Rn
             (n (rix (mas-base op1) :gp))))
    (:gbr (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                  "STC.L @Rm+,GBR incompatible with this architecture type.")
     (masque "0100NNNN.00010011" ;; stc.l GBR,@-Rn
             (n (rix (mas-base op1) :gp))))
    (:vbr
     (masque "0100NNNN.00100011" ;; stc.l VBR,@-Rn
             (n (rix (mas-base op1) :gp))))
    (:mod (assert (matching-types :dsp) ()
                  "STC.L @Rm+,MOD incompatible with this architecture type.")
     (masque "0100NNNN.01010011" ;; stc.l MOD,@-Rn
             (n (rix (mas-base op1) :gp))))
    (:re  (assert (matching-types :dsp) ()
                  "STC.L @Rm+,RE incompatible with this architecture type.")
     (masque "0100NNNN.01110011" ;; stc.l RE,@-Rn
             (n (rix (mas-base op1) :gp))))
    (:rs  (assert (matching-types :dsp) ()
                  "STC.L @Rm+,RS incompatible with this architecture type.")
     (masque "0100NNNN.01100011" ;; stc.l RS,@-Rn
             (n (rix (mas-base op1) :gp))))
    (:sgr (assert (matching-types           :sh4a :privileged) ()
                  "STC.L @Rm+,SGR incompatible with this architecture type.")
     (masque "0100NNNN.00110010" ;; stc.l SGR,@-Rn
             (n (rix (mas-base op1) :gp))))
    (:ssr (assert (matching-types :sh3 :sh4 :sh4a :privileged) ()
                  "STC.L @Rm+,SSR incompatible with this architecture type.")
     (masque "0100NNNN.00110011" ;; stc.l SSR,@-Rn
             (n (rix (mas-base op1) :gp))))
    (:spc (assert (matching-types      :sh4 :sh4a :privileged) ()
                  "STC.L @Rm+,SPC incompatible with this architecture type.")
     (masque "0100NNNN.01000011" ;; stc.l SPC,@-Rn
             (n (rix (mas-base op1) :gp))))
    (:dbr (assert (matching-types      :sh4 :sh4a :privileged) ()
                  "STC.L @Rm+,DBR incompatible with this architecture type.")
     (masque "0100NNNN.11110010" ;; stc.l DBR,@-Rn
             (n (rix (mas-base op1) :gp))))
    (t (if (gprb-p op0)
           (progn (assert (matching-types :sh3 :sh4 :sh4a :privileged) ()
                          "STC.L @Rm+,Rn_BANK is incompatible with this architecture type.")
                  (masque "0100NNNN.1MMM0011" ;; stc.l Rm_BANK,@-Rn
                          (n (rix (mas-base op1) :gp)) (m (rix op0 :rb))))
           (error "Invalid operands for STC.L.")))))

(readops-sh stc.sr (word read) ;; stc SR,Rn
  (unmasque "0000NNNN.00000010" word (n)
    (list :stc :sr (drv-gpr n))))

(readops-sh stc.l.sr (word read) ;; stc.l SR,@-Rn
  (unmasque "0100NNNN.00000011" word (n)
    (list :stc.l :sr (list '-@ (drv-gpr n)))))

(readops-sh stc.tbr (word read) ;; stc TBR,Rn
  (unmasque "0000NNNN.01001010" word (n)
    (list :stc :tbr (drv-gpr n))))

(readops-sh stc.gbr (word read) ;; stc GBR,Rn
  (unmasque "0000NNNN.00010010" word (n)
    (list :stc :gbr (drv-gpr n))))

(readops-sh stc.l.gbr (word read) ;; stc.l GBR,@-Rn
  (unmasque "0100NNNN.00010011" word (n)
    (list :stc.l :gbr (list '-@ (drv-gpr n)))))

(readops-sh stc.vbr (word read) ;; stc VBR,Rn
  (unmasque "0000NNNN.00100010" word (n)
    (list :stc :vbr (drv-gpr n))))

(readops-sh stc.l.vbr (word read) ;; stc.l VBR,@-Rn
  (unmasque "0100NNNN.00100011" word (n)
    (list :stc.l :vbr (list '-@ (drv-gpr n)))))

(readops-sh stc.mod (word read) ;; stc MOD,Rn
  (unmasque "0000NNNN.01010010" word (n)
    (list :stc :mod (drv-gpr n))))

(readops-sh stc.l.mod (word read) ;; stc.l MOD,@-Rn
  (unmasque "0100NNNN.01010011" word (n)
    (list :stc.l :mod (list '-@ (drv-gpr n)))))

(readops-sh stc.re (word read) ;; stc RE,Rn
  (unmasque "0000NNNN.01110010" word (n)
    (list :stc :re (drv-gpr n))))

(readops-sh stc.l.re (word read) ;; stc.l RE,@-Rn
  (unmasque "0100NNNN.01110011" word (n)
    (list :stc.l :re (list '-@ (drv-gpr n)))))

(readops-sh stc.rs (word read) ;; stc RS,Rn
  (unmasque "0000NNNN.01100010" word (n)
    (list :stc :rs (drv-gpr n))))

(readops-sh stc.l.rs (word read) ;; stc.l RS,@-Rn
  (unmasque "0100NNNN.01100011" word (n)
    (list :stc.l :rs (list '-@ (drv-gpr n)))))

(readops-sh stc.sgr (word read) ;; stc SGR,Rn
  (unmasque "0000NNNN.00111010" word (n)
    (list :stc :sgr (drv-gpr n))))

(readops-sh stc.l.sgr (word read) ;; stc.l SGR,@-Rn
  (unmasque "0100NNNN.00110010" word (n)
    (list :stc.l :sgr (list '-@ (drv-gpr n)))))

(readops-sh stc.ssr (word read) ;; stc SSR,Rn
  (unmasque "0000NNNN.00110010" word (n)
    (list :stc :ssr (drv-gpr n))))

(readops-sh stc.l.ssr (word read) ;; stc.l SSR,@-Rn
  (unmasque "0100NNNN.00110011" word (n)
    (list :stc.l :ssr (list '-@ (drv-gpr n)))))

(readops-sh stc.spc (word read) ;; stc SPC,Rn
  (unmasque "0000NNNN.01000010" word (n)
    (list :stc :spc (drv-gpr n))))

(readops-sh stc.l.spc (word read) ;; stc.l SPC,@-Rn
  (unmasque "0100NNNN.01000011" word (n)
    (list :stc.l :spc (list '-@ (drv-gpr n)))))

(readops-sh stc.dbr (word read) ;; stc DBR,Rn
  (unmasque "0000NNNN.11111010" word (n)
    (list :stc :dbr (drv-gpr n))))

(readops-sh stc.l.dbr (word read) ;; stc.l DBR,@-Rn
  (unmasque "0100NNNN.11110010" word (n)
    (list :stc.l :dbr (list '-@ (drv-gpr n)))))

(readops-sh stc.bank (word read) ;; stc Rm_BANK,Rn
  (unmasque "0000NNNN.1MMM0010" word (n m)
    (list :stc (drv-gprb m) (drv-gpr n))))

(readops-sh stc.l.bank (word read) ;; stc.l Rm_BANK,@-Rn
  (unmasque "0100NNNN.1MMM0011" word (n m)
    (list :stc.l (drv-gprb m) (list '-@ (drv-gpr n)))))

(specops-sh sts (op0 op1)
    ((:type-matcher . matching-types)
     (:for-types :sh1 :sh2 :sh3 :sh2e :sh3e :sh4 :sh4a :sh2a :dsp :privileged))
  (case op0
    (:mach  (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                    "STS Rm,MACH incompatible with this architecture type.")
     (masque "0000NNNN.00001010" ;; sts MACH,Rn
             (n (rix op1 :gp))))
    (:macl  (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                    "STS Rm,MACL incompatible with this architecture type.")
     (masque "0000NNNN.00011010" ;; sts MACL,Rn
             (n (rix op1 :gp))))
    (:pr    (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                    "STS Rm,PR incompatible with this architecture type.")
     (masque "0000NNNN.00101010" ;; sts PR,Rn
             (n (rix op1 :gp))))

    ;; floating point instructions

    (:fpscr (assert (matching-types :sh2e :sh3e :sh4 :sh4a :sh2a) ()
                    "STS FPSCR,Rn incompatible with this architecture type.")
     (masque "0000NNNN.01101010" ;; sts FPSCR,Rn
             (n (rix op1 :gp))))
    (:fpul  (assert (matching-types :sh2e :sh3e :sh4 :sh4a :sh2a) ()
                    "STS FPUL,Rn incompatible with this architecture type.")
     (masque "0000NNNN.01011010" ;; sts FPUL,Rn
             (n (rix op1 :gp))))

    ;; DSP instructions
    
    (:dsr   (assert (matching-types :dsp) ()
                    "STS Rm,DSR incompatible with this architecture type.")
     (masque "0000NNNN.01101010" ;; sts DSR,Rn
             (n (rix op1 :gp))))
    (:a0    (assert (matching-types :dsp) ()
                    "STS Rm,A0 incompatible with this architecture type.")
     (masque "0000NNNN.01111010" ;; sts A0,Rn
             (n (rix op1 :gp))))
    (:x0    (assert (matching-types :dsp) ()
                    "STS Rm,X0 incompatible with this architecture type.")
     (masque "0000NNNN.10001010" ;; sts X0,Rn
             (n (rix op1 :gp))))
    (:x1    (assert (matching-types :dsp) ()
                    "STS Rm,X1 incompatible with this architecture type.")
     (masque "0000NNNN.10011010" ;; sts X1,Rn
             (n (rix op1 :gp))))
    (:y0    (assert (matching-types :dsp) ()
                    "STS Rm,Y0 incompatible with this architecture type.")
     (masque "0000NNNN.10101010" ;; sts Y0,Rn
             (n (rix op1 :gp))))
    (:y1    (assert (matching-types :dsp) ()
                    "STS Rm,Y1 incompatible with this architecture type.")
     (masque "0000NNNN.10111010" ;; sts Y1,Rn
             (n (rix op1 :gp))))))

(specops-sh sts.l (op0 op1)
    ((:type-matcher . matching-types)
     (:for-types :sh1 :sh2 :sh3 :sh2e :sh3e :sh4 :sh4a :sh2a :dsp :privileged))
  (case op0
    (:mach  (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                    "STS.L @Rm+,MACH incompatible with this architecture type.")
     (masque "0100NNNN.00000010" ;; sts.l MACH,@-Rn
             (n (rix (mas-base op1) :gp))))
    (:macl  (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                    "STS.L @Rm+,MACL incompatible with this architecture type.")
     (masque "0100NNNN.00010010" ;; sts.l MACL,@-Rn
             (n (rix (mas-base op1) :gp))))
    (:pr    (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                    "STS.L @Rm+,PR incompatible with this architecture type.")
     (masque "0100NNNN.00100010" ;; sts.l PR,@-Rn
             (n (rix (mas-base op1) :gp))))

    ;; floating point instructions

    (:fpscr (assert (matching-types :sh2e :sh3e :sh4 :sh4a :sh2a) ()
                    "STS.L FPSCR,@-Rn incompatible with this architecture type.")
     (masque "0100NNNN.01100010" ;; sts.l FPSCR,@-Rn
             (n (rix (mas-base op1) :gp))))
    (:fpul  (assert (matching-types :sh2e :sh3e :sh4 :sh4a :sh2a) ()
                    "STS.L FPUL,@-Rn incompatible with this architecture type.")
     (masque "0100NNNN.01010010" ;; sts.l FPUL,@-Rn
             (n (rix (mas-base op1) :gp))))

    ;; DSP instructions
    
    (:dsr   (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                    "STS.L @Rm+,DSR incompatible with this architecture type.")
     (masque "0100NNNN.01100010" ;; sts.l DSR,@-Rn
             (n (rix (mas-base op1) :gp))))
    (:a0    (assert (matching-types :dsp) ()
                    "STS.L @Rm+,A0 incompatible with this architecture type.")
     (masque "0100NNNN.01100010" ;; sts.l A0,@-Rn
             (n (rix (mas-base op1) :gp))))
    (:x0    (assert (matching-types :dsp) ()
                    "STS.L @Rm+,X0 incompatible with this architecture type.")
     (masque "0100NNNN.10000010" ;; sts.l X0,@-Rn
             (n (rix (mas-base op1) :gp))))
    (:x1    (assert (matching-types :dsp) ()
                    "STS.L @Rm+,X1 incompatible with this architecture type.")
     (masque "0100NNNN.10010010" ;; sts.l X1,@-Rn
             (n (rix (mas-base op1) :gp))))
    (:y0    (assert (matching-types :dsp) ()
                    "STS.L @Rm+,Y0 incompatible with this architecture type.")
     (masque "0100NNNN.10100010" ;; sts.l Y0,@-Rn
             (n (rix (mas-base op1) :gp))))
    (:y1    (assert (matching-types :dsp) ()
                    "STS.L @Rm+,Y1 incompatible with this architecture type.")
     (masque "0100NNNN.10110010" ;; sts.l Y1,@-Rn
             (n (rix (mas-base op1) :gp))))))

(readops-sh sts.mach (word read) ;; sts MACH,Rn
  (unmasque "0000NNNN.00001010" word (n)
    (list :sts :mach (drv-gpr n))))

(readops-sh sts.l.mach (word read) ;; sts.l MACH,@-Rn
  (unmasque "0100NNNN.00000010" word (n)
    (list :sts.l :mach (list '-@ (drv-gpr n)))))

(readops-sh sts.macl (word read) ;; sts MACL,Rn
  (unmasque "0000NNNN.00011010" word (n)
    (list :sts :macl (drv-gpr n))))

(readops-sh sts.l.macl (word read) ;; sts.l MACL,@-Rn
  (unmasque "0100NNNN.00010010" word (n)
    (list :sts.l :macl (list '-@ (drv-gpr n)))))

(readops-sh sts.pr (word read) ;; sts PR,Rn
  (unmasque "0000NNNN.00101010" word (n)
    (list :sts :pr (drv-gpr n))))

(readops-sh sts.l.pr (word read) ;; sts.l PR,@-Rn
  (unmasque "0100NNNN.00100010" word (n)
    (list :sts.l :pr (list '-@ (drv-gpr n)))))

(readops-sh sts.dsr (word read) ;; sts DSR,Rn
  (unmasque "0000NNNN.01101010" word (n)
    (list :sts :dsr (drv-gpr n))))

(readops-sh sts.l.dsr (word read) ;; sts.l DSR,@-Rn
  (unmasque "0100NNNN.01100010" word (n)
    (list :sts.l :dsr (list '-@ (drv-gpr n)))))

(readops-sh sts.a0 (word read) ;; sts A0,Rn
  (unmasque "0000NNNN.01111010" word (n)
    (list :sts :ao (drv-gpr n))))

(readops-sh sts.l.a0 (word read)
  (unmasque "0100NNNN.01100010" word (n)
    (list :sts.l :a0 (list '-@ (drv-gpr n)))))

(readops-sh sts.x0 (word read) ;; sts X0,Rn
  (unmasque "0000NNNN.10001010" word (n)
    (list :sts :xo (drv-gpr n))))

(readops-sh sts.l.x0 (word read) ;; sts.l X0,@-Rn
  (unmasque "0100NNNN.10000010" word (n)
    (list :sts.l :x0 (list '-@ (drv-gpr n)))))

(readops-sh sts.x1 (word read) ;; sts X1,Rn
  (unmasque "0000NNNN.10011010" word (n)
    (list :sts :x1 (drv-gpr n))))

(readops-sh sts.l.x1 (word read) ;; sts.l X1,@-Rn
  (unmasque "0100NNNN.10010010" word (n)
    (list :sts.l :x1 (list '-@ (drv-gpr n)))))

(readops-sh sts.y0 (word read) ;; sts Y0,Rn
  (unmasque "0000NNNN.10101010" word (n)
    (list :sts :y0 (drv-gpr n))))

(readops-sh sts.l.y0 (word read) ;; sts.l Y0,@-Rn
  (unmasque "0100NNNN.10100010" word (n)
    (list :sts.l :y0 (list '-@ (drv-gpr n)))))

(readops-sh sts.y1 (word read) ;; sts Y1,Rn
  (unmasque "0000NNNN.10111010" word (n)
    (list :sts :y1 (drv-gpr n))))

(readops-sh sts.l.y1 (word read) ;; sts.l Y1,@-Rn
  (unmasque "0100NNNN.10110010" word (n)
    (list :sts.l :y1 (list '-@ (drv-gpr n)))))

(specops-sh synco ()
  ((:for-types :sh4a))
  (masque "00000000.10101011")) ;; synco

(readops-sh (masque "00000000.10101011") (read)
  (list :synco))

(specops-sh trapa (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "11000011.IIIIIIII"
          (i op0))) ;; trapa #imm

(readops-sh trapa (word read)
  (unmasque "11000011.IIIIIIII" word (i)
    (list :trapa i)))

;;; *** END SYSTEM FUNCTIONS - BEGIN FP FLOATING POINT FPR

(specops-sh fmov (op0 op1)
    ((:type-matcher . matching-types)
     (:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (cond ((match-types op0 op1  fpr fpr)
         (masque "1111NNNN.MMMM1100" ;; fmov FRm,FRn
                 (n (rix op1 :fp)) (m (rix op0 :fp))))
        ((matching-types :sh4 :sh4a :sh2a)
         (cond ((match-types op0 op1  dpr dpr)
                (masque "1111NNN0.MMM01100" ;; fmov DRm,DRn
                        (n (rix op1 :dp)) (m (rix op0 :dp))))))
        ((matching-types :sh4 :sh4a)
         (cond ((match-types op0 op1  dpr xdr)
                (masque "1111NNN1.MMM01100" ;; fmov DRm,XDn
                        (n (rix op1 :xd)) (m (rix op0 :dp))))
               ((match-types op0 op1  xdr dpr)
                (masque "1111NNN0.MMM11100" ;; fmov XDm,DRn
                        (n (rix op1 :dp)) (m (rix op0 :xd))))
               ((match-types op0 op1  xdr xdr)
                (masque "1111NNN1.MMM11100" ;; fmov XDm,XDn
                        (n (rix op1 :xd)) (m (rix op0 :xd))))))))

(specops-sh fmov.s (op0 op1)
    ((:type-matcher . matching-types)
     (:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (cond ((matching-types :sh2a)
         (cond ((match-types op0 op1  mas-disp12 fpr)
                (masque "0011NNNN.MMMM0001.0111DDDD.DDDDDDDD" ;; fmov.s @(disp12,Rm),FRn
                        (n (rix (mas-base op1) :gp)) (m (rix op0 :gp)) (d (mas-displ op0))))
               ((match-types op0 op1  fpr mas-disp12)
                (masque "0011NNNN.MMMM0001.0011DDDD.DDDDDDDD" ;; fmov.s FRm,@(disp12,Rn)
                        (n (rix (mas-base op1) :gp)) (m (rix op0 :gp)) (d (mas-displ op1))))))
        ((match-types op0 op1  mas-simple fpr)
         (masque "1111NNNN.MMMM1000" ;; fmov.s @Rm,FRn
                 (n (rix op1 :fp)) (m (rix (mas-base op0) :gp))))
        ((match-types op0 op1  fpr mas-simple)
         (masque "1111NNNN.MMMM1010" ;; fmov.s FRm,@Rn
                 (n (rix (mas-base op1) :gp)) (m (rix op0 :fp))))
        ((match-types op0 op1  mas-postinc fpr)
         (masque "1111NNNN.MMMM1001" ;; fmov.s @Rm+,FRn
                 (n (rix op1 :fp)) (m (rix (mas-base op0) :gp))))
        ((match-types op0 op1  fpr mas-simple)
         (masque "1111NNNN.MMMM1011" ;; fmov.s FRm,@-Rn
                 (n (rix (mas-base op1) :gp)) (m (rix op0 :fp))))
        ((match-types op0 op1  mas-b+rzero fpr)
         (masque "1111NNNN.MMMM0110" ;; fmov.s @(R0,Rm),FRn
                 (n (rix op1 :fp)) (m (rix (mas-base op0) :gp))))
        ((match-types op0 op1  fpr mas-b+rzero)
         (masque "1111NNNN.MMMM0111" ;; fmov.s FRm,@(R0,Rn)
                 (n (rix (mas-base op1) :gp)) (m (rix op0 :fp))))
        ((match-types op0 op1  mas-disp12 fpr)
         (masque "0011NNNN.MMMM0001.0111DDDD.DDDDDDDD" ;; fmov.s @(disp12,Rm),FRn
                 (n (rix op1 :fp)) (m (rix (mas-base op0) :gp)) (d (mas-displ op0))))))

(readops-sh fmov.f-f (word read) ;; fmov FRm,FRn
  (unmasque "1111NNNN.MMMM1100" word (n m)
    (list :fmov (drv-fpr m) (drv-fpr n))))

(readops-sh fmov.s.@-f (word read) ;; fmov.s @Rm,FRn
  (unmasque "1111NNNN.MMMM1000" word (n m)
    (list :fmov.s (list '@ (drv-gpr n)) (drv-fpr m))))

(readops-sh fmov.s.f-@ (word read) ;; fmov.s FRm,@Rn
  (unmasque "1111NNNN.MMMM1010" word (n m)
    (list :fmov.s (drv-fpr m) (list '@ (drv-gpr n)))))

(readops-sh fmov.s.+-f (word read) ;; fmov.s @Rm+,FRn
  (unmasque "1111NNNN.MMMM1001" word (n m)
    (list :fmov.s (list '@+ (drv-gpr m)) (drv-fpr n))))

(readops-sh fmov.s.f-- (word read) ;; fmov.s FRm,@-Rn
  (unmasque "1111NNNN.MMMM1011" word (n m)
    (list :fmov.s (drv-fpr m) (list '-@ (drv-gpr n)))))

(readops-sh fmov.s.0r-f (word read) ;; fmov.s @(R0,Rm),FRn
  (unmasque "1111NNNN.MMMM0110" word (n m)
    (list :fmov.s (list '@0 (drv-gpr m)) (drv-fpr n))))

(readops-sh fmov.s.f-0r (word read) ;; fmov.s FRm,@(R0,Rn)
  (unmasque "1111NNNN.MMMM0111" word (n m)
    (list :fmov.s (drv-fpr m) (list '@0 (drv-gpr n)))))

(readops-sh fmov.s.d12r-f (word read) ;; fmov.s @(disp12,Rm),FRn
  (unmasque "0011NNNN.MMMM0001.0111DDDD.DDDDDDDD" word (n m d)
    (list :fmov.s (list '@> (drv-gpr m) d) (drv-fpr n))))

(readops-sh fmov.s.f-d12r (word read) ;; fmov.s FRm,@(disp12,Rn)
  (unmasque "0011NNNN.MMMM0001.0011DDDD.DDDDDDDD" word (n m d)
    (list :fmov.s (drv-fpr m) (list '@> (drv-gpr n) d))))

(readops-sh fmov.d-d (word read) ;; fmov DRm,DRn
  (unmasque "1111NNN0.MMM01100" word (n m)
    (list :fmov (drv-dpr m) (drv-dpr n))))

(readops-sh fmov.d-x (word read) ;; fmov DRm,XDn
  (unmasque "1111NNN1.MMM01100" word (n m)
    (list :fmov (drv-dpr m) (drv-xdr n))))

(readops-sh fmov.x-d (word read) ;; fmov XDm,DRn
  (unmasque "1111NNN0.MMM11100" word (n m)
    (list :fmov (drv-xdr m) (drv-dpr n))))

(readops-sh fmov.x-x (word read) ;; fmov XDm,XDn
  (unmasque "1111NNN1.MMM11100" word (n m)
    (list :fmov (drv-xdr m) (drv-xdr n))))

(specops-sh fmov.d (op0 op1)
    ((:type-matcher . matching-types)
     (:for-types :sh4 :sh4a :sh2a))
  (cond ((matching-types :sh2a)
         (cond ((match-types op0 op1  mas-disp12 dpr)
                (masque "0011NNN0.MMMM0001.0111DDDD.DDDDDDDD" ;; fmov.d @(disp12,Rm),DRn
                        (n (rix op1 :dp)) (m (rix (mas-base op0) :gp)) (d (mas-displ op0))))
               ((match-types op0 op1  dpr mas-disp12)
                (masque "0011NNNN.MMM00001.0011DDDD.DDDDDDDD" ;; fmov.d DRm,@(disp12,Rn)
                        (n (rix (mas-base op1) :gp)) (m (rix op0 :dp)) (d (mas-displ op1))))))
        ((matching-types :sh4 :sh4a)
         (cond ((match-types op0 op1  mas-simple xdr)
                (masque "1111NNN1.MMMM1000" ;; fmov.d @Rm,XDn
                        (n (rix op1 :xd)) (m (rix (mas-base op0) :gp))))
               ((match-types op0 op1  xdr mas-simple)
                (masque "1111NNNN.MMM11010" ;; fmov.d XDm,@Rn
                        (n (rix (mas-base op1) :gp)) (m (rix op0 :xd))))
               ((match-types op0 op1  mas-postinc xdr)
                (masque "1111NNN1.MMMM1001" ;; fmov.d @Rm+,XDn
                        (n (rix op1 :xd)) (m (rix (mas-base op0) :gp))))
               ((match-types op0 op1  xdr mas-predecr)
                (masque "1111NNNN.MMM11011" ;; fmov.d XDm,@-Rn
                        (n (rix (mas-base op1) :gp)) (m (rix op0 :xd))))
               ((match-types op0 op1  mas-b+rzero xdr)
                (masque "1111NNN1.MMMM0110" ;; fmov.d @(R0,Rm),XDn
                        (n (rix op1 :xd)) (m (rix (mas-base op0) :gp))))
               ((match-types op0 op1  xdr mas-b+rzero)
                (masque "1111NNNN.MMM10111" ;; fmov.d XDm,@(R0,Rn)
                        (n (rix (mas-base op1) :gp)) (m (rix op0 :xd))))))
        ((match-types op0 op1  mas-simple dpr)
         (masque "1111NNN0.MMMM1000" ;; fmov.d @Rm,DRn
                 (n (rix op1 :dp)) (m (rix (mas-base op0) :gp))))
        ((match-types op0 op1  dpr mas-simple)
         (masque "1111NNNN.MMM01010" ;; fmov.d DRm,@Rn
                 (n (rix (mas-base op1) :gp)) (m (rix op0 :dp))))
        ((match-types op0 op1  mas-postinc dpr)
         (masque "1111NNN0.MMMM1001" ;; fmov.d @Rm+,DRn
                 (n (rix op1 :dp)) (m (rix (mas-base op0) :gp))))
        ((match-types op0 op1  dpr mas-predecr)
         (masque "1111NNNN.MMM01011" ;; fmov.d DRm,@-Rn
                 (n (rix (mas-base op1) :gp)) (m (rix op0 :dp))))
        ((match-types op0 op1  mas-b+rzero dpr)
         (masque "1111NNN0.MMMM0110" ;; fmov.d @(R0,Rm),DRn
                 (n (rix op1 :dp)) (m (rix (mas-base op0) :gp))))
        ((match-types op0 op1  dpr mas-b+rzero)
         (masque "1111NNNN.MMM00111" ;; fmov.d DRm,@(R0,Rn)
                 (n (rix (mas-base op1) :gp)) (m (rix op0 :dp))))))

(readops-sh fmov.d.@-d (word read) ;; fmov.d @Rm,DRn
  (unmasque "1111NNN0.MMMM1000" word (n m)
    (list :fmov.d (list '@ (drv-gpr m)) (drv-dpr n))))

(readops-sh fmov.d.@-x (word read) ;; fmov.d @Rm,XDn
  (unmasque "1111NNN1.MMMM1000" word (n m)
    (list :fmov.d (list '@ (drv-gpr m)) (drv-xdr n))))

(readops-sh fmov.d.d-@ (word read) ;; fmov.d DRm,@Rn
  (unmasque "1111NNNN.MMM01010" word (n m)
    (list :fmov.d (drv-dpr m) (list '@ (drv-gpr n)))))

(readops-sh fmov.d.x-@ (word read) ;; fmov.d XDm,@Rn
  (unmasque "1111NNNN.MMM11010" word (n m)
    (list :fmov.d (drv-xdr m) (list '@ (drv-gpr n)))))

(readops-sh fmov.d.+-d (word read) ;; fmov.d @Rm+,DRn
  (unmasque "1111NNN0.MMMM1001" word (n m)
    (list :fmov.d (list '@+ (drv-gpr m)) (drv-dpr n))))

(readops-sh fmov.d.+-x (word read) ;; fmov.d @Rm+,XDn
  (unmasque "1111NNN1.MMMM1001" word (n m)
    (list :fmov.d (list '@+ (drv-gpr m)) (drv-xdr n))))

(readops-sh fmov.d.d-- (word read) ;; fmov.d DRm,@-Rn
  (unmasque "1111NNNN.MMM01011" word (n m)
    (list :fmov.d (drv-dpr m) (list '-@ (drv-gpr n)))))

(readops-sh fmov.d.x-- (word read) ;; fmov.d XDm,@-Rn
  (unmasque "1111NNNN.MMM11011" word (n m)
    (list :fmov.d (drv-xdr m) (list '-@ (drv-gpr n)))))

(readops-sh fmov.d.0r-d (word read) ;; fmov.d @(R0,Rm),DRn
  (unmasque "1111NNN0.MMMM0110" word (n m)
    (list :fmov.d (list '@0 (drv-gpr m)) (drv-dpr n))))

(readops-sh fmov.d.0r-x (word read) ;; fmov.d @(R0,Rm),XDn
  (unmasque "1111NNN1.MMMM0110" word (n m)
    (list :fmov.d (list '@0 (drv-gpr m)) (drv-xdr n))))

(readops-sh fmov.d.d-0r (word read) ;; fmov.d DRm,@(R0,Rn)
  (unmasque "1111NNNN.MMM00111" word (n m)
    (list :fmov.d (drv-dpr m) (list '@0 (drv-gpr n)))))

(readops-sh fmov.d.x-0r (word read) ;; fmov.d XDm,@(R0,Rn)
  (unmasque "1111NNNN.MMM10111" word (n m)
    (list :fmov.d (drv-xdr m) (list '@0 (drv-gpr n)))))

(readops-sh fmov.d.d12-d (word read) ;; fmov.d @(disp12,Rm),DRn
  (unmasque "0011NNN0.MMMM0001.0111DDDD.DDDDDDDD" word (n m d)
    (list :fmov.d (list '@> (drv-gpr m) d) (drv-dpr n))))

(readops-sh fmov.d.d-d12 (word read) ;; fmov.d DRm,@(disp12,Rn)
  (unmasque "0011NNNN.MMM00001.0011DDDD.DDDDDDDD" word (n m d)
    (list :fmov.d (drv-dpr m) (list '@> (drv-gpr n) d))))

;;; *** END FP DATA MOVES - BEGIN FP MATH

(specops-sh fldi0 (op0)
    ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (masque "1111NNNN.10001101" ;; fldi0 FRn
          (n (rix op0 :fp))))

(readops-sh fldi0 (word read) ;; fldi0 FRn
  (unmasque "1111NNNN.10001101" word (n)
    (list :fldi0 (drv-fpr n)))

(specops-sh fldi1 (op0)
    ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (masque "1111NNNN.10011101" ;; fldi1 FRn
          (n (rix op0 :fp))))

(readops-sh fldi1 (word read) ;; fldi1 FRn
  (unmasque "1111NNNN.10011101" word (n)
    (list :fldi1 (drv-fpr n))))

(specops-sh flds (op0 op1)
    ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (if (eq op1 :fpul)
      (masque "1111MMMM.00011101" ;; flds FRm,FPUL
              (m (rix op0 :fp)))
      (error "FLDS can only load data from the FPUL register.")))

(readops-sh flds (word read) ;; flds FRm,FPUL
  (unmasque "1111MMMM.00011101" word (m)
    (list :flds (drv-fpr m) :fpul)))

(specops-sh fsts (op0 op1)
    ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (if (eq op0 :fpul)
      (masque "1111NNNN.00001101" ;; fsts FPUL,FRn
              (n (rix op1 :fp)))
      (error "FSTS can only store data to the FPUL register.")))

(readops-sh fsts (word read) ;; fsts FPUL,FRn
  (unmasque "1111NNNN.00001101" word (n)
    (list :fsts :fpul (drv-fpr n))))

(specops-sh fabs (op0)
    ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (masque "1111NNNN.01011101" ;; fabs FRn
          (n (rix op0 :fp))))

(readops-sh fabs (word read) ;; fabs FRn
  (unmasque "1111NNNN.01011101" word (n)
    (list :fabs (drv-fpr n))))

(specops-sh fneg (op0)
    ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (masque "1111NNNN.01001101" ;; fneg FRn
          (n (rix op0 :fp))))

(readops-sh fneg (word read) ;; fneg FRn
  (unmasque "1111NNNN.01001101" word (n)
    (list :fneg (drv-fpr n))))

(specops-sh fadd (op0 op1)
    ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (masque "1111NNNN.MMMM0000" ;; fadd FRm,FRn
          (n (rix op1 :fp)) (m (rix op0 :fp))))

(readops-sh fadd (word read) ;; fadd FRm,FRn
  (unmasque "1111NNNN.MMMM0000" word (n m)
    (list :fadd (drv-fpr m) (drv-fpr n))))

(specops-sh fsub (op0 op1)
    ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (masque "1111NNNN.MMMM0001" ;; fsub FRm,FRn
          (n (rix op1 :fp)) (m (rix op0 :fp))))

(readops-sh fsub (word read) ;; fsub FRm,FRn
  (unmasque "1111NNNN.MMMM0001" word (n m)
    (list :fsub (drv-fpr m) (drv-fpr n))))

(specops-sh fmul (op0 op1)
    ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (masque "1111NNNN.MMMM0010" ;; fmul FRm,FRn
          (n (rix op1 :fp)) (m (rix op0 :fp))))

(readops-sh fmul (word read) ;; fmul FRm,FRn
  (unmasque "1111NNNN.MMMM0010" word (n m)
    (list :fmul (drv-fpr m) (drv-fpr n))))

(specops-sh fmac (op0 op1)
    ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (masque "1111NNNN.MMMM1110" ;; fmac FR0,FRm,FRn
          (n (rix op1 :fp)) (m (rix op0 :fp))))

(readops-sh fmac (word read) ;; fmac FR0,FRm,FRn
  (unmasque "1111NNNN.MMMM1110" word (n m)
    (list :fmac (drv-fpr m) (drv-fpr n))))

(specops-sh fdiv (op0 op1)
    ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (masque "1111NNNN.MMMM0011" ;; fdiv FRm,FRn
          (n (rix op1 :fp)) (m (rix op0 :fp))))

(readops-sh fdiv (word read) ;; fdiv FRm,FRn
  (unmasque "1111NNNN.MMMM0011" word (n m)
    (list :fdiv (drv-fpr m) (drv-fpr n))))

(specops-sh fsqrt (op0)
    ((:for-types :sh3e :sh4 :sh4a :sh2a))
  (masque "1111NNNN.01101101" ;; fsqrt FRn
          (n (rix op0 :fp))))

(readops-sh fsqrt (word read) ;; fsqrt FRn
  (unmasque "1111NNNN.01101101" word (n)
    (list :fsqrt (drv-fpr n))))

(specops-sh fcmp/eq (op0 op1)
    ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (masque "1111NNNN.MMMM0100" ;; fcmp/eq FRm,FRn
          (n (rix op1 :fp)) (m (rix op0 :fp))))

(readops-sh fcmp/eq (word read) ;; fcmp/eq FRm,FRn
  (unmasque "1111NNNN.MMMM0100" word (n m)
    (list :fcmp/eq (drv-fpr m) (drv-fpr n))))

(specops-sh fcmp/gt (op0 op1)
    ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (masque "1111NNNN.MMMM0101" ;; fcmp/gt FRm,FRn
          (n (rix op1 :fp)) (m (rix op0 :fp))))

(readops-sh fcmp/gt (word read) ;; fcmp/gt FRm,FRn
  (unmasque "1111NNNN.MMMM0101" word (n m)
    (list :fcmp/gt (drv-fpr m) (drv-fpr n))))

(specops-sh float (op0 op1)
    ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (if (eq op0 :fpul)
      (masque "1111NNNN.00101101" ;; float FPUL,FRn
              (n (rix op1 :fp)))
      (error "FLOAT can only move a value to the FPUL register.")))

(readops-sh float (word read)
  (unmasque "1111NNNN.00101101" word (n)
    (list :float :fpul (drv-fpr n))))

(specops-sh ftrc (op0 op1)
    ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (if (eq op1 :fpul)
      (masque "1111MMMM.00111101" ;; ftrc FRm,FPUL
              (m (rix op0 :fp)))
      (error "FTRC can only move a value from the FPUL register."))))
         
(readops-sh ftrc (word read) ;; ftrc FRm,FPUL
  (unmasque "1111MMMM.00111101" word (m)
    (list :ftrc (drv-fpr m) :fpul)))

(specops-sh fipr (op0 op1)
  ((:for-types :sh4 :sh4a))
  (masque "1111NNMM.11101101" ;; fipr FVm,FVn
          (n (rix op1 :fv)) (m (rix op0 :fv))))

(readops-sh fipr (word read) ;; fipr FVm,FVn
  (unmasque "1111NNMM.11101101" word (n m)
    (list :fipr (drv-fvr m) (drv-fvr n))))

(specops-sh ftrv (op0 op1)
    ((:for-types :sh4 :sh4a))
  (if (eq op0 :xmtrx)
      (masque "1111NN01.11111101" ;; ftrv XMTRX,FVn
              (n (rix op1 :fv)))
      (error "FTRV's first operand may only be the XMTRX matrix register.")))

(readops-sh ftrv (word read) ;; ftrv XMTRX,FVn
  (unmasque "1111NN01.11111101" word (n)
    (list :ftrv :xmtrx (drv-fvr n))))

(specops-sh fsrra (op0)
  ((:for-types :sh4a))
  (masque "1111NNNN.01111101" ;; fsrra FRn
          (n (rix op0 :fv))))

(readops-sh fsrra (word read) ;; fsrra FRn
  (unmasque "1111NNNN.01111101" word (n)
    (list :fsrra (drv-fvr n))))

(specops-sh fsca (op0 op1)
    ((:for-types :sh4a))
  (if (eq op0 :fpul)
      (masque "1111NNN0.11111101" ;; fsca FPUL,DRn
              (n (rix op1 :dp)))
      (error "FSCA's first operand may only be the FPUL register.")))

(readops-sh fsca (word read) ;; fsca FPUL,DRn
  (unmasque "1111NNN0.11111101" word (n)
    (list :fsca :fpul (drv-dpr n))))

;; (specops-sh fabs (w op0 op1) ;;; xxx START DUPLICATE
;;   ;; fabs DRn
;;   ((:for-types :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "1111NNN0.01011101"
;;             )))

;; (readops-sh fabs (word read)
;;   (unmasque "1111NNN0.01011101" word ()
;;     (list :fabs )))

;; (specops-sh fneg (w op0 op1) ;;; xxx
;;   ;; fneg DRn
;;   ((:for-types :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "1111NNN0.01001101"
;;             )))

;; (readops-sh fneg (word read)
;;   (unmasque "1111NNN0.01001101" word ()
;;     (list :fneg )))

;; (specops-sh fadd (w op0 op1) ;;; xxx
;;   ;; fadd DRm,DRn
;;   ((:for-types :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "1111NNN0.MMM00000"
;;             )))

;; (readops-sh fadd (word read)
;;   (unmasque "1111NNN0.MMM00000" word ()
;;     (list :fadd )))

;; (specops-sh fsub (w op0 op1) ;;; xxx
;;   ;; fsub DRm,DRn
;;   ((:for-types :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "1111NNN0.MMM00001"
;;             )))

;; (readops-sh fsub (word read)
;;   (unmasque "1111NNN0.MMM00001" word ()
;;     (list :fsub )))

;; (specops-sh fmul (w op0 op1)
;;   ;; fmul DRm,DRn
;;   ((:for-types :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "1111NNN0.MMM00010"
;;             )))

;; (readops-sh fmul (word read)
;;   (unmasque "1111NNN0.MMM00010" word ()
;;     (list :fmul )))

;; (specops-sh fdiv (w op0 op1)
;;   ;; fdiv DRm,DRn
;;   ((:for-types :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "1111NNN0.MMM00011"
;;             )))

;; (readops-sh fdiv (word read)
;;   (unmasque "1111NNN0.MMM00011" word ()
;;     (list :fdiv )))

;; (specops-sh fsqrt (w op0 op1)
;;   ;; fsqrt DRn
;;   ((:for-types :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "1111NNN0.01101101"
;;             )))

;; (readops-sh fsqrt (word read)
;;   (unmasque "1111NNN0.01101101" word ()
;;     (list :fsqrt )))

;; (specops-sh fcmp/eq (w op0 op1)
;;   ;; fcmp/eq DRm,DRn
;;   ((:for-types :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "1111NNN0.MMM00100"
;;             )))

;; (readops-sh fcmp/eq (word read)
;;   (unmasque "1111NNN0.MMM00100" word ()
;;     (list :fcmp/eq )))

;; (specops-sh fcmp/gt (w op0 op1)
;;   ;; fcmp/gt DRm,DRn
;;   ((:for-types :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "1111NNN0.MMM00101"
;;             )))

;; (readops-sh fcmp/gt (word read)
;;   (unmasque "1111NNN0.MMM00101" word ()
;;     (list :fcmp/gt )))

;; (specops-sh float (w op0 op1)
;;   ;; float FPUL,DRn
;;   ((:for-types :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "1111NNN0.00101101"
;;             )))

;; (readops-sh float (word read)
;;   (unmasque "1111NNN0.00101101" word ()
;;     (list :float )))

;; (specops-sh ftrc (w op0 op1)
;;   ;; ftrc DRm,FPUL
;;   ((:for-types :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "1111MMM0.00111101"
;;             ))) ;;; END DUPLICATE

;; (readops-sh ftrc (word read)
;;   (unmasque "1111MMM0.00111101" word ()
;;     (list :ftrc )))

(specops-sh fcnvds (op0 op1)
  ((:for-types :sh4 :sh4a :sh2a))
  (if (eq op1 :fpul)
      (masque "1111MMM0.10111101" ;; fcnvds DRm,FPUL
              (m (rix op0 :dp)))
      (error "FCNVDS's second operand may only be the FPUL register.")))

(readops-sh fcnvds (word read)
  (unmasque "1111MMM0.10111101" word (m)
    (list :fcnvds (drv-dpr m) :fpul)))

(specops-sh fcnvsd (op0 op1)
  ((:for-types :sh4 :sh4a :sh2a))
  (if (eq op0 :fpul)
      (masque "1111NNN0.10101101" ;; fcnvsd FPUL,DRn
              (n (rix op1 :dp)))
      (error "FCNVSD's first operand may only be the FPUL register.")))

(readops-sh fcnvsd (word read) ;; fcnvsd FPUL,DRn
  (unmasque "1111NNN0.10101101" word (n)
    (list :fcnvsd :fpul (drv-dpr n))))

;; (specops-sh lds (w op0 op1)
;;   ;; lds Rm,FPSCR
;;   ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "0100MMMM.01101010"
;;             )))

(readops-sh lds.r-fps (word read) ;; lds Rm,FPSCR
  (unmasque "0100MMMM.01101010" word (m)
    (list :lds (drv-gpr m) :fpscr)))

;; (specops-sh sts (w op0 op1)
;;   ;; sts FPSCR,Rn
;;   ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "0000NNNN.01101010"
;;             )))

(readops-sh sts.fps-r (word read) ;; sts FPSCR,Rn
  (unmasque "0000NNNN.01101010" word (n)
    (list :sts :fpscr (drv-gpr n))))

;; (specops-sh lds.l (w op0 op1)
;;   ;; lds.l @Rm+,FPSCR
;;   ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "0100MMMM.01100110"
;;             )))

(readops-sh lds.l.+-fps (word read) ;; lds.l @Rm+,FPSCR
  (unmasque "0100MMMM.01100110" word (m)
    (list :lds.l (list '@+ (drv-gpr m)) :fpscr)))

;; (specops-sh sts.l (w op0 op1)
;;   ;; sts.l FPSCR,@-Rn
;;   ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "0100NNNN.01100010"
;;             )))

(readops-sh sts.l.fps-- (word read) ;; sts.l FPSCR,@-Rn
  (unmasque "0100NNNN.01100010" word (n)
    (list :sts.l :fpscr) (list '@+ (drv-gpr n))))

;; (specops-sh lds (w op0 op1)
;;   ;; lds Rm,FPUL
;;   ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "0100MMMM.01011010"
;;             )))

(readops-sh lds.r-fpu (word read) ;; lds Rm,FPUL
  (unmasque "0100MMMM.01011010" word (m)
    (list :lds (drv-gpr m) :fpul)))

;; (specops-sh sts (w op0 op1)
;;   ;; sts FPUL,Rn
;;   ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "0000NNNN.01011010"
;;             )))

(readops-sh sts.fpu-r (word read) ;; sts FPUL,Rn
  (unmasque "0000NNNN.01011010" word (n)
    (list :sts :fpul (drv-gpr n))))

;; (specops-sh lds.l (w op0 op1)
;;   ;; lds.l @Rm+,FPUL
;;   ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "0100MMMM.01010110"
;;             )))

(readops-sh lds.l.+-fpu (word read) ;; lds.l @Rm+,FPUL
  (unmasque "0100MMMM.01010110" word (m)
    (list :lds.l (list '@+ (drv-gpr m)) :fpul)))

;; (specops-sh sts.l (w op0 op1)
;;   ;; sts.l FPUL,@-Rn
;;   ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
;;   (address (op0 op1) (index0 index1)
;;     (masque "0100NNNN.01010010"
;;             )))

(readops-sh sts.l.fpu-- (word read) ;; sts.l FPUL,@-Rn
  (unmasque "0100NNNN.01010010" word (n)
    (list :sts.l :fpul (list '-@ (drv-gpr n)))))

(specops-sh frchg ()
  ((:for-types :sh4 :sh4a))
  (masque "11111011.11111101")) ;; frchg

(readops-sh (masque "11111011.11111101") (read)
  (list :frchg))

(specops-sh fschg ()
  ((:for-types :sh4 :sh4a :sh2a))
  (masque "11110011.11111101")) ;; fschg

(readops-sh (masque "11110011.11111101") (read)
  (list :fschg))

(specops-sh fpchg ()
  ((:for-types :sh4a))
  (masque "11110111.11111101")) ;; fpchg

(readops-sh (masque "11110111.11111101") (read)  
  (list :fpchg))



