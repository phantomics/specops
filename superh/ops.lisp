;;;; ops.lisp

(in-package #:specops.superh)

(specops mov (w op0 op1)
    ((:type-matcher matching-types)
     (:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (cond ((matching-types :sh2a) ;; the SH2A-specific MOV instructions
         (cond ((match-types op0 op1  gpr mas-bs+disp)
                (case w
                  (:b (masque "0011NNNN.MMMM0001.0000DDDD.DDDDDDDD" ;; mov.b Rm,@(disp12,Rn)
                              (n (gprix (mas-base op1))) (m (gprix op0)) (d (mas-displ op1))))
                  (:w (masque "0011NNNN.MMMM0001.0001DDDD.DDDDDDDD" ;; mov.w Rm,@(disp12,Rn)
                              (n (gprix (mas-base op1))) (m (gprix op0)) (d (mas-displ op1))))
                  (:l (masque "0011NNNN.MMMM0001.0010DDDD.DDDDDDDD" ;; mov.l Rm,@(disp12,Rn)
                              (n (gprix (mas-base op1))) (m (gprix op0)) (d (mas-displ op1))))))
               ((match-types op0 op1  mas-bs+disp gpr)
                (case w
                  (:b (masque "0011NNNN.MMMM0001.0100DDDD.DDDDDDDD" ;; mov.b @(disp12,Rm),Rn
                              (n (gprix (mas-base op1))) (m (gprix op0)) (d (mas-displ op1))))
                  (:w (masque "0011NNNN.MMMM0001.0101DDDD.DDDDDDDD" ;; mov.w @(disp12,Rm),Rn
                              (n (gprix (mas-base op1))) (m (gprix op0)) (d (mas-displ op1))))
                  (:l (masque "0011NNNN.MMMM0001.0110DDDD.DDDDDDDD" ;; mov.l @(disp12,Rm),Rn
                              (n (gprix (mas-base op1))) (m (gprix op0)) (d (mas-displ op1))))))))
        ((match-types op0 op1  gpr gpr)
         (assert (eq w :l) (op0)
                 "MOVing data between registers can only be done at width L (32 bits).")
         (masque "0110NNNN.MMMM0011" ;; mov Rm,Rn
                 (n (gprix op1)) (m (gprix op0))))
        ((match-types op0 op1  integer gpr)
         (assert (eq w :b) (op0)
                 "MOVing immediate data to a register can only be done at width B (8 bits).")
         (masque "1110NNNN.IIIIIIII" ;; mov #imm,Rn
                 (n (gprix op1)) (m op0)))
        ((match-types op0 op1  mas-pc+disp gpr)
         (case w
           (:w (masque "1001NNNN.DDDDDDDD" ;; mov.w @(disp,PC),Rn
                       (n (gprix op1)) (d (mas-displ op0))))
           (:l (masque "1101NNNN.DDDDDDDD" ;; mov.l @(disp,PC),Rn
                       (n (gprix op1)) (d (mas-displ op0))))))
        ((match-types op0 op1  mas-simple gpr)
         (case w
           (:b (masque "0110NNNN.MMMM0000" ;; mov.b @Rm,Rn
                       (n (gprix op1)) (m (gprix (mas-base op0)))))
           (:w (masque "0110NNNN.MMMM0001" ;; mov.w @Rm,Rn
                       (n (gprix op1)) (m (gprix (mas-base op0)))))
           (:l (masque "0110NNNN.MMMM0010" ;; mov.l @Rm,Rn
                       (n (gprix op1)) (m (gprix (mas-base op0)))))))
        ((match-types op0 op1  gpr mas-simple)
         (case w
           (:b (masque "0010NNNN.MMMM0000" ;; mov.b Rm,@Rn
                       (n (gprix (mas-base op1))) (m (gprix op0))))
           (:w (masque "0010NNNN.MMMM0001" ;; mov.w Rm,@Rn
                       (n (gprix (mas-base op1))) (m (gprix op0))))
           (:l (masque "0010NNNN.MMMM0010" ;; mov.l Rm,@Rn
                       (n (gprix (mas-base op1))) (m (gprix op0))))))
        ((match-types op0 op1  mas-postinc gpr)
         (case w
           (:b (masque "0110NNNN.MMMM0100" ;; mov.b @Rm+,Rn
                       (n (gprix op1)) (m (gprix (mas-base op0)))))
           (:w (masque "0110NNNN.MMMM0101" ;; mov.w @Rm+,Rn
                       (n (gprix op1)) (m (gprix (mas-base op0)))))
           (:l (masque "0110NNNN.MMMM0101" ;; mov.l @Rm+,Rn
                       (n (gprix op1)) (m (gprix (mas-base op0)))))))
        ((match-types op0 op1  gpr mas-predecr)
         (case w
           (:b (masque "0010NNNN.MMMM0100" ;; mov.b Rm,@-Rn
                       (n (gprix (mas-base op1))) (m (gprix op0))))
           (:w (masque "0010NNNN.MMMM0101" ;; mov.w Rm,@-Rn
                       (n (gprix (mas-base op1))) (m (gprix op0))))
           (:l (masque "0010NNNN.MMMM0110" ;; mov.l Rm,@-Rn
                       (n (gprix (mas-base op1))) (m (gprix op0))))))
        
        ((and (eq :r0 op1) (match-types op1  mas-predecr))
         (case w
           (:b (masque "0100MMMM.11001011" ;; mov.b @-Rm,R0
                       (m (gprix (mas-base op0)))))
           (:w (masque "0100MMMM.11011011" ;; mov.w @-Rm,R0
                       (m (gprix (mas-base op0)))))
           (:l (masque "0100MMMM.11101011" ;; mov.l @-Rm,R0
                       (m (gprix (mas-base op0)))))))
        ((and (eq :r0 op0) (match-types op1  mas-postinc))
         (case w
           (:b (masque "0100NNNN.10001011" ;; mov.b R0,@Rn+
                       (n (gprix (mas-base op1)))))
           (:w (masque "0100NNNN.10011011" ;; mov.w R0,@Rn+
                       (m (gprix (mas-base op0)))))
           (:l (masque "0100NNNN.10101011" ;; mov.l R0,@Rn+
                       (m (gprix (mas-base op0)))))))
        
        ((and (eq :r0 op1) (match-types op0  mas-bs+disp))
         (case w
           (:b (masque "10000100.MMMMDDDD" ;; mov.b @(disp,Rm),R0
                       (m (gprix (mas-base op0))) (d (mas-displ op0))))
           (:w (masque "10000101.MMMMDDDD" ;; mov.w @(disp,Rm),R0
                       (m (gprix (mas-base op0))) (d (mas-displ op0))))))
        ((match-types op0 op1  mas-bs+disp gpr)
         (case w
           (:l (masque "0101NNNN.MMMMDDDD" ;; mov.l @(disp,Rm),Rn
                       (n (gprix op1)) (m (gprix (mas-base op0))) (d (mas-displ op0))))))
        ((and (eq :r0 op0) (match-types op1  mas-bs+disp))
         (case w
           (:b (masque "10000000.NNNNDDDD" ;; mov.b R0,@(disp,Rn)
                       (n (gprix (mas-base op1))) (d (mas-displ op1))))
           (:w (masque "10000001.NNNNDDDD" ;; mov.w R0,@(disp,Rn)
                       (n (gprix (mas-base op1))) (d (mas-displ op1))))))
        ((match-types op0 op1  gpr mas-bs+disp)
         (case w
           (:l (masque "0001NNNN.MMMMDDDD" ;; mov.l Rm,@(disp,Rn)
                       (n (gprix (mas-base op1))) (m (gprix op0)) (d (mas-displ op1))))))
        
        ((match-types op0 op1  mas-b+rzero gpr)
         (case w
           (:b (masque "0000NNNN.MMMM1100" ;; mov.b @(R0,Rm),Rn
                       (n (gprix op1)) (m (gprix (mas-base op0)))))
           (:w (masque "0000NNNN.MMMM1101" ;; mov.w @(R0,Rm),Rn
                       (n (gprix op1)) (m (gprix (mas-base op0)))))
           (:l (masque "0000NNNN.MMMM1110" ;; mov.l @(R0,Rm),Rn
                       (n (gprix op1)) (m (gprix (mas-base op0)))))))
        ((match-types op0 op1  gpr mas-b+rzero)
         (case w
           (:b (masque "0000NNNN.MMMM0100" ;; mov.b Rm,@(R0,Rn)
                       (n (gprix (mas-base op1))) (m (gprix op0))))
           (:w (masque "0000NNNN.MMMM0101" ;; mov.w Rm,@(R0,Rn)
                       (n (gprix (mas-base op1))) (m (gprix op0))))
           (:l (masque "0000NNNN.MMMM0110" ;; mov.l Rm,@(R0,Rn)
                       (n (gprix (mas-base op1))) (m (gprix op0))))))
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

(readops mov.l.r-r (word read) ;; mov Rm,Rn
  (unmasque "0110NNNN.MMMM0011" word (n m)
    (list :mov :l (drv-gpr m) (drv-gpr n))))

(readops mov.l.i-r (word read) ;; mov #imm,Rn
  (unmasque "1110NNNN.IIIIIIII" word (n i)
    (list :mov :b i (drv-gpr n))))

(readops mov.w.dspc-r (word read) ;; mov.w @(disp,PC),Rn
  (unmasque "1001NNNN.DDDDDDDD" word (n d)
    (list :mov :w (list '@pc d) (drv-gpr n))))

(readops mov.l.dspc-r (word read) ;; mov.l @(disp,PC),Rn
  (unmasque "1101NNNN.DDDDDDDD" word (n d)
    (list :mov :l (list '@pc d) (drv-gpr n))))

(readops mov.b.@-r (word read) ;; mov.b @Rm,Rn
  (unmasque "0110NNNN.MMMM0000" word (n m)
    (list :mov :b (list '@ (drv-gpr m)) (drv-gpr n))))

(readops mov.w.@-r (word read) ;; mov.w @Rm,Rn
  (unmasque "0110NNNN.MMMM0001" word (n m)
    (list :mov :w (list '@ (drv-gpr m)) (drv-gpr n))))

(readops mov.l.@-r (word read) ;; mov.l @Rm,Rn
  (unmasque "0110NNNN.MMMM0010" word (n m)
    (list :mov :l (list '@ (drv-gpr m)) (drv-gpr n))))

(readops mov.b.r-@ (word read) ;; mov.b Rm,@Rn
  (unmasque "0010NNNN.MMMM0000" word (n m)
    (list :mov :b (drv-gpr m) (list '@ (drv-gpr n)))))

(readops mov.w.r-@ (word read) ;; mov.w Rm,@Rn
  (unmasque "0010NNNN.MMMM0001" word (n m)
    (list :mov :w (drv-gpr m) (list '@ (drv-gpr n)))))

(readops mov.l.r-@ (word read) ;; mov.l Rm,@Rn
  (unmasque "0010NNNN.MMMM0010" word (n m)
    (list :mov :l (drv-gpr m) (list '@ (drv-gpr n)))))

(readops mov.b.+-r (word read) ;; mov.b @Rm+,Rn
  (unmasque "0110NNNN.MMMM0100" word (n m)
    (list :mov :b (list '@+ (drv-gpr m)) (drv-gpr n))))

(readops mov.w.+-r (word read) ;; mov.w @Rm+,Rn
  (unmasque "0110NNNN.MMMM0101" word (n m)
    (list :mov :w (list '@+ (drv-gpr m)) (drv-gpr n))))

(readops mov.l.+-r (word read) ;; mov.l @Rm+,Rn
  (unmasque "0110NNNN.MMMM0110" word (n m)
    (list :mov :l (list '@+ (drv-gpr m)) (drv-gpr n))))

(readops mov.b.r-- (word read) ;; mov.b Rm,@-Rn
  (unmasque "0010NNNN.MMMM0100" word (n m)
    (list :mov :b (drv-gpr m) (list '-@ (drv-gpr n)))))

(readops mov.w.r-- (word read) ;; mov.w Rm,@-Rn
  (unmasque "0010NNNN.MMMM0101" word (n m)
    (list :mov :w (drv-gpr m) (list '-@ (drv-gpr n)))))

(readops mov.l.r-- (word read) ;; mov.l Rm,@-Rn
  (unmasque "0010NNNN.MMMM0110" word (n m)
    (list :mov :l (drv-gpr m) (list '-@ (drv-gpr n)))))

(readops mov.b.--0 (word read) ;; mov.b @-Rm,R0
  (unmasque "0100MMMM.11001011" word (m)
    (list :mov :b (list '-@ (drv-gpr m)) :r0)))

(readops mov.w.--0 (word read) ;; mov.w @-Rm,R0
  (unmasque "0100MMMM.11011011" word (m)
    (list :mov :w (list '-@ (drv-gpr m)) :r0)))

(readops mov.l.--0 (word read) ;; mov.l @-Rm,R0
  (unmasque "0100MMMM.11101011" word (m)
    (list :mov :l (list '-@ (drv-gpr m)) :r0)))

(readops mov.b.0-+ (word read) ;; mov.b R0,@Rn+
  (unmasque "0100NNNN.10001011" word (n)
    (list :mov :b :r0 (list '@+ (drv-gpr n)))))

(readops mov.w.0-+ (word read) ;; mov.w R0,@Rn+
  (unmasque "0100NNNN.10011011" word (n)
    (list :mov.w :r0 (list '@+ (drv-gpr n)))))

(readops mov.l.0-+ (word read) ;; mov.l R0,@Rn+
  (unmasque "0100NNNN.10101011" word (n)
    (list :mov :l :r0 (list '@+ (drv-gpr n)))))

(readops mov.b.dr-0 (word read) ;; mov.b @(disp,Rm),R0
  (unmasque "10000100.MMMMDDDD" word (m d)
    (list :mov :b (list '@> (drv-gpr m) d) :r0)))

(readops mov.w.dr-0 (word read) ;; mov.w @(disp,Rm),R0
  (unmasque "10000101.MMMMDDDD" word (m d)
    (list :mov :w (list '@> (drv-gpr m) d) :r0)))

(readops mov.l.dr-r (word read) ;; mov.l @(disp,Rm),Rn
  (unmasque "0101NNNN.MMMMDDDD" word (n m d)
    (list :mov :l (list '@> (drv-gpr m) d) (drv-gpr n))))

(readops mov.b.r-dr (word read) ;; mov.b R0,@(disp,Rn)
  (unmasque "10000000.NNNNDDDD" word (n d)
    (list :mov :b :r0 (list '@> (drv-gpr n) d))))

(readops mov.w.r-dr (word read) ;; mov.w R0,@(disp,Rn)
  (unmasque "10000001.NNNNDDDD" word (n d)
    (list :mov :w :r0 (list '@> (drv-gpr n) d))))

(readops mov.l.r-dr (word read) ;; mov.l Rm,@(disp,Rn)
  (unmasque "0001NNNN.MMMMDDDD" word (n m d)
    (list :mov :l (drv-gpr m) (list '@> (drv-gpr n) d))))

(readops mov.b.0r-r (word read) ;; mov.b @(R0,Rm),Rn
  (unmasque "0000NNNN.MMMM1100" word (n m)
    (list :mov :b (list '@0 (drv-gpr m)) (drv-gpr n))))

(readops mov.w.0r-r (word read) ;; mov.w @(R0,Rm),Rn
  (unmasque "0000NNNN.MMMM1101" word (n m)
    (list :mov :w (list '@0 (drv-gpr m)) (drv-gpr n))))

(readops mov.l.0r-r (word read) ;; mov.l @(R0,Rm),Rn
  (unmasque "0000NNNN.MMMM1110" word (n m)
    (list :mov :l (list '@0 (drv-gpr m)) (drv-gpr n))))

(readops mov.b.r-0r (word read) ;; mov.b Rm,@(R0,Rn)
  (unmasque "0000NNNN.MMMM0100" word (n m)
    (list :mov :b (drv-gpr m) (list '@0 (drv-gpr n)))))

(readops mov.w.r-0r (word read) ;; mov.w Rm,@(R0,Rn)
  (unmasque "0000NNNN.MMMM0101" word (n m)
    (list :mov :w (drv-gpr m) (list '@0 (drv-gpr n)))))

(readops mov.l.r-0r (word read) ;; mov.l Rm,@(R0,Rn)
  (unmasque "0000NNNN.MMMM0110" word (n m)
    (list :mov :l (drv-gpr m) (list '@0 (drv-gpr n)))))

(readops mov.b.dg-0 (word read) ;; mov.b @(disp,GBR),R0
  (unmasque "11000100.DDDDDDDD" word (d)
    (list :mov :b (list '@0 d) :r0)))

(readops mov.w.dg-0 (word read) ;; mov.w @(disp,GBR),R0
  (unmasque "11000101.DDDDDDDD" word (d)
    (list :mov :w (list '@0 d) :r0)))

(readops mov.l.dg-0 (word read) ;; mov.l @(disp,GBR),R0
  (unmasque "11000110.DDDDDDDD" word (d)
    (list :mov :l (list '@0 d) :r0)))

(readops mov.b.0-dg (word read) ;; mov.b R0,@(disp,GBR)
  (unmasque "11000000.DDDDDDDD" word (d)
    (list :mov :b :r0 (list '@0 d))))

(readops mov.w.0-dg (word read) ;; mov.w R0,@(disp,GBR)
  (unmasque "11000001.DDDDDDDD" word (d)
    (list :mov :w :r0 (list '@0 d))))

(readops mov.l.0-dg (word read) ;; mov.l R0,@(disp,GBR)
  (unmasque "11000010.DDDDDDDD" word (d)
    (list :mov :l :r0 (list '@0 d))))


(readops mov.b (word read) ;; mov.b Rm,@(disp12,Rn)
  (unmasque "0011NNNN.MMMM0001.0000DDDD.DDDDDDDD" word (n m d)
    (list :mov :b (drv-gpr n) (list '@> (drv-gpr m) d))))

(readops mov.w (word read) ;; mov.w Rm,@(disp12,Rn)
  (unmasque "0011NNNN.MMMM0001.0001DDDD.DDDDDDDD" word (n m d)
    (list :mov :w (drv-gpr n) (list '@> (drv-gpr m)))))

(readops mov.l (word read) ;; mov.l Rm,@(disp12,Rn)
  (unmasque "0011NNNN.MMMM0001.0010DDDD.DDDDDDDD" word (n m d)
    (list :mov :l (drv-gpr n) (list '@> (drv-gpr m)))))

(readops mov.b (word read) ;; mov.b @(disp12,Rm),Rn
  (unmasque "0011NNNN.MMMM0001.0100DDDD.DDDDDDDD" word (n m d)
    (list :mov :b (list '@> (drv-gpr m) d) (drv-gpr n))))

(readops mov.w (word read) ;; mov.w @(disp12,Rm),Rn
  (unmasque "0011NNNN.MMMM0001.0101DDDD.DDDDDDDD" word (n m d)
    (list :mov :w (list '@> (drv-gpr m) d) (drv-gpr n))))

(readops mov.l (word read) ;; mov.l @(disp12,Rm),Rn
  (unmasque "0011NNNN.MMMM0001.0110DDDD.DDDDDDDD" word (n m d)
    (list :mov :l (list '@> (drv-gpr m) d) (drv-gpr n))))

;;;

(specops movi20 (w op0 op1)
  ((:for-types :sh2a))
  (assert (match-types op0 op1  integer gpr) (op0 op1)
          "MOVI20 may take only an immediate integer value and a general-purpose register as its operands.")
  (masque "0000NNNN.HHHH0000.IIIIIIII.IIIIIIII" ;; movi20 #imm20,Rn
          (n (gprix op1)) (h (rs16 op0)) (i (lo16 op0))))

(readops movi20 (word read) ;; movi20 #imm20,Rn
  (unmasque "0000NNNN.HHHH0000.IIIIIIII.IIIIIIII" word (n h i)
    (list :movi20 (+ i (ash h 16)) (drv-gpr n))))

(specops movi20s (w op0 op1)
  ((:for-types :sh2a))
  (assert (match-types op0 op1  integer gpr) (op0 op1)
          "MOVI20S may take only an immediate integer value and a general-purpose register as its operands.")
  (masque "0000NNNN.IIII0001.IIIIIIII.IIIIIIII" ;; movi20s #imm20,Rn
          (n (gprix op1)) (h (rs16 op0)) (i (lo16 op0))))

(readops movi20s (word read) ;; movi20s #imm20,Rn
  (unmasque "0000NNNN.IIII0001.IIIIIIII.IIIIIIII" word (n h i)
    (list :movi20s (+ i (ash h 16)) (drv-gpr n))))

(specops mova (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (and (eq op1 :r0) (match-types op0  mas-pc+disp)) (op0)
          "MOVA may take only a displacement+PC memory address and R0 as its operands.")
  (masque "11000111.DDDDDDDD" ;; mova @(disp,PC),R0
          (d (mas-displ op0))))

(readops mova (word read)
  (unmasque "11000111.DDDDDDDD" word (d)
    (list :mova (list '@pc d) :r0)))

(specops movu (w op0 op1)
  ((:for-types :sh2a))
  (assert (match-types op0 op1  mas-bs+disp gpr) (op0)
          "MOVA may take only a base+displacement memory address and a general-purpose register as its operands.")
  (case w
    (:b (masque "0011NNNN.MMMM0001.1000DDDD.DDDDDDDD" ;; movu.b @(disp12,Rm),Rn
                (n (gprix op1)) (m (gprix (mas-base op0))) (d (mas-displ op0))))
    (:w (masque "0011NNNN.MMMM0001.1001DDDD.DDDDDDDD" ;; movu.w @(disp12,Rm),Rn
                (n (gprix op1)) (m (gprix (mas-base op0))) (d (mas-displ op0))))))

(readops movu-b (word read) ;; movu.b @(disp12,Rm),Rn
  (unmasque "0011NNNN.MMMM0001.1000DDDD.DDDDDDDD" word (n m d)
    (list :movu :b (list '@> (drv-gpr m) d) (drv-gpr n))))

(readops movu-w (word read) ;; movu.w @(disp12,Rm),Rn
  (unmasque "0011NNNN.MMMM0001.1001DDDD.DDDDDDDD" word (n m d)
    (list :movu :w (list '@> (drv-gpr m) d) (drv-gpr n))))

(specops movco (w op0 op1)
  ((:for-types :sh4a))
  (assert (and (eq w :l) (eq op0 :r0) (match-types op1  mas-simple)) (op0)
          "MOVCO only works at width L and takes only R0 and a displacement+PC memory address as its operands.")
  (masque "0000NNNN.01110011" ;; movco.l R0,@Rn
          (n (gprix (mas-base op1)))))

(readops movco.l (word read)
  (unmasque "0000NNNN.01110011" word (n)
    (list :movco :l :r0 (list '@ (drv-gpr n)))))

(specops movli (w op0 op1)
  ((:for-types :sh4a))
  (assert (and (eq w :l) (eq op1 :r0) (match-types op0  mas-simple)) (op0)
          "MOVLI only works at width L and takes only a simple memory address and R0 as its operands.")
  (masque "0000MMMM.01100011" ;; movli.l @Rm,R0
          (m (gprix (mas-base op0)))))

(readops movli-l (word read) ;; movli.l @Rm,R0
  (unmasque "0000MMMM.01100011" word (m)
    (list :movli.l (list '@ (drv-gpr m)) :r0)))

(specops movua (w op0 op1)
  ((:for-types :sh4a))
  (assert (and (eq w :l) (eq op1 :r0) (match-types op0  mas-simple)) (op0)
          "MOVUA only works at width L and takes only a simple memory address and R0 as its operands.")
  (masque "0100MMMM.10101001" ;; movua.l @Rm,R0
          (m (gprix (mas-base op0)))))

(readops movua-l (word read)
  (unmasque "0100MMMM.10101001" word (m) ;; movua.l @Rm,R0
    (list :movua :l (list '@ (drv-gpr m)) :r0)))

(specops movua (w op0 op1)
  ((:for-types :sh4a))
  (assert (and (eq w :l) (eq op1 :r0) (match-types op0  mas-postinc)) (op0)
          "MOVUA only works at width L and takes only a post-incrementing memory address and R0 as its operands.")
  (masque "0100MMMM.11101001" ;; movua @Rm+,R0
          (m (gprix (mas-base op0)))))

(readops movua-l (word read) ;; movua.l @Rm+,R0
  (unmasque "0100MMMM.11101001" word (m)
    (list :movua :l (list '@+ (drv-gpr m)) :r0)))

(specops movml (w op0 op1)
  ((:for-types :sh2a))
  (assert (and (eq w :l) (match-types op0 op1  gpr mas-predecr)
               (eq (mas-displ op1) :r15))
          (op0) "MOVML only works at width L and takes only a general-purpose register and pre-decrementing memory address with R15 base as its operands.")
  (masque "0100MMMM.11110001" ;; movml.l Rm,@-R15
          (m (gprix op0))))

(readops movml-l (word read) ;; movml.l Rm,@-R15
  (unmasque "0100MMMM.11110001" word (m)
    (list :movml :l (drv-gpr m) '(-@ :r15))))

(specops movml (w op0 op1)
  ((:for-types :sh2a))
  (assert (and (eq w :l) (match-types op0 op1  mas-postinc gpr)
               (eq (mas-displ op0) :r15))
          (op0) "MOVML only works at width L and takes only a post-incrementing memory address and a general-purpose register with R15 base as its operands.")
  (masque "0100NNNN.11110101" ;; movml.l @R15+,Rn
          (n (gprix op1))))

(readops movml-l (word read) ;; movml.l @R15+,Rn
  (unmasque "0100NNNN.11110101" word (n)
    (list :movml :l '(@+ :r15) (drv-gpr n))))

(specops movmu (w op0 op1)
  ((:for-types :sh2a))
  (assert (and (eq w :l) (match-types op0 op1  gpr mas-predecr)
               (eq (mas-displ op1) :r15))
          (op0) "MOVMU only works at width L and takes only a general-purpose register and pre-decrementing memory address with R15 base as its operands.")
  (masque "0100MMMM.11110000" ;; movmu.l Rm,@-R15
          (m (gprix op0))))

(readops movmu.l (word read) ;; movmu.l Rm,@-R15
  (unmasque "0100MMMM.11110000" word (m)
    (list :movmu :l (drv-gpr m) '(-@ :r15))))

(specops movmu.l (w op0 op1)
  ((:for-types :sh2a))
  (assert (and (eq w :l) (match-types op0 op1  mas-postinc gpr)
               (eq (mas-displ op0) :r15))
          (op0) "MOVML only works at width L and takes only a post-incrementing memory address and a general-purpose register with R15 base as its operands.")
  (masque "0100NNNN.11110100" ;; movmu.l @R15+,Rn
           (n (gprix op0))))

(readops movmu.l (word read) ;; movmu.l @R15+,Rn
  (unmasque "0100NNNN.11110100" word (n)
    (list :movmu :l '(@+ :r15) (drv-gpr n))))

(specops movrt (w op0 op1)
  ((:for-types :sh2a))
  (assert (match-types op0  gpr) (op0)
          "MOVRT may take only a general-purpose register as its operand.")
  (masque "0000NNNN.00111001" ;; movrt Rn
          (n (gprix op0))))

(readops movrt (word read) ;; movrt Rn
  (unmasque "0000NNNN.00111001" word (n)
    (list :movrt (drv-gpr n))))

(specops movt (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "MOVT may take only a general-purpose register as its operand.")
  (masque "0000NNNN.00101001" ;; movt Rn
          (n (gprix op0))))

(readops movt (word read) ;; movt Rn
  (unmasque "0000NNNN.00101001" word (n)
    (list :movt (drv-gpr n))))

;;; *** MOVE STOPS

(specops nott ()
  ((:for-types :sh2a))
  (masque "00000000.01101000")) ;; nott

(readops (unmasque "00000000.01101000") (read) 
         (list :nott))

;; (specops swap (w op0 op1)
;;   ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
;;   (address (op0 op1) ((index0) (index1))
;; *** FIX THIS

(specops bandnot (w op0 op1)
  ((:for-types :sh2a))
  (if (and (eq w :b) (match-types op0 op1 op2  integer mas-gb+rzro))
      (masque "0011NNNN.0III1001.1100DDDD.DDDDDDDD" ;; bandnot.b #imm3,@(disp12,Rn)
              (n (gprix (mas-base op1))) (i op0) (d (mas-displ op1)))
      (error "BANDNOT can only be called at width B and take an immediate value and base+displacement memory access as operands.")))

(readops bandnot (word read)
  (unmasque "0011NNNN.0III1001.1100DDDD.DDDDDDDD" word (n i d) 
    (list :bandnot :b i (list '@> (drv-gpr n) d))))

(specops bclr (op0 op1 &optional op2)
  ((:for-types :sh2a))
  (cond ((match-types op1 op2  gpr mas-bs+disp)
         (if (eq op0 :b)
             (masque "0011NNNN.0III1001.0000DDDD.DDDDDDDD" ;; bclr.b #imm3,@(disp12,Rn)
                     (gprix (mas-base op2)) (i op1) (d (mas-displ op2)))
             (error "BCLR can only be called with a displacement operand at width B.")))
        ((match-types op0 op1  integer gpr)
         (masque "10000110.NNNN0III" ;; bclr #imm3,Rn
                 (n (gprix op1)) (i op0)))
        (t (error "Invalid operands passed to BCLR."))))

(readops bclr-b (word read) ;; bclr.b #imm3,@(disp12,Rn)
  (unmasque "0011NNNN.0III1001.0000DDDD.DDDDDDDD" word (n i d)
    (list :bclr :b i (list '@> (drv-gpr n) d))))

(readops bclr (word read) ;; bclr #imm3,Rn
  (unmasque "10000110.NNNN0III" word (n i)
    (list :bclr i (drv-gpr n))))

(specops bld (op0 op1 &optional op2)
  ((:for-types :sh2a))
  (cond ((match-types op1 op2  integer mas-bs+disp)
         (if (eq op0 :b)
             (masque "0011NNNN.0III1001.0011DDDD.DDDDDDDD" ;; bld.b #imm3,@(disp12,Rn)
                     (gprix (mas-base op2)) (i op1) (d (mas-displ op2)))
             (error "BLD can only be called with a displacement operand at width B.")))
        ((match-types op0 op1  integer gpr)
         (masque "10000111.NNNN1III" ;; bld #imm3,Rn
                 (n (gprix op1)) (i op0)))
        (t (error "Invalid operands passed to BLD."))))

(readops bld-b (word read) ;; bld.b #imm3,@(disp12,Rn)
  (unmasque "0011NNNN.0III1001.0011DDDD.DDDDDDDD" word (n i d)
    (list :bld :b i (list '@> (drv-gpr n) d))))

(readops bld (word read) ;; bld #imm3,Rn
  (unmasque "10000111.NNNN1III" word (n)
    (list :bld i (drv-gpr n))))

(specops bldnot (w op1 op2)
  ((:for-types :sh2a))
  (cond ((match-types op1 op2  gpr mas-bs+disp)
         (if (eq w :b)
             (masque "0011NNNN.0III1001.1011DDDD.DDDDDDDD" ;; bldnot.b #imm3,@(disp12,Rn)
                     (gprix (mas-base op1)) (i op0) (d (mas-displ op1)))
             (error "BLDNOT can only be called with a displacement operand at width B.")))
        (t (error "Invalid operands passed to BLDNOT."))))

(readops bldnot-b (word read) ;; bldnot.b #imm3,@(disp12,Rn)
  (unmasque "0011NNNN.0III1001.1011DDDD.DDDDDDDD" word ()
    (list :bldnot :b i (list '@> (drv-gpr n) d))))

(specops bor (w op1 op2)
  ((:for-types :sh2a))
  (if (and (eq w :b) (match-types op1 op2  integer mas-bs+disp))
      (masque "0011NNNN.0III1001.0101DDDD.DDDDDDDD" ;; bor.b #imm3,@(disp12,Rn)
              (gprix (mas-base op1)) (i op0) (d (mas-displ op1)))
      (error "BOR can only be called at width B with an immediate value and base+displacement memory access as its operands.")))

(readops bor-b (word read) ;; bor.b #imm3,@(disp12,Rn)
  (unmasque "0011NNNN.0III1001.0101DDDD.DDDDDDDD" word (n i d)
    (list :bor :b i (list '@> (drv-gpr n) d))))

(specops bornot (w op1 op2)
  ((:for-types :sh2a))
  (if (and (eq w :b) (match-types op1 op2  integer mas-bs+disp))
      (masque "0011NNNN.0III1001.1101DDDD.DDDDDDDD" ;; bornot.b #imm3,@(disp12,Rn)
              (gprix (mas-base op1)) (i op0) (d (mas-displ op1)))
      (error "BORNOT can only be called at width B with an immediate value and base+displacement memory access as its operands.")))

(readops bornot-b (word read) ;; bornot.b #imm3,@(disp12,Rn)
  (unmasque "0011NNNN.0III1001.0101DDDD.DDDDDDDD" word (n i d)
    (list :bornot :b i (list '@> (drv-gpr n) d))))

(specops bset (op0 op1 &optional op2)
  ((:for-types :sh2a))
  (cond ((match-types op1 op2  integer mas-bs+disp)
         (if (eq op0 :b)
             (masque "0011NNNN.0III1001.0001DDDD.DDDDDDDD" ;; bset.b #imm3,@(disp12,Rn)
                     (gprix (mas-base op2)) (i op1) (d (mas-displ op2)))
             (error "BSET can only be called with an immediate value and base+displacement memory access as its operands at width B.")))
        ((match-types op0 op1  integer gpr)
         (masque "10000110.NNNN1III" ;; bset #imm3,Rn
                 (n (gprix op1)) (i op0)))
        (t (error "Invalid operands passed to BSET."))))

(readops bset-b (word read) ;; bset.b #imm3,@(disp12,Rn)
  (unmasque "0011NNNN.0III1001.0001DDDD.DDDDDDDD" word (n i d)
    (list :bset :b i (list '@> (drv-gpr n) d))))

(readops bset (word read) ;; bset #imm3,Rn
  (unmasque "10000110.NNNN1III" word (n)
    (list :bset i (drv-gpr n))))

(specops bst (op0 op1 &optional op2)
  ((:for-types :sh2a))
  (cond ((match-types op1 op2  integer mas-bs+disp)
         (if (eq op0 :b)
             (masque "0011NNNN.0III1001.0010DDDD.DDDDDDDD" ;; bst.b #imm3,@(disp12,Rn)
                     (gprix (mas-base op2)) (i op1) (d (mas-displ op2)))
             (error "BST can only be called with an immediate value and base+displacement memory access as its operands at width B.")))
        ((match-types op0 op1  integer gpr)
         (masque "10000111.NNNN0III" ;; bst #imm3,Rn
                 (n (gprix op1)) (i op0)))
        (t (error "Invalid operands passed to BSET."))))

(readops bst-b (word read) ;; bst.b #imm3,@(disp12,Rn)
  (unmasque "0011NNNN.0III1001.0010DDDD.DDDDDDDD" word (n i d)
    (list :bst :b i (list '@> (drv-gpr n) d))))

(readops bst (word read) ;; bst #imm3,Rn
  (unmasque "10000111.NNNN0III" word (n)
    (list :bst i (drv-gpr n))))

(specops bxor (w op0 op1)
  ((:for-types :sh2a))
  (cond ((match-types op1 op2  gpr mas-bs+disp)
         (if (eq w :b)
             (masque "0011NNNN.0III1001.0110DDDD.DDDDDDDD" ;; bxor.b #imm3,@(disp12,Rn)
                     (gprix (mas-base op1)) (i op0) (d (mas-displ op1)))
             (error "BXOR can only be called with a displacement operand at width B.")))
        (t (error "Invalid operands passed to BXOR."))))

(readops bxor-b (word read) ;; bxor.b #imm3,@(disp12,Rn)
  (unmasque "0011NNNN.0III1001.0110DDDD.DDDDDDDD" word (n i d)
    (list :bxor :b i (list '@> (drv-gpr n) d))))

;; *** BITS END ***

(specops add (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (cond ((match-types op0 op1  gpr gpr)
         (masque "0011NNNN.MMMM1100" ;; add Rm,Rn
                 (n (gprix op1)) (m (gprix op0))))
        ((match-types op0 op1  integer gpr)
         (masque "0111NNNN.IIIIIIII" ;; add #imm,Rn
                 (n (gprix op1)) (i op0)))))

(readops add (word read) ;; add Rm,Rn
  (unmasque "0011NNNN.MMMM1100" word (n m)
    (list :add (drv-gpr m) (drv-gpr n))))

(readops add (word read) ;; add #imm,Rn
  (unmasque "0111NNNN.IIIIIIII" word (n i)
    (list :add i (drv-gpr n))))

(specops addc (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0011NNNN.MMMM1110" ;; addc Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops addc (word read) ;; addc Rm,Rn
  (unmasque "0011NNNN.MMMM1110" word (n m)
    (list :addc (drv-gpr m) (drv-gpr n))))

(specops addv (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0011NNNN.MMMM1111" ;; addv Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops addv (word read)
  (unmasque "0011NNNN.MMMM1111" word (n m)
    (list :addv (drv-gpr m) (drv-gpr n))))

(specops cmp/eq (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (cond ((and (eq op1 :r0) (typep op0 'integer))
         (masque "10001000.IIIIIIII" ;; cmp/eq #imm,R0
                 (i op0)))
        ((match-types op0 op1  gpr gpr)
         (masque "0011NNNN.MMMM0000" ;; cmp/eq Rm,Rn
                 (n (gprix op1)) (m (gprix op0))))))

(readops cmp/eq (word read) ;; cmp/eq #imm,R0
  (unmasque "10001000.IIIIIIII" word (i)
    (list :cmp/eq i)))

(readops cmp/eq (word read) ;; cmp/eq Rm,Rn
  (unmasque "0011NNNN.MMMM0000" word (n m)
    (list :cmp/eq (drv-gpr m) (drv-gpr n))))

(specops cmp/hs (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0011NNNN.MMMM0010" ;; cmp/hs Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops cmp/hs (word read) ;; cmp/hs Rm,Rn
  (unmasque "0011NNNN.MMMM0010" word (n m)
    (list :cmp/hs (drv-gpr m) (drv-gpr n))))

(specops cmp/ge (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0011NNNN.MMMM0011" ;; cmp/ge Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops cmp/ge (word read) ;; cmp/ge Rm,Rn
  (unmasque "0011NNNN.MMMM0011" word (n m)
    (list :cmp/ge (drv-gpr m) (drv-gpr n))))

(specops cmp/hi (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0011NNNN.MMMM0110" ;; cmp/hi Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops cmp/hi (word read) ;; cmp/hi Rm,Rn
  (unmasque "0011NNNN.MMMM0110" word (n m)
    (list :cmp/hi (drv-gpr m) (drv-gpr n))))

(specops cmp/gt (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0011NNNN.MMMM0111" ;; cmp/gt Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops cmp/gt (word read)
  (unmasque "0011NNNN.MMMM0111" word (n m)
    (list :cmp/gt (drv-gpr m) (drv-gpr n))))

(specops cmp/pl (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0100NNNN.00010101" ;; cmp/pl Rn
          (n (gprix op0))))

(readops cmp/pl (word read) ;; cmp/pl Rn
  (unmasque "0100NNNN.00010101" word (n)
    (list :cmp/pl (drv-gpr n))))

(specops cmp/pz (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0100NNNN.00010001" ;; cmp/pz Rn
          (n (gprix op0))))

(readops cmp/pz (word read) ;; cmp/pz Rn
  (unmasque "0100NNNN.00010001" word (n)
    (list :cmp/pz (drv-gpr n))))

(specops cmp/str (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "0010NNNN.MMMM1100" ;; cmp/str Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops cmp/str (word read) ;; cmp/str Rm,Rn
  (unmasque "0010NNNN.MMMM1100" word (n m)
    (list :cmp/str (drv-gpr m) (drv-gpr n))))

(specops clips (w op0)
  ((:for-types :sh2a))
  (assert (match-types op0  gpr) (op0)
          "CLIPS may take only a general-purpose register as its operand.")
  (case w
    (:b (masque "0100NNNN.10010001" ;; clips.b Rn
                (n (gprix op0))))
    (:w (masque "0100NNNN.10010101" ;; clips.w Rn
                (n (gprix op0))))
    (t  (error "CLIPS may only operate at widths B or W."))))

(readops clips-b (word read)
  (unmasque "0100NNNN.10010001" word (n)
    (list :clips :b (drv-gpr n))))

(readops clips-w (word read)
  (unmasque "0100NNNN.10010101" word (n)
    (list :clips :w (drv-gpr n))))

(specops clipu (w op0)
  ((:for-types :sh2a))
  (assert (match-types op0  gpr) (op0)
          "CLIPU may take only a general-purpose register as its operand.")
  (case w
    (:b (masque "0100NNNN.10000001" ;; clipu.b Rn
                (n (gprix op0))))
    (:w (masque "0100NNNN.10000101" ;; clipu.w Rn
                (n (gprix op0))))
    (t  (error "CLIPU may only operate at widths B or W."))))

(readops clipu.b (word read) ;; clipu.b Rn
  (unmasque "0100NNNN.10000001" word (n)
    (list :clipu :b (drv-gpr n))))

(readops clipu.w (word read) ;; clipu.w Rn
  (unmasque "0100NNNN.10000101" word (n)
    (list :clipu :w (drv-gpr n))))

(specops div0s (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "DIV0S may take only general-purpose registers as its operands.")
  (masque "0010NNNN.MMMM0111" ;; div0s Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops div0s (word read) ;; div0s Rm,Rn
  (unmasque "0010NNNN.MMMM0111" word (m n)
    (list :div0s (drv-gpr m) (drv-gpr n))))

(specops div0u ()
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "00000000.00011001")) ;; div0u

(readop (masque "00000000.00011001") (read) ;; div0u
  (list :div0u))

(specops div1 (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "DIV1 may take only general-purpose registers as its operands.")
  (masque "0011NNNN.MMMM0100" ;; div1 Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops div1 (word read) ;; div1 Rm,Rn
  (unmasque "0011NNNN.MMMM0100" word (n m)
    (list :div1 (drv-gpr m) (drv-gpr n))))

(specops divs (op0 op1)
  ((:for-types :sh2a))
  (assert (and (eq :r0 op0) (match-types op1  gpr)) (op0 op1)
          "DIVS may take only R0 and a general-purpose register as operands.")
  (masque "0100NNNN.10010100" ;; divs R0,Rn
            (n (gprix op1))))

(readops divs (word read)
  (unmasque "0100NNNN.10010100" word (n)
    (list :divs :r0 (drv-gpr n))))

(specops divu (op0 op1)
  ((:for-types :sh2a))
  (assert (and (eq :r0 op0) (match-types op1  gpr)) (op0 op1)
          "DIVU may take only R0 and a general-purpose register as operands.")
  (masque "0100NNNN.10000100" ;; divu R0,Rn
            (n (gprix op1))))

(readops divu (word read)
  (unmasque "0100NNNN.10000100" word (n)
    (list :divu :r0 (drv-gpr n))))

(specops dmuls (w op0 op1)
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (and (eq w :l) (match-types op0 op1  gpr gpr)) (op0 op1)
          "DMULS works only at width L and may take only general-purpose registers as its operands.")
  (masque "0011NNNN.MMMM1101" ;; dmuls.l Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops dmuls-l (word read) ;; dmuls.l Rm,Rn
  (unmasque "0011NNNN.MMMM1101" word (n m)
    (list :dmuls :l (drv-gpr m) (drv-gpr n))))

(specops dmulu (w op0 op1)
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (and (eq w :l) (match-types op0 op1  gpr gpr)) (op0 op1)
          "DMULU works only at width L and may take only general-purpose registers as its operands.")
  (masque "0011NNNN.MMMM0101" ;; dmulu.l Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops dmulu-l (word read)
  (unmasque "0011NNNN.MMMM0101" word (n m)
    (list :dmulu :l (drv-gpr m) (drv-gpr n))))

(specops dt (op0)
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (and (match-types op0  gpr)) (op0)
          "DT may take only .")
  (masque "0100NNNN.00010000" ;; dt Rn
          (n (gprix op0))))

(readops dt (word read) ;; dt Rn
  (unmasque "0100NNNN.00010000" word (n)
    (list :dt (drv-gpr n))))

(specops exts (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "EXTS may take only general-purpose registers as its operands.")
  (case w
    (:b (masque "0110NNNN.MMMM1110" ;; exts.b Rm,Rn
                (n (gprix op1)) (m (gprix op0))))
    (:w (masque "0110NNNN.MMMM1111" ;; exts.w Rm,Rn
                (n (gprix op1)) (m (gprix op0))))
    (t  (error "EXTS may only operate at widths B or W."))))

(readops exts.b (word read)
  (unmasque "0110NNNN.MMMM1110" word (n m)
    (list :exts :b (drv-gpr m) (drv-gpr n))))

(readops exts.w (word read)
  (unmasque "0110NNNN.MMMM1111" word (n m)
    (list :exts :w (drv-gpr m) (drv-gpr n))))

(specops extu (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "EXTU may take only general-purpose registers as its operands.")
  (case w
    (:b (masque "0110NNNN.MMMM1100" ;; extu.b Rm,Rn
                (n (gprix op1)) (m (gprix op0))))
    (:w (masque "0110NNNN.MMMM1101" ;; extu.w Rm,Rn
                (n (gprix op1)) (m (gprix op0))))
    (t  (error "EXTS may only operate at widths B or W."))))

(readops extu-b (word read) ;; extu.b Rm,Rn
  (unmasque "0110NNNN.MMMM1100" word (n m)
    (list :extu :b (drv-gpr m) (drv-gpr n))))

(readops extu-w (word read) ;; extu.w Rm,Rn
  (unmasque "0110NNNN.MMMM1101" word ()
    (list :extu :w (drv-gpr m) (drv-gpr n))))

(specops mac (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  mas-postinc mas-postinc) (op0 op1)
          "MAC may take only post-incrementing memory access points as its operands.")
  (case w
    (:l (masque "0000NNNN.MMMM1111" ;; mac.l @Rm+,@Rn+
                (n (gprix op1)) (m (gprix op0))))
    (:w (masque "0100NNNN.MMMM1111" ;; mac.w @Rm+,@Rn+
                (n (gprix op1)) (m (gprix op0))))
    (t  (error "MAC may only operate at widths L or W."))))

(readops mac-l (word read) ;; mac.l @Rm+,@Rn+
  (unmasque "0000NNNN.MMMM1111" word (n m)
    (list :mac :l (drv-gpr m) (drv-gpr n))))

(readops mac-w (word read) ;; mac.w @Rm+,@Rn+
  (unmasque "0100NNNN.MMMM1111" word (n m)
    (list :mac :w (drv-gpr m) (drv-gpr n))))

(specops mul (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "MUL may take only general-purpose registers as its operands.")
  (case w
    (:l (masque "0000NNNN.MMMM0111" ;; mul.l Rm,Rn
                (n (gprix op1)) (m (gprix op0))))
    (t  (error "MUL may only operate at width L."))))

(readops mul-l (word read) ;; mul.l Rm,Rn
  (unmasque "0000NNNN.MMMM0111" word (n m)
    (list :mul :l (drv-gpr m) (drv-gpr n))))

(specops mulr (op0 op1)
  ((:for-types :sh2a))
  (assert (and (eq :r0 op0) (match-types op1  gpr)) (op0 op1)
          "MULR may take only R0 and a general-purpose register as operands.")
  (masque "0100NNNN.10000000" ;; mulr R0,Rn
          (n (gprix op1))))

(readops mulr (word read) ;; mulr R0,Rn
  (unmasque "0100NNNN.10000000" word (n)
    (list :mulr (drv-gpr n))))

(specops muls (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "MULS may take only general-purpose registers as its operands.")
  (case w
    (:w (masque "0010NNNN.MMMM1111" ;; muls.w Rm,Rn
                (n (gprix op1)) (m (gprix op0))))
    (t  (error "MULS may only operate at width L."))))

(readops muls (word read) ;; muls.w Rm,Rn
  (unmasque "0010NNNN.MMMM1111" word (n m)
    (list :muls :w (drv-gpr m) (drv-gpr n))))

(specops mulu (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "MULS may take only general-purpose registers as its operands.")
  (case w
    (:w (masque "0010NNNN.MMMM1110" ;; mulu.w Rm,Rn
                (n (gprix op1)) (m (gprix op0))))
    (t  (error "MULU may only operate at width L."))))

(readops mulu-w (word read) ;; mulu.w Rm,Rn
  (unmasque "0010NNNN.MMMM1110" word (n m)
    (list :mulu :w (drv-gpr m) (drv-gpr n))))

(specops neg (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "NEG may take only general-purpose registers as its operands.")
  (masque "0110NNNN.MMMM1011" ;; neg Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops neg (word read) ;; neg Rm,Rn
  (unmasque "0110NNNN.MMMM1011" word (n m)
    (list :neg (drv-gpr m) (drv-gpr n))))

(specops negc (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "NEGC may take only general-purpose registers as its operands.")
  (masque "0110NNNN.MMMM1010" ;; negc Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops negc (word read) ;; negc Rm,Rn
  (unmasque "0110NNNN.MMMM1010" word (n m)
    (list :negc (drv-gpr m) (drv-gpr n))))

(specops sub (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "SUB may take only general-purpose registers as its operands.")
  (masque "0011NNNN.MMMM1000" ;; sub Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops sub (word read)
  (unmasque "0011NNNN.MMMM1000" word (n m)
    (list :sub (drv-gpr m) (drv-gpr n))))

(specops subc (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "SUBC may take only general-purpose registers as its operands.")
  (masque "0011NNNN.MMMM1010" ;; subc Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops subc (word read) ;; subc Rm,Rn
  (unmasque "0011NNNN.MMMM1010" word (n m)
    (list :subc (drv-gpr m) (drv-gpr n))))

(specops subv (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "SUBV may take only general-purpose registers as its operands.")
  (masque "0011NNNN.MMMM1011" ;; subv Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops subv (word read) ;; subv Rm,Rn
  (unmasque "0011NNNN.MMMM1011" word (n m)
    (list :subv (drv-gpr m) (drv-gpr n))))

;;; *** ARITHMETIC ENDS - LOGIC BEGINS

(specops and (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (cond ((match-types op0 op1  gpr gpr)
         (masque "0010NNNN.MMMM1001" ;; and Rm,Rn
                 (n (gprix op1)) (m (gprix op0))))
        ((and (eq op1 :r0) (typep op0 'integer))
         (masque "11001001.IIIIIIII" ;; and #imm,R0
                 (i op0)))))

(readops and (word read) ;; and Rm,Rn
  (unmasque "0010NNNN.MMMM1001" word (n m)
    (list :and (drv-gpr m) (drv-gpr n))))

(readops and (word read) ;; and #imm,R0
  (unmasque "11001001.IIIIIIII" word (i)
    (list :and i :r0)))

(specops andb (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (and (eq w :b) (match-types op0 op1  integer mas-gb+rzro)) (op0 op1)
          "ANDB may take only an immediate value and a (@gbr0) memory access point as operands.")
  (masque "11001101.IIIIIIII" ;; and.b #imm,@(R0,GBR)
          (i op0)))

(readops andb (word read) ;; and.b #imm,@(R0,GBR)
  (unmasque "11001101.IIIIIIII" word (i)
    (list :and :b i '(@gbr0))))

(specops not (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "NOT may take only general-purpose registers as its operands.")
  (masque "0110NNNN.MMMM0111" ;; not Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops not (word read)
  (unmasque "0110NNNN.MMMM0111" word (n m)
    (list :not (drv-gpr m) (drv-gpr n))))

(specops or (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (cond ((match-types op0 op1  gpr gpr)
         (masque "0010NNNN.MMMM1011" ;; or Rm,Rn
                 (n (gprix op1)) (m (gprix op0))))
        ((and (eq op1 :r0) (typep op0 'integer))
         (masque "11001011.IIIIIIII" ;; or #imm,R0
                 (i op0)))))

(readops or (word read) ;; or Rm,Rn
  (unmasque "0010NNNN.MMMM1011" word (n m)
    (list :or (drv-gpr m) (drv-gpr n))))

(readops or (word read) ;; or #imm,R0
  (unmasque "11001011.IIIIIIII" word (i)
    (list :or i :r0)))

(specops orb (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (and (eq w :b) (match-types op0 op1  integer mas-gb+rzro)) (op0 op1)
          "ORB may take only an immediate value and a (@gbr0) memory access point as operands.")
  (masque "11001111.IIIIIIII" ;; or.b #imm,@(R0,GBR)
          (i op0)))

(readops or.b (word read) ;; or.b #imm,@(R0,GBR)
  (unmasque "11001111.IIIIIIII" word (i)
    (list :orb i '(@gbr0))))

(specops tas (w op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (and (eq w :b) (match-types op0  mas-simple)) (op0)
          "TAS only operates at width B and may take only a plain memory access scheme as its operand.")
  (masque "0100NNNN.00011011" ;; tas.b @Rn
          (gprix (mas-base op0))))

(readops tas.b (word read)
  (unmasque "0100NNNN.00011011" word (n)
    (list :tas :b (list '@ (drv-gpr n)))))

(specops tst (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (cond ((match-types op0 op1  gpr gpr)
         (masque "0010NNNN.MMMM1000" ;; tst Rm,Rn
                 (n (gprix op1)) (m (gprix op0))))
        ((and (eq op1 :r0) (typep op0 'integer))
         (masque "11001000.IIIIIIII" ;; or #imm,R0
                 (i op0)))))

(readops tst (word read) ;; tst Rm,Rn
  (unmasque "0010NNNN.MMMM1000" word (n m)
    (list :tst (drv-gpr m) (drv-gpr n))))

(readops tst (word read) ;; or #imm,R0
  (unmasque "11001000.IIIIIIII" word (i)
    (list :tst i :r0)))

(specops tstb (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (and (eq w :b) (match-types op0 op1  integer mas-gb+rzro)) (op0 op1)
          "TSTB may take only an immediate value and a (@gbr0) memory access point as operands.")
  (masque "11001100.IIIIIIII" ;; tst.b #imm,@(R0,GBR)
          (i op0)))

(readops tstb (word read)
  (unmasque "11001100.IIIIIIII" word (i)
    (list :tstb i '(@gbr0))))

(specops tst (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (cond ((match-types op0 op1  gpr gpr)
         (masque "0010NNNN.MMMM1000" ;; tst Rm,Rn
                 (n (gprix op1)) (m (gprix op0))))
        ((and (eq op1 :r0) (typep op0 'integer))
         (masque "11001000.IIIIIIII" ;; or #imm,R0
                 (i op0)))))

(specops xor (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (cond ((match-types op0 op1  gpr gpr)
         (masque "0010NNNN.MMMM1010" ;; xor Rm,Rn
                 (n (gprix op1)) (m (gprix op0))))
        ((and (eq op1 :r0) (typep op0 'integer))
         (masque "11001010.IIIIIIII" ;; or #imm,R0
                 (i op0)))))

(readops xor (word read) ;; xor Rm,Rn
  (unmasque "0010NNNN.MMMM1010" word (n m)
    (list :xor (drv-gpr m) (drv-gpr n))))

(readops xor (word read) ;; or #imm,R0
  (unmasque "11001010.IIIIIIII" word (i)
    (list :xor i :r0)))

(specops xorb (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (and (eq w :b) (match-types op0 op1  integer mas-gb+rzro)) (op0 op1)
          "XORB may take only an immediate value and a (@gbr0) memory access point as operands.")
  (masque "11001110.IIIIIIII" ;; xor.b #imm,@(R0,GBR)
          (i op0)))

(readops xorb (word read)
  (unmasque "11001110.IIIIIIII" word (i)
    (list :xorb i '(@gbr0))))

;;; *** LOGICAL OPS END, SHIFTS BEGIN

(specops rotcl (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "ROTCL may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00100100" ;; rotcl Rn
            (n (gprix op0))))

(readops rotcl (word read) ;; rotcl Rn
  (unmasque "0100NNNN.00100100" word (n)
    (list :rotcl (drv-gpr n))))

(specops rotcr (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "ROTCR may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00100101" ;; rotcr Rn
            (n (gprix op0))))

(readops rotcr (word read) ;; rotcr Rn
  (unmasque "0100NNNN.00100101" word (n)
    (list :rotcr (drv-gpr n))))

(specops rotl (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "ROTL may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00000100" ;; rotl Rn
            (n (gprix op0))))

(readops rotl (word read) ;; rotl Rn
  (unmasque "0100NNNN.00000100" word (n)
    (list :rotl (drv-gpr n))))

(specops rotr (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "ROTR may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00000101" ;; rotr Rn
            (n (gprix op0))))

(readops rotr (word read) ;; rotr Rn
  (unmasque "0100NNNN.00000101" word (n)
    (list :rotr (drv-gpr n))))

(specops shad (op0 op1)
  ((:for-types :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "SHAD may take only general-purpose registers as its operands.")
  (masque "0100NNNN.MMMM1100" ;; shad Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops shad (word read) ;; shad Rm,Rn
  (unmasque "0100NNNN.MMMM1100" word (n m)
    (list :shad (drv-gpr m) (drv-gpr n))))

(specops shal (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHAL may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00100000" ;; shal Rn
            (n (gprix op0))))

(readops shal (word read) ;; shal Rn
  (unmasque "0100NNNN.00100000" word (n)
    (list :shal (drv-gpr n))))

(specops shar (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHAR may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00100001" ;; shal Rn
            (n (gprix op0))))

(readops shar (word read) ;; shal Rn
  (unmasque "0100NNNN.00100001" word (n)
    (list :shar (drv-gpr n))))

(specops shld (op0 op1)
  ((:for-types :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0 op1  gpr gpr) (op0 op1)
          "SHLD may take only general-purpose registers as its operands.")
  (masque "0100NNNN.MMMM1101" ;; shld Rm,Rn
          (n (gprix op1)) (m (gprix op0))))

(readops shld (word read) ;; shld Rm,Rn
  (unmasque "0100NNNN.MMMM1101" word (n m)
    (list :shld (drv-gpr m) (drv-gpr n))))

(specops shll (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHLL may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00000000" ;; shll Rn
            (n (gprix op0))))

(readops shll (word read) ;; shll Rn
  (unmasque "0100NNNN.00000000" word (n)
    (list :shll (drv-gpr n))))

(specops shll2 (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHLL2 may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00001000" ;; shll2 Rn
          (n (gprix op0))))

(readops shll2 (word read) ;; shll2 Rn
  (unmasque "0100NNNN.00001000" word (n)
    (list :shll2 (drv-gpr n))))

(specops shll8 (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHLL8 may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00011000" ;; shll8 Rn
          (n (gprix op0))))

(readops shll8 (word read) ;; shll8 Rn
  (unmasque "0100NNNN.00011000" word (n)
    (list :shll8 (drv-gpr n))))

(specops shll16 (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHLL16 may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00101000" ;; shll16 Rn
          (n (gprix op0))))

(readops shll16 (word read) ;; shll16 Rn
  (unmasque "0100NNNN.00101000" word (n)
    (list :shll16 (drv-gpr n))))

(specops shlr (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHLR may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00000001" ;; shlr Rn
          (n (gprix op0))))

(readops shlr (word read) ;; shlr Rn
  (unmasque "0100NNNN.00000001" word (n)
    (list :shlr (drv-gpr n))))

(specops shlr2 (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHLR2 may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00001001" ;; shlr2 Rn
          (n (gprix op0))))

(readops shlr2 (word read) ;; shlr2 Rn
  (unmasque "0100NNNN.00001001" word (n)
    (list :shlr2 (drv-gpr n))))

(specops shlr8 (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHLR8 may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00011001" ;; shlr8 Rn
          (n (gprix op0))))

(readops shlr8 (word read) ;; shlr8 Rn
  (unmasque "0100NNNN.00011001" word (n)
    (list :shlr8 (drv-gpr n))))

(specops shlr16 (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "SHLR16 may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00101001" ;; shlr16 Rn
          (n (gprix op0))))

(readops shlr16 (word read) ;; shlr16 Rn
  (unmasque "0100NNNN.00101001" word (n)
    (list :shlr16 (drv-gpr n))))

;;; *** SHIFTS END, BRANCHES BEGIN

(specops bf (op0)
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

(readops bf (word read) ;; bf label
  (unmasque "10001011.DDDDDDDD" word (d)
    (list :bf d)))

(specops bf/s (op0)
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

(readops bf/s (word read) ;; bf/s label
  (unmasque "10001111.DDDDDDDD" word (d)
    (list :bf/s d)))

(specops bt (op0)
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

(readops bt (word read) ;; bt label
  (unmasque "10001001.DDDDDDDD" word (d)
    (list :bt d)))

(specops bt/s (op0)
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

(readops bt/s (word read) ;; bt/s label
  (unmasque "10001101.DDDDDDDD" word (d)
    (list :bt/s d)))

(specops bra (op0)
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

(readops bra (word read) ;; bra label
  (unmasque "1010DDDD.DDDDDDDD" word (d)
    (list :bra d)))

(specops braf (op0)
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "BRAF may take only a general-purpose register as its operand.")
  (masque "0000MMMM.00100011" ;; braf Rm
            (m (gprix op0))))

(readops braf (word read) ;; braf Rm
  (unmasque "0000MMMM.00100011" word (m)
    (list :braf (drv-gpr m))))

(specops bsr (op0)
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

(readops bsr (word read) ;; bsr label
  (unmasque "1011DDDD.DDDDDDDD" word (d)
    (list :bsr d)))

(specops bsrf (op0)
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "BRSF may take only a general-purpose register as its operand.")
  (masque "0000MMMM.00000011" ;; bsrf Rm
            (m (gprix op0))))

(readops bsrf (word read) ;; bsrf Rm
  (unmasque "0000MMMM.00000011" word (m)
    (list :bsrf (drv-gpr m))))

(specops jmp (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  mas-simple) (op0)
          "JMP may only take a plain memory access scheme as its operand.")
  (masque "0100MMMM.00101011" ;; jmp @Rm
          (m (gprix (mas-base op0)))))

(readops jmp (word read) ;; jmp @Rm
  (unmasque "0100MMMM.00101011" word (m)
    (list :jmp (list '@ (drv-gpr m)))))

(specops jsr (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  mas-simple) (op0)
          "JMP may only take a plain memory access scheme as its operand.")
  (masque "0100MMMM.00001011" ;; jsr @Rm
          (m (gprix (mas-base op0)))))

(readops jsr (word read) ;; jsr @Rm
  (unmasque "0100MMMM.00001011" word (m)
    (list :jsr (list '@ (drv-gpr m)))))

(specops jsr/n (w op0 op1)
  ((:for-types :sh2a))
  (cond ((match-types op0  mas-simple)
         (masque "0100MMMM.01001011" ;; jsr/n @Rm
                 (m (gprix (mas-base op0)))))
        ((match-types op0  mas-tb+dis4)
         (masque "10000011.DDDDDDDD" ;; jsr/n @@(disp8,TBR)
                 (d (mas-displ op0))))
        (t (error "JSR/N may only take a plain memory access scheme or a TBR displacement memory access as its operand."))))

(readops jsr/n (word read) ;; jsr/n @Rm
  (unmasque "0100MMMM.01001011" word (m)
    (list :jsr/n (list '@ (drv-gpr m)))))

(readops jsr/n (word read) ;; jsr/n @@(disp8,TBR)
  (unmasque "10000011.DDDDDDDD" word (d)
    (list :jsr/n (list '@@tbr d))))

(specops rts ()
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "00000000.00001011")) ;; rts

(readops rts (word read) ;; rts
  (unmasque "00000000.00001011" word ()
    (list :rts)))

(specops rts/n (w op0 op1)
  ((:for-types :sh2a))
  (masque "00000000.01101011")) ;; rts/n

(readops rts/n (word read) ;; rts/n
  (unmasque "00000000.01101011" word ()
    (list :rts/n)))

(specops rtv/n (w op0 op1)
  ((:for-types :sh2a))
  (assert (match-types op0  gpr) (op0)
          "RTV/N may take only a general-purpose register as its operand.")
  (masque "0000MMMM.01111011" ;; rtv/n Rm
          (m (gprix op0))))

(readops rtv/n (word read)
  (unmasque "0000MMMM.01111011" word (m)
    (list :rtv/n (drv-gpr m))))

;;; *** BRANCHES END, SYSTEM INSTRUCTIONS BEGIN

(specops clrmac ()
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "00000000.00101000")) ;; clrmac

(readops (unmasque "00000000.00101000") (read)
         (list :clrmac))

(specops clrs (w op0 op1)
  ((:for-types :sh3 :sh4 :sh4a))
  (masque "00000000.01001000")) ;; clrs

(readops (unmasque "00000000.01001000") (read)
         (list :clrs))

(specops clrt (w op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "00000000.00001000")) ;; clrt

(readops (unmasque "00000000.00001000") (read)
    (list :clrt))

(specops icbi (op0)
  ((:for-types :sh4a))
  (assert (match-types op0  gpr) (op0)
          "ICBI may take only a general-purpose register as its operand.")
  (masque "0000NNNN.11100011" ;; icbi @Rn
          (n (gprix op0))))

(readops icbi (word read)
  (unmasque "0000NNNN.11100011" word (n)
    (list :icbi (drv-gpr n))))

(specops ldbank (w op0 op1)
  ((:for-types :sh2a))
  (assert (and (eq op1 :r0) (match-types op0  mas-simple)) (op0)
          "LDBANK takes only a simple memory address and R0 as its operands.")
  (masque "0100MMMM.11100101" ;; ldbank @Rm,R0
          (m (gprix (mas-base op0)))))

(readops ldbank (word read)
  (unmasque "0100MMMM.11100101" word (m)
    (list :ldbank (list '@ (drv-gpr m)) :r0)))
    
(specops ldc (op0 op1 &optional op2)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :dsp :privileged))
  (typecase op0
    (gpr
     (case op1
       (:sr
        (masque "0100MMMM.00001110" ;; ldc Rm,SR
                (m (gprix op0))))
       (:tbr (assert (matching-types                           :sh2a) ()
                     "LDC Rm,TBR incompatible with this architecture type.")
        (masque "0100MMMM.01001010" ;; ldc Rm,TBR
                (m (gprix op0))))
       (:gbr (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                     "LDC Rm,GBR incompatible with this architecture type.")
        (masque "0100MMMM.00011110" ;; ldc Rm,GBR
                (m (gprix op0))))
       (:vbr
        (masque "0100MMMM.00101110" ;; ldc Rm,VBR
                (m (gprix op0))))
       (:mod (assert (matching-types :dsp) ()
                     "LDC Rm,MOD incompatible with this architecture type.")
        (masque "0100MMMM.01011110" ;; ldc Rm,MOD
                (m (gprix op0))))
       (:re  (assert (matching-types :dsp) ()
                     "LDC Rm,RE incompatible with this architecture type.")
        (masque "0100MMMM.01111110" ;; ldc Rm,RE
                (m (gprix op0))))
       (:rs  (assert (matching-types :dsp) ()
                     "LDC Rm,RS incompatible with this architecture type.")
        (masque "0100MMMM.01101110" ;; ldc Rm,RS
                (m (gprix op0))))
       (:sgr (assert (matching-types           :sh4a :privileged) ()
                     "LDC Rm,SGR incompatible with this architecture type.")
        (masque "0100MMMM.00111010" ;; ldc Rm,SGR
                (m (gprix op0))))
       (:ssr (assert (matching-types :sh3 :sh4 :sh4a :privileged) ()
                     "LDC Rm,SSR incompatible with this architecture type.")
        (masque "0100MMMM.00111110" ;; ldc Rm,SSR
                (m (gprix op0))))
       (:spc (assert (matching-types :sh3 :sh4 :sh4a :privileged) ()
                     "LDC Rm,SPC incompatible with this architecture type.")
        (masque "0100MMMM.01001110" ;; ldc Rm,SPC
                (m (gprix op0))))
       (:dbr (assert (matching-types      :sh4 :sh4a :privileged) ()
                     "LDC Rm,DBR incompatible with this architecture type.")
        (masque "0100MMMM.11111010" ;; ldc Rm,DBR
                (m (gprix op0))))
       (t (if (typep op1 'gprb)
              (progn (assert (matching-types :sh3 :sh4 :sh4a :privileged) ()
                             "LDC Rm,Rn_BANK is incompatible with this architecture type.")
                     (masque "0100MMMM.1NNN1110" ;; ldc Rm,Rn_BANK
                             (m (gprix op0)) (n (gprbix op1))))
              (error "Invalid operands for LDC.")))))
    (mas-postinc
     (if (not (eq op0 :l))
         (error "LDC only works at width L.")
         (case op2
           (:sr
            (masque "0100MMMM.00000111" ;; ldc.l @Rm+,SR
                    (m (gprix (mas-base op1)))))
           (:gbr (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                         "LDC.L @Rm+,GBR incompatible with this architecture type.")
            (masque "0100MMMM.00010111" ;; ldc.l @Rm+,GBR
                    (m (gprix (mas-base op1)))))
           (:vbr
            (masque "0100MMMM.00100111" ;; ldc.l @Rm+,VBR
                    (m (gprix (mas-base op1)))))
           (:mod (assert (matching-types :dsp) ()
                         "LDC.L @Rm+,MOD incompatible with this architecture type.")
            (masque "0100MMMM.01010111" ;; ldc.l @Rm+,MOD
                    (m (gprix (mas-base op1)))))
           (:re  (assert (matching-types :dsp) ()
                         "LDC.L @Rm+,RE incompatible with this architecture type.")
            (masque "0100MMMM.01110111" ;; ldc.l @Rm+,RE
                    (m (gprix (mas-base op1)))))
           (:rs  (assert (matching-types :dsp) ()
                         "LDC.L @Rm+,RS incompatible with this architecture type.")
            (masque "0100MMMM.01100111" ;; ldc.l @Rm+,RS
                    (m (gprix (mas-base op1)))))
           (:sgr (assert (matching-types           :sh4a :privileged) ()
                         "LDC.L @Rm+,SGR incompatible with this architecture type.")
            (masque "0100MMMM.00110110" ;; ldc.l @Rm+,SGR
                    (m (gprix (mas-base op1)))))
           (:ssr (assert (matching-types :sh3 :sh4 :sh4a :privileged) ()
                         "LDC.L @Rm+,SSR incompatible with this architecture type.")
            (masque "0100MMMM.00110111" ;; ldc.l @Rm+,SSR
                    (m (gprix (mas-base op1)))))
           (:spc (assert (matching-types      :sh4 :sh4a :privileged) ()
                         "LDC.L @Rm+,SPC incompatible with this architecture type.")
            (masque "0100MMMM.01000111" ;; ldc.l @Rm+,SPC
                    (m (gprix (mas-base op1)))))
           (:dbr (assert (matching-types      :sh4 :sh4a :privileged) ()
                         "LDC.L @Rm+,DBR incompatible with this architecture type.")
            (masque "0100MMMM.11110110" ;; ldc.l @Rm+,DBR
                    (m (gprix (mas-base op1)))))
           (t (if (typep op1 'gprb)
                  (progn (assert (matching-types :sh3 :sh4 :sh4a :privileged) ()
                                 "LDC @Rm+,Rn_BANK is incompatible with this architecture type.")
                         (masque "0100MMMM.1NNN0111" ;; ldc.l @Rm+,Rn_BANK
                                 (m (gprix op0)) (n (gprbix op1))))
                  (error "Invalid operands for LDC."))))))))
    
(readops ldc (word read) ;; ldc Rm,SR
  (unmasque "0100MMMM.00001110" word (m)
    (list :ldc (drv-gprb m) :sr)))

(readops ldc.l (word read) ;; ldc.l @Rm+,SR
  (unmasque "0100MMMM.00000111" word (m)
    (list :ldc :l (list '@+ (drv-gpr m)) :sr)))

(readops ldc (word read) ;; ldc Rm,TBR
  (unmasque "0100MMMM.01001010" word (m)
    (list :ldc (drv-gprb m) :tbr)))

(readops ldc (word read) ;; ldc Rm,GBR
  (unmasque "0100MMMM.00011110" word (m)
    (list :ldc (drv-gprb m) :gbr)))

(readops ldc.l (word read) ;; ldc.l @Rm+,GBR
  (unmasque "0100MMMM.00010111" word (m)
    (list :ldc :l (list '@+ (drv-gpr m)) :gbr)))

(readops ldc (word read) ;; ldc Rm,VBR
  (unmasque "0100MMMM.00101110" word (m)
    (list :ldc (drv-gprb m) :vbr)))

(readops ldc.l (word read) ;; ldc.l @Rm+,VBR
  (unmasque "0100MMMM.00100111" word (m)
    (list :ldc :l (list '@+ (drv-gpr m)) :vbr)))

(readops ldc (word read) ;; ldc Rm,MOD
  (unmasque "0100MMMM.01011110" word (m)
    (list :ldc (drv-gprb m) :mod)))

(readops ldc.l (word read) ;; ldc.l @Rm+,MOD
  (unmasque "0100MMMM.01010111" word (m)
    (list :ldc :l (list '@+ (drv-gpr m)) :mod)))

(readops ldc (word read) ;; ldc Rm,RE
  (unmasque "0100MMMM.01111110" word (m)
    (list :ldc (drv-gprb m) :re)))

(readops ldc.l (word read) ;; ldc.l @Rm+,RE
  (unmasque "0100MMMM.01110111" word (m)
    (list :ldc :l (list '@+ (drv-gpr m)) :re)))

(readops ldc (word read) ;; ldc Rm,RS
  (unmasque "0100MMMM.01101110" word (m)
    (list :ldc (drv-gprb m) :rs)))

(readops ldc.l (word read) ;; ldc.l @Rm+,RS
  (unmasque "0100MMMM.01100111" word (m)
    (list :ldc :l (list '@+ (drv-gpr m)) :rs)))

(readops ldc (word read) ;; ldc Rm,SGR
  (unmasque "0100MMMM.00111010" word (m)
    (list :ldc (drv-gprb m) :sgr)))

(readops ldc.l (word read) ;; ldc.l @Rm+,SGR
  (unmasque "0100MMMM.00110110" word (m)
    (list :ldc :l (list '@+ (drv-gpr m)) :sgr)))

(readops ldc (word read) ;; ldc Rm,SSR
  (unmasque "0100MMMM.00111110" word (m)
    (list :ldc (drv-gprb m) :ssr)))

(readops ldc.l (word read) ;; ldc.l @Rm+,SSR
  (unmasque "0100MMMM.00110111" word (m)
    (list :ldc :l (list '@+ (drv-gpr m)) :ssr)))

(readops ldc (word read) ;; ldc Rm,SPC
  (unmasque "0100MMMM.01001110" word (m)
    (list :ldc (drv-gprb m) :spc)))

(readops ldc.l (word read) ;; ldc.l @Rm+,SPC
  (unmasque "0100MMMM.01000111" word (m)
    (list :ldc :l (list '@+ (drv-gpr m)) :spc)))

(readops ldc (word read) ;; ldc Rm,DBR
  (unmasque "0100MMMM.11111010" word (m)
    (list :ldc (drv-gprb m) :dbr)))

(readops ldc.l (word read) ;; ldc.l @Rm+,DBR
  (unmasque "0100MMMM.11110110" word (m)
    (list :ldc :l (list '@+ (drv-gpr m)) :dbr)))

(readops ldc (word read) ;; ldc Rm,Rn_BANK
  (unmasque "0100MMMM.1NNN1110" word (m n)
    (list :ldc (drv-gprb m) (drv-gprb n))))

(readops ldc.l (word read) ;; ldc.l @Rm+,Rn_BANK
  (unmasque "0100MMMM.1NNN0111" word (m n)
    (list :ldc :l (list '@+ (drv-gpr m)) (drv-gprb n))))

(specops ldre (op0)
  ((:for-types :dsp))
  (assert (match-types op0  mas-pc+disp) ()
          "LDRE takes only a PC+displacement memory access as its operand.")
  (masque "10001110.DDDDDDDD" ;; ldre @(disp,PC)
          (d (mas-displ op0))))

(readops ldre (word read)
  (unmasque "10001110.DDDDDDDD" word (d)
    (list :ldre (list '@pc d))))

(specops ldrs (w op0 op1)
  ((:for-types :dsp))
  (assert (match-types op0  mas-pc+disp) ()
          "LDRE takes only a PC+displacement memory access as its operand.")
  (masque "10001100.DDDDDDDD" ;; ldrs @(disp,PC)
          (d (mas-displ op0))))

(readops ldrs (word read)
  (unmasque "10001100.DDDDDDDD" word (d)
    (list :ldrs (list '@pc d))))

(specops lds (op0 op1 &optional op2)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :dsp :privileged))
  (typecase op0
    (gpr
     (case op1
       (:mach (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                      "LDS Rm,MACH incompatible with this architecture type.")
        (masque "0100MMMM.00001010" ;; lds Rm,MACH
                (m (gprix op0))))
       (:macl (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                      "LDS Rm,MACL incompatible with this architecture type.")
        (masque "0100MMMM.00011010" ;; lds Rm,MACL
                (m (gprix op0))))
       (:pr   (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                      "LDS Rm,PR incompatible with this architecture type.")
        (masque "0100MMMM.00101010" ;; lds Rm,PR
                (m (gprix op0))))
       (:dsr  (assert (matching-types :dsp) ()
                      "LDS Rm,DSR incompatible with this architecture type.")
        (masque "0100MMMM.01101010" ;; lds Rm,DSR
                (m (gprix op0))))
       (:a0   (assert (matching-types :dsp) ()
                      "LDS Rm,A0 incompatible with this architecture type.")
        (masque "0100MMMM.01110110" ;; lds Rm,A0
                (m (gprix op0))))
       (:x0   (assert (matching-types :dsp) ()
                      "LDS Rm,X0 incompatible with this architecture type.")
        (masque "0100MMMM.10001010" ;; lds Rm,X0
                (m (gprix op0))))
       (:x1   (assert (matching-types :dsp) ()
                      "LDS Rm,X1 incompatible with this architecture type.")
        (masque "0100MMMM.10011010" ;; lds Rm,X1
                (m (gprix op0))))
       (:y0   (assert (matching-types :dsp) ()
                      "LDS Rm,Y0 incompatible with this architecture type.")
        (masque "0100MMMM.10101010" ;; lds Rm,Y0
                (m (gprix op0))))
       (:y1   (assert (matching-types :dsp) ()
                      "LDS Rm,Y1 incompatible with this architecture type.")
        (masque "0100MMMM.10111010" ;; lds Rm,Y1
                (m (gprix op0))))))
    (mas-postinc
     (if (not (eq op0 :l))
         (error "LDS only works at width L.")
         (case op2
           (:mach (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                          "LDS.L @Rm+,MACH incompatible with this architecture type.")
            (masque "0100MMMM.00000110" ;; lds.l @Rm+,MACH
                    (m (gprix (mas-base op1)))))
           (:macl (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                          "LDS.L @Rm+,MACL incompatible with this architecture type.")
            (masque "0100MMMM.00010110" ;; lds.l @Rm+,MACL
                    (m (gprix (mas-base op1)))))
           (:pr   (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                          "LDS.L @Rm+,PR incompatible with this architecture type.")
            (masque "0100MMMM.00100110" ;; lds.l @Rm+,PR
                    (m (gprix (mas-base op1)))))
           (:dsr  (assert (matching-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a) ()
                          "LDS.L @Rm+,DSR incompatible with this architecture type.")
            (masque "0100MMMM.01100110" ;; lds.l @Rm+,DSR
                    (m (gprix (mas-base op1)))))
           (:a0   (assert (matching-types :dsp) ()
                          "LDS.L @Rm+,A0 incompatible with this architecture type.")
            (masque "0100MMMM.01100110" ;; lds.l @Rm+,A0
                    (m (gprix (mas-base op1)))))
           (:x0   (assert (matching-types :dsp) ()
                          "LDS.L @Rm+,X0 incompatible with this architecture type.")
            (masque "0100MMMM.10000110" ;; lds.l @Rm+,X0
                    (m (gprix (mas-base op1)))))
           (:x1   (assert (matching-types :dsp) ()
                          "LDS.L @Rm+,X1 incompatible with this architecture type.")
            (masque "0100MMMM.10010110" ;; lds.l @Rm+,X0
                    (m (gprix (mas-base op1)))))
           (:y0   (assert (matching-types :dsp) ()
                          "LDS.L @Rm+,Y0 incompatible with this architecture type.")
            (masque "0100NNNN.10100110" ;; lds.l @Rm+,Y0
                    (m (gprix (mas-base op1)))))
           (:y1   (assert (matching-types :dsp) ()
                          "LDS.L @Rm+,Y1 incompatible with this architecture type.")
            (masque "0100NNNN.10110110" ;; lds.l @Rm+,Y1
                    (m (gprix (mas-base op1))))))))))

(readops lds (word read) ;; lds Rm,MACH
  (unmasque "0100MMMM.00001010" word (m)
    (list :lds (drv-gpr m) :mach)))

(readops lds.l (word read) ;; lds.l @Rm+,MACH
  (unmasque "0100MMMM.00000110" word (m)
    (list :lds :l (list '@+ (drv-gpr m)) :mach)))

(readops lds (word read) ;; lds Rm,MACL
  (unmasque "0100MMMM.00011010" word (m)
    (list :lds (drv-gpr m) :macl)))

(readops lds.l (word read) ;; lds.l @Rm+,MACL
  (unmasque "0100MMMM.00010110" word (m)
    (list :lds :l (list '@+ (drv-gpr m)) :macl)))

(readops lds (word read) ;; lds Rm,PR
  (unmasque "0100MMMM.00101010" word (m)
    (list :lds (drv-gpr m) :pr)))

(readops lds.l (word read) ;; lds.l @Rm+,PR
  (unmasque "0100MMMM.00100110" word (m)
    (list :lds :l (list '@+ (drv-gpr m)) :pr)))

(readops lds (word read) ;; lds Rm,DSR
  (unmasque "0100MMMM.01101010" word (m)
    (list :lds (drv-gpr m) :dsr)))

(readops lds.l (word read) ;; lds.l @Rm+,DSR
  (unmasque "0100MMMM.01100110" word (m)
    (list :lds :l (list '@+ (drv-gpr m)) :dsr)))

(readops lds (word read) ;; lds Rm,A0
  (unmasque "0100MMMM.01110110" word (m)
    (list :lds (drv-gpr m) :a0)))

(readops lds.l (word read) ;; lds.l @Rm+,A0
  (unmasque "0100MMMM.01110110" word (m)
    (list :lds :l (list '@+ (drv-gpr m)) :a0)))

(readops lds (word read) ;; lds Rm,X0
  (unmasque "0100MMMM.10001010" word (m)
    (list :lds (drv-gpr m) :x0)))

(readops lds.l (word read) ;; lds.l @Rm+,X0
  (unmasque "0100NNNN.10000110" word (m)
    (list :lds :l (list '@+ (drv-gpr m)) :x0)))

(readops lds (word read) ;; lds Rm,X1
  (unmasque "0100MMMM.10011010" word (m)
    (list :lds (drv-gpr m) :x1)))

(readops lds.l (word read) ;; lds.l @Rm+,X1
  (unmasque "0100MMMM.10010110" word (m)
    (list :lds :l (list '@+ (drv-gpr m)) :x1)))

(readops lds (word read) ;; lds Rm,Y0
  (unmasque "0100MMMM.10101010" word (m)
    (list :lds (drv-gpr m) :y0)))

(readops lds.l (word read) ;; lds.l @Rm+,Y0
  (unmasque "0100MMMM.10100110" word (m)
    (list :lds :l (list '@+ (drv-gpr m)) :y0)))

(readops lds (word read) ;; lds Rm,Y1
  (unmasque "0100MMMM.10111010" word (m)
    (list :lds (drv-gpr m) :y1)))

(readops lds.l (word read) ;; lds.l @Rm+,Y1
  (unmasque "0100MMMM.10110110" word (m)
    (list :lds :l (list '@+ (drv-gpr m)) :y1)))

(specops ldtlb ()
  ((:for-types :sh3 :sh4 :sh4a :privileged))
  (masque "00000000.00111000")) ;; ldtlb

(readops ldtlb (word read)
  (unmasque "00000000.00111000" word ()
    (list :ldtlb )))

(specops movca (w op0 op1)
    ((:for-types :sh4 :sh4a))
  (assert (and (eq w :l) (eq :r0 op0) (match-types op1  mas-simple)) ()
          "MOVCA only works at width L and takes only R0 and a simple memory address as its operands.")
  (masque "0000NNNN.11000011" ;; movca.l R0,@Rn
          (n (gprix (mas-base op1)))))

(readops movca.l (word read) ;; movca.l R0,@Rn
  (unmasque "0000NNNN.11000011" word (n)
    (list :movca :l :r0 (list '@ (drv-gpr n)))))

(specops nop ()
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "00000000.00001001")) ;; nop

(readops nop (unmasque "00000000.00001001") (read)
  (list :nop))

(specops ocbi (op0)
  ((:for-types :sh4 :sh4a))
  (assert (match-types op0  mas-simple) ()
          "OCBI takes only a simple memory address as its operand.")
  (masque "0000NNNN.10010011" ;; ocbi @Rn
          (n (gprix (mas-base op0)))))

(readops ocbi (word read) ;; ocbi @Rn
  (unmasque "0000NNNN.10010011" word (n)
    (list :ocbi (list '@ (drv-gpr n)))))

(specops ocbp (op0)
  ((:for-types :sh4 :sh4a))
  (assert (match-types op0  mas-simple) ()
          "OCBI takes only a simple memory address as its operand.")
  (masque "0000NNNN.10100011" ;; ocbp @Rn
          (n (gprix (mas-base op0)))))

(readops ocbp (word read)
  (unmasque "0000NNNN.10100011" word (n)
    (list :ocbp (list '@ (drv-gpr n)))))

(specops ocbwb (op0)
  ((:for-types :sh4 :sh4a))
  (assert (match-types op0  mas-simple) ()
          "OCBWB takes only a simple memory address as its operand.")
  (masque "0000NNNN.10110011" ;; ocbwb @Rn
          (n (gprix (mas-base op0)))))

(readops ocbwb (word read) ;; ocbwb @Rn
  (unmasque "0000NNNN.10110011" word (n)
    (list :ocbwb (list '@ (drv-gpr n)))))

(specops pref (op0)
  ((:for-types :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  mas-simple) ()
          "PREF takes only a simple memory address as its operand.")
  (masque "0000NNNN.10000011" ;; pref @Rn
          (n (gprix (mas-base op0)))))

(readops pref (word read)
  (unmasque "0000NNNN.10000011" word (n)
    (list :pref (list '@ (drv-gpr n)))))

(specops prefi (op0)
  ((:for-types :sh4a))
  (assert (match-types op0  mas-simple) ()
          "PREFI takes only a simple memory address as its operand.")
  (masque "0000NNNN.11010011" ;; prefi @Rn
          (n (gprix (mas-base op0)))))

(readops prefi (word read)
  (unmasque "0000NNNN.11010011" word (n)
    (list :prefi (list '@ (drv-gpr n)))))

(specops resbank ()
  ((:for-types :sh2a))
  (masque "00000000.01011011")) ;; resbank

(readops (unmasque "00000000.01011011") (read)
  (list :resbank))

(specops rte ()
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :privileged))
  (masque "00000000.00101011")) ;; rte

(readops (unmasque "00000000.00101011") (read)
  (list :rte))

(specops setrc (op0)
  ((:for-types :dsp))
  (typecase op0
    (gpr     (masque "0100NNNN.00010100" ;; setrc Rn
                     (n (gprix op0))))
    (integer (masque "10000010.IIIIIIII" ;; setrc #imm
                     (i op0)))
    (t (error "SETRC takes either a general-purpose register or 8-bit immediate value as its operand."))))
  
(readops setrc (word read)
  (unmasque "0100NNNN.00010100" word (n)
    (list :setrc (drv-gpr n))))

(readops setrc (word read)
  (unmasque "10000010.IIIIIIII" word (i)
    (list :setrc i)))

(specops sets ()
  ((:for-types :sh3 :sh4 :sh4a))
  (masque "00000000.01011000")) ;; sets

(readops (unmasque "00000000.01011000") (read)
  (list :sets))

(specops sett ()
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (masque "00000000.00011000")) ;; sett

(readops (unmasque "00000000.00011000") (read)
  (list :sett))

(specops sleep ()
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :privileged))
  (masque "00000000.00011011")) ;; sleep

(readops (unmasque "00000000.00011011") (read)
  (list :sleep))

(specops stbank (op0 op1)
  ((:for-types :sh2a))
  (assert (and (eq :r0 op0) (match-types op1  mas-simple)) ()
          "STBANK only works at width L and takes only R0 and a simple memory address as its operands.")
  (masque "0100NNNN.11100001" ;; stbank R0,@Rn
          (n (gprix (mas-base op1)))))

(readops stbank (word read) ;; stbank R0,@Rn
  (unmasque "0100NNNN.11100001" word (n)
    (list :stbank :r0 (list '@ (drv-gpr n)))))

(specops stc (w op0 op1)
  ;; stc SR,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.00000010"
            )))

(readops stc (word read)
  (unmasque "0000NNNN.00000010" word ()
    (list :stc )))

(specops stc.l (w op0 op1)
  ;; stc.l SR,@-Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00000011"
            )))

(readops stc.l (word read)
  (unmasque "0100NNNN.00000011" word ()
    (list :stc.l )))

(specops stc (w op0 op1)
  ;; stc TBR,Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.01001010"
            )))

(readops stc (word read)
  (unmasque "0000NNNN.01001010" word ()
    (list :stc )))

(specops stc (w op0 op1)
  ;; stc GBR,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.00010010"
            )))

(readops stc (word read)
  (unmasque "0000NNNN.00010010" word ()
    (list :stc )))

(specops stc.l (w op0 op1)
  ;; stc.l GBR,@-Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00010011"
            )))

(readops stc.l (word read)
  (unmasque "0100NNNN.00010011" word ()
    (list :stc.l )))

(specops stc (w op0 op1)
  ;; stc VBR,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.00100010"
            )))

(readops stc (word read)
  (unmasque "0000NNNN.00100010" word ()
    (list :stc )))

(specops stc.l (w op0 op1)
  ;; stc.l VBR,@-Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00100011"
            )))

(readops stc.l (word read)
  (unmasque "0100NNNN.00100011" word ()
    (list :stc.l )))

(specops stc (w op0 op1)
  ;; stc MOD,Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.01010010"
            )))

(readops stc (word read)
  (unmasque "0000NNNN.01010010" word ()
    (list :stc )))

(specops stc.l (w op0 op1)
  ;; stc.l MOD,@-Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.01010011"
            )))

(readops stc.l (word read)
  (unmasque "0100NNNN.01010011" word ()
    (list :stc.l )))

(specops stc (w op0 op1)
  ;; stc RE,Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.01110010"
            )))

(readops stc (word read)
  (unmasque "0000NNNN.01110010" word ()
    (list :stc )))

(specops stc.l (w op0 op1)
  ;; stc.l RE,@-Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.01110011"
            )))

(readops stc.l (word read)
  (unmasque "0100NNNN.01110011" word ()
    (list :stc.l )))

(specops stc (w op0 op1)
  ;; stc RS,Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.01100010"
            )))

(readops stc (word read)
  (unmasque "0000NNNN.01100010" word ()
    (list :stc )))

(specops stc.l (w op0 op1)
  ;; stc.l RS,@-Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.01100011"
            )))

(readops stc.l (word read)
  (unmasque "0100NNNN.01100011" word ()
    (list :stc.l )))

(specops stc (w op0 op1)
  ;; stc SGR,Rn
  ((:for-types :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.00111010"
            )))

(readops stc (word read)
  (unmasque "0000NNNN.00111010" word ()
    (list :stc )))

(specops stc.l (w op0 op1)
  ;; stc.l SGR,@-Rn
  ((:for-types :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00110010"
            )))

(readops stc.l (word read)
  (unmasque "0100NNNN.00110010" word ()
    (list :stc.l )))

(specops stc (w op0 op1)
  ;; stc SSR,Rn
  ((:for-types :sh3 :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.00110010"
            )))

(readops stc (word read)
  (unmasque "0000NNNN.00110010" word ()
    (list :stc )))

(specops stc.l (w op0 op1)
  ;; stc.l SSR,@-Rn
  ((:for-types :sh3 :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00110011"
            )))

(readops stc.l (word read)
  (unmasque "0100NNNN.00110011" word ()
    (list :stc.l )))

(specops stc (w op0 op1)
  ;; stc SPC,Rn
  ((:for-types :sh3 :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.01000010"
            )))

(readops stc (word read)
  (unmasque "0000NNNN.01000010" word ()
    (list :stc )))

(specops stc.l (w op0 op1)
  ;; stc.l SPC,@-Rn
  ((:for-types :sh3 :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.01000011"
            )))

(readops stc.l (word read)
  (unmasque "0100NNNN.01000011" word ()
    (list :stc.l )))

(specops stc (w op0 op1)
  ;; stc DBR,Rn
  ((:for-types :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.11111010"
            )))

(readops stc (word read)
  (unmasque "0000NNNN.11111010" word ()
    (list :stc )))

(specops stc.l (w op0 op1)
  ;; stc.l DBR,@-Rn
  ((:for-types :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.11110010"
            )))

(readops stc.l (word read)
  (unmasque "0100NNNN.11110010" word ()
    (list :stc.l )))

(specops stc (w op0 op1)
  ;; stc Rm_BANK,Rn
  ((:for-types :sh3 :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.1MMM0010"
            )))

(readops stc (word read)
  (unmasque "0000NNNN.1MMM0010" word ()
    (list :stc )))

(specops stc.l (w op0 op1)
  ;; stc.l Rm_BANK,@-Rn
  ((:for-types :sh3 :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.1MMM0011"
            )))

(readops stc.l (word read)
  (unmasque "0100NNNN.1MMM0011" word ()
    (list :stc.l )))

(specops sts (w op0 op1)
  ;; sts MACH,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.00001010"
            )))

(readops sts (word read)
  (unmasque "0000NNNN.00001010" word ()
    (list :sts )))

(specops sts.l (w op0 op1)
  ;; sts.l MACH,@-Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00000010"
            )))

(readops sts.l (word read)
  (unmasque "0100NNNN.00000010" word ()
    (list :sts.l )))

(specops sts (w op0 op1)
  ;; sts MACL,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.00011010"
            )))

(readops sts (word read)
  (unmasque "0000NNNN.00011010" word ()
    (list :sts )))

(specops sts.l (w op0 op1)
  ;; sts.l MACL,@-Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00010010"
            )))

(readops sts.l (word read)
  (unmasque "0100NNNN.00010010" word ()
    (list :sts.l )))

(specops sts (w op0 op1)
  ;; sts PR,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.00101010"
            )))

(readops sts (word read)
  (unmasque "0000NNNN.00101010" word ()
    (list :sts )))

(specops sts.l (w op0 op1)
  ;; sts.l PR,@-Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00100010"
            )))

(readops sts.l (word read)
  (unmasque "0100NNNN.00100010" word ()
    (list :sts.l )))

(specops sts (w op0 op1)
  ;; sts DSR,Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.01101010"
            )))

(readops sts (word read)
  (unmasque "0000NNNN.01101010" word ()
    (list :sts )))

(specops sts.l (w op0 op1)
  ;; sts.l DSR,@-Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.01100010"
            )))

(readops sts.l (word read)
  (unmasque "0100NNNN.01100010" word ()
    (list :sts.l )))

(specops sts (w op0 op1)
  ;; sts A0,Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.01111010"
            )))

(readops sts (word read)
  (unmasque "0000NNNN.01111010" word ()
    (list :sts )))

(specops sts.l (w op0 op1)
  ;; sts.l A0,@-Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.01100010"
            )))

(readops sts.l (word read)
  (unmasque "0100NNNN.01100010" word ()
    (list :sts.l )))

(specops sts (w op0 op1)
  ;; sts X0,Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.10001010"
            )))

(readops sts (word read)
  (unmasque "0000NNNN.10001010" word ()
    (list :sts )))

(specops sts.l (w op0 op1)
  ;; sts.l X0,@-Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.10000010"
            )))

(readops sts.l (word read)
  (unmasque "0100NNNN.10000010" word ()
    (list :sts.l )))

(specops sts (w op0 op1)
  ;; sts X1,Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.10011010"
            )))

(readops sts (word read)
  (unmasque "0000NNNN.10011010" word ()
    (list :sts )))

(specops sts.l (w op0 op1)
  ;; sts.l X1,@-Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.10010010"
            )))

(readops sts.l (word read)
  (unmasque "0100NNNN.10010010" word ()
    (list :sts.l )))

(specops sts (w op0 op1)
  ;; sts Y0,Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.10101010"
            )))

(readops sts (word read)
  (unmasque "0000NNNN.10101010" word ()
    (list :sts )))

(specops sts.l (w op0 op1)
  ;; sts.l Y0,@-Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.10100010"
            )))

(readops sts.l (word read)
  (unmasque "0100NNNN.10100010" word ()
    (list :sts.l )))

(specops sts (w op0 op1)
  ;; sts Y1,Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.10111010"
            )))

(readops sts (word read)
  (unmasque "0000NNNN.10111010" word ()
    (list :sts )))

(specops sts.l (w op0 op1)
  ;; sts.l Y1,@-Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.10110010"
            )))

(readops sts.l (word read)
  (unmasque "0100NNNN.10110010" word ()
    (list :sts.l )))

(specops synco (w op0 op1)
  ;; synco
  ((:for-types :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "00000000.10101011"
            )))

(readops synco (word read)
  (unmasque "00000000.10101011" word ()
    (list :synco )))

(specops trapa (w op0 op1)
  ;; trapa #imm
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "11000011.IIIIIIII"
            )))

(readops trapa (word read)
  (unmasque "11000011.IIIIIIII" word ()
    (list :trapa )))

;;; *** END SYSTEM FUNCTIONS - BEGIN FP

(specops fmov (w op0 op1)
  ;; fmov FRm,FRn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMMM1100"
            )))

(readops fmov (word read)
  (unmasque "1111NNNN.MMMM1100" word ()
    (list :fmov )))

(specops fmov.s (w op0 op1)
  ;; fmov.s @Rm,FRn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMMM1000"
            )))

(readops fmov.s (word read)
  (unmasque "1111NNNN.MMMM1000" word ()
    (list :fmov.s )))

(specops fmov.s (w op0 op1)
  ;; fmov.s FRm,@Rn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMMM1010"
            )))

(readops fmov.s (word read)
  (unmasque "1111NNNN.MMMM1010" word ()
    (list :fmov.s )))

(specops fmov.s (w op0 op1)
  ;; fmov.s @Rm+,FRn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMMM1001"
            )))

(readops fmov.s (word read)
  (unmasque "1111NNNN.MMMM1001" word ()
    (list :fmov.s )))

(specops fmov.s (w op0 op1)
  ;; fmov.s FRm,@-Rn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMMM1011"
            )))

(readops fmov.s (word read)
  (unmasque "1111NNNN.MMMM1011" word ()
    (list :fmov.s )))

(specops fmov.s (w op0 op1)
  ;; fmov.s @(R0,Rm),FRn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMMM0110"
            )))

(readops fmov.s (word read)
  (unmasque "1111NNNN.MMMM0110" word ()
    (list :fmov.s )))

(specops fmov.s (w op0 op1)
  ;; fmov.s FRm,@(R0,Rn)
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMMM0111"
            )))

(readops fmov.s (word read)
  (unmasque "1111NNNN.MMMM0111" word ()
    (list :fmov.s )))

(specops fmov.s (w op0 op1)
  ;; fmov.s @(disp12,Rm),FRn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM0001.0111DDDD.DDDDDDDD"
            )))

(readops fmov.s (word read)
  (unmasque "0011NNNN.MMMM0001.0111DDDD.DDDDDDDD" word ()
    (list :fmov.s )))

(specops fmov.s (w op0 op1)
  ;; fmov.s FRm,@(disp12,Rn)
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM0001.0011DDDD.DDDDDDDD"
            )))

(readops fmov.s (word read)
  (unmasque "0011NNNN.MMMM0001.0011DDDD.DDDDDDDD" word ()
    (list :fmov.s )))

(specops fmov (w op0 op1)
  ;; fmov DRm,DRn
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN0.MMM01100"
            )))

(readops fmov (word read)
  (unmasque "1111NNN0.MMM01100" word ()
    (list :fmov )))

(specops fmov (w op0 op1)
  ;; fmov DRm,XDn
  ((:for-types :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN1.MMM01100"
            )))

(readops fmov (word read)
  (unmasque "1111NNN1.MMM01100" word ()
    (list :fmov )))

(specops fmov (w op0 op1)
  ;; fmov XDm,DRn
  ((:for-types :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN0.MMM11100"
            )))

(readops fmov (word read)
  (unmasque "1111NNN0.MMM11100" word ()
    (list :fmov )))

(specops fmov (w op0 op1)
  ;; fmov XDm,XDn
  ((:for-types :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN1.MMM11100"
            )))

(readops fmov (word read)
  (unmasque "1111NNN1.MMM11100" word ()
    (list :fmov )))

(specops fmov.d (w op0 op1)
  ;; fmov.d @Rm,DRn
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN0.MMMM1000"
            )))

(readops fmov.d (word read)
  (unmasque "1111NNN0.MMMM1000" word ()
    (list :fmov.d )))

(specops fmov.d (w op0 op1)
  ;; fmov.d @Rm,XDn
  ((:for-types :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN1.MMMM1000"
            )))

(readops fmov.d (word read)
  (unmasque "1111NNN1.MMMM1000" word ()
    (list :fmov.d )))

(specops fmov.d (w op0 op1)
  ;; fmov.d DRm,@Rn
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMM01010"
            )))

(readops fmov.d (word read)
  (unmasque "1111NNNN.MMM01010" word ()
    (list :fmov.d )))

(specops fmov.d (w op0 op1)
  ;; fmov.d XDm,@Rn
  ((:for-types :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMM11010"
            )))

(readops fmov.d (word read)
  (unmasque "1111NNNN.MMM11010" word ()
    (list :fmov.d )))

(specops fmov.d (w op0 op1)
  ;; fmov.d @Rm+,DRn
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN0.MMMM1001"
            )))

(readops fmov.d (word read)
  (unmasque "1111NNN0.MMMM1001" word ()
    (list :fmov.d )))

(specops fmov.d (w op0 op1)
  ;; fmov.d @Rm+,XDn
  ((:for-types :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN1.MMMM1001"
            )))

(readops fmov.d (word read)
  (unmasque "1111NNN1.MMMM1001" word ()
    (list :fmov.d )))

(specops fmov.d (w op0 op1)
  ;; fmov.d DRm,@-Rn
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMM01011"
            )))

(readops fmov.d (word read)
  (unmasque "1111NNNN.MMM01011" word ()
    (list :fmov.d )))

(specops fmov.d (w op0 op1)
  ;; fmov.d XDm,@-Rn
  ((:for-types :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMM11011"
            )))

(readops fmov.d (word read)
  (unmasque "1111NNNN.MMM11011" word ()
    (list :fmov.d )))

(specops fmov.d (w op0 op1)
  ;; fmov.d @(R0,Rm),DRn
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN0.MMMM0110"
            )))

(readops fmov.d (word read)
  (unmasque "1111NNN0.MMMM0110" word ()
    (list :fmov.d )))

(specops fmov.d (w op0 op1)
  ;; fmov.d @(R0,Rm),XDn
  ((:for-types :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN1.MMMM0110"
            )))

(readops fmov.d (word read)
  (unmasque "1111NNN1.MMMM0110" word ()
    (list :fmov.d )))

(specops fmov.d (w op0 op1)
  ;; fmov.d DRm,@(R0,Rn)
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMM00111"
            )))

(readops fmov.d (word read)
  (unmasque "1111NNNN.MMM00111" word ()
    (list :fmov.d )))

(specops fmov.d (w op0 op1)
  ;; fmov.d XDm,@(R0,Rn)
  ((:for-types :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMM10111"
            )))

(readops fmov.d (word read)
  (unmasque "1111NNNN.MMM10111" word ()
    (list :fmov.d )))

(specops fmov.d (w op0 op1)
  ;; fmov.d @(disp12,Rm),DRn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNN0.MMMM0001.0111DDDD.DDDDDDDD"
            )))

(readops fmov.d (word read)
  (unmasque "0011NNN0.MMMM0001.0111DDDD.DDDDDDDD" word ()
    (list :fmov.d )))

(specops fmov.d (w op0 op1)
  ;; fmov.d DRm,@(disp12,Rn)
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMM00001.0011DDDD.DDDDDDDD"
            )))

(readops fmov.d (word read)
  (unmasque "0011NNNN.MMM00001.0011DDDD.DDDDDDDD" word ()
    (list :fmov.d )))

(specops fldi0 (w op0 op1)
  ;; fldi0 FRn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.10001101"
            )))

(readops fldi0 (word read)
  (unmasque "1111NNNN.10001101" word ()
    (list :fldi0 )))

(specops fldi1 (w op0 op1)
  ;; fldi1 FRn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.10011101"
            )))

(readops fldi1 (word read)
  (unmasque "1111NNNN.10011101" word ()
    (list :fldi1 )))

(specops flds (w op0 op1)
  ;; flds FRm,FPUL
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111MMMM.00011101"
            )))

(readops flds (word read)
  (unmasque "1111MMMM.00011101" word ()
    (list :flds )))

(specops fsts (w op0 op1)
  ;; fsts FPUL,FRn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.00001101"
            )))

(readops fsts (word read)
  (unmasque "1111NNNN.00001101" word ()
    (list :fsts )))

(specops fabs (w op0 op1)
  ;; fabs FRn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.01011101"
            )))

(readops fabs (word read)
  (unmasque "1111NNNN.01011101" word ()
    (list :fabs )))

(specops fneg (w op0 op1)
  ;; fneg FRn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.01001101"
            )))

(readops fneg (word read)
  (unmasque "1111NNNN.01001101" word ()
    (list :fneg )))

(specops fadd (w op0 op1)
  ;; fadd FRm,FRn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMMM0000"
            )))

(readops fadd (word read)
  (unmasque "1111NNNN.MMMM0000" word ()
    (list :fadd )))

(specops fsub (w op0 op1)
  ;; fsub FRm,FRn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMMM0001"
            )))

(readops fsub (word read)
  (unmasque "1111NNNN.MMMM0001" word ()
    (list :fsub )))

(specops fmul (w op0 op1)
  ;; fmul FRm,FRn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMMM0010"
            )))

(readops fmul (word read)
  (unmasque "1111NNNN.MMMM0010" word ()
    (list :fmul )))

(specops fmac (w op0 op1)
  ;; fmac FR0,FRm,FRn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMMM1110"
            )))

(readops fmac (word read)
  (unmasque "1111NNNN.MMMM1110" word ()
    (list :fmac )))

(specops fdiv (w op0 op1)
  ;; fdiv FRm,FRn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMMM0011"
            )))

(readops fdiv (word read)
  (unmasque "1111NNNN.MMMM0011" word ()
    (list :fdiv )))

(specops fsqrt (w op0 op1)
  ;; fsqrt FRn
  ((:for-types :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.01101101"
            )))

(readops fsqrt (word read)
  (unmasque "1111NNNN.01101101" word ()
    (list :fsqrt )))

(specops fcmp/eq (w op0 op1)
  ;; fcmp/eq FRm,FRn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMMM0100"
            )))

(readops fcmp/eq (word read)
  (unmasque "1111NNNN.MMMM0100" word ()
    (list :fcmp/eq )))

(specops fcmp/gt (w op0 op1)
  ;; fcmp/gt FRm,FRn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.MMMM0101"
            )))

(readops fcmp/gt (word read)
  (unmasque "1111NNNN.MMMM0101" word ()
    (list :fcmp/gt )))

(specops float (w op0 op1)
  ;; float FPUL,FRn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.00101101"
            )))

(readops float (word read)
  (unmasque "1111NNNN.00101101" word ()
    (list :float )))

(specops ftrc (w op0 op1)
  ;; ftrc FRm,FPUL
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111MMMM.00111101"
            )))

(readops ftrc (word read)
  (unmasque "1111MMMM.00111101" word ()
    (list :ftrc )))

(specops fipr (w op0 op1)
  ;; fipr FVm,FVn
  ((:for-types :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNMM.11101101"
            )))

(readops fipr (word read)
  (unmasque "1111NNMM.11101101" word ()
    (list :fipr )))

(specops ftrv (w op0 op1)
  ;; ftrv XMTRX,FVn
  ((:for-types :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "1111NN01.11111101"
            )))

(readops ftrv (word read)
  (unmasque "1111NN01.11111101" word ()
    (list :ftrv )))

(specops fsrra (w op0 op1)
  ;; fsrra FRn
  ((:for-types :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNNN.01111101"
            )))

(readops fsrra (word read)
  (unmasque "1111NNNN.01111101" word ()
    (list :fsrra )))

(specops fsca (w op0 op1)
  ;; fsca FPUL,DRn
  ((:for-types :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN0.11111101"
            )))

(readops fsca (word read)
  (unmasque "1111NNN0.11111101" word ()
    (list :fsca )))

(specops fabs (w op0 op1)
  ;; fabs DRn
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN0.01011101"
            )))

(readops fabs (word read)
  (unmasque "1111NNN0.01011101" word ()
    (list :fabs )))

(specops fneg (w op0 op1)
  ;; fneg DRn
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN0.01001101"
            )))

(readops fneg (word read)
  (unmasque "1111NNN0.01001101" word ()
    (list :fneg )))

(specops fadd (w op0 op1)
  ;; fadd DRm,DRn
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN0.MMM00000"
            )))

(readops fadd (word read)
  (unmasque "1111NNN0.MMM00000" word ()
    (list :fadd )))

(specops fsub (w op0 op1)
  ;; fsub DRm,DRn
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN0.MMM00001"
            )))

(readops fsub (word read)
  (unmasque "1111NNN0.MMM00001" word ()
    (list :fsub )))

(specops fmul (w op0 op1)
  ;; fmul DRm,DRn
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN0.MMM00010"
            )))

(readops fmul (word read)
  (unmasque "1111NNN0.MMM00010" word ()
    (list :fmul )))

(specops fdiv (w op0 op1)
  ;; fdiv DRm,DRn
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN0.MMM00011"
            )))

(readops fdiv (word read)
  (unmasque "1111NNN0.MMM00011" word ()
    (list :fdiv )))

(specops fsqrt (w op0 op1)
  ;; fsqrt DRn
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN0.01101101"
            )))

(readops fsqrt (word read)
  (unmasque "1111NNN0.01101101" word ()
    (list :fsqrt )))

(specops fcmp/eq (w op0 op1)
  ;; fcmp/eq DRm,DRn
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN0.MMM00100"
            )))

(readops fcmp/eq (word read)
  (unmasque "1111NNN0.MMM00100" word ()
    (list :fcmp/eq )))

(specops fcmp/gt (w op0 op1)
  ;; fcmp/gt DRm,DRn
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN0.MMM00101"
            )))

(readops fcmp/gt (word read)
  (unmasque "1111NNN0.MMM00101" word ()
    (list :fcmp/gt )))

(specops float (w op0 op1)
  ;; float FPUL,DRn
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN0.00101101"
            )))

(readops float (word read)
  (unmasque "1111NNN0.00101101" word ()
    (list :float )))

(specops ftrc (w op0 op1)
  ;; ftrc DRm,FPUL
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111MMM0.00111101"
            )))

(readops ftrc (word read)
  (unmasque "1111MMM0.00111101" word ()
    (list :ftrc )))

(specops fcnvds (w op0 op1)
  ;; fcnvds DRm,FPUL
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111MMM0.10111101"
            )))

(readops fcnvds (word read)
  (unmasque "1111MMM0.10111101" word ()
    (list :fcnvds )))

(specops fcnvsd (w op0 op1)
  ;; fcnvsd FPUL,DRn
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1111NNN0.10101101"
            )))

(readops fcnvsd (word read)
  (unmasque "1111NNN0.10101101" word ()
    (list :fcnvsd )))

(specops lds (w op0 op1)
  ;; lds Rm,FPSCR
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01101010"
            )))

(readops lds (word read)
  (unmasque "0100MMMM.01101010" word ()
    (list :lds )))

(specops sts (w op0 op1)
  ;; sts FPSCR,Rn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.01101010"
            )))

(readops sts (word read)
  (unmasque "0000NNNN.01101010" word ()
    (list :sts )))

(specops lds.l (w op0 op1)
  ;; lds.l @Rm+,FPSCR
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01100110"
            )))

(readops lds.l (word read)
  (unmasque "0100MMMM.01100110" word ()
    (list :lds.l )))

(specops sts.l (w op0 op1)
  ;; sts.l FPSCR,@-Rn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.01100010"
            )))

(readops sts.l (word read)
  (unmasque "0100NNNN.01100010" word ()
    (list :sts.l )))

(specops lds (w op0 op1)
  ;; lds Rm,FPUL
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01011010"
            )))

(readops lds (word read)
  (unmasque "0100MMMM.01011010" word ()
    (list :lds )))

(specops sts (w op0 op1)
  ;; sts FPUL,Rn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.01011010"
            )))

(readops sts (word read)
  (unmasque "0000NNNN.01011010" word ()
    (list :sts )))

(specops lds.l (w op0 op1)
  ;; lds.l @Rm+,FPUL
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01010110"
            )))

(readops lds.l (word read)
  (unmasque "0100MMMM.01010110" word ()
    (list :lds.l )))

(specops sts.l (w op0 op1)
  ;; sts.l FPUL,@-Rn
  ((:for-types :sh2e :sh3e :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.01010010"
            )))

(readops sts.l (word read)
  (unmasque "0100NNNN.01010010" word ()
    (list :sts.l )))

(specops frchg (w op0 op1)
  ;; frchg
  ((:for-types :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "11111011.11111101"
            )))

(readops frchg (word read)
  (unmasque "11111011.11111101" word ()
    (list :frchg )))

(specops fschg (w op0 op1)
  ;; fschg
  ((:for-types :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "11110011.11111101"
            )))

(readops fschg (word read)
  (unmasque "11110011.11111101" word ()
    (list :fschg )))

(specops fpchg (w op0 op1)
  ;; fpchg
  ((:for-types :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "11110111.11111101"
            )))

(readops fpchg (word read)
  (unmasque "11110111.11111101" word ()
    (list :fpchg )))

(specops nopx (w op0 op1)
  ;; nopx
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "1111000*.0*0*00**"
            )))

(readops nopx (word read)
  (unmasque "1111000*.0*0*00**" word ()
    (list :nopx )))

(specops movx.w (w op0 op1)
  ;; movx.w @Ax,Dx
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111100A*.D*0*01**"
            )))

(readops movx.w (word read)
  (unmasque "111100A*.D*0*01**" word ()
    (list :movx.w )))

(specops movx.w (w op0 op1)
  ;; movx.w @Ax+,Dx
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111100A*.D*0*10**"
            )))

(readops movx.w (word read)
  (unmasque "111100A*.D*0*10**" word ()
    (list :movx.w )))

(specops movx.w (w op0 op1)
  ;; movx.w @Ax+Ix,Dx
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111100A*.D*0*11**"
            )))

(readops movx.w (word read)
  (unmasque "111100A*.D*0*11**" word ()
    (list :movx.w )))

(specops movx.w (w op0 op1)
  ;; movx.w Da,@Ax
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111100A*.D*1*01**"
            )))

(readops movx.w (word read)
  (unmasque "111100A*.D*1*01**" word ()
    (list :movx.w )))

(specops movx.w (w op0 op1)
  ;; movx.w Da,@Ax+
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111100A*.D*1*10**"
            )))

(readops movx.w (word read)
  (unmasque "111100A*.D*1*10**" word ()
    (list :movx.w )))

(specops movx.w (w op0 op1)
  ;; movx.w Da,@Ax+Ix
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111100A*.D*1*11**"
            )))

(readops movx.w (word read)
  (unmasque "111100A*.D*1*11**" word ()
    (list :movx.w )))

(specops nopy (w op0 op1)
  ;; nopy
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111100*0.*0*0**00"
            )))

(readops nopy (word read)
  (unmasque "111100*0.*0*0**00" word ()
    (list :nopy )))

(specops movy.w (w op0 op1)
  ;; movy.w @Ay,Dy
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111100*A.*D*0**01"
            )))

(readops movy.w (word read)
  (unmasque "111100*A.*D*0**01" word ()
    (list :movy.w )))

(specops movy.w (w op0 op1)
  ;; movy.w @Ay+,Dy
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111100*A.*D*0**10"
            )))

(readops movy.w (word read)
  (unmasque "111100*A.*D*0**10" word ()
    (list :movy.w )))

(specops movy.w (w op0 op1)
  ;; movy.w @Ay+Iy,Dy
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111100*A.*D*0**11"
            )))

(readops movy.w (word read)
  (unmasque "111100*A.*D*0**11" word ()
    (list :movy.w )))

(specops movy.w (w op0 op1)
  ;; movy.w Da,@Ay
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111100*A.*D*1**01"
            )))

(readops movy.w (word read)
  (unmasque "111100*A.*D*1**01" word ()
    (list :movy.w )))

(specops movy.w (w op0 op1)
  ;; movy.w Da,@Ay+
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111100*A.*D*1**10"
            )))

(readops movy.w (word read)
  (unmasque "111100*A.*D*1**10" word ()
    (list :movy.w )))

(specops movy.w (w op0 op1)
  ;; movy.w Da,@Ay+Iy
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111100*A.*D*1**11"
            )))

(readops movy.w (word read)
  (unmasque "111100*A.*D*1**11" word ()
    (list :movy.w )))

(specops movs.w (w op0 op1)
  ;; movs.w @-As,Ds
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111101AA.DDDD0000"
            )))

(readops movs.w (word read)
  (unmasque "111101AA.DDDD0000" word ()
    (list :movs.w )))

(specops movs.w (w op0 op1)
  ;; movs.w @As,Ds
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111101AA.DDDD0100"
            )))

(readops movs.w (word read)
  (unmasque "111101AA.DDDD0100" word ()
    (list :movs.w )))

(specops movs.w (w op0 op1)
  ;; movs.w @As+,Ds
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111101AA.DDDD1000"
            )))

(readops movs.w (word read)
  (unmasque "111101AA.DDDD1000" word ()
    (list :movs.w )))

(specops movs.w (w op0 op1)
  ;; movs.w @As+Ix,Ds
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111101AA.DDDD1100"
            )))

(readops movs.w (word read)
  (unmasque "111101AA.DDDD1100" word ()
    (list :movs.w )))

(specops movs.w (w op0 op1)
  ;; movs.w Ds,@-As
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111101AA.DDDD0001"
            )))

(readops movs.w (word read)
  (unmasque "111101AA.DDDD0001" word ()
    (list :movs.w )))

(specops movs.w (w op0 op1)
  ;; movs.w Ds,@As
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111101AA.DDDD0101"
            )))

(readops movs.w (word read)
  (unmasque "111101AA.DDDD0101" word ()
    (list :movs.w )))

(specops movs.w (w op0 op1)
  ;; movs.w Ds,@As+
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111101AA.DDDD1001"
            )))

(readops movs.w (word read)
  (unmasque "111101AA.DDDD1001" word ()
    (list :movs.w )))

(specops movs.w (w op0 op1)
  ;; movs.w Ds,@As+Is
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111101AA.DDDD1101"
            )))

(readops movs.w (word read)
  (unmasque "111101AA.DDDD1101" word ()
    (list :movs.w )))

(specops movs.l (w op0 op1)
  ;; movs.l @-As,Ds
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111101AA.DDDD0010"
            )))

(readops movs.l (word read)
  (unmasque "111101AA.DDDD0010" word ()
    (list :movs.l )))

(specops movs.l (w op0 op1)
  ;; movs.l @As,Ds
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111101AA.DDDD0110"
            )))

(readops movs.l (word read)
  (unmasque "111101AA.DDDD0110" word ()
    (list :movs.l )))

(specops movs.l (w op0 op1)
  ;; movs.l @As+,Ds
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111101AA.DDDD1010"
            )))

(readops movs.l (word read)
  (unmasque "111101AA.DDDD1010" word ()
    (list :movs.l )))

(specops movs.l (w op0 op1)
  ;; movs.l @As+Is,Ds
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111101AA.DDDD1110"
            )))

(readops movs.l (word read)
  (unmasque "111101AA.DDDD1110" word ()
    (list :movs.l )))

(specops movs.l (w op0 op1)
  ;; movs.l Ds,@-As
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111101AA.DDDD0011"
            )))

(readops movs.l (word read)
  (unmasque "111101AA.DDDD0011" word ()
    (list :movs.l )))

(specops movs.l (w op0 op1)
  ;; movs.l Ds,@As
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111101AA.DDDD0111"
            )))

(readops movs.l (word read)
  (unmasque "111101AA.DDDD0111" word ()
    (list :movs.l )))

(specops movs.l (w op0 op1)
  ;; movs.l Ds,@As+
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111101AA.DDDD1011"
            )))

(readops movs.l (word read)
  (unmasque "111101AA.DDDD1011" word ()
    (list :movs.l )))

(specops movs.l (w op0 op1)
  ;; movs.l Ds,@As+Is
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111101AA.DDDD1111"
            )))

(readops movs.l (word read)
  (unmasque "111101AA.DDDD1111" word ()
    (list :movs.l )))

(specops pabs (w op0 op1)
  ;; pabs  Sx,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10001000.XX00ZZZZ"
            )))

(readops pabs (word read)
  (unmasque "111110**.********.10001000.XX00ZZZZ" word ()
    (list :pabs )))

(specops pabs (w op0 op1)
  ;; pabs  Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10101000.00YYZZZZ"
            )))

(readops pabs (word read)
  (unmasque "111110**.********.10101000.00YYZZZZ" word ()
    (list :pabs )))

(specops padd (w op0 op1)
  ;; padd  Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10110001.XXYYZZZZ"
            )))

(readops padd (word read)
  (unmasque "111110**.********.10110001.XXYYZZZZ" word ()
    (list :padd )))

(specops dct (w op0 op1)
  ;; dct padd Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10110010.XXYYZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.10110010.XXYYZZZZ" word ()
    (list :dct )))

(specops dcf (w op0 op1)
  ;; dcf padd Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10110011.XXYYZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.10110011.XXYYZZZZ" word ()
    (list :dcf )))

(specops padd (w op0 op1)
  ;; padd  Sx,Sy,Du
pmuls  Se,Sf,Dg
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.0111EEFF.XXYYGGUU"
            )))

(readops padd (word read)
  (unmasque "111110**.********.0111EEFF.XXYYGGUU" word ()
    (list :padd )))

(specops paddc (w op0 op1)
  ;; paddc  Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10110000.XXYYZZZZ"
            )))

(readops paddc (word read)
  (unmasque "111110**.********.10110000.XXYYZZZZ" word ()
    (list :paddc )))

(specops pclr (w op0 op1)
  ;; pclr  Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10001101.0000ZZZZ"
            )))

(readops pclr (word read)
  (unmasque "111110**.********.10001101.0000ZZZZ" word ()
    (list :pclr )))

(specops dct (w op0 op1)
  ;; dct pclr Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10001110.0000ZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.10001110.0000ZZZZ" word ()
    (list :dct )))

(specops dcf (w op0 op1)
  ;; dcf pclr Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10001111.0000ZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.10001111.0000ZZZZ" word ()
    (list :dcf )))

(specops pcmp (w op0 op1)
  ;; pcmp  Sx,Sy
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10000100.XXYY0000"
            )))

(readops pcmp (word read)
  (unmasque "111110**.********.10000100.XXYY0000" word ()
    (list :pcmp )))

(specops pcopy (w op0 op1)
  ;; pcopy  Sx,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11011001.XX00ZZZZ"
            )))

(readops pcopy (word read)
  (unmasque "111110**.********.11011001.XX00ZZZZ" word ()
    (list :pcopy )))

(specops pcopy (w op0 op1)
  ;; pcopy  Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11111001.00YYZZZZ"
            )))

(readops pcopy (word read)
  (unmasque "111110**.********.11111001.00YYZZZZ" word ()
    (list :pcopy )))

(specops dct (w op0 op1)
  ;; dct pcopy Sx,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11011010.XX00ZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.11011010.XX00ZZZZ" word ()
    (list :dct )))

(specops dct (w op0 op1)
  ;; dct pcopy Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11111010.00YYZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.11111010.00YYZZZZ" word ()
    (list :dct )))

(specops dcf (w op0 op1)
  ;; dcf pcopy Sx,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11011011.XX00ZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.11011011.XX00ZZZZ" word ()
    (list :dcf )))

(specops dcf (w op0 op1)
  ;; dcf pcopy Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11111011.00YYZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.11111011.00YYZZZZ" word ()
    (list :dcf )))

(specops pneg (w op0 op1)
  ;; pneg  Sx,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11001001.XX00ZZZZ"
            )))

(readops pneg (word read)
  (unmasque "111110**.********.11001001.XX00ZZZZ" word ()
    (list :pneg )))

(specops pneg (w op0 op1)
  ;; pneg  Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11101001.00YYZZZZ"
            )))

(readops pneg (word read)
  (unmasque "111110**.********.11101001.00YYZZZZ" word ()
    (list :pneg )))

(specops dct (w op0 op1)
  ;; dct pneg Sx,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11001010.XX00ZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.11001010.XX00ZZZZ" word ()
    (list :dct )))

(specops dct (w op0 op1)
  ;; dct pneg Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11101010.00YYZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.11101010.00YYZZZZ" word ()
    (list :dct )))

(specops dcf (w op0 op1)
  ;; dcf pneg Sx,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11001011.XX00ZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.11001011.XX00ZZZZ" word ()
    (list :dcf )))

(specops dcf (w op0 op1)
  ;; dcf pneg Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11101011.00YYZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.11101011.00YYZZZZ" word ()
    (list :dcf )))

(specops psub (w op0 op1)
  ;; psub  Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10100001.XXYYZZZZ"
            )))

(readops psub (word read)
  (unmasque "111110**.********.10100001.XXYYZZZZ" word ()
    (list :psub )))

(specops dct (w op0 op1)
  ;; dct psub Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10100010.XXYYZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.10100010.XXYYZZZZ" word ()
    (list :dct )))

(specops dcf (w op0 op1)
  ;; dcf psub  Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10100011.XXYYZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.10100011.XXYYZZZZ" word ()
    (list :dcf )))

(specops psub (w op0 op1)
  ;; psub  Sx,Sy,Du
pmuls  Se,Sf,Dg
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.0110EEFF.XXYYGGUU"
            )))

(readops psub (word read)
  (unmasque "111110**.********.0110EEFF.XXYYGGUU" word ()
    (list :psub )))

(specops psubc (w op0 op1)
  ;; psubc  Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10100000.XXYYZZZZ"
            )))

(readops psubc (word read)
  (unmasque "111110**.********.10100000.XXYYZZZZ" word ()
    (list :psubc )))

(specops pdec (w op0 op1)
  ;; pdec  Sx,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10001001.XX00ZZZZ"
            )))

(readops pdec (word read)
  (unmasque "111110**.********.10001001.XX00ZZZZ" word ()
    (list :pdec )))

(specops pdec (w op0 op1)
  ;; pdec  Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10101001.00YYZZZZ"
            )))

(readops pdec (word read)
  (unmasque "111110**.********.10101001.00YYZZZZ" word ()
    (list :pdec )))

(specops dct (w op0 op1)
  ;; dct pdec Sx,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10001010.XX00ZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.10001010.XX00ZZZZ" word ()
    (list :dct )))

(specops dct (w op0 op1)
  ;; dct pdec Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10101010.00YYZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.10101010.00YYZZZZ" word ()
    (list :dct )))

(specops dcf (w op0 op1)
  ;; dcf pdec Sx,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10001011.XX00ZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.10001011.XX00ZZZZ" word ()
    (list :dcf )))

(specops dcf (w op0 op1)
  ;; dcf pdec Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10101011.00YYZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.10101011.00YYZZZZ" word ()
    (list :dcf )))

(specops pinc (w op0 op1)
  ;; pinc  Sx,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10011001.XX00ZZZZ"
            )))

(readops pinc (word read)
  (unmasque "111110**.********.10011001.XX00ZZZZ" word ()
    (list :pinc )))

(specops pinc (w op0 op1)
  ;; pinc  Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10111001.00YYZZZZ"
            )))

(readops pinc (word read)
  (unmasque "111110**.********.10111001.00YYZZZZ" word ()
    (list :pinc )))

(specops dct (w op0 op1)
  ;; dct pinc Sx,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10011010.XX00ZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.10011010.XX00ZZZZ" word ()
    (list :dct )))

(specops dct (w op0 op1)
  ;; dct pinc Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10111010.00YYZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.10111010.00YYZZZZ" word ()
    (list :dct )))

(specops dcf (w op0 op1)
  ;; dcf pinc Sx,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10011011.XX00ZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.10011011.XX00ZZZZ" word ()
    (list :dcf )))

(specops dcf (w op0 op1)
  ;; dcf pinc Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10111011.00YYZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.10111011.00YYZZZZ" word ()
    (list :dcf )))

(specops pdmsb (w op0 op1)
  ;; pdmsb  Sx,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10011101.XX00ZZZZ"
            )))

(readops pdmsb (word read)
  (unmasque "111110**.********.10011101.XX00ZZZZ" word ()
    (list :pdmsb )))

(specops pdmsb (w op0 op1)
  ;; pdmsb  Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10111101.00YYZZZZ"
            )))

(readops pdmsb (word read)
  (unmasque "111110**.********.10111101.00YYZZZZ" word ()
    (list :pdmsb )))

(specops dct (w op0 op1)
  ;; dct pdmsb Sx,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10011110.XX00ZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.10011110.XX00ZZZZ" word ()
    (list :dct )))

(specops dct (w op0 op1)
  ;; dct pdmsb Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10111110.00YYZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.10111110.00YYZZZZ" word ()
    (list :dct )))

(specops dcf (w op0 op1)
  ;; dcf pdmsb Sx,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10011111.XX00ZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.10011111.XX00ZZZZ" word ()
    (list :dcf )))

(specops dcf (w op0 op1)
  ;; dcf pdmsb Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10111111.00YYZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.10111111.00YYZZZZ" word ()
    (list :dcf )))

(specops prnd (w op0 op1)
  ;; prnd  Sx,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10011000.XX00ZZZZ"
            )))

(readops prnd (word read)
  (unmasque "111110**.********.10011000.XX00ZZZZ" word ()
    (list :prnd )))

(specops prnd (w op0 op1)
  ;; prnd  Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10111000.00YYZZZZ"
            )))

(readops prnd (word read)
  (unmasque "111110**.********.10111000.00YYZZZZ" word ()
    (list :prnd )))

(specops pand (w op0 op1)
  ;; pand  Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10010101.XXYYZZZZ"
            )))

(readops pand (word read)
  (unmasque "111110**.********.10010101.XXYYZZZZ" word ()
    (list :pand )))

(specops dct (w op0 op1)
  ;; dct pand Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10010110.XXYYZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.10010110.XXYYZZZZ" word ()
    (list :dct )))

(specops dcf (w op0 op1)
  ;; dcf pand Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10010111.XXYYZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.10010111.XXYYZZZZ" word ()
    (list :dcf )))

(specops por (w op0 op1)
  ;; por  Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10110101.XXYYZZZZ"
            )))

(readops por (word read)
  (unmasque "111110**.********.10110101.XXYYZZZZ" word ()
    (list :por )))

(specops dct (w op0 op1)
  ;; dct por  Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10110110.XXYYZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.10110110.XXYYZZZZ" word ()
    (list :dct )))

(specops dcf (w op0 op1)
  ;; dcf por  Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10110111.XXYYZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.10110111.XXYYZZZZ" word ()
    (list :dcf )))

(specops pxor (w op0 op1)
  ;; pxor  Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10100101.XXYYZZZZ"
            )))

(readops pxor (word read)
  (unmasque "111110**.********.10100101.XXYYZZZZ" word ()
    (list :pxor )))

(specops dct (w op0 op1)
  ;; dct pxor Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10100110.XXYYZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.10100110.XXYYZZZZ" word ()
    (list :dct )))

(specops dcf (w op0 op1)
  ;; dcf pxor Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10100111.XXYYZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.10100111.XXYYZZZZ" word ()
    (list :dcf )))

(specops pmuls (w op0 op1)
  ;; pmuls Se,Sf,Dg
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.0100EEFF.0000GG00"
            )))

(readops pmuls (word read)
  (unmasque "111110**.********.0100EEFF.0000GG00" word ()
    (list :pmuls )))

(specops psha (w op0 op1)
  ;; psha  Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10010001.XXYYZZZZ"
            )))

(readops psha (word read)
  (unmasque "111110**.********.10010001.XXYYZZZZ" word ()
    (list :psha )))

(specops dct (w op0 op1)
  ;; dct psha Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10010010.XXYYZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.10010010.XXYYZZZZ" word ()
    (list :dct )))

(specops dcf (w op0 op1)
  ;; dcf psha Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10010011.XXYYZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.10010011.XXYYZZZZ" word ()
    (list :dcf )))

(specops psha (w op0 op1)
  ;; psha  #imm,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.00000III.IIIIZZZZ"
            )))

(readops psha (word read)
  (unmasque "111110**.********.00000III.IIIIZZZZ" word ()
    (list :psha )))

(specops pshl (w op0 op1)
  ;; pshl  Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10000001.XXYYZZZZ"
            )))

(readops pshl (word read)
  (unmasque "111110**.********.10000001.XXYYZZZZ" word ()
    (list :pshl )))

(specops dct (w op0 op1)
  ;; dct pshl Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10000010.XXYYZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.10000010.XXYYZZZZ" word ()
    (list :dct )))

(specops dcf (w op0 op1)
  ;; dcf pshl Sx,Sy,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.10000011.XXYYZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.10000011.XXYYZZZZ" word ()
    (list :dcf )))

(specops pshl (w op0 op1)
  ;; pshl  #imm,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.00010III.IIIIZZZZ"
            )))

(readops pshl (word read)
  (unmasque "111110**.********.00010III.IIIIZZZZ" word ()
    (list :pshl )))

(specops plds (w op0 op1)
  ;; plds  Dz,MACH
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11101101.0000ZZZZ"
            )))

(readops plds (word read)
  (unmasque "111110**.********.11101101.0000ZZZZ" word ()
    (list :plds )))

(specops plds (w op0 op1)
  ;; plds  Dz,MACL
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11111101.0000ZZZZ"
            )))

(readops plds (word read)
  (unmasque "111110**.********.11111101.0000ZZZZ" word ()
    (list :plds )))

(specops dct (w op0 op1)
  ;; dct plds Dz,MACH
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11101110.0000ZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.11101110.0000ZZZZ" word ()
    (list :dct )))

(specops dct (w op0 op1)
  ;; dct plds Dz,MACL
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11111110.0000ZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.11111110.0000ZZZZ" word ()
    (list :dct )))

(specops dcf (w op0 op1)
  ;; dcf plds Dz,MACH
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11101111.0000ZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.11101111.0000ZZZZ" word ()
    (list :dcf )))

(specops dcf (w op0 op1)
  ;; dcf plds Dz,MACL
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11111111.0000ZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.11111111.0000ZZZZ" word ()
    (list :dcf )))

(specops psts (w op0 op1)
  ;; psts  MACH,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11001101.0000ZZZZ"
            )))

(readops psts (word read)
  (unmasque "111110**.********.11001101.0000ZZZZ" word ()
    (list :psts )))

(specops psts (w op0 op1)
  ;; psts  MACL,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11011101.0000ZZZZ"
            )))

(readops psts (word read)
  (unmasque "111110**.********.11011101.0000ZZZZ" word ()
    (list :psts )))

(specops dct (w op0 op1)
  ;; dct psts MACH,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11001110.0000ZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.11001110.0000ZZZZ" word ()
    (list :dct )))

(specops dct (w op0 op1)
  ;; dct psts MACL,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11011110.0000ZZZZ"
            )))

(readops dct (word read)
  (unmasque "111110**.********.11011110.0000ZZZZ" word ()
    (list :dct )))

(specops dcf (w op0 op1)
  ;; dcf psts MACH,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11001111.0000ZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.11001111.0000ZZZZ" word ()
    (list :dcf )))

(specops dcf (w op0 op1)
  ;; dcf psts MACL,Dz
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "111110**.********.11011111.0000ZZZZ"
            )))

(readops dcf (word read)
  (unmasque "111110**.********.11011111.0000ZZZZ" word ()
    (list :dcf )))

