;;;; ops.lisp

(in-package #:specops.superh)

(specops mov (w op0 op1)
    ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (cond ((match-types op0 op1  gpr gpr)
         (assert (eq w :l) (op0)
                 "MOVing data between registers can only be done at width L (32 bits).")
         (masque "0110NNNN.MMMM0011" ;; mov Rm,Rn
                 (n (gprix op1)) (m (gprix op0))))
        ((match-types op0 op1  integer gpr)
         (assert (eq w :b) (op0)
                 "MOVing data between registers can only be done at width B (8 bits).")
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
                       (d (mas-displ op1))))))
        ))

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





(specops mov.b (w op0 op1)
  ;; mov.b @(disp12,Rm),Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM0001.0100DDDD.DDDDDDDD"
            )))

(readops mov.b (word read)
  (unmasque "0011NNNN.MMMM0001.0100DDDD.DDDDDDDD" word ()
    (list :mov.b )))

(specops movi20 (w op0 op1)
  ;; movi20 #imm20,Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.IIII0000.IIIIIIII.IIIIIIII"
            )))

(readops movi20 (word read)
  (unmasque "0000NNNN.IIII0000.IIIIIIII.IIIIIIII" word ()
    (list :movi20 )))

(specops movi20s (w op0 op1)
  ;; movi20s #imm20,Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.IIII0001.IIIIIIII.IIIIIIII"
            )))

(readops movi20s (word read)
  (unmasque "0000NNNN.IIII0001.IIIIIIII.IIIIIIII" word ()
    (list :movi20s )))

(specops mova (w op0 op1)
  ;; mova @(disp,PC),R0
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "11000111.DDDDDDDD"
            )))

(readops mova (word read)
  (unmasque "11000111.DDDDDDDD" word ()
    (list :mova )))

(specops mov.w (w op0 op1)
  ;; mov.w @(disp12,Rm),Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM0001.0101DDDD.DDDDDDDD"
            )))

(readops mov.w (word read)
  (unmasque "0011NNNN.MMMM0001.0101DDDD.DDDDDDDD" word ()
    (list :mov.w )))

(specops mov.l (w op0 op1)
  ;; mov.l @(disp12,Rm),Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM0001.0110DDDD.DDDDDDDD"
            )))

(readops mov.l (word read)
  (unmasque "0011NNNN.MMMM0001.0110DDDD.DDDDDDDD" word ()
    (list :mov.l )))

(specops mov.b (w op0 op1)
  ;; mov.b Rm,@(disp12,Rn)
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM0001.0000DDDD.DDDDDDDD"
            )))

(readops mov.b (word read)
  (unmasque "0011NNNN.MMMM0001.0000DDDD.DDDDDDDD" word ()
    (list :mov.b )))

(specops mov.w (w op0 op1)
  ;; mov.w Rm,@(disp12,Rn)
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM0001.0001DDDD.DDDDDDDD"
            )))

(readops mov.w (word read)
  (unmasque "0011NNNN.MMMM0001.0001DDDD.DDDDDDDD" word ()
    (list :mov.w )))

(specops mov.l (w op0 op1)
  ;; mov.l Rm,@(disp12,Rn)
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM0001.0010DDDD.DDDDDDDD"
            )))

(readops mov.l (word read)
  (unmasque "0011NNNN.MMMM0001.0010DDDD.DDDDDDDD" word ()
    (list :mov.l )))

(specops movu.b (w op0 op1)
  ;; movu.b @(disp12,Rm),Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM0001.1000DDDD.DDDDDDDD"
            )))

(readops movu.b (word read)
  (unmasque "0011NNNN.MMMM0001.1000DDDD.DDDDDDDD" word ()
    (list :movu.b )))

(specops movu.w (w op0 op1)
  ;; movu.w @(disp12,Rm),Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM0001.1001DDDD.DDDDDDDD"
            )))

(readops movu.w (word read)
  (unmasque "0011NNNN.MMMM0001.1001DDDD.DDDDDDDD" word ()
    (list :movu.w )))

(specops movco.l (w op0 op1)
  ;; movco.l R0,@Rn
  ((:for-types :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.01110011"
            )))

(readops movco.l (word read)
  (unmasque "0000NNNN.01110011" word ()
    (list :movco.l )))

(specops movli.l (w op0 op1)
  ;; movli.l @Rm,R0
  ((:for-types :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "0000MMMM.01100011"
            )))

(readops movli.l (word read)
  (unmasque "0000MMMM.01100011" word ()
    (list :movli.l )))

(specops movua.l (w op0 op1)
  ;; movua.l @Rm,R0
  ((:for-types :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.10101001"
            )))

(readops movua.l (word read)
  (unmasque "0100MMMM.10101001" word ()
    (list :movua.l )))

(specops movua.l (w op0 op1)
  ;; movua.l @Rm+,R0
  ((:for-types :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.11101001"
            )))

(readops movua.l (word read)
  (unmasque "0100MMMM.11101001" word ()
    (list :movua.l )))

(specops movml.l (w op0 op1)
  ;; movml.l Rm,@-R15
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.11110001"
            )))

(readops movml.l (word read)
  (unmasque "0100MMMM.11110001" word ()
    (list :movml.l )))

(specops movml.l (w op0 op1)
  ;; movml.l @R15+,Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.11110101"
            )))

(readops movml.l (word read)
  (unmasque "0100NNNN.11110101" word ()
    (list :movml.l )))

(specops movmu.l (w op0 op1)
  ;; movmu.l Rm,@-R15
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.11110000"
            )))

(readops movmu.l (word read)
  (unmasque "0100MMMM.11110000" word ()
    (list :movmu.l )))

(specops movmu.l (w op0 op1)
  ;; movmu.l @R15+,Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.11110100"
            )))

(readops movmu.l (word read)
  (unmasque "0100NNNN.11110100" word ()
    (list :movmu.l )))

(specops movrt (w op0 op1)
  ;; movrt Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.00111001"
            )))

(readops movrt (word read)
  (unmasque "0000NNNN.00111001" word ()
    (list :movrt )))

(specops movt (w op0 op1)
  ;; movt Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.00101001"
            )))

(readops movt (word read)
  (unmasque "0000NNNN.00101001" word ()
    (list :movt )))

;;; *** MOVE STOPS

(specops nott ()
  ;; nott
  ((:for-types :sh2a))
  (masque "00000000.01101000"))

(readops (unmasque "00000000.01101000") (read) 
    (list :nott))


(specops swap (w op0 op1)
  ;; swap.b Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) ((index0) (index1))
    (case w
      (:b (masque "0110NNNN.MMMM1000"
                  (n op1) (m op0)))
      (:w (masque "0110NNNN.MMMM1001"
                  (n op1) (m op0))))))

(readops swap-b (word read)
  (unmasque "0110NNNN.MMMM1000" word (n m) 
    (list :swap :b (derive-reg :gpr m) (derive-reg :gpr n))))

(readops swap-w (word read)
  (unmasque "0110NNNN.MMMM1001" word (n m) 
    (list :swap :w (derive-reg :gpr m) (derive-reg :gpr n))))

(specops xtrct (op0 op1)
  ;; xtrct Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) ((index0) (index1))
    (masque "0010NNNN.MMMM1101"
            (n op1) (m op0))))

(readops xtrct (word read)
  (unmasque "0010NNNN.MMMM1101" word (n m)
    (list :xtrct (derive-reg :gpr m) (derive-reg :gpr n))))

(specops band (op0 op1 op2)
  ;; band.b     #imm3,@disp12,Rn
  ((:for-types :sh2a))
  (address (op2) ((index2))
    (masque "0011NNNN.0III1001.0100DDDD.DDDDDDDD"
            (n index2) (i op0) (d op1))))

(readops band (word read)
  (unmasque "0011NNNN.0III1001.0100DDDD.DDDDDDDD" word (n i d) 
    (list :band i d (derive-reg :gpr n))))

(specops bandnot (w op0 op1)
  ;; bandnot.b  #imm3,@(disp12,Rn)
  ((:for-types :sh2a))
  (address (op2) ((index2))
    (masque "0011NNNN.0III1001.1100DDDD.DDDDDDDD"
            (n index2) (i op0) (d op1))))

(readops bandnot (word read)
  (unmasque "0011NNNN.0III1001.1100DDDD.DDDDDDDD" word (n i d) 
    (list :bandnot i d (derive-reg :gpr n))))

(specops bclr (w op0 op1)
  ;; bclr.b     #imm3,@(disp12,Rn)
  ((:for-types :sh2a))
  (address (op2) ((index2))
    (masque "0011NNNN.0III1001.0000DDDD.DDDDDDDD"
            (n index2) (i op0) (d op1))))

(readops bclr (word read)
  (unmasque "0011NNNN.0III1001.0000DDDD.DDDDDDDD" word ()
    (list :bclr )))


;; *** STOP

(specops bclr (w op0 op1)
  ;; bclr       #imm3,Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "10000110.NNNN0III"
            )))

(readops bclr (word read)
  (unmasque "10000110.NNNN0III" word ()
    (list :bclr )))

(specops bld.b (w op0 op1)
  ;; bld.b      #imm3,@(disp12,Rn)
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.0III1001.0011DDDD.DDDDDDDD"
            )))

(readops bld.b (word read)
  (unmasque "0011NNNN.0III1001.0011DDDD.DDDDDDDD" word ()
    (list :bld.b )))

(specops bld (w op0 op1)
  ;; bld        #imm3,Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "10000111.NNNN1III"
            )))

(readops bld (word read)
  (unmasque "10000111.NNNN1III" word ()
    (list :bld )))

(specops bldnot.b (w op0 op1)
  ;; bldnot.b   #imm3,@(disp12,Rn)
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.0III1001.1011DDDD.DDDDDDDD"
            )))

(readops bldnot.b (word read)
  (unmasque "0011NNNN.0III1001.1011DDDD.DDDDDDDD" word ()
    (list :bldnot.b )))

(specops bor.b (w op0 op1)
  ;; bor.b      #imm3,@(disp12,Rn)
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.0III1001.0101DDDD.DDDDDDDD"
            )))

(readops bor.b (word read)
  (unmasque "0011NNNN.0III1001.0101DDDD.DDDDDDDD" word ()
    (list :bor.b )))

(specops bornot.b (w op0 op1)
  ;; bornot.b   #imm3,@(disp12,Rn)
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.0III1001.1101DDDD.DDDDDDDD"
            )))

(readops bornot.b (word read)
  (unmasque "0011NNNN.0III1001.1101DDDD.DDDDDDDD" word ()
    (list :bornot.b )))

(specops bset.b (w op0 op1)
  ;; bset.b     #imm3,@(disp12,Rn)
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.0III1001.0001DDDD.DDDDDDDD"
            )))

(readops bset.b (word read)
  (unmasque "0011NNNN.0III1001.0001DDDD.DDDDDDDD" word ()
    (list :bset.b )))

(specops bset (w op0 op1)
  ;; bset       #imm3,Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "10000110.NNNN1III"
            )))

(readops bset (word read)
  (unmasque "10000110.NNNN1III" word ()
    (list :bset )))

(specops bst.b (w op0 op1)
  ;; bst.b      #imm3,@(disp12,Rn)
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.0III1001.0010DDDD.DDDDDDDD"
            )))

(readops bst.b (word read)
  (unmasque "0011NNNN.0III1001.0010DDDD.DDDDDDDD" word ()
    (list :bst.b )))

(specops bst (w op0 op1)
  ;; bst        #imm3,Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "10000111.NNNN0III"
            )))

(readops bst (word read)
  (unmasque "10000111.NNNN0III" word ()
    (list :bst )))

(specops bxor.b (w op0 op1)
  ;; bxor.b     #imm3,@(disp12,Rn)
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.0III1001.0110DDDD.DDDDDDDD"
            )))

(readops bxor.b (word read)
  (unmasque "0011NNNN.0III1001.0110DDDD.DDDDDDDD" word ()
    (list :bxor.b )))

(specops add (w op0 op1)
  ;; add Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM1100"
            )))

(readops add (word read)
  (unmasque "0011NNNN.MMMM1100" word ()
    (list :add )))

(specops add (w op0 op1)
  ;; add #imm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0111NNNN.IIIIIIII"
            )))

(readops add (word read)
  (unmasque "0111NNNN.IIIIIIII" word ()
    (list :add )))

(specops addc (w op0 op1)
  ;; addc Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM1110"
            )))

(readops addc (word read)
  (unmasque "0011NNNN.MMMM1110" word ()
    (list :addc )))

(specops addv (w op0 op1)
  ;; addv Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM1111"
            )))

(readops addv (word read)
  (unmasque "0011NNNN.MMMM1111" word ()
    (list :addv )))

(specops cmp/eq (w op0 op1)
  ;; cmp/eq #imm,R0
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "10001000.IIIIIIII"
            )))

(readops cmp/eq (word read)
  (unmasque "10001000.IIIIIIII" word ()
    (list :cmp/eq )))

(specops cmp/eq (w op0 op1)
  ;; cmp/eq Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM0000"
            )))

(readops cmp/eq (word read)
  (unmasque "0011NNNN.MMMM0000" word ()
    (list :cmp/eq )))

(specops cmp/hs (w op0 op1)
  ;; cmp/hs Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM0010"
            )))

(readops cmp/hs (word read)
  (unmasque "0011NNNN.MMMM0010" word ()
    (list :cmp/hs )))

(specops cmp/ge (w op0 op1)
  ;; cmp/ge Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM0011"
            )))

(readops cmp/ge (word read)
  (unmasque "0011NNNN.MMMM0011" word ()
    (list :cmp/ge )))

(specops cmp/hi (w op0 op1)
  ;; cmp/hi Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM0110"
            )))

(readops cmp/hi (word read)
  (unmasque "0011NNNN.MMMM0110" word ()
    (list :cmp/hi )))

(specops cmp/gt (w op0 op1)
  ;; cmp/gt Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM0111"
            )))

(readops cmp/gt (word read)
  (unmasque "0011NNNN.MMMM0111" word ()
    (list :cmp/gt )))

(specops cmp/pl (w op0 op1)
  ;; cmp/pl Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00010101"
            )))

(readops cmp/pl (word read)
  (unmasque "0100NNNN.00010101" word ()
    (list :cmp/pl )))

(specops cmp/pz (w op0 op1)
  ;; cmp/pz Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00010001"
            )))

(readops cmp/pz (word read)
  (unmasque "0100NNNN.00010001" word ()
    (list :cmp/pz )))

(specops cmp/str (w op0 op1)
  ;; cmp/str Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0010NNNN.MMMM1100"
            )))

(readops cmp/str (word read)
  (unmasque "0010NNNN.MMMM1100" word ()
    (list :cmp/str )))

(specops clips.b (w op0 op1)
  ;; clips.b Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.10010001"
            )))

(readops clips.b (word read)
  (unmasque "0100NNNN.10010001" word ()
    (list :clips.b )))

(specops clips.w (w op0 op1)
  ;; clips.w Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.10010101"
            )))

(readops clips.w (word read)
  (unmasque "0100NNNN.10010101" word ()
    (list :clips.w )))

(specops clipu.b (w op0 op1)
  ;; clipu.b Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.10000001"
            )))

(readops clipu.b (word read)
  (unmasque "0100NNNN.10000001" word ()
    (list :clipu.b )))

(specops clipu.w (w op0 op1)
  ;; clipu.w Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.10000101"
            )))

(readops clipu.w (word read)
  (unmasque "0100NNNN.10000101" word ()
    (list :clipu.w )))

(specops div0s (w op0 op1)
  ;; div0s Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0010NNNN.MMMM0111"
            )))

(readops div0s (word read)
  (unmasque "0010NNNN.MMMM0111" word ()
    (list :div0s )))

(specops div0u (w op0 op1)
  ;; div0u
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "00000000.00011001"
            )))

(readops div0u (word read)
  (unmasque "00000000.00011001" word ()
    (list :div0u )))

(specops div1 (w op0 op1)
  ;; div1 Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM0100"
            )))

(readops div1 (word read)
  (unmasque "0011NNNN.MMMM0100" word ()
    (list :div1 )))

(specops divs (w op0 op1)
  ;; divs R0,Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.10010100"
            )))

(readops divs (word read)
  (unmasque "0100NNNN.10010100" word ()
    (list :divs )))

(specops divu (w op0 op1)
  ;; divu R0,Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.10000100"
            )))

(readops divu (word read)
  (unmasque "0100NNNN.10000100" word ()
    (list :divu )))

(specops dmuls.l (w op0 op1)
  ;; dmuls.l Rm,Rn
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM1101"
            )))

(readops dmuls.l (word read)
  (unmasque "0011NNNN.MMMM1101" word ()
    (list :dmuls.l )))

(specops dmulu.l (w op0 op1)
  ;; dmulu.l Rm,Rn
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM0101"
            )))

(readops dmulu.l (word read)
  (unmasque "0011NNNN.MMMM0101" word ()
    (list :dmulu.l )))

(specops dt (w op0 op1)
  ;; dt Rn
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00010000"
            )))

(readops dt (word read)
  (unmasque "0100NNNN.00010000" word ()
    (list :dt )))

(specops exts.b (w op0 op1)
  ;; exts.b Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0110NNNN.MMMM1110"
            )))

(readops exts.b (word read)
  (unmasque "0110NNNN.MMMM1110" word ()
    (list :exts.b )))

(specops exts.w (w op0 op1)
  ;; exts.w Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0110NNNN.MMMM1111"
            )))

(readops exts.w (word read)
  (unmasque "0110NNNN.MMMM1111" word ()
    (list :exts.w )))

(specops extu.b (w op0 op1)
  ;; extu.b Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0110NNNN.MMMM1100"
            )))

(readops extu.b (word read)
  (unmasque "0110NNNN.MMMM1100" word ()
    (list :extu.b )))

(specops extu.w (w op0 op1)
  ;; extu.w Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0110NNNN.MMMM1101"
            )))

(readops extu.w (word read)
  (unmasque "0110NNNN.MMMM1101" word ()
    (list :extu.w )))

(specops mac.l (w op0 op1)
  ;; mac.l @Rm+,@Rn+
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.MMMM1111"
            )))

(readops mac.l (word read)
  (unmasque "0000NNNN.MMMM1111" word ()
    (list :mac.l )))

(specops mac.w (w op0 op1)
  ;; mac.w @Rm+,@Rn+
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.MMMM1111"
            )))

(readops mac.w (word read)
  (unmasque "0100NNNN.MMMM1111" word ()
    (list :mac.w )))

(specops mul.l (w op0 op1)
  ;; mul.l Rm,Rn
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.MMMM0111"
            )))

(readops mul.l (word read)
  (unmasque "0000NNNN.MMMM0111" word ()
    (list :mul.l )))

(specops mulr (w op0 op1)
  ;; mulr R0,Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.10000000"
            )))

(readops mulr (word read)
  (unmasque "0100NNNN.10000000" word ()
    (list :mulr )))

(specops muls.w (w op0 op1)
  ;; muls.w Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0010NNNN.MMMM1111"
            )))

(readops muls.w (word read)
  (unmasque "0010NNNN.MMMM1111" word ()
    (list :muls.w )))

(specops mulu.w (w op0 op1)
  ;; mulu.w Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0010NNNN.MMMM1110"
            )))

(readops mulu.w (word read)
  (unmasque "0010NNNN.MMMM1110" word ()
    (list :mulu.w )))

(specops neg (w op0 op1)
  ;; neg Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0110NNNN.MMMM1011"
            )))

(readops neg (word read)
  (unmasque "0110NNNN.MMMM1011" word ()
    (list :neg )))

(specops negc (w op0 op1)
  ;; negc Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0110NNNN.MMMM1010"
            )))

(readops negc (word read)
  (unmasque "0110NNNN.MMMM1010" word ()
    (list :negc )))

(specops sub (w op0 op1)
  ;; sub Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM1000"
            )))

(readops sub (word read)
  (unmasque "0011NNNN.MMMM1000" word ()
    (list :sub )))

(specops subc (w op0 op1)
  ;; subc Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM1010"
            )))

(readops subc (word read)
  (unmasque "0011NNNN.MMMM1010" word ()
    (list :subc )))

(specops subv (w op0 op1)
  ;; subv Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0011NNNN.MMMM1011"
            )))

(readops subv (word read)
  (unmasque "0011NNNN.MMMM1011" word ()
    (list :subv )))

(specops and (w op0 op1)
  ;; and Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0010NNNN.MMMM1001"
            )))

(readops and (word read)
  (unmasque "0010NNNN.MMMM1001" word ()
    (list :and )))

(specops and (w op0 op1)
  ;; and #imm,R0
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "11001001.IIIIIIII"
            )))

(readops and (word read)
  (unmasque "11001001.IIIIIIII" word ()
    (list :and )))

(specops and.b (w op0 op1)
  ;; and.b #imm,@(R0,GBR)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "11001101.IIIIIIII"
            )))

(readops and.b (word read)
  (unmasque "11001101.IIIIIIII" word ()
    (list :and.b )))

(specops not (w op0 op1)
  ;; not Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0110NNNN.MMMM0111"
            )))

(readops not (word read)
  (unmasque "0110NNNN.MMMM0111" word ()
    (list :not )))

(specops or (w op0 op1)
  ;; or Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0010NNNN.MMMM1011"
            )))

(readops or (word read)
  (unmasque "0010NNNN.MMMM1011" word ()
    (list :or )))

(specops or (w op0 op1)
  ;; or #imm,R0
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "11001011.IIIIIIII"
            )))

(readops or (word read)
  (unmasque "11001011.IIIIIIII" word ()
    (list :or )))

(specops or.b (w op0 op1)
  ;; or.b #imm,@(R0,GBR)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "11001111.IIIIIIII"
            )))

(readops or.b (word read)
  (unmasque "11001111.IIIIIIII" word ()
    (list :or.b )))

(specops tas.b (w op0 op1)
  ;; tas.b @Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00011011"
            )))

(readops tas.b (word read)
  (unmasque "0100NNNN.00011011" word ()
    (list :tas.b )))

(specops tst (w op0 op1)
  ;; tst Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0010NNNN.MMMM1000"
            )))

(readops tst (word read)
  (unmasque "0010NNNN.MMMM1000" word ()
    (list :tst )))

(specops tst (w op0 op1)
  ;; tst #imm,R0
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "11001000.IIIIIIII"
            )))

(readops tst (word read)
  (unmasque "11001000.IIIIIIII" word ()
    (list :tst )))

(specops tst.b (w op0 op1)
  ;; tst.b #imm,@(R0,GBR)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "11001100.IIIIIIII"
            )))

(readops tst.b (word read)
  (unmasque "11001100.IIIIIIII" word ()
    (list :tst.b )))

(specops xor (w op0 op1)
  ;; xor Rm,Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0010NNNN.MMMM1010"
            )))

(readops xor (word read)
  (unmasque "0010NNNN.MMMM1010" word ()
    (list :xor )))

(specops xor (w op0 op1)
  ;; xor #imm,R0
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "11001010.IIIIIIII"
            )))

(readops xor (word read)
  (unmasque "11001010.IIIIIIII" word ()
    (list :xor )))

(specops xor.b (w op0 op1)
  ;; xor.b #imm,@(R0,GBR)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "11001110.IIIIIIII"
            )))

(readops xor.b (word read)
  (unmasque "11001110.IIIIIIII" word ()
    (list :xor.b )))

(specops rotcl (w op0 op1)
  ;; rotcl Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00100100"
            )))

(readops rotcl (word read)
  (unmasque "0100NNNN.00100100" word ()
    (list :rotcl )))

(specops rotcr (w op0 op1)
  ;; rotcr Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00100101"
            )))

(readops rotcr (word read)
  (unmasque "0100NNNN.00100101" word ()
    (list :rotcr )))

(specops rotl (w op0 op1)
  ;; rotl Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00000100"
            )))

(readops rotl (word read)
  (unmasque "0100NNNN.00000100" word ()
    (list :rotl )))

(specops rotr (w op0 op1)
  ;; rotr Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00000101"
            )))

(readops rotr (word read)
  (unmasque "0100NNNN.00000101" word ()
    (list :rotr )))

(specops shad (w op0 op1)
  ;; shad Rm,Rn
  ((:for-types :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.MMMM1100"
            )))

(readops shad (word read)
  (unmasque "0100NNNN.MMMM1100" word ()
    (list :shad )))

(specops shal (w op0 op1)
  ;; shal Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00100000"
            )))

(readops shal (word read)
  (unmasque "0100NNNN.00100000" word ()
    (list :shal )))

(specops shar (w op0 op1)
  ;; shar Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00100001"
            )))

(readops shar (word read)
  (unmasque "0100NNNN.00100001" word ()
    (list :shar )))

(specops shld (w op0 op1)
  ;; shld Rm,Rn
  ((:for-types :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.MMMM1101"
            )))

(readops shld (word read)
  (unmasque "0100NNNN.MMMM1101" word ()
    (list :shld )))

(specops shll (w op0 op1)
  ;; shll Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00000000"
            )))

(readops shll (word read)
  (unmasque "0100NNNN.00000000" word ()
    (list :shll )))

(specops shll2 (w op0 op1)
  ;; shll2 Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00001000"
            )))

(readops shll2 (word read)
  (unmasque "0100NNNN.00001000" word ()
    (list :shll2 )))

(specops shll8 (w op0 op1)
  ;; shll8 Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00011000"
            )))

(readops shll8 (word read)
  (unmasque "0100NNNN.00011000" word ()
    (list :shll8 )))

(specops shll16 (w op0 op1)
  ;; shll16 Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00101000"
            )))

(readops shll16 (word read)
  (unmasque "0100NNNN.00101000" word ()
    (list :shll16 )))

(specops shlr (w op0 op1)
  ;; shlr Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00000001"
            )))

(readops shlr (word read)
  (unmasque "0100NNNN.00000001" word ()
    (list :shlr )))

(specops shlr2 (w op0 op1)
  ;; shlr2 Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00001001"
            )))

(readops shlr2 (word read)
  (unmasque "0100NNNN.00001001" word ()
    (list :shlr2 )))

(specops shlr8 (w op0 op1)
  ;; shlr8 Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00011001"
            )))

(readops shlr8 (word read)
  (unmasque "0100NNNN.00011001" word ()
    (list :shlr8 )))

(specops shlr16 (w op0 op1)
  ;; shlr16 Rn
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.00101001"
            )))

(readops shlr16 (word read)
  (unmasque "0100NNNN.00101001" word ()
    (list :shlr16 )))

(specops bf (w op0 op1)
  ;; bf label
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "10001011.DDDDDDDD"
            )))

(readops bf (word read)
  (unmasque "10001011.DDDDDDDD" word ()
    (list :bf )))

(specops bf/s (w op0 op1)
  ;; bf/s label
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "10001111.DDDDDDDD"
            )))

(readops bf/s (word read)
  (unmasque "10001111.DDDDDDDD" word ()
    (list :bf/s )))

(specops bt (w op0 op1)
  ;; bt label
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "10001001.DDDDDDDD"
            )))

(readops bt (word read)
  (unmasque "10001001.DDDDDDDD" word ()
    (list :bt )))

(specops bt/s (w op0 op1)
  ;; bt/s label
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "10001101.DDDDDDDD"
            )))

(readops bt/s (word read)
  (unmasque "10001101.DDDDDDDD" word ()
    (list :bt/s )))

(specops bra (w op0 op1)
  ;; bra label
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1010DDDD.DDDDDDDD"
            )))

(readops bra (word read)
  (unmasque "1010DDDD.DDDDDDDD" word ()
    (list :bra )))

(specops braf (w op0 op1)
  ;; braf Rm
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0000MMMM.00100011"
            )))

(readops braf (word read)
  (unmasque "0000MMMM.00100011" word ()
    (list :braf )))

(specops bsr (w op0 op1)
  ;; bsr label
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "1011DDDD.DDDDDDDD"
            )))

(readops bsr (word read)
  (unmasque "1011DDDD.DDDDDDDD" word ()
    (list :bsr )))

(specops bsrf (w op0 op1)
  ;; bsrf Rm
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0000MMMM.00000011"
            )))

(readops bsrf (word read)
  (unmasque "0000MMMM.00000011" word ()
    (list :bsrf )))

(specops jmp (w op0 op1)
  ;; jmp @Rm
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00101011"
            )))

(readops jmp (word read)
  (unmasque "0100MMMM.00101011" word ()
    (list :jmp )))

(specops jsr (w op0 op1)
  ;; jsr @Rm
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00001011"
            )))

(readops jsr (word read)
  (unmasque "0100MMMM.00001011" word ()
    (list :jsr )))

(specops jsr/n (w op0 op1)
  ;; jsr/n @Rm
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01001011"
            )))

(readops jsr/n (word read)
  (unmasque "0100MMMM.01001011" word ()
    (list :jsr/n )))

(specops jsr/n (w op0 op1)
  ;; jsr/n @@(disp8,TBR)
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "10000011.DDDDDDDD"
            )))

(readops jsr/n (word read)
  (unmasque "10000011.DDDDDDDD" word ()
    (list :jsr/n )))

(specops rts (w op0 op1)
  ;; rts
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "00000000.00001011"
            )))

(readops rts (word read)
  (unmasque "00000000.00001011" word ()
    (list :rts )))

(specops rts/n (w op0 op1)
  ;; rts/n
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "00000000.01101011"
            )))

(readops rts/n (word read)
  (unmasque "00000000.01101011" word ()
    (list :rts/n )))

(specops rtv/n (w op0 op1)
  ;; rtv/n Rm
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0000MMMM.01111011"
            )))

(readops rtv/n (word read)
  (unmasque "0000MMMM.01111011" word ()
    (list :rtv/n )))

(specops clrmac (w op0 op1)
  ;; clrmac
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "00000000.00101000"
            )))

(readops clrmac (word read)
  (unmasque "00000000.00101000" word ()
    (list :clrmac )))

(specops clrs (w op0 op1)
  ;; clrs
  ((:for-types :sh3 :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "00000000.01001000"
            )))

(readops clrs (word read)
  (unmasque "00000000.01001000" word ()
    (list :clrs )))

(specops clrt (w op0 op1)
  ;; clrt
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "00000000.00001000"
            )))

(readops clrt (word read)
  (unmasque "00000000.00001000" word ()
    (list :clrt )))

(specops icbi (w op0 op1)
  ;; icbi @Rn
  ((:for-types :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.11100011"
            )))

(readops icbi (word read)
  (unmasque "0000NNNN.11100011" word ()
    (list :icbi )))

(specops ldbank (w op0 op1)
  ;; ldbank @Rm,R0
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.11100101"
            )))

(readops ldbank (word read)
  (unmasque "0100MMMM.11100101" word ()
    (list :ldbank )))

(specops ldc (w op0 op1)
  ;; ldc Rm,SR
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00001110"
            )))

(readops ldc (word read)
  (unmasque "0100MMMM.00001110" word ()
    (list :ldc )))

(specops ldc.l (w op0 op1)
  ;; ldc.l @Rm+,SR
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00000111"
            )))

(readops ldc.l (word read)
  (unmasque "0100MMMM.00000111" word ()
    (list :ldc.l )))

(specops ldc (w op0 op1)
  ;; ldc Rm,TBR
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01001010"
            )))

(readops ldc (word read)
  (unmasque "0100MMMM.01001010" word ()
    (list :ldc )))

(specops ldc (w op0 op1)
  ;; ldc Rm,GBR
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00011110"
            )))

(readops ldc (word read)
  (unmasque "0100MMMM.00011110" word ()
    (list :ldc )))

(specops ldc.l (w op0 op1)
  ;; ldc.l @Rm+,GBR
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00010111"
            )))

(readops ldc.l (word read)
  (unmasque "0100MMMM.00010111" word ()
    (list :ldc.l )))

(specops ldc (w op0 op1)
  ;; ldc Rm,VBR
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00101110"
            )))

(readops ldc (word read)
  (unmasque "0100MMMM.00101110" word ()
    (list :ldc )))

(specops ldc.l (w op0 op1)
  ;; ldc.l @Rm+,VBR
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00100111"
            )))

(readops ldc.l (word read)
  (unmasque "0100MMMM.00100111" word ()
    (list :ldc.l )))

(specops ldc (w op0 op1)
  ;; ldc Rm,MOD
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01011110"
            )))

(readops ldc (word read)
  (unmasque "0100MMMM.01011110" word ()
    (list :ldc )))

(specops ldc.l (w op0 op1)
  ;; ldc.l @Rm+,MOD
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01010111"
            )))

(readops ldc.l (word read)
  (unmasque "0100MMMM.01010111" word ()
    (list :ldc.l )))

(specops ldc (w op0 op1)
  ;; ldc Rm,RE
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01111110"
            )))

(readops ldc (word read)
  (unmasque "0100MMMM.01111110" word ()
    (list :ldc )))

(specops ldc.l (w op0 op1)
  ;; ldc.l @Rm+,RE
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01110111"
            )))

(readops ldc.l (word read)
  (unmasque "0100MMMM.01110111" word ()
    (list :ldc.l )))

(specops ldc (w op0 op1)
  ;; ldc Rm,RS
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01101110"
            )))

(readops ldc (word read)
  (unmasque "0100MMMM.01101110" word ()
    (list :ldc )))

(specops ldc.l (w op0 op1)
  ;; ldc.l @Rm+,RS
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01100111"
            )))

(readops ldc.l (word read)
  (unmasque "0100MMMM.01100111" word ()
    (list :ldc.l )))

(specops ldc (w op0 op1)
  ;; ldc Rm,SGR
  ((:for-types :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00111010"
            )))

(readops ldc (word read)
  (unmasque "0100MMMM.00111010" word ()
    (list :ldc )))

(specops ldc.l (w op0 op1)
  ;; ldc.l @Rm+,SGR
  ((:for-types :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00110110"
            )))

(readops ldc.l (word read)
  (unmasque "0100MMMM.00110110" word ()
    (list :ldc.l )))

(specops ldc (w op0 op1)
  ;; ldc Rm,SSR
  ((:for-types :sh3 :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00111110"
            )))

(readops ldc (word read)
  (unmasque "0100MMMM.00111110" word ()
    (list :ldc )))

(specops ldc.l (w op0 op1)
  ;; ldc.l @Rm+,SSR
  ((:for-types :sh3 :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00110111"
            )))

(readops ldc.l (word read)
  (unmasque "0100MMMM.00110111" word ()
    (list :ldc.l )))

(specops ldc (w op0 op1)
  ;; ldc Rm,SPC
  ((:for-types :sh3 :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01001110"
            )))

(readops ldc (word read)
  (unmasque "0100MMMM.01001110" word ()
    (list :ldc )))

(specops ldc.l (w op0 op1)
  ;; ldc.l @Rm+,SPC
  ((:for-types :sh3 :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01000111"
            )))

(readops ldc.l (word read)
  (unmasque "0100MMMM.01000111" word ()
    (list :ldc.l )))

(specops ldc (w op0 op1)
  ;; ldc Rm,DBR
  ((:for-types :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.11111010"
            )))

(readops ldc (word read)
  (unmasque "0100MMMM.11111010" word ()
    (list :ldc )))

(specops ldc.l (w op0 op1)
  ;; ldc.l @Rm+,DBR
  ((:for-types :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.11110110"
            )))

(readops ldc.l (word read)
  (unmasque "0100MMMM.11110110" word ()
    (list :ldc.l )))

(specops ldc (w op0 op1)
  ;; ldc Rm,Rn_BANK
  ((:for-types :sh3 :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.1NNN1110"
            )))

(readops ldc (word read)
  (unmasque "0100MMMM.1NNN1110" word ()
    (list :ldc )))

(specops ldc.l (w op0 op1)
  ;; ldc.l @Rm+,Rn_BANK
  ((:for-types :sh3 :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.1NNN0111"
            )))

(readops ldc.l (word read)
  (unmasque "0100MMMM.1NNN0111" word ()
    (list :ldc.l )))

(specops ldre (w op0 op1)
  ;; ldre @(disp,PC)
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "10001110.DDDDDDDD"
            )))

(readops ldre (word read)
  (unmasque "10001110.DDDDDDDD" word ()
    (list :ldre )))

(specops ldrs (w op0 op1)
  ;; ldrs @(disp,PC)
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "10001100.DDDDDDDD"
            )))

(readops ldrs (word read)
  (unmasque "10001100.DDDDDDDD" word ()
    (list :ldrs )))

(specops lds (w op0 op1)
  ;; lds Rm,MACH
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00001010"
            )))

(readops lds (word read)
  (unmasque "0100MMMM.00001010" word ()
    (list :lds )))

(specops lds.l (w op0 op1)
  ;; lds.l @Rm+,MACH
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00000110"
            )))

(readops lds.l (word read)
  (unmasque "0100MMMM.00000110" word ()
    (list :lds.l )))

(specops lds (w op0 op1)
  ;; lds Rm,MACL
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00011010"
            )))

(readops lds (word read)
  (unmasque "0100MMMM.00011010" word ()
    (list :lds )))

(specops lds.l (w op0 op1)
  ;; lds.l @Rm+,MACL
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00010110"
            )))

(readops lds.l (word read)
  (unmasque "0100MMMM.00010110" word ()
    (list :lds.l )))

(specops lds (w op0 op1)
  ;; lds Rm,PR
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00101010"
            )))

(readops lds (word read)
  (unmasque "0100MMMM.00101010" word ()
    (list :lds )))

(specops lds.l (w op0 op1)
  ;; lds.l @Rm+,PR
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00100110"
            )))

(readops lds.l (word read)
  (unmasque "0100MMMM.00100110" word ()
    (list :lds.l )))

(specops lds (w op0 op1)
  ;; lds Rm,DSR
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01101010"
            )))

(readops lds (word read)
  (unmasque "0100MMMM.01101010" word ()
    (list :lds )))

(specops lds.l (w op0 op1)
  ;; lds.l @Rm+,DSR
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01100110"
            )))

(readops lds.l (word read)
  (unmasque "0100MMMM.01100110" word ()
    (list :lds.l )))

(specops lds (w op0 op1)
  ;; lds Rm,A0
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01110110"
            )))

(readops lds (word read)
  (unmasque "0100MMMM.01110110" word ()
    (list :lds )))

(specops lds.l (w op0 op1)
  ;; lds.l @Rm+,A0
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.01110110"
            )))

(readops lds.l (word read)
  (unmasque "0100MMMM.01110110" word ()
    (list :lds.l )))

(specops lds (w op0 op1)
  ;; lds Rm,X0
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.10001010"
            )))

(readops lds (word read)
  (unmasque "0100MMMM.10001010" word ()
    (list :lds )))

(specops lds.l (w op0 op1)
  ;; lds.l @Rm+,X0
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.10000110"
            )))

(readops lds.l (word read)
  (unmasque "0100NNNN.10000110" word ()
    (list :lds.l )))

(specops lds (w op0 op1)
  ;; lds Rm,X1
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.10011010"
            )))

(readops lds (word read)
  (unmasque "0100MMMM.10011010" word ()
    (list :lds )))

(specops lds.l (w op0 op1)
  ;; lds.l @Rm+,X1
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.10010110"
            )))

(readops lds.l (word read)
  (unmasque "0100NNNN.10010110" word ()
    (list :lds.l )))

(specops lds (w op0 op1)
  ;; lds Rm,Y0
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.10101010"
            )))

(readops lds (word read)
  (unmasque "0100MMMM.10101010" word ()
    (list :lds )))

(specops lds.l (w op0 op1)
  ;; lds.l @Rm+,Y0
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.10100110"
            )))

(readops lds.l (word read)
  (unmasque "0100NNNN.10100110" word ()
    (list :lds.l )))

(specops lds (w op0 op1)
  ;; lds Rm,Y1
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.10111010"
            )))

(readops lds (word read)
  (unmasque "0100MMMM.10111010" word ()
    (list :lds )))

(specops lds.l (w op0 op1)
  ;; lds.l @Rm+,Y1
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.10110110"
            )))

(readops lds.l (word read)
  (unmasque "0100NNNN.10110110" word ()
    (list :lds.l )))

(specops ldtlb (w op0 op1)
  ;; ldtlb
  ((:for-types :sh3 :sh4 :sh4a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "00000000.00111000"
            )))

(readops ldtlb (word read)
  (unmasque "00000000.00111000" word ()
    (list :ldtlb )))

(specops movca.l (w op0 op1)
  ;; movca.l R0,@Rn
  ((:for-types :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.11000011"
            )))

(readops movca.l (word read)
  (unmasque "0000NNNN.11000011" word ()
    (list :movca.l )))

(specops nop (w op0 op1)
  ;; nop
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "00000000.00001001"
            )))

(readops nop (word read)
  (unmasque "00000000.00001001" word ()
    (list :nop )))

(specops ocbi (w op0 op1)
  ;; ocbi @Rn
  ((:for-types :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.10010011"
            )))

(readops ocbi (word read)
  (unmasque "0000NNNN.10010011" word ()
    (list :ocbi )))

(specops ocbp (w op0 op1)
  ;; ocbp @Rn
  ((:for-types :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.10100011"
            )))

(readops ocbp (word read)
  (unmasque "0000NNNN.10100011" word ()
    (list :ocbp )))

(specops ocbwb (w op0 op1)
  ;; ocbwb @Rn
  ((:for-types :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.10110011"
            )))

(readops ocbwb (word read)
  (unmasque "0000NNNN.10110011" word ()
    (list :ocbwb )))

(specops pref (w op0 op1)
  ;; pref @Rn
  ((:for-types :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.10000011"
            )))

(readops pref (word read)
  (unmasque "0000NNNN.10000011" word ()
    (list :pref )))

(specops prefi (w op0 op1)
  ;; prefi @Rn
  ((:for-types :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "0000NNNN.11010011"
            )))

(readops prefi (word read)
  (unmasque "0000NNNN.11010011" word ()
    (list :prefi )))

(specops resbank (w op0 op1)
  ;; resbank
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "00000000.01011011"
            )))

(readops resbank (word read)
  (unmasque "00000000.01011011" word ()
    (list :resbank )))

(specops rte (w op0 op1)
  ;; rte
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "00000000.00101011"
            )))

(readops rte (word read)
  (unmasque "00000000.00101011" word ()
    (list :rte )))

(specops setrc (w op0 op1)
  ;; setrc Rn
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "0100MMMM.00010100"
            )))

(readops setrc (word read)
  (unmasque "0100MMMM.00010100" word ()
    (list :setrc )))

(specops setrc (w op0 op1)
  ;; setrc #imm
  ((:for-types :dsp))
  (address (op0 op1) (index0 index1)
    (masque "10000010.IIIIIIII"
            )))

(readops setrc (word read)
  (unmasque "10000010.IIIIIIII" word ()
    (list :setrc )))

(specops sets (w op0 op1)
  ;; sets
  ((:for-types :sh3 :sh4 :sh4a))
  (address (op0 op1) (index0 index1)
    (masque "00000000.01011000"
            )))

(readops sets (word read)
  (unmasque "00000000.01011000" word ()
    (list :sets )))

(specops sett (w op0 op1)
  ;; sett
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "00000000.00011000"
            )))

(readops sett (word read)
  (unmasque "00000000.00011000" word ()
    (list :sett )))

(specops sleep (w op0 op1)
  ;; sleep
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a :privileged))
  (address (op0 op1) (index0 index1)
    (masque "00000000.00011011"
            )))

(readops sleep (word read)
  (unmasque "00000000.00011011" word ()
    (list :sleep )))

(specops stbank (w op0 op1)
  ;; stbank R0,@Rn
  ((:for-types :sh2a))
  (address (op0 op1) (index0 index1)
    (masque "0100NNNN.11100001"
            )))

(readops stbank (word read)
  (unmasque "0100NNNN.11100001" word ()
    (list :stbank )))

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

