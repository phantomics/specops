

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
