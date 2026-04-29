;;;; ebcdic.lisp
;;;; EBCDIC character encoding tables

(defpackage #:specops/format.ebcdic
  (:use :cl)
  (:shadowing-import-from #:specops #:defcodetable)
  (:export #:ebcdic-code-cp037 #:ebcdic-code-cp310))

(in-package #:specops/format.ebcdic)

(defcodetable ebcdic-code-cp037
  ((:store . :vector))
  (_    #x0  #x1  #x2  #x3  #x4  #x5  #x6  #x7  #x8  #x9  #xA  #xB  #xC  #xD  #xE  #xF )
  (#x00 :nul :soh :stx :etx :sel :ht  :rnl :del :ge  :sps :rpt :vt  :ff  :cr  :so  :si )
  (#x10 :dle :dc1 :dc2 :dc3 :res :nl  :bs  :poc :can :em  :ubs :cu1 :ifs :igs :irs :ius)
  (#x20 :ds  :sos :fs  :wus :byp :lf  :etb :esc :sa  :sfe :sm  :csp :mfa :enq :ack :bel)
  (#x30 _    _    :syn :ir  :pp  :trn :nbs :eot :sbs :it  :rff :cu3 :dc4 :nak _    :sub)
  (#x40 #\   :rsp #\â  #\ä  #\à  #\á  #\ã  #\å  #\ç  #\ñ  #\¢  #\.  #\<  #\(  #\+  #\| )
  (#x50 #\&  #\é  #\ê  #\ë  #\è  #\í  #\î  #\ï  #\ì  #\ß  #\!  #\$  #\*  #\)  #\;  #\¬ )
  (#x60 #\-  #\/  #\Â  #\Ä  #\À  #\Á  #\Ã  #\Å  #\Ç  #\Ñ  #\¦  #\,  #\%  #\_  #\>  #\? )
  (#x70 #\ø  #\É  #\Ê  #\Ë  #\È  #\Í  #\Î  #\Ï  #\Ì  #\`  #\:  #\#  #\@  #\'  #\=  #\" )
  (#x80 #\Ø  #\a  #\b  #\c  #\d  #\e  #\f  #\g  #\h  #\i  #\«  #\»  #\ð  #\ý  #\þ  #\± )
  (#x90 #\°  #\j  #\k  #\l  #\m  #\n  #\o  #\p  #\q  #\r  #\ª  #\º  #\æ  #\¸  #\Æ  #\¤ )
  (#xA0 #\µ  #\~  #\s  #\t  #\u  #\v  #\w  #\x  #\y  #\z  #\¡  #\¿  #\Ð  #\Ý  #\Þ  #\® )
  (#xB0 #\^  #\£  #\¥  #\·  #\©  #\§  #\¶  #\¼  #\½  #\¾  #\[  #\]  #\¯  #\¨  #\´  #\× )
  (#xC0 #\{  #\A  #\B  #\C  #\D  #\E  #\F  #\G  #\H  #\I  :shy #\ô  #\ö  #\ò  #\ó  #\õ )
  (#xD0 #\}  #\J  #\K  #\L  #\M  #\N  #\O  #\P  #\Q  #\R  #\¹  #\û  #\ü  #\ù  #\ú  #\ÿ )
  (#xE0 #\\  #\÷  #\S  #\T  #\U  #\V  #\W  #\X  #\Y  #\Z  #\²  #\Ô  #\Ö  #\Ò  #\Ó  #\Õ )
  (#xF0 #\0  #\1  #\2  #\3  #\4  #\5  #\6  #\7  #\8  #\9  #\³  #\Û  #\Ü  #\Ù  #\Ú  :eo ))

;;; Code Page 310 - Graphic Escape APL/TN
;;; Used alongside Code Page 37 with codes prefixed by Graphic Escape (0x08)
;;; Underscored alphabet characters omitted (left as empty cells)
;;; Rows 0x00-0x3F are unused in CP310
 
(defcodetable ebcdic-code-cp310 ;; chars over 125 -- select from here
  ((:store . :htable))
  (_    #x0  #x1  #x2  #x3  #x4  #x5  #x6  #x7  #x8  #x9  #xA  #xB  #xC  #xD  #xE  #xF )
  (#x00 _    _    _    _    _    _    _    _    _    _    _    _    _    _    _    _   )
  (#x10 _    _    _    _    _    _    _    _    _    _    _    _    _    _    _    _   )
  (#x20 _    _    _    _    _    _    _    _    _    _    _    _    _    _    _    _   )
  (#x30 _    _    _    _    _    _    _    _    _    _    _    _    _    _    _    _   )
  (#x40 #\   _    _    _    _    _    _    _    _    _    _    _    _    _    _    _   )
  (#x50 _    _    _    _    _    _    _    _    _    _    _    _    _    _    _    _   )
  (#x60 _    _    _    _    _    _    _    _    _    _    _    _    _    _    _    _   )
  (#x70 #\⋄  #\∧  #\¨  #\⌻  #\⍸  #\⍷  #\⊢  #\⊣  #\∨  _    _    _    _    _    _    _   )
  (#x80 #\∼  #\║  #\═  #\⎸  #\⎹  #\│  _    _    _    _    #\↑  #\↓  #\≤  #\⌈  #\⌊  #\→ )
  (#x90 #\⎕  #\▌  #\▐  #\▀  #\▄  #\█  _    _    _    _    #\⊃  #\⊂  #\⌑  #\○  #\±  #\← )
  (#xA0 #\¯  #\°  #\─  #\∙  #\ₙ  _    _    _    _    _    #\∩  #\∪  #\⊥  #\[  #\≥  #\∘ )
  (#xB0 #\⍺  #\∊  #\⍳  #\⍴  #\⍵  _    #\×  #\∖  #\÷  _    #\∇  #\∆  #\⊤  #\]  #\≠  #\∣ )
  (#xC0 #\{  #\⁽  #\⁺  #\■  #\└  #\┌  #\├  #\┴  #\§  _    #\⍲  #\⍱  #\⌷  #\⌽  #\⍂  #\⍉ )
  (#xD0 #\}  #\⁾  #\⁻  #\┼  #\┘  #\┐  #\┤  #\┬  #\¶  _    #\⌶  #\!  #\⍒  #\⍋  #\⍞  #\⍝ )
  (#xE0 #\≡  #\₁  #\₂  #\₃  #\⍤  #\⍥  #\⍪  #\€  _    _    #\⌿  #\⍀  #\∵  #\⊖  #\⌹  #\⍕ )
  (#xF0 #\⁰  #\¹  #\²  #\³  #\⁴  #\⁵  #\⁶  #\⁷  #\⁸  #\⁹  _    #\⍫  #\⍙  #\⍟  #\⍎  _   ))
