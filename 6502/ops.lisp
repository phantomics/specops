;;;; ops.lisp

(in-package #:specops.6502)

(specops 0 (op0 &optional op1) *assembler-prototype-6502*
  ;; the table of 6502 instructions, including illegal instructions
  ((:tabular :cross-adding :matcher match-ops) (:duplex . of-decoder))
  ((    ) (#x0    ) (#x1      ) (#x2    ) (#x3      ) (#x4      ) (#x5      ) (#x6      ) (#x7      ) (#x8 ) (#x9      ) (#xA   ) (#xB      ) (#xC      ) (#xD      ) (#xE      ) (#xF      ))
  ((#x00) (:brk   ) (:ora x in) ( jam   ) (:slo x in) ( nop zp  ) (:ora zp  ) (:asl zp  ) ( slo zp  ) (:php) (:ora iv  ) (:asl a) ( anc iv  ) ( nop ab  ) (:ora ab  ) (:asl ab  ) ( slo ab  ))
  ((#x10) (:bpl rl) (:ora in y) ( jam   ) (:slo in y) ( nop zp x) (:ora zp x) (:asl zp x) ( slo zp x) (:clc) (:ora ab y) ( nop  ) ( slo ab y) ( nop ab x) (:ora ab x) (:asl ab x) ( slo ab x))
  ((#x20) (:jsr ab) (:and x in) ( jam   ) (:rla x in) (:bit zp  ) (:and zp  ) (:rol zp  ) ( rla zp  ) (:plp) (:and iv  ) (:rol a) ( anc iv  ) (:bit ab  ) (:and ab  ) (:rol ab  ) ( rla ab  ))
  ((#x30) (:bmi rl) (:and in y) ( jam   ) (:rla in y) ( nop zp x) (:and zp x) (:rol zp x) ( rla zp x) (:sec) (:and ab y) ( nop  ) ( rla ab y) ( nop ab x) (:and ab x) (:rol ab x) ( rla ab x))
  ((#x40) (:rti   ) (:eor x in) ( jam   ) (:sre x in) ( nop zp  ) (:eor zp  ) (:lsr zp  ) ( sre zp  ) (:pha) (:eor iv  ) (:lsr a) ( alr iv  ) (:jmp ab  ) (:eor ab  ) (:lsr ab  ) ( sre ab  ))
  ((#x50) (:bvc rl) (:eor in y) ( jam   ) (:sre in y) ( nop zp x) (:eor zp x) (:lsr zp x) ( sre zp x) (:cli) (:eor ab y) ( nop  ) ( sre ab y) ( nop ab x) (:eor ab x) (:lsr ab x) ( sre ab x))
  ((#x60) (:rts   ) (:adc x in) ( jam   ) (:rra x in) ( nop zp  ) (:adc zp  ) (:ror zp  ) ( rra zp  ) (:pla) (:adc iv  ) (:ror a) ( arr iv  ) (:jmp inw ) (:adc ab  ) (:ror ab  ) ( rra ab  ))
  ((#x70) (:bvs rl) (:adc in y) ( jam   ) (:rra in y) ( nop zp x) (:adc zp x) (:ror zp x) ( rra zp x) (:sei) (:adc ab y) ( nop  ) ( rra ab y) ( nop ab x) (:adc ab x) (:ror ab x) ( rra ab x))
  ((#x80) ( nop iv) (:sta x in) ( nop iv) (:sax x in) (:sty zp  ) (:sta zp  ) (:stx zp  ) ( sax zp  ) (:dey) (:nop iv  ) (:txa  ) ( ane iv  ) (:sty ab  ) (:sta ab  ) (:stx ab  ) ( sax ab  ))
  ((#x90) (:bcc rl) (:sta in y) ( jam   ) (:sha in y) (:sty zp x) (:sta zp x) (:stx zp y) ( sax zp y) (:tya) (:sta ab y) (:txs  ) ( tas ab y) ( shy ab x) (:sta ab x) ( shx ab y) ( sha ab y))
  ((#xA0) (:ldy iv) (:lda x in) (:ldx iv) (:lax x in) (:ldy zp  ) (:lda zp  ) (:ldx zp  ) ( lax zp  ) (:tay) (:lda iv  ) (:tax  ) ( lxa iv  ) (:ldy ab  ) (:lda ab  ) (:ldx ab  ) ( lax ab  ))
  ((#xB0) (:bcs rl) (:lda in y) ( jam   ) (:lax in y) (:ldy zp x) (:lda zp x) (:ldx zp y) ( lax zp y) (:clv) (:lda ab y) (:tsx  ) ( las ab y) (:ldy ab x) (:lda ab x) (:ldx ab y) ( lax ab y))
  ((#xC0) (:cpy iv) (:cmp x in) ( nop iv) (:dcp x in) (:cpy zp  ) (:cmp zp  ) (:dec zp  ) ( dcp zp  ) (:iny) (:cmp iv  ) (:dex  ) ( sbx iv  ) (:cpy ab  ) (:cmp ab  ) (:dec ab  ) ( dcp ab  ))
  ((#xD0) (:bne rl) (:cmp in y) ( jam   ) (:dcp in y) ( nop zp x) (:cmp zp x) (:dec zp x) ( dcp zp x) (:cld) (:cmp ab y) ( nop  ) ( dcp ab y) ( nop ab x) (:cmp ab x) (:dec ab x) ( dcp ab x))
  ((#xE0) (:cpx iv) (:sbc x in) ( nop iv) (:isc x in) (:cpx zp  ) (:sbc zp  ) (:inc zp  ) ( isc zp  ) (:inx) (:sbc iv  ) (:nop  ) ( usbc  iv) (:cpx ab  ) (:sbc ab  ) (:inc ab  ) ( isc ab  ))
  ((#xF0) (:beq rl) (:sbc in y) ( jam   ) (:isc in y) ( nop zp x) (:sbc zp x) (:inc zp x) ( isc zp x) (:sed) (:sbc ab y) ( nop  ) ( isc ab y) ( nop ab x) (:sbc ab x) (:inc ab x) ( isc ab x)))
