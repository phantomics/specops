;;;; ops.lisp

(in-package #:specops.6502)

(specops nil (op0 &optional op1) *assembler-prototype-6502*
  ;; the table of 6502 instructions, including illegal instructions
  ((:tabular :cross-adding :matcher match-ops))
  ((    ) (#x0      ) (#x1       ) (#x2      ) (#x3         ) (#x4        ) (#x5      ) (#x6      ) (#x7        ) (#x8    ) (#x9        ) (#xA      ) (#xB        ) (#xC        ) (#xD      ) (#xE        ) (#xF        ))
  ((#x00) (  :BRK IM) (:ORA X IND) (! :JAM   ) (! :SLO X IND) (! :NOP ZP  ) (:ORA ZP  ) (:ASL ZP  ) (! :SLO ZP  ) (:PHP IM) (  :ORA IV  ) (  :ASL  A) (! :ANC IV  ) (! :NOP AB  ) (:ORA AB  ) (  :ASL AB  ) (! :SLO AB  ))
  ((#x10) (  :BPL RL) (:ORA IND Y) (! :JAM   ) (! :SLO IND Y) (! :NOP ZP X) (:ORA ZP X) (:ASL ZP X) (! :SLO ZP X) (:CLC IM) (  :ORA AB Y) (! :NOP IM) (! :SLO AB Y) (! :NOP AB X) (:ORA AB X) (  :ASL AB X) (! :SLO AB X))
  ((#x20) (  :JSR AB) (:AND X IND) (! :JAM   ) (! :RLA X IND) (  :BIT ZP  ) (:AND ZP  ) (:ROL ZP  ) (! :RLA ZP  ) (:PLP IM) (  :AND IV  ) (  :ROL  A) (! :ANC IV  ) (  :BIT AB  ) (:AND AB  ) (  :ROL AB  ) (! :RLA AB  ))
  ((#x30) (  :BMI RL) (:AND IND Y) (! :JAM   ) (! :RLA IND Y) (! :NOP ZP X) (:AND ZP X) (:ROL ZP X) (! :RLA ZP X) (:SEC IM) (  :AND AB Y) (! :NOP IM) (! :RLA AB Y) (! :NOP AB X) (:AND AB X) (  :ROL AB X) (! :RLA AB X))
  ((#x40) (  :RTI IM) (:EOR X IND) (! :JAM   ) (! :SRE X IND) (! :NOP ZP  ) (:EOR ZP  ) (:LSR ZP  ) (! :SRE ZP  ) (:PHA IM) (  :EOR IV  ) (  :LSR  A) (! :ALR IV  ) (  :JMP AB  ) (:EOR AB  ) (  :LSR AB  ) (! :SRE AB  ))
  ((#x50) (  :BVC RL) (:EOR IND Y) (! :JAM   ) (! :SRE IND Y) (! :NOP ZP X) (:EOR ZP X) (:LSR ZP X) (! :SRE ZP X) (:CLI IM) (  :EOR AB Y) (! :NOP IM) (! :SRE AB Y) (! :NOP AB X) (:EOR AB X) (  :LSR AB X) (! :SRE AB X))
  ((#x60) (  :RTS IM) (:ADC X IND) (! :JAM   ) (! :RRA X IND) (! :NOP ZP  ) (:ADC ZP  ) (:ROR ZP  ) (! :RRA ZP  ) (:PLA IM) (  :ADC IV  ) (  :ROR  A) (! :ARR IV  ) (  :JMP IND ) (:ADC AB  ) (  :ROR AB  ) (! :RRA AB  ))
  ((#x70) (  :BVS RL) (:ADC IND Y) (! :JAM   ) (! :RRA IND Y) (! :NOP ZP X) (:ADC ZP X) (:ROR ZP X) (! :RRA ZP X) (:SEI IM) (  :ADC AB Y) (! :NOP IM) (! :RRA AB Y) (! :NOP AB X) (:ADC AB X) (  :ROR AB X) (! :RRA AB X))
  ((#x80) (! :NOP IV) (:STA X IND) (! :NOP IV) (! :SAX X IND) (  :STY ZP  ) (:STA ZP  ) (:STX ZP  ) (! :SAX ZP  ) (:DEY IM) (! :NOP IV  ) (  :TXA IM) (! :ANE IV  ) (  :STY AB  ) (:STA AB  ) (  :STX AB  ) (! :SAX AB  ))
  ((#x90) (  :BCC RL) (:STA IND Y) (! :JAM   ) (! :SHA IND Y) (  :STY ZP X) (:STA ZP X) (:STX ZP Y) (! :SAX ZP Y) (:TYA IM) (  :STA AB Y) (  :TXS IM) (! :TAS AB Y) (! :SHY AB X) (:STA AB X) (! :SHX AB Y) (! :SHA AB Y))
  ((#xA0) (  :LDY IV) (:LDA X IND) (! :LDX IV) (! :LAX X IND) (  :LDY ZP  ) (:LDA ZP  ) (:LDX ZP  ) (! :LAX ZP  ) (:TAY IM) (  :LDA IV  ) (  :TAX IM) (! :LXA IV  ) (  :LDY AB  ) (:LDA AB  ) (  :LDX AB  ) (! :LAX AB  ))
  ((#xB0) (  :BCS RL) (:LDA IND Y) (! :JAM   ) (! :LAX IND Y) (  :LDY ZP X) (:LDA ZP X) (:LDX ZP Y) (! :LAX ZP Y) (:CLV IM) (  :LDA AB Y) (  :TSX IM) (! :LAS AB Y) (  :LDY AB X) (:LDA AB X) (  :LDX AB Y) (! :LAX AB Y))
  ((#xC0) (  :CPY IV) (:CMP X IND) (! :NOP IV) (! :DCP X IND) (  :CPY ZP  ) (:CMP ZP  ) (:DEC ZP  ) (! :DCP ZP  ) (:INY IM) (  :CMP IV  ) (  :DEX IM) (! :SBX IV  ) (  :CPY AB  ) (:CMP AB  ) (  :DEC AB  ) (! :DCP AB  ))
  ((#xD0) (  :BNE RL) (:CMP IND Y) (! :JAM   ) (! :DCP IND Y) (! :NOP ZP X) (:CMP ZP X) (:DEC ZP X) (! :DCP ZP X) (:CLD IM) (  :CMP AB Y) (! :NOP IM) (! :DCP AB Y) (! :NOP AB X) (:CMP AB X) (  :DEC AB X) (! :DCP AB X))
  ((#xE0) (  :CPX IV) (:SBC X IND) (! :NOP IV) (! :ISC X IND) (  :CPX ZP  ) (:SBC ZP  ) (:INC ZP  ) (! :ISC ZP  ) (:INX IM) (  :SBC IV  ) (  :NOP IM) (! :USBC  IV) (  :CPX AB  ) (:SBC AB  ) (  :INC AB  ) (! :ISC AB  ))
  ((#xF0) (  :BEQ RL) (:SBC IND Y) (! :JAM   ) (! :ISC IND Y) (! :NOP ZP X) (:SBC ZP X) (:INC ZP X) (! :ISC ZP X) (:SED IM) (  :SBC AB Y) (! :NOP IM) (! :ISC AB Y) (! :NOP AB X) (:SBC AB X) (  :INC AB X) (! :ISC AB X)))
