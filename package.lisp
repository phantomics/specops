;;;; package.lisp

(defpackage #:specops
  (:use #:cl)
  (:export #:make-array-writer #:width-spec #:wspec-name #:wspec-bits
           #:register #:reg-name #:reg-index #:reg-type #:of-register-type-index
           #:immediate #:imm-value #:make-immediate ;; #:imm
           #:memory-access-scheme
           #:mas-based #:mas-base #:mas-indexed #:mas-index #:mas-displaced #:mas-displ
           #:mas-scaling-displaced #:mas-sdisp-scale #:mas-absolute #:mas-addr
           #:assembler #:assembler-encoding #:assembler-masking
           #:asm-name #:asm-type #:asm-storage #:asm-lexicon
           #:asm-domains #:asm-reserve #:asm-breadth #:asm-joiner #:asm-exmodes
           #:asm-enc-breadth #:asm-enc-decoder #:asm-msk-segment #:asm-msk-battery
           #:join #:joinw #:program-api #:of-program
           #:flipbits #:masque #:unmasque #:mqbase
           #:types-of #:derive-domains #:of-lexicon #:of-decoder #:of-battery #:of-storage
           #:storage-type-p #:specify-ops #:qualify-ops
           #:locate #:compose #:process-operands #:extend-clauses #:clause-processor #:label-delta
           #:assemble #:specops #:readops #:interpret
           #:match-types #:to-tag #:determine-in-context #:complete-dforms
           ))
    
