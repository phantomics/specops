;;;; package.lisp

(defpackage #:specops
  (:use #:cl)
  (:export #:register
           #:reg-name #:reg-index #:reg-width
           #:memory-access-scheme
           #:mas-based #:mas-base #:mas-indexed #:mas-index #:mas-displaced #:mas-displ
           #:assembler #:assembler-encoding #:assembler-masking
           #:asm-name #:asm-type #:asm-storage #:asm-lexicon
           #:asm-domains #:asm-reserve #:asm-joiner #:asm-enc-breadth #:asm-enc-decoder
           #:asm-msk-segment #:asm-msk-battery
           #:join #:joinw
           #:flipbits #:masque #:unmasque #:mqbase
           #:types-of #:derive-domains #:of-lexicon #:of-decoder #:of-battery #:of-storage
           #:storage-type-p #:specify-ops #:qualify-ops
           #:locate #:compose #:process-operands #:clause-processor 
           #:assemble #:specops
           #:match-types
           ))
