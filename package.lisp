;;;; package.lisp

(defpackage #:specops
  (:use #:cl)
  (:export #:register
           #:reg-name #:reg-index #:reg-width
           #:memory-access-scheme
           #:mas-based #:mas-based #:mas-indexed #:mas-index #:mas-displaced #:mas-displ
           #:assembler #:assembler-encoding
           #:asm-name #:asm-type #:asm-storage #:asm-lexicon
           #:asm-domains #:asm-reserve #:asm-joiner #:asm-enc-table
           #:join #:joinw
           #:flipbits #:masque
           #:types-of #:derive-domains #:of-lexicon #:of-encoded #:of-storage
           #:specify-ops #:qualify-ops
           #:locate #:compose #:process-operands #:clause-processor 
           #:assemble #:specops))
