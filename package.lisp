;;;; package.lisp

(defpackage #:specops
  (:use #:cl)
  (:export #:register
           #:reg-name #:reg-index #:reg-width
           #:mem-access
           #:mac-width #:mac-bsreg
           #:assembler
           #:asm-name #:asm-type #:asm-storage #:asm-lexicon #:asm-domains #:asm-reserve
           #:join #:flipbits #:masque
           #:types-of #:derive-domains #:of-lexicon #:of-storage #:specify-ops #:qualify-ops
           #:locate #:compose #:assemble #:specops))
