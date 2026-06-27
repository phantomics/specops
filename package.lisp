;;;; package.lisp

(defpackage #:specops
  (:use #:cl)
  (:export
   ;; -- Assembler API
   #:defcodetable
   #:make-array-writer
   #:width-spec
   #:wspec-name
   #:wspec-bits

   ;; -- Register API
   #:register
   #:reg-name
   #:reg-index
   #:reg-type
   #:of-register-type-index

   ;; -- Immediate API
   #:immediate
   #:imm-value
   #:make-immediate ;; #:imm

   ;; -- Memory access scheme API
   #:memory-access-scheme
   #:mas-based
   #:mas-base
   #:mas-indexed
   #:mas-index
   #:mas-displaced
   #:mas-displ
   #:mas-scaling-displaced
   #:mas-sdisp-scale
   #:mas-absolute
   #:mas-addr
   #:as-register-file
   #:deriving-file

   ;; -- Assembler API
   #:assembler
   #:assembler-encoding
   #:assembler-masking
   #:asm-name
   #:asm-extns
   #:asm-storage
   #:asm-lexicon
   #:asm-promodel
   #:asm-reserve
   #:asm-breadth
   #:asm-joiner
   #:asm-exmodes
   #:asm-enc-breadth
   #:asm-enc-decoder
   #:asm-msk-segment
   #:asm-msk-battery

   ;; -- Bit operations
   #:join
   #:joinw
   #:program-api
   #:of-program
   #:flipbits

   ;; -- Masque macros
   #:masque
   #:unmasque
   #:mqbase

   ;; -- Marshal macros
   #:marshal
   
   ;; -- Main utility functions and macros
   #:extns-of
   #:derive-promodel
   #:of-lexicon
   #:of-decoder
   #:of-battery
   #:of-storage
   #:storage-type-p
   #:specify-ops
   #:qualify-ops
   #:locate
   #:compose
   #:process-operands
   #:extend-clauses
   #:clause-processor
   #:label-delta
   #:assemble
   #:specops
   #:readops
   #:interpret
   #:match-types
   #:to-tag
   #:determine-in-context
   #:complete-dforms

   ;; -- Program API
   ;; -- Program class & methods
   #:program
   #:pgm-assembler
   #:pgm-units
   #:pgm-entry-point
   #:pgm-properties
   #:program-unit
   #:pun-name
   #:pun-program
   #:pun-segments
   #:pun-symbols
   #:pun-relocations
   #:pun-properties

   ;; -- Segment class & methods
   #:segment
   #:seg-name
   #:seg-kind
   #:seg-items
   #:seg-origin
   #:seg-size
   #:seg-align
   #:seg-bytes
   #:seg-flags
   #:seg-properties

   ;; -- Symbol class & methods
   #:symbol-entry
   #:sym-name
   #:sym-segment
   #:sym-offset
   #:sym-binding
   #:sym-type
   #:sym-size
   #:sym-properties

   ;; -- Relocation class & methods
   #:relocation
   #:rel-symbol
   #:rel-segment
   #:rel-offset
   #:rel-type
   #:rel-width
   #:rel-addend
   
   #:segment-item
   #:raw-bytes-item
   #:rbi-data
   #:instruction-item
   #:ii-expression
   #:label-def-item
   #:ldi-name
   #:ldi-binding
   #:label-ref-item
   #:lri-label
   #:lri-width
   #:lri-rel-type
   #:lri-addend
   #:align-item
   #:ali-boundary
   #:ali-fill-byte
   #:function-item
   #:fi-name
   #:fi-body
   #:fi-properties
   #:add-segment
   #:add-symbol
   #:add-relocation
   #:add-item
   #:add-unit
   #:lookup-segment
   #:lookup-symbol
   #:finalize-segment
   #:make-program
   #:make-unit
   #:make-segment
   #:make-symbol-entry
   #:emit-program
   #:build-program
   #:asm-endianness
   #:asm-elf-machine
   #:data-word-item
   #:dwi-value
   #:dwi-width))
    
