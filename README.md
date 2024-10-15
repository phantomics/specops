# SpecOps
### To Specify Operations
#### An Assembler Framework For Common Lisp

The computing landscape is split along many axes. Some programs are written to be easily readable by humans, while others are written for the fastest possible evaluation. Different software models prioritize different performance metrics and systems are further divided between many different varieties of computer hardware, each with distinct features which are exposed in different ways to programmers.

What kind of tool or design pattern could bridge these divides? In an increasingly heterogenous computing ecosystem, the task of bringing into concert various kinds of hardware has come into a stronger focus. The SpecOps assembler framework aims to meet this challenge, leveraging the expressive potential of Common Lisp to specify assembly and disassembly functions for multiple computer architectures, and augment low-level software development with tools of symbolic programming.

SpecOps is written in pure idiomatic Common Lisp without using any libraries, it's designed for maximum portability. This document gives an overview of some SpecOps assembler implementations, detailing the system's design and goals.

## Basic Usage: Motorola 68000

Assembling computer instructions is in essence a fairly simple task - numbers are added together to compose larger numbers expressing the different operations available within a given computer architecture. These numbers are placed in a linear structure, cross-references and other special expressions within the code are reconciled and it is given whatever additional formatting extensions are required for it to be evaluated on the target computer system. The result is a complete program ready for use.

Let's see how this looks in practice and assemble some code for the Motorola 68000 processor, a classic CPU used in notable computers including the Commodore Amiga, early Apple Macintosh models, the Sharp X68K, the Atari ST, the Sega Genesis/Mega Drive game console, and many arcade game systems.

```lisp
;; load the SpecOps M68K module

* (asdf:load-system 'specops.m68k) (in-package :specops.m68k)
...system loads...

* (assemble *assembler-prototype-m68k*
    (:addi :b 10 :d1))
#(1537 10)
```

In this case, the M68K `ADDI` operation is being used to add an immediate value of 10 to the register d1. The instruction is expressed as the Lisp form `(:addi :b 10 :d1)`. The output is rendered as a vector of 16-bit integers, since all M68K instructions are divided into 16-bit segments. Prior to the single `ADDI` instruction, the assembelr macro takes as its argument `*assembler-prototype-m68k*`, an assembler class instance expressing default properties for an M68K code assembler. Let's take a closer look at what was output.

```lisp
* (write (assemble *assembler-prototype-m68k*
           (:addi :b 10 :d1)) ;; ADDI.B 10,D1
         :base 2)
#(11000000001 1010)
#(1537 10)
```

The `ADDI` instruction is 16 bits long, like all M68K instructions, and is formatted according to the scheme `00000110.SSMMMXXX`. The initial byte of value 6 is the opcode. In the next byte, the 2-bit field S indicates the size of the data being operated upon - in this case a byte, as represented by the encoded value 0 and the `:b` keyword passed in the instruction form. The M field is the addressing mode (0 in this case) indicating that the instruction is addressing a data register. The X field determines the location being addressed. Its value of 1, along with the 0 value in the M field, indicates that the operation is addressing the data register 1. The following 16-bit word contains the value 10, which the instruction adds to the value in that data register.

### Implementing Instructions

SpecOps provides a toolkit for creating schemes that govern such conversions of human-readable instructions into binary program code. Let's take a look at how M68K operations are specified.

```
(specop addi (w op0 op1)
  (assert (match-types op0 op1  integer location)
          (op0 op1)
          "ADDI operands ~a and ~a must be an immediate integer fitting within 32 bits and a location.")
  (address (op1) ((index1 amode1))
    (joinw (masque "00000110.SSMMMXXX"
                   (s (determine-width w)) (m amode1) (x index1))
           op0)))
```

This is the implementation of the `ADDI` operation from the SpecOps M68K module. It checks whether the instruction's given operands are an integer and a location (in the register file or in memory), returning an error if not. It uses a macro called `(address)` to derive the numbers indicating the index and addressing mode for the given operand, and the width, addressing mode and index are used as arguments to a macro called `(masque)` that generates code composing the instruction word.

The `(masque)` macro is frequently used within SpecOps to model binary formats. It's easy to read and generates efficient code: a series of operations incrementing a base value by masked and shifted integers to generate an output number expressing an instruction. A primary goal of SpecOps is the development of tools like this that can support assembly tasks for many different systems.

## Moving Forward: Intel X86

Let's take a look at assembly for a complex, contemporary hardware platform: the Intel X86 CPU family. Powering vast numbers of personal computer systems and servers, it's known as a daunting target for assembly due to its many variations and complex variable-length encoding.

```lisp
;; load the SpecOps X86 module

* (asdf:load-system 'specops.x86) (in-package :specops.x86)
...system loads...

* (write (assemble *assembler-prototype-x86*
           (:add :w :c 20)) ;; ADD CX,20
         :base 16)
#(66 81 C1 0 14)
#(102 129 193 0 20)
```

This is an `ADD` operation at word size (16 bits), indicated by the parameter `:w`. The C register (whose lower 16-bit span is referred to as CX) has an immediate value of 20 added to it. In the assembled code, the first byte is the prefix `#x66`, whose significance is addressed in the next paragraph. Following it is the opcode `#x81`, expressing the operation for adding an immediate value to a register or memory location. The byte `#xC1` expresses the MOD prefix indicating an operation targeting general-purpose register index 1 (the C-series registers) and the latter two bytes encode the immediate value 20 to be added to the CX register.

Note that by default, the SpecOps X86 assembler generates programs to be run in the architecture's 64-bit long mode. This means that the default width for many variable-width operations is 32 bits, and the `#x66` byte in the instruction prefix changes the operation size to address 16 bits. If the assembler were generating output for the 16-bit real or protected modes, 16-bit operations would instead be the default and the `#x66` size prefix would indicate operation at a 32-bit width.

### Assembly in Context

The above syntax for X86 assembly differs notably from that used by standard assemblers, where the width of an operation is determined by the names used to reference the registers. In the above example a width directive is passed to the instruction, similar to way that M68K operations are formatted. If desired, a more traditional X86 could be implemented fairly easily, mapping register references like `:eax` to width specifications in operation calls.

The width specification is used by default because it corresponds more closely to how X86 instructions are encoded and because it's easier to write macros to generate instructions with explicit width specs. There is another reason best illustrated by the following example:

```lisp
* (write (assemble *assembler-prototype-x86*
           ((:store (abc :gpr) (def :gpr :bind :a)))
           (:add :d abc 10) (:add :d def 7)) ;; ADD E_X,10 ; ADD EAX,7
         :base 16)
#(83 C7 A 5 0 0 0 7)
#(131 199 10 5 0 0 0 7)
```

This assembly reserves two general-purpose registers for use by the program, referencing them by the symbols `abc` and `def`. While `abc` may be assigned to any available register, `def` must be bound to the A register series, whose full 64-bit length is referenced in standard X86 assembly code as RAX and whose lower ranges are referenced by the names EAX, AX, etc. The `:bind :a` arguments in the `(def)` form require that the symbol `def` be bound to the A-series registers. This use of symbols makes it more expedient to pass explicit width specifications to instructions rather than expressing width through the names used for registers.

This system of aliasing registers as symbols allows for an assembler to reserve certain registers that a generated program may not use. This is useful in many cases - for example, recent releases of Steel Bank Common Lisp (SBCL) reserve the X86 registers R12 and R13 for system use, and writing to them often causes crashes and other bugs. On Apple's M-series ARM64 architecture, the register x18 is likewise designated as off-limits for use by regular programs. The SpecOps assembler classes may be extended so as to bar the use of such reserved resources in programs written for those contexts.

The example above shows two considerably different ways of encoding an `ADD` instruction. First, the `#x83` opcode is used to add an 8-bit integer to a 32-bit register. Second, the `#x05` opcode, which exclusively targets the A-series registers, is used to add a value of 7 to EAX. The nature of this encoding requires that the increment be encoded as a 32-bit immediate value.

### Adaptive Specification

An architecture like M68K is relatively simple to assemble for, but X86 requires a more nuanced approach. Operations may have a wide range of permutations for different combinations of operands and instructions can range from 1 to 15 bytes in length. Below is the code specifying the `ADD` instruction from the SpecOps X86 module.

```lisp
(specops add (w op0 op1) *assembler-prototype-x86*
  ((:provisions
    (rex (determine-pfrex w op0 op1)) (modz0 (determine-modrm 0 op0))
    (sz? (determine-pfsize w op0 op1 (of-program :exmode)))
    (mod10 (determine-modrm op1 op0)) (mod01 (determine-modrm op0 op1))
    (sib0 (determine-sib op0)) (sib1 (determine-sib op1)))
   (:priority                           0       2         1))
  ((    rex #x04                 op1 ) ((:w :b) (:gpr :a) (:imm   )))
  ((sz? rex #x05            (w 2 op1)) ((:w :w) (:gpr :a) (:imm   )))
  ((sz? rex #x05            (w 4 op1)) ((:w :d) (:gpr :a) (:imm   )))
  ((    rex #x05            (w 4 op1)) ((:w :q) (:gpr :a) (:imm   )))
  ((sz? rex #x80 modz0 sib0      op1 ) ((:w :b) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 modz0 sib0      op1 ) ((:w :w) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 modz0 sib0      op1 ) ((:w :d) (:gxm   ) (:imm  8)))
  ((sz? rex #x83 modz0 sib0      op1 ) ((:w :q) (:gxm   ) (:imm  8)))
  ((sz? rex #x81 modz0 sib0 (w 2 op1)) ((:w :w) (:gxm   ) (:imm 16)))
  ((sz? rex #x81 modz0 sib0 (w 4 op1)) ((:w :d) (:gxm   ) (:imm 32)))
  ((sz? rex #x81 modz0 sib0 (w 4 op1)) ((:w :q) (:gxm   ) (:imm 64)))
  ((sz? rex #x00 mod10 sib0          ) ((:w :b) (:gxm   ) (:gpr   )))
  ((sz? rex #x01 mod10 sib0          ) ((:w :w) (:gxm   ) (:gpr   )))
  ((sz? rex #x01 mod10 sib0          ) ((:w :d) (:gxm   ) (:gpr   )))
  ((sz? rex #x01 mod10 sib0          ) ((:w :q) (:gxm   ) (:gpr   )))
  ((sz? rex #x02 mod01 sib1          ) ((:w :b) (:gpr   ) (:gxm   )))
  ((sz? rex #x03 mod01 sib1          ) ((:w :w) (:gpr   ) (:gxm   )))
  ((sz? rex #x03 mod01 sib1          ) ((:w :d) (:gpr   ) (:gxm   )))
  ((sz? rex #x03 mod01 sib1          ) ((:w :q) (:gpr   ) (:gxm   ))))
```

When this macro expands, the `(:provisions)` form populates a `(symbol-macrolet)` assigning the various bytes. The operand symbols are determined by the second argument to the macro, in this case `(w op0 op1)`. Each of the lines following the parameters expresses one possible encoding, with the first form in the line containing a list of values that may be present in the output instruction. The `sz?` symbol may express the `#x66` register size prefix and/or the `#x67` address size prefix if appropriate conditions are in place, such as an operation targeting a 16-bit-wide register in the 64-bit long execution mode. The `rex` symbol may express the REX prefix required for 64-bit wide operations or in many cases. This prefix is followed by the opcode, which in many cases may be followed by MOD and SIB bytes and sometimes by an immediate integer value. The `(w)` forms encapsulating some of these immediate specifications determine that in those cases, the immediate value must be 2 or 4 bytes wide even if the operand value would fit into a smaller width.

The second form in each line specifies the kinds of operands that determine the use of the encoding. For instance, the first line determines that the `#x04` opcode will be used in the case of an 8-bit wide operation `(:w :b)` whose first operand is the general-purpose register series A `(:gpr :a)` and whose second operand is an immediate value `(:imm   )`, implicitly an 8-bit value since the operation's given width is 8 bits. In some lines the `(:imm)` form contains an integer value N, indicating that the immediate value given may be no wider than N bits.

The `(:priority)` form determines the order in which the operands will be tested. First, the operation's width will be checked, then the second argument's type will be checked, and finally the first operand's type will be checked. A proper priority order can speed up the encoding dispatch.

## A More Consistent Approach: IBM System Z

Not all complex instruction sets are as challenging to implement as X86. IBM's System Z is an even older architecture, originally released as System/360 in 1965. This lineage of mainframe computers hosted the first true operating system software and their instruction set has retained backward compatibility for every version since its inception, even as its memory addresses have grown from 24 to 31 to 64 bits.

System Z instructions are structured according to a set of formats. Each format specifies not only the layout of the bit fields (which are always nibble-aligned) but their specific purposes. Thus, an instruction where the latter byte is an immediate integer value and an instruction where the latter byte is a memory offset are considered to belong to two different formats, even if their structure is otherwise identical.

Following is an example of assembly for a very simple System Z program:

```lisp
;; load the SpecOps System Z module

* (asdf:load-system 'specops.z) (in-package :specops.z)
...system loads...

* (assemble *assembler-prototype-z*
    (:lhi 1 10)          ;; LHI 1,10
    (:lhi 2 20)          ;; LHI 2,20
    (:lhi 3 3)           ;; LHI 3, 2
    (:lr  4 1)           ;; LR  4, 1
    (:ar  4 2)           ;; AR  4, 2
    (:mr  4 3)           ;; MR  4, 3
    (:sll 4 (@% 1))      ;; SLL 4, 1(0)
    (:st  4 (@ 7 8 90))) ;; ST  4,90(8,7)
#(42776 10 42792 20 42808 3 6209 6722 7235 35136 1 20552 28762)
```

This program loads some numbers into registers, performs arithmetic operations on them and stores the result in memory. The output is a vector of 16-bit numbers -- because all Z instructions are either 2, 4 or 6 bytes, dividing the output into 16-bit blocks makes the most sense. Below are the above instructions' specifications from the SpecOps System Z module:

```lisp
(specop-z ar      zformat-rr    #x001A) 

(specop-z lhi     zformat-ri-a  #x0A78)

(specop-z lr      zformat-rr    #x0018)

(specop-z mr      zformat-rr    #x001C)

(specop-z sll     zformat-rs-a  #x0089)

(specop-z st      zformat-rx-a  #x0050)
```

Each operation spec is one line - a far cry from X86. Thanks to the regular structure of System Z instructions, they can be implemented simply as associations of opcodes and formats. Let's now take a look at the formats used for the above instructions:

```lisp
(mqbase zformat-rr opc mne (r1 r2)
    "AAAAAAAA.RRRRSSSS"
  ((:static (a opc)))
  ((r (rix r1)) (s (rix r2)))
  (list mne r s))
 
(mqbase zformat-ri-a opc mne (r1 i2)
    "AAAAAAAA.RRRRZZZZ.IIIIIIII.IIIIIIII"
  ((:static (a (rs4 opc)) (z (lo4 opc))))
  ((r (rix r1)) (i i2))
  (list mne r i))

(mqbase zformat-rs-a opc mne (r1 bd2 &optional r3)
    "AAAAAAAA.RRRRSSSS.BBBBDDDD.DDDDDDDD"
  ((:static (a opc)))
  ((r (rix r1)) (s (rix (or r3 0))) (b (mas-base bd2)) (d (mas-displ bd2)))
  (list mne r (derive-mas b nil d) s))

(mqbase zformat-rx-a opc mne (r1 bdx2)
    "AAAAAAAA.RRRRXXXX.BBBBDDDD.DDDDDDDD"
  ((:static (a opc)))
  ((r (rix r1)) (x (mas-index bdx2)) (b (mas-base bdx2)) (d (mas-displ bdx2)))
  (list mne r (derive-mas b x d)))
```

The RR format, one of the most common formats, addresses two registers along with the opcode. The four-bit fields allow for a file of 16 general-purpose registers. The RI-A format addresses one register, an immediate value and a 12-bit opcode split into A and Z fields in the `(masque)` bitfield scheme. The RS-A and RX-A formats both address registers and memory locations composed of a base register, displacement value and optionally (as in format RX-A) an index register. Lacking the index register field, format RS-A may address a second general-purpose register, but in the case of the instruction SLL that second register field is unused and should contain zeroes.

### More Specification Details

Let's take a closer look at the macros above - these aren't the `(masque)` seen before, but something called `(mqbase)`. This is essentially a currying macro that generates another macro producing functions built around `(masque)`. The `(specop-z)` macro takes a formatting function like `zformat-rr` generated by `(mqbase)` and uses it as the basis to construct an assembling function for an instruction like `AR`. The bitfields in the string given as the fourth argument to the macro are populated on the basis of the forms expressed in the fifth and sixth arguments. The `:static` forms in the fifth argument express static fields within the bitfield that will be the same regardless of the specific operands given to an instruction of a given format, those those fields are fixed for a given instruction and they don't need to be evaluated during the assembly process.

Also of note in the above assembly code is the presence of `(@)` and `(@%)` forms. Functions with names including `@` are a convention in SpecOps assemblers to express different ways of addressing memory. Generally, these functions produce instances of classes collectively known as "memory access schemes" or MASes. This is a catch-all category to describe any way a given architecture offers programmers to interact with memory. In the Specops System Z module, the `(@)` function takes as arguments a base register and optionally an index register and displacement value. The `(@%)` function takes a base register and optionally a displacement value as arguments, and in the absence of a second argument it expresses only a displacement value. This is useful for operations like `SLL`, which performs a left shift on a register using that displacement value as the number of bits by which to shift. The `ST` instruction stores a register value to memory using a standard memory access scheme with base, index and displacement.

### Duplex Operation

There's one more element of the `(mqbase)` macros yet to be addressed - the `(list)` form at the end. This doesn't specify how these formats are assembled but how they're disassembled. A major goal of SpecOps is to facilitate "duplex" specificatons that express rules both for assembling and disassembling code. Along with generating an assembly function, `(mqbase)` produces a disassembly function that can derive assembly code from a vector of words. The words in the vector are put through a series of tests comparing their contents to the static opcode fields in the instruction formats, with the variable fields specifying addresses and immediate values masked off. When a word or word series is found to match an instruction, the values of the non-opcode fields are derived from the binary and used to populate a list expressing the instruction.  System Z's regularity makes it simple to support such duplex operation, but some architectures require a more explicit format.

### Disassembly in Practice

Let's take a look at how disassembly is done in SpecOps. Following from the last assembly example:

```lisp
* (defvar output)
OUTPUT

* (setf output (assemble *assembler-prototype-z*
                         (:lhi 1 10)           ;; LHI 1,10
                         (:lhi 2 20)           ;; LHI 2,20
                         (:lhi 3 3)            ;; LHI 3, 2
                         (:lr  4 1)            ;; LR  4, 1
                         (:ar  4 2)            ;; AR  4, 2
                         (:mr  4 3)            ;; MR  4, 3
                         (:sll 4 (@% 1))       ;; SLL 4, 1(0)
                         (:st  4 (@ 7 8 90)))) ;; ST  4,90(8,7)
#(42776 10 42792 20 42808 3 6209 6722 7235 35136 1 20552 28762)

* (interpret *assembler-prototype-z* nil output)
((:LHI 1 10) (:LHI 2 20) (:LHI 3 3) (:LR 4 1) (:AR 4 2) (:MR 4 3)
 (:SLL 4 (@ 0 1) 0) (:ST 4 (@ 7 8 90)))
```

The `(interpret)` macro is the main tool for disassembly in Specops. Given a vector of encoded instructions, it will convert them to code in the format of SpecOps. Note that said vectors need to be of the appropriate integer types. If you copy and paste the text of the vector above into a Common Lisp REPL and attempt to disassemble it, it will typically be evaluated as a T-type vector rather than having the type `(unsigned-byte 16)` as System Z code should. This will cause disassembly to fail. 

## Mirrored Specs: Hitachi Super-H

RISC (Reduced Instruction Set Computer) hardware architectures are known for executing code at consistently high speeds, but this often comes at the expense of elegance in instruction design. Platforms like ARM and M68K can be haphazard in their organization of bit fields so as to fit all the needed information into a fixed width. A standout in this sector is the Super-H architecture, originally designed by Hitachi. This line of CPUs powered the Sega Saturn, Sega Dreamcast and a wide range of arcade game hardware along with mechanical control systems in many vehicles. Super-H uses mostly 16-bit instructions (with a few 32-bit extensions added in later revisions) with all fields nibble-aligned.

The orderly encoding of Super-H instructions makes an assembler for the architecture straightforward to implement, and the output can even be easy to read in hexadecimal. It's also straightforward to write rules for disassembling Super-H instructions, and you can see both in the following code:

```lisp
(specops dt (op0)
  ((:for-types :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (and (match-types op0  gpr)) (op0)
          "DT may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00010000" ;; dt Rn
          (n (gprix op0))))

(readops dt (word read) ;; dt Rn
  (unmasque "0100NNNN.00010000" word (n)
    (list :dt (drv-gpr n))))

(specops rotl (op0)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (assert (match-types op0  gpr) (op0)
          "ROTL may take only a general-purpose register as its operand.")
  (masque "0100NNNN.00000100" ;; rotl Rn
            (n (gprix op0))))

(readops rotl (word read) ;; rotl Rn
  (unmasque "0100NNNN.00000100" word (n)
    (list :rotl (drv-gpr n))))

(specops add (op0 op1)
  ((:for-types :sh1 :sh2 :sh3 :sh4 :sh4a :sh2a))
  (cond ((match-types op0 op1  gpr gpr)
         (masque "0011NNNN.MMMM1100" ;; add Rm,Rn
                 (n (gprix op1)) (m (gprix op0))))
        ((match-types op0 op1  integer gpr)
         (masque "0111NNNN.IIIIIIII" ;; add #imm,Rn
                 (n (gprix op1)) (i op0)))))

(readops add (word read) ;; add Rm,Rn
  (unmasque "0011NNNN.MMMM1100" word (n m)
    (list :add (drv-gpr m) (drv-gpr n))))

(readops add (word read) ;; add #imm,Rn
  (unmasque "0111NNNN.IIIIIIII" word (n i)
    (list :add i (drv-gpr n))))
```

Above are specifications for assembling SuperH decrement, rotate left and add instructions along with `(readops)` forms that specify how to disassemble the same. These specs are based on the `(unmasque)` macro which, as the name suggests, acts like `(masque)` in reverse, decomposing a number into a set of numbers according to bit fields. Notably, this decomposition will return nil if the static bits in the field do not match those of the tested number. Functionally, the static bits given to `(unmasque)` are masked, the same mask is applied to an input number and if they are not equal the code generated by the macro will return a nil value. Otherwise, the code following the third argument to `(unmasque)` is evaluated, with the numbers gleaned from the fields assigned to lexical variables referenced by the symbols in the list passed as the macro's third argument.

It's notable that many of the assembler and disassembler specs are asymmetrical. The `ADD` operation has a single assembler spec expressing two permutations of the instruction: one to add the contents of two registers and one to add an immediate value to a register. Disassembling this instruction is accomplished by implementing two functions, one that recognizes the register-to-register format by masking instruction values with `1111000000001111` and then comparing them to `0011000000001100`, and a second function that recognizes the immediate-to-register format by checking for its bit pattern in the same way.

## Operation Tables: Zilog Z80

The venerable Z80 CPU from Zilog has powered a wide array of devices, including early microcomputers like the Sinclair ZX Spectrum, the Amstrad CPC, Tandy TRS-80, and the MSX computer family along with early Sega video game consoles and many arcade games. It's a much less common target platform for development today and manufacture of the original Z80 IC was discontinued by Zilog a few months before this writing. So why write an assembler for it?

Every architecture has lessons to be learned in implementing its instruction set, lessons whose understanding can guide the process of building assemblers for other architectures. 8-bit architectures like the Z80 and its contemporary the MOS Technology 6502 are simple enough that their instruction encodings can be expressed as a table of heuristics. For example:

```lisp
(specops 0 (op0 &optional op1) *assembler-prototype-z80*
  ((:tabular :cross-adding :matcher match-ops) (:duplex . of-decoder))
  ((    ) (#x0       ) (#x1       ) (#x2        ) (#x3        ) (#x4        ) (#x5       ) ...)
  ((#x00) (:nop      ) (:ld  bc xx) (:ld  @bc  a) (:inc     bc) (:inc      b) (:dec     b) ...)
  ((#x10) (:djnz    x) (:ld  de xx) (:ld  @de  a) (:inc     de) (:inc      d) (:dec     d) ...)
  ((#x20) (:jr   nz x) (:ld  hl xx) (:ld  @xx hl) (:inc     hl) (:inc      h) (:dec     h) ...)
  ((#x30) (:jr   nc x) (:ld  sp xx) (:ld  @xx  a) (:inc     sp) (:inc    @hl) (:dec   @hl) ...)
  ((#x40) (:ld    b b) (:ld  b   c) (:ld    b  d) (:ld    b  e) (:ld     b h) (:ld    b l) ...)
  ((#x50) (:ld    d b) (:ld  d   c) (:ld    d  d) (:ld    d  e) (:ld     d h) (:ld    d l) ...)
  ...)

(specops #xCB00 (op0 &optional op1) *assembler-prototype-z80*
  ((:tabular :cross-adding :matcher match-ops) (:duplex . of-decoder))
  ((    ) (#x0     ) (#x1     ) (#x2     ) (#x3     ) (#x4     ) (#x5     ) (#x6       ) ...)
  ((#x00) (:rlc   b) (:rlc   c) (:rlc   d) (:rlc   e) (:rlc   h) (:rlc   l) (:rlc   @hl) ...)
  ((#x10) (:rl    b) (:rl    c) (:rl    d) (:rl    e) (:rl    h) (:rl    l) (:rl    @hl) ...)
  ((#x20) (:sla   b) (:sla   c) (:sla   d) (:sla   e) (:sla   h) (:sla   l) (:sla   @hl) ...)
  ((#x30) (:sll   b) (:sll   c) (:sll   d) (:sll   e) (:sll   h) (:sll   l) (:sll   @hl) ...)
  ((#x40) (:bit 0 b) (:bit 0 c) (:bit 0 d) (:bit 0 e) (:bit 0 h) (:bit 0 l) (:bit 0 @hl) ...)
  ((#x50) (:bit 2 b) (:bit 2 c) (:bit 2 d) (:bit 2 e) (:bit 2 h) (:bit 2 l) (:bit 2 @hl) ...)
  ...)
```

These are truncated opcode tables from the SpecOps Z80 module. The first table expresses some of the most common operations, including load, increment and decrement operations, while the second table expresses bitwise operations. One thing that may seem odd is that while the `(specops)` forms shown thus far have an instruction symbol as their first argument, in these forms the first argument is a number. That is because these macros don't specify just one instruction but many, and some instruction tables share a prefix in common.

Each column in these tables has a number at the top counting from 0, while each row begins with a multiple of 16. The full tables are at most 16 columns by 16 rows in size, and the instruction's 8-bit encoding comes from the sum of the column and row numbers. Z80 has more than 256 instructions, so instructions outside the base table are specified with a prefix byte, which in the case of the binary operations table is `#xCB`, so the second argument to the table spec is that value shifted left by 8 bits. Thus the full encoding for any instruction is found by adding the column numer, row number and table number.

Let's take a closer look at what is produced when these macros expand.

```lisp
(of-lexicon *assembler-prototype-z80* :inc
            (lambda (program-api op0 &optional op1)
              (flet ((of-program (&rest args)
                       (apply program-api args))
                     (declare (ignorable (function of-program)))
                     (cond ((match-ops op0 :@hl) 52)
                           ((match-ops op0 :@ix+) (+ 14496768 (mas-displ op0)))
                           ((match-ops op0 :@iy+) (+ 16593920 (mas-displ op0)))
                           ((match-ops op0 :hl) 35)
                           ((match-ops op0 :ix) 56611)
                           ((match-ops op0 :iy) 64803)
                           ((match-ops op0 :d) 20)
                           ((match-ops op0 :de) 19)
                           ((match-ops op0 :b) 4)
                           ((match-ops op0 :bc) 3))))))
                           
(of-decoder *assembler-prototype-z80* 52
            (lambda (program-api)
              (flet ((of-program (&rest args)
                       (apply program-api args)))
                (declare (ignorable (function of-program)))
                (list :inc (list '@ :hl)))))
                
(of-decoder *assembler-prototype-z80* 56628
            (lambda (program-api)
              (flet ((of-program (&rest args)
                       (apply program-api args)))
                (declare (ignorable (function of-program)))
                (list :inc (list '@ :ix (of-program :next-bytes 1))))))
                
(of-decoder *assembler-prototype-z80* 64820
            (lambda (program-api)
              (flet ((of-program (&rest args)
                       (apply program-api args)))
                (declare (ignorable (function of-program)))
                (list :inc (list '@ :iy (of-program :next-bytes 1))))))
                
(of-decoder *assembler-prototype-z80* 35
            (lambda (program-api)
              (flet ((of-program (&rest args)
                       (apply program-api args)))
                (declare (ignorable (function of-program)))
                (list :inc :hl))))
                
(of-decoder *assembler-prototype-z80* 56611
            (lambda (program-api)
              (flet ((of-program (&rest args)
                       (apply program-api args)))
                (declare (ignorable (function of-program)))
                (list :inc :ix))))
                
(of-decoder *assembler-prototype-z80* 64803
            (lambda (program-api)
              (flet ((of-program (&rest args)
                       (apply program-api args)))
                (declare (ignorable (function of-program)))
                (list :inc :iy))))
                
(of-decoder *assembler-prototype-z80* 20
            (lambda (program-api)
              (flet ((of-program (&rest args)
                       (apply program-api args)))
                (declare (ignorable (function of-program)))
                (list :inc :d))))
                
(of-decoder *assembler-prototype-z80* 19
            (lambda (program-api)
              (flet ((of-program (&rest args)
                       (apply program-api args)))
                (declare (ignorable (function of-program)))
                (list :inc :de))))
                
(of-decoder *assembler-prototype-z80* 4
            (lambda (program-api)
              (flet ((of-program (&rest args)
                       (apply program-api args)))
                (declare (ignorable (function of-program)))
                (list :inc :b))))
                
(of-decoder *assembler-prototype-z80* 3
            (lambda (program-api)
              (flet ((of-program (&rest args)
                       (apply program-api args)))
                (declare (ignorable (function of-program)))
                (list :inc :bc))))
```

The above code reflects part of the `INC` increment instruction implementation forms that are produced by the instruction table macro. As the presence of `(of-lexicon)` and `(of-decoder)` forms side by side may suggest, the instruction table manifests a duplex assembler/disassembler system. The `(of-lexicon)` form determines the assembler's behavior when encountering an `:inc` form generating an `INC` instruction. It checks for the presence of registers like `B`, `D`, `DE` and `HL` among the operands passed to it, and when matching a given register name it produces the appropriate opcode, like 35 or `#x23` for `(:inc :hl)`.

Another notable quality of the Z80 architecture is that it has two alternate instruction tables that mirror the structure of the base table, but with some register checks that differ. These alternate tables have opcodes that start with the prefixes `#xDD` and `#xFD`. Any instruction in the base table that addresses the `HL` register has an alternate form in the `#xDD` table that addresses the `IX` register and another in the `#xFD` table that addresses the `IY` register.

Therefore the `(:inc :hl)` instruction has two "shadow forms" that take `:ix` and `:iy` and have their opcodes incremented by `#xDD00` and #xFD00` respectively. The `L` register is replaced by `IXL` and `IYL` in the two alternate tables as well, among other equivalencies. The most unique of these is that for any operation taking as an argument the memory address found in the `HL` register, it is replaced in the alternate tables by the memory address in the `IX` or `IY` registers *plus a displacement*.

For this reason, the shadow implementations of `INC` for `:@hl` (which indicates a memory location contained in the register HL) add to the incremented, shifted opcode a last byte that indicates the displacement from the address in `IX` or `IY` at which to access memory. Thus to encode `(:inc (@ :ix 5))`, for incrementing the byte at the address in `IX` plus a displacement of 5, a three-byte instruction is composed by adding `#xDD0000`, `#x3400` and `#x05`.

### Disassembly by the Numbers

One of the properties of the function used to generate the Z80 main table assembler code is that it automatically generates these shadow forms for any operation that takes a shadowable register as an operand. It also automatically generates the `(of-decoder)` forms implementing disassembly. These forms, like `(of-lexicon)`, are assembler class methods that associate an integer value with a list to return that expresses the instruction. The numbers given as the second arguments to the `(of-decoder)` methods make entries in a hash table using those numbers as the keys.

As seen previously in the Super-H specs, the assembly and disassembly code for Z80 has a different topology. Many possible encodings are matched to a single instruction mnemonic, while each possible encoded instruction has a unique disassembling function matched to it. The next section further details the process of disassembly in SpecOps. 

## Decoders and Batteries: Two Approaches to Disassembly

SpecOps modules use two main approaches to disassembling instructions. The first is through the use of decoders. A decoder is implemented using a hash table whose keys are the integer values of different encoded instructions. The `(of-decoder)` forms above create entries in the decoder hash table for the Z80 architecture. As the Z80 disassembler reads instructions, it checks them against the hash table and retrieves the disassembler when it finds a match. In many cases, these disassemblers are simply static lists that express the instruction form corresponding to the found integer. In other cases, these are functions that call on the program API to return a completed instruction code. This may mean reading the next byte(s) in the program to retrieve another value to complete the instruction form.

One thing to note is that when using decoders, the appearance of some integer values indicates that the disassembler must read another binary value to proceed. For example, since `#xDD` is a prefix for a permutation of some operations in Z80's main table as mentioned before, there is a rule in place that when a Z80 disassembler encounters a `#xDD` byte it will shift that byte to `#xDD00`, add the next byte to it and attempt to fetch an entry from the decoder table using that number as the key.

A more complex form of disassembly is accomplished with a test battery. This is a series of functions that may or may not match a given integer, and if matching will compose the appropriate instruction form. The `(readops)` forms manifesting a disassembler for the Super-H architecture seen previously are one example. Each of those forms expresses a disassembler function that will return an instruction form when given a valid instruction encoding. When using test batteries, integers in the vector are run through the test battery one at a time until a match is found. Said match may cause the disassembler to read more integers from the vector to complete composing the instruction, and so those integers will not be run through the test battery.

Decoders and batteries may also be used together and often are. In architectures like M68K, Super-H, and System Z some instructions are expressed through static opcodes with no variable bitfields and thus they can be tested using a decoder, while other instruction codes must be run through a test battery in order to be decomposed. The integers in a code vector will first be checked against the decoder table and then, if no match is found, be subject to the test battery.

### One More Note: On Binary Composition

As shown in the preceding examples, SpecOps assemblers produce vectors of unsigned integers as their output. The instruction processing functions shown up to this point return integers as their output. How does the latter translate to the former?

The numbers output by assembling each instruction are collected in a list which is processed by a function called `(serialize)`, a compositional Swiss army knife that can combine vectors and integers into a larger vector with some options for special configuration. Serialization is always performed at a specific integer width. As of now, all architectures have their serialization width set at 8 or 16 bits - for platforms like M68K or System Z, this is set at 16 bits because all instructions and immediate data they encode are 16-bit-aligned. For 8-bit platforms like the Z80 and 6502 or X86, with its variable 8-bit-aligned encoding, the binary is serialized to a vector of 8-bit elements.

Any integer returned by an assembler function is split according to the encoding width and those elements are pushed to the vector. For instance, the aforementioned encoding of `(:inc (@ :ix 5))` on the Z80 architecture produces output of `#xDD3405`, which will be split into its three component bytes for entry into the output vector.

A special parameter is required if a number is to be encoded with leading zero values. By default, a number is encoded into the minimum number of elements needed to contain its nominal width, as for `#xDD3405` being split into three elements, but there are cases where a wider encoding is needed, as in the case of encoding `(:add :d :c 7)` for the X86 platform to produce `#(5 0 0 0 7)`. Without prefixes, the "add to A-series" opcode 5 takes a 32-bit immediate value as its second operand, and so the 7 value being added must be extended to fill 4 bytes of the output. In the X86 `ADD` spec, this behavior is specified in the first line:

```lisp
((sz? rex #x05            (w 4 op1)) ((:w :d) (:gpr :a) (:imm   )))
((    rex #x05            (w 4 op1)) ((:w :q) (:gpr :a) (:imm   )))
((sz? rex #x80 modz0 sib0      op1 ) ((:w :b) (:gxm   ) (:imm  8)))
((sz? rex #x83 modz0 sib0      op1 ) ((:w :w) (:gxm   ) (:imm  8)))
((sz? rex #x83 modz0 sib0      op1 ) ((:w :d) (:gxm   ) (:imm  8)))
```

The second operand `op1` is given as an argument to the `(w)` function along with 4. This function produces the dotted pair `(4 . 7)`. When `(serialize)` encounters a dotted pair of integers, it responds by encoding the second number using the number of elements expressed by the first number. If `(serialize)` encounters a vector of integers, each number within will be encoded in the output vector in order, just as numeric arguments are. As seen above, in the cases of instructions taking only 8-bit immediate values, no width specification is required.

Finally, if a nil value is encountered by `(serialize)` it will do nothing and skip to the next value. This is useful particularly for architectures like X86 with many optional components of an encoding. In the specification above, note the `sz?` symbols. These are symbol macros expanding to `(determine-pfsize w op0 op1 (of-program :exmode))`, a function that will return one or both of the size prefixes `#x66` and `#x67` or return nil, in which case no size prefix will be present. Note the use of the SpecOps program api, accessed through `(of-program)`. This determines the specific X86 execution mode in use, which is a factor in determining whether a size prefix should be used for a given encoding.

In this specification, the `rex` and `sib0` functions may also return nil - the REX prefix isn't needed for some encodings and registers, and the SIB byte is only used for certain memory access patterns. The flexibility of `(serialize)` allows complex X86 encoding rules to be expressed in a succinct way.

## Future Work

SpecOps is at the beginning stages of its development. A few architectures have most of their instructions encodings implemented at this point in time, but complex platforms like X86 and ARM will take a great deal more work to complete. There is also more work to be done in the areas of formatting binary code. The potential scope of the SpecOps project is vast, with many challenges to overcome and possibilities to be realized by more closely connecting various hardware platforms to Common Lisp.
