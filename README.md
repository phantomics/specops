# SpecOps
#### To Specify Operations,
#### Augment Assembly Processes,
#### And Unite Systems Electronic and Semantic
### An Assembler Framework For Common Lisp

The computing landscape is split along many axes. Some programs are written to be easily readable by humans, while others are written for the fastest possible evaluation. Different software models prioritize different performance metrics and systems are further divided between many different varieties of computer hardware, each with distinct features which are exposed in different ways to programmers.

What kind of tool or design pattern could bridge these divides? In an increasingly heterogenous computing ecosystem, the task of bringing into concert various kinds of hardware has come into a stronger focus. The SpecOps assembler framework aims to meet this challenge, leveraging the expressive potential of Common Lisp to specify assembly and disassembly functions for multiple computer architectures, and augment  with tools of symbolic programming.


### Basic Use

Assembling computer instructions is in essence a fairly simple task - numbers are added together to compose larger numbers expressing the different operations available within a given computer architecture. These numbers are placed in a linear structure, cross-references and other special expressions within the code are reconciled and it is given whatever additional formatting extensions are required for it to be evaluated on the target computer system. The result is a complete program ready for use.

Let's see how this looks in practice and assemble some code for the Motorola 68K processor, a classic CPU used in notable computers including the Commodore Amiga, early Apple Macintosh models, the Sharp X68K, the Atari ST, the Sega Genesis/Mega Drive game console, and many arcade game systems.

```lisp
;; load the SpecOps M68K module

* (asdf:load-system 'specops.m68k) (in-package :specops.m68k)
...system loads...

* (assemble *assembler-prototype-m68k*
    (:addi :b 10 :d1))
#(1537 10)
```

In this case, the M68K `ADDI` operation is being used to add an immediate value of 10 to the register. The isntruction is expressed as the Lisp form `(:addi :b 10 :d1)`. The output is rendered as a vector of 16-bit integers, since all M68K instructions are divided into 16-bit segments. Prior to the single `ADDI` instruction, the assembelr macro takes as its argument `*assembler-prototype-m68k*`, an assembler class instance expressing default properties for an M68K code assembler. Let's take a closer look at what was output.

```lisp
* (write (assemble *assembler-prototype-m68k*
           (:addi :b 10 :d1)) ;; ADDI.B 10,D1
         :base 2)
#(11000000001 1010)
#(1537 10)
NIL
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

### Another Architecture

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

The width specification is used by default because it corresponds more closely to how X86 instructions are encoded and for another reason, best illustrated in the following example:

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

When this macro expands, the `(:provisions)` form populates a `(symbol-macrolet)` form assigning the various bytes. The operand symbols are determined by the second argument to the macro, in this case `(w op0 op1)`. Each of the lines following the parameters expresses one possible encoding, with the first form in the line containing a list of values that may be present in the output instruction. The `sz?` symbol may express the `#x66` register size prefix and/or the `#x67` address size prefix if appropriate conditions are in place, such as an operation targeting a 16-bit-wide register in the 64-bit long execution mode. The `rex` symbol may express the REX prefix required for 64-bit wide operations or in many cases. This prefix is followed by the opcode, which in many cases may be followed by MOD and SIB bytes and sometimes by an immediate integer value. The `(w)` forms encapsulating some of these immediate specifications determine that in those cases, the immediate value must be 2 or 4 bytes wide even if the operand value would fit into a smaller width.

The second form in each line specifies the kinds of operands that determine the use of the encoding. For instance, the first line determines that the `#x04` opcode will be used in the case of an 8-bit wide operation (`(:w :b)`) whose first operand is the general-purpose register series A (`(:gpr :a)`) and whose second operand is an immediate value (`(:imm   )`), implicitly an 8-bit value since the operation's given width is 8 bits. The `(:priority)` form determines the order in which the operands will be tested. First, the operation's width will be checked, then the second argument's type will be checked, and finally the first operand's type will be checked. Careful selection of priorities can speed up the encoding dispatch.


## License

BSD 3-Clause

