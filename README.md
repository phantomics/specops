# SpecOps
### To Specify Operations
#### An Assembler Framework For Common Lisp

The landscape of computing is split along many axes. Some programs are written to be easily readable by humans, while others are written for the most efficient possible evaluation. Different software models prioritize different metrics of performance, and users are further divided between many different varieties of computer hardware, each with distinct features which are exposed in different ways to programmers working at higher levels of abstraction.

What kind of tool or design pattern could bridge these divides? In an increasingly heterogenous computing ecosystem, the task of bringing into concert various kinds of hardware has come into a stronger focus. The SpecOps assembler framework offers a possible answer to this challenge. It leverages the expressive power of Common Lisp to specify assembly and disassembly functions for multiple computer architectures.


### Basic Use

Assembly of computer instructions is in essence a fairly simple task as software goes - numbers are added together to compose larger numbers expressing the different operations available within a given computer architecture. These numbers are placed in a linear code structure, cross-references and other special expressions within the code are reconciled and it is given whatever additional formatting extensions are required for it to be evaluated on the target computer system. The result is a complete program ready for use.

Let's see how this looks in practice, assembling some code for the Motorola 68K processor, a classic CPU used in notable computers including early Apple Macintosh models, the Sharp X68K, the Sega Genesis/Mega Drive game console, and many arcade game systems.

```lisp
* (assemble *assembler-prototype-m68k* ()
            (:addi :b 10 :d1))
#(1537 10)
```

In this case, the M68K's `ADDI` operation is being used to add an immediate value of 10 to the register. The isntruction is expressed as the Lisp form `(:addi :b 10 :d1)`. The output is rendered as a vector of 16-bit integers, since all M68K instructions are divided into 16-bit segments. Let's take a closer look at what was output.

```lisp
* (format t "~v,'0B~%" 16 (assemble *assembler-prototype-m68k* ()
                                    (:addi :b 10 :d1)))
#(11000000001 1010)
```

The `ADDI` instruction is 16 bits long, like all M68K instructions, and is formatted according to the scheme `00000110.SSMMMXXX`. The field S indicates the size of the data being operated upon - in this case a byte, as represented by the encoded value 0 and the `:b` keyword passed in the instruction form. The M field is the addressing mode (0 in this case) indicating that the instruction is addressing a data register. The X field indicates the location being addressed. Its value of 1, along with the 0 value in the M field, indicates that the operation is addressing the data register 1. The following 16-bit word contains the value 10, which the instruction adds to the value in that data register.

SpecOps provides a toolkit for creating schemes that govern such conversions of human-readable instructions into binary program code. Let's take a look at how this transformation is specified.

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

This is the implementation of the `ADDI` operation from the SpecOps M68K module. It checks whether the instruction's operands are an integer and a location (in the register file or in memory), returning an error if not. It uses a macro called `(address)` to derive the numbers indicating both the index and addressing mode for the given operand, and the width, addressing mode and index are then used within a macro called `(masque)` that generates code composing the instruction word.

The `(masque)` macro is frequently used within SpecOps to model binary formats. It's easy to read and generates efficient code: a series of increment operations of masked and shifted integers to generate an output number. A primary goal of SpecOps is the development of tools like this that can support assembling programs for many different architectures.

## License

BSD 3-Clause

