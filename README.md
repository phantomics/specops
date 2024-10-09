# SpecOps
## To Specify Operations
### An Assembler Framework For Common Lisp

The landscape of computing is split along many axes. Some programs are written to be easily readable by humans, while others are written for the most efficient possible evaluation. Different software models prioritize different metrics of performance, and users are further divided between many different varieties of computer hardware, each with distinct features which are exposed in different ways to programmers working at higher levels of abstraction.

What kind of tool or design pattern could bridge these divides? In an increasingly heterogenous computing ecosystem, the task of bringing into concert various kinds of hardware has come into a stronger focus. The SpecOps assembler framework offers a possible answer to this challenge. It leverages the expressive power of Common Lisp to specify assembly and disassembly functions for multiple computer architectures.


### Basic Use

Assembly of computer instructions is in essence a fairly simple task as software goes - numbers are added together to compose larger numbers expressing the different operations available within a given computer architecture. These numbers are placed in a linear code structure, cross-references and other special expressions within the code are reconciled and it is given whatever additional formatting extensions are required for it to be evaluated on the target computer system. The result is a complete program ready for use.

Let's see how this looks in practice, assembling some code for the Motorola 68K processor, a classic CPU used in notable computers including early Apple Macintosh models, the Sharp X68K, the Sega Genesis/Mega Drive game console, and many arcade game systems.

```lisp
* (assemble *assembler-prototype-m68k* (:with (:store (abc :gpr)))
            (:addi :b 10 abc))
#(1540 10)
```

In this case, the M68K's `ADDI` operation is being used to add an immediate value of 10 to the register. The output is rendered as a vector of 16-bit integers, since all M68K instructions are divided into 16-bit segments.


## License

BSD 3-Clause

