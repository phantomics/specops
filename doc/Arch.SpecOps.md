---
id:            SPECOPS-DRAFT-specops
title:         The SpecOps Architecture
genre:         Architecture
scope:         project
project:       SpecOps
language:      en
status:        Draft
provenance:
  assistant:   claude-opus (OpenCode)
  session:     Compass adoption; Restructure
relates-to:
  - SPECOPS-DRAFT-manifest
  - SPECOPS-DRAFT-restructure
decisions:
  - SPECOPS-DRAFT-specops-D1
  - SPECOPS-DRAFT-specops-D2
  - SPECOPS-DRAFT-specops-D3
  - SPECOPS-DRAFT-specops-D4
  - SPECOPS-DRAFT-specops-D5
  - SPECOPS-DRAFT-specops-D6
  - SPECOPS-DRAFT-specops-D7
  - SPECOPS-DRAFT-specops-D8
  - SPECOPS-DRAFT-specops-D9
  - SPECOPS-DRAFT-specops-D10
  - SPECOPS-DRAFT-specops-D11
open-questions:
  - SPECOPS-DRAFT-specops-O1
  - SPECOPS-DRAFT-specops-O2
  - SPECOPS-DRAFT-specops-O3
  - SPECOPS-DRAFT-specops-O4
  - SPECOPS-DRAFT-specops-O5
  - SPECOPS-DRAFT-specops-O6
glossary:      SPECOPS-DRAFT-glossary
cites:
  - title:    z/Architecture Principles of Operation
    locator:  instruction formats (RR, RX, RS, RI, RIL, SS and others)
    external: true
  - title:    M68000 Family Programmer's Reference Manual
    locator:  instruction encoding diagrams
    external: true
---

# The SpecOps Architecture

SpecOps is a framework for writing assemblers and disassemblers in Common
Lisp. This record states the design the framework is built to: how an
instruction set is described, how operands are checked and encoded, how code
is serialized, disassembled and packaged into object files, and what an
architecture module must provide to join the framework. It is written against
commit `1e53ae7`, which has modules for Motorola 68000, Intel x86, IBM
System Z, Hitachi SuperH, Zilog Z80, MOS 6502 and WebAssembly at very
different stages of completion. Where the design and the code differ, the
design is the target, and the difference is an open question or a Roadmap
step. The declarative binary-format layer is covered in detail by the
[Manifest development log](Log.Manifest.md); this record places it in the
whole. The repository was later reorganized into a core and modules of three
kinds, and its systems renamed, as chronicled in the
[Restructure development log, SPECOPS-DRAFT-restructure](Log.Restructure.md).
Module design, the object-format and binary-format sections, the Project
structure appendix, and notes in Open Questions and the Roadmap describe that
layout; code references remain pinned to `1e53ae7`. Terms are defined in the
[SpecOps glossary](Glossary.SpecOps.md).

## Canonical Terms

Table: The central terms of this design; the glossary defines the rest.

| Term | Definition |
|---|---|
| assembler prototype | The instance of an architecture's assembler class, bound to `*assembler-prototype-<arch>*`, whose symbol every specification names |
| lexicon | Mnemonic keyword to encoder |
| decoder | Whole encoded value to disassembled form |
| battery | Unmasker functions tried in turn against an encoded value |
| specop / readop | One encoder or disassembler specification |
| duplex | A specification that yields both encoder and decoder |
| memory access scheme (MAS) | An object describing a memory operand, built by an `@` function |
| program API | The closure through which encoders query their context (`of-program`) |
| breadth | The bit width of one output unit |
| architecture, format and map modules | The three kinds of system under `mod/`: one instruction set, one object or container format, or one two-way mapping such as a code table |

## Thesis / Concerns

**Thesis.** An instruction set is best described to a computer the way its
manufacturer describes it to a person: as bit-field diagrams, opcode tables
and named formats. If Lisp macros can take such descriptions nearly verbatim
and expand them into efficient encoders and decoders, then one framework can
serve many unlike architectures. Symbolic programming (symbols for registers,
macros generating instruction sequences, Lisp data as assembly input) then
applies to low-level code on all of them at once.

**Concerns**, by stakeholder:

- *Module authors* adding an instruction set need specifications that read
  like the manual, fail at load time when malformed, and do not require
  restating the encoding again for disassembly.
- *Users* generating code from Lisp (compilers, code generators, tools for
  retro and mainframe platforms) need a uniform `assemble`/`interpret`
  interface, clear errors for invalid operands, and control over which
  registers a program may touch.
- *Format writers* need a model of a program (sections, symbols, relocations)
  that is independent of both the instruction set and the object format, and a
  declarative way to lay out binary records.
- *Everyone* needs SpecOps to load on any conforming Common Lisp without
  pulling in libraries.

## Influences

- **Manufacturers' encoding diagrams.** The mask strings of `masque`
  (`"00000110.SSMMMXXX"`) transcribe the bit diagrams of manuals such as the
  M68000 Family Programmer's Reference Manual (see `cites:`): one character per
  bit, constant bits as digits, fields as letters. Contributes: specifications
  that can be checked against the manual by eye. Rejected: the manuals'
  per-instruction prose, replaced by `determine` operand types.
- **Named instruction formats.** System Z's `zformat-rr`, `zformat-rx-a` and
  the other 76 formats follow the format names of the z/Architecture
  Principles of Operation (see `cites:`), so that most instructions are one
  line pairing a mnemonic, a format and an opcode. Contributes: the `mqbase`
  pattern of format-level currying. Rejected: the manual's extended
  mnemonics, which are not yet specified.
- **Opcode maps.** The Z80 and 6502 modules transcribe the conventional
  16×16 opcode maps of those processors as operation tables. Contributes: the
  tabular specification and automatic duplex decoders. Rejected: listing the
  `IX`/`IY` pages separately, since they are derived as shadow forms.
- **lisp-binary**, the Common Lisp binary-format library, was reviewed as
  prior art for the manifest layer (see the Manifest log's `cites:`).
  Contributes: the feature checklist for a format layer. Rejected: its
  imperative read/write split, and a dependency on it.
- **Conventional text assemblers.** Deliberately not followed: SpecOps input
  is Lisp data, and width is an explicit operand instead of being implied by
  register names (SPECOPS-DRAFT-specops-D2, SPECOPS-DRAFT-specops-D3).

**Comparable systems.** These also generate assemblers or disassemblers from
instruction-set descriptions. They are not recorded as influences; the steward
should confirm which, if any, were consulted.

- **GNU CGEN**, a CPU description language in Scheme from which binutils
  assemblers, disassemblers and simulators are generated. Like SpecOps, it
  describes fields and formats in a Lisp; unlike SpecOps, it generates C
  source for a separate toolchain rather than running in the Lisp image.
- **LLVM TableGen** instruction records, from which LLVM's machine-code
  emitters and disassemblers are generated. It shares the one-description,
  two-directions aim, but serves a compiler back end with a fixed instruction
  selection model.
- **Ghidra SLEIGH**, a processor specification language for disassembly and
  semantic lifting. It covers the decoding direction only.
- **SBCL's assembler**, whose `define-instruction-format` and
  `define-instruction` pair emitters with disassembler printers inside a Lisp
  image. It is the closest precedent for format-level definitions in Lisp, but
  is internal to the compiler and covers only SBCL's own back ends.

## Settled Decisions

These record choices already visible in the code at `1e53ae7`. The
alternatives were mostly not written down when the choices were made. Each
record's Alternatives therefore names its source: *Recorded*, with the place
where the alternative was discussed, or *Reconstructed*, for an alternative
inferred while drafting this record, for the steward to confirm or correct.

### SPECOPS-DRAFT-specops-D1 — Portable Common Lisp with no library dependencies

**Status:** Proposed
**Context:** SpecOps aims to bridge platforms, and a Lisp library dependency
narrows the implementations and environments it can be loaded into. Some tasks
(checksums, string codecs, LEB128) are commonly delegated to libraries.
**Decision:** The core system `specops` and every module depend only on
Common Lisp, ASDF and SpecOps's own systems: the core, and the other modules
the dependency rule of Module design allows. What a library would supply is
implemented natively (`checksum.lisp:crc@1e53ae7`,
`specops.lisp:encode-leb128-unsigned@1e53ae7`, `defcodetable` code pages).
Implementation-specific code is allowed only behind a conditional with a
portable fallback, as in `checksum.lisp:adler32-checksum@1e53ae7`.
Development tools that need libraries, such as the `lquery` scraper
`z80/comber.lisp`, stay outside every ASDF system.
**Alternatives:** Recorded (SPECOPS-DRAFT-manifest-D9): depending on Ironclad
for checksums, rejected to keep the core free of dependencies.

### SPECOPS-DRAFT-specops-D2 — Assembly input is Lisp list forms

**Status:** Proposed
**Context:** Assembly input must be easy to generate from Lisp programs and
macros, and must carry Lisp values (symbols bound to registers, computed
immediates) as operands.
**Decision:** An instruction is a list headed by a mnemonic keyword, with
operands patterned on the architecture's standard syntax:
`(:addi :b 10 :d1)`, `(:st 4 (@ 7 8 90))`. Operands are evaluated at run
time, so they may be any Lisp expression. Disassembly returns the same form.
**Alternatives:** Recorded (README, "Basic Usage"): conventional text syntax,
which is left to front ends that produce list forms rather than parsed in the
core (SPECOPS-DRAFT-specops-O1).

### SPECOPS-DRAFT-specops-D3 — Operation width is an explicit operand

**Status:** Proposed
**Context:** On x86 and M68K the same mnemonic acts at several widths.
Standard x86 syntax implies the width through register names (`EAX`, `AX`),
which cannot work when a register is a symbol bound at assembly time.
**Decision:** Width is passed as an operand (`(:add :w :c 20)`,
`(:addi :b 10 :d1)`), registers are named by their series (`:c`, `:a`), and
encodings dispatch on the width keyword. This is also closer to how the
instructions are encoded and simpler for macros to generate.
**Alternatives:** Recorded (README, "Assembly in Context"): width-named
registers in the conventional style, rejected for the core because they are
incompatible with symbolic register binding. An architecture may still offer
them as a mapping layer.

### SPECOPS-DRAFT-specops-D4 — One prototype instance per architecture, consulted at macroexpansion

**Status:** Proposed
**Context:** Specification macros must expand differently for each
architecture: x86 clauses become a `symbol-macrolet` with ordered tests, Z80
tables become many lexicon and decoder entries. The expansion therefore has to
dispatch on the architecture while the specification is compiled.
**Decision:** Each architecture defines an assembler subclass and binds one
instance to `*assembler-prototype-<arch>*`. Specification macros
(`specops.lisp:specops@1e53ae7`, `readops`, `assemble`, `program`) take that
variable's symbol and call generic functions such as `specify-ops` on its value
during expansion; the expansion refers to the symbol for run-time table access.
Instruction tables are class-allocated slots, so every instance of the class
shares them.
**Alternatives:** Reconstructed: plain per-architecture functions or global
tables without CLOS dispatch. These would avoid the load-order constraint (the
prototype must be bound before any specification compiles), but would give up
method specialization of the expansion.

### SPECOPS-DRAFT-specops-D5 — Bit fields are composed from mask strings at compile time

**Status:** Proposed
**Context:** Most fixed-width encodings are fields packed into a word. Writing
the shifts and masks by hand is error-prone and hard to compare with the
manual.
**Decision:** `specops.lisp:masque@1e53ae7` parses a mask string at
macroexpansion (`specops.lisp:quantify-mask-string@1e53ae7`) and emits a
constant base plus one `logior`/`ash` per field. `unmasque` is its inverse for
decoding. Static fields fold into the base. The same notation is used for
manifest masques.
**Alternatives:** Reconstructed: run-time field-packing functions (slower, and
the layout is not visible as a picture of the word), or explicit `dpb`/`ldb`
forms (no single readable layout).

### SPECOPS-DRAFT-specops-D6 — Operands are checked by generated `determine` forms

**Status:** Proposed
**Context:** Every instruction must reject invalid operands with a useful
message, and derive the numbers that encode valid ones. Repeating this logic
per instruction would bury the encodings.
**Decision:** An instruction body states its operand types in a `determine`
form. The architecture supplies three functions (qualify, derive, verbalize),
and the core macro `specops.lisp:determine-in-context@1e53ae7` expands the
form into the tests, the bindings of derived values, and an error that lists
the instruction's accepted operand formats. Architectures attach the rewrite
of `determine` through the `:process-forms` parameter (for example
`m68k/templates.lisp:form-process@1e53ae7`).
**Alternatives:** Reconstructed: hand-written checks in each instruction,
which would repeat the same tests hundreds of times and let error messages
drift from the accepted operands; or a generic type check with a fixed
message, which cannot tell the user which operand formats an instruction
accepts.

### SPECOPS-DRAFT-specops-D7 — Two disassembly strategies, combinable

**Status:** Proposed
**Context:** Some instruction sets map whole encoded values to instructions
(Z80, 6502); others must be recognized by masking off operand fields (System
Z); many need both.
**Decision:** `assembler-encoding` carries a decoder table keyed by encoded
value; `assembler-masking` carries a battery of unmasker functions.
`specops.lisp:interpret-element@1e53ae7` uses `or` method combination, so an
assembler inheriting both classes (M68K, SuperH) tries the decoder first and
then the battery.
**Alternatives:** Reconstructed: a battery alone (simple, but slow for
table-driven sets); a decoder alone (impossible for masked fields without
enumerating every operand value).

### SPECOPS-DRAFT-specops-D8 — Duplex specification where the encoding allows

**Status:** Proposed
**Context:** Writing the encoder and decoder separately doubles the work and
lets them drift apart. But some encodings are asymmetric: one SuperH `ADD`
encoder covers two encodings that disassemble through two different masks.
**Decision:** Where the structure permits, one specification yields both:
`specops.lisp:mqbase@1e53ae7` formats produce an encoder and a battery
unmasker, and operation tables with `(:duplex . of-decoder)` produce decoder
entries. Where it does not, decoders are written separately with `readops`,
next to their encoder.
**Alternatives:** Reconstructed, from the README's "Duplex Operation" and
"Mirrored Specs" sections: always separate (rejected: the duplication duplex
removes); always duplex (rejected: asymmetric encodings cannot be expressed
without contortion).

### SPECOPS-DRAFT-specops-D9 — Output is a vector of fixed-width units composed by `serialize`

**Status:** Proposed
**Context:** Encoders produce integers of varying widths, sometimes optional
parts (prefixes) or values needing leading zeros, while instruction sets are
aligned on 8- or 16-bit units.
**Decision:** Encoder results are collected, absent (`nil`) parts are
dropped, and the rest pass through `specops.lisp:serialize@1e53ae7` at the
assembler's breadth: an integer is split into the fewest units, a width pair
`(n . v)` into exactly `n`, and a vector element by element. `assemble`
returns an `(unsigned-byte breadth)` vector.
**Alternatives:** Reconstructed: writing bytes from each encoder (rejected:
forces every encoder to know the output width and byte order).

### SPECOPS-DRAFT-specops-D10 — Registers may be bound symbolically and reserved

**Status:** Proposed
**Context:** Generated code often needs "some free register", and host
environments reserve registers (SBCL's use of x86-64 `R12`/`R13`; register
`x18` on Apple ARM64).
**Decision:** `assemble` and `program` accept a `:store` parameter binding
symbols to registers (`specops.lisp:locate@1e53ae7`), optionally constrained
with `:bind`. Registers are drawn from the assembler's register pool, from
which blocked register files are excluded. Assembler subclasses may extend
this to bar reserved resources. At `1e53ae7` the pool is built but not yet
used: `locate` draws from the program model directly, so blocking a file has
no effect on `:store` (see also SPECOPS-DRAFT-specops-O6).
**Alternatives:** Reconstructed: leaving allocation to a register allocator
outside the assembler, so that the assembler accepts only concrete registers.
That keeps the assembler simple, but then every code generator must
reimplement allocation and know each host's reserved registers.

### SPECOPS-DRAFT-specops-D11 — A program model independent of instruction set and object format

**Status:** Proposed
**Context:** Raw `assemble` produces a code vector, but real output needs
sections, symbols, relocations and an object-file format (ELF, GOFF,
WebAssembly), and these combinations multiply.
**Decision:** `program.lisp:program@1e53ae7` describes a program as units
holding segments of items (instructions, labels, data, alignment, functions),
with symbols and relocations. `program.lisp:build-program@1e53ae7` assembles
it through the lexicon in two passes, and
`program.lisp:emit-program@1e53ae7` writes it in a format chosen by keyword.
The architecture supplies only facts such as endianness and ELF machine
number.
**Alternatives:** Recorded in history: per-architecture format writers, the
first draft of the format system (`system-z/format.lisp` and
`wasm/format.lisp`, added in `a2ea739` and since removed). They were replaced
by the shared model, which `system-z/program.lisp` now extends with
architecture facts and GOFF constants.

## Specification design

An instruction set is described by top-level forms, compiled once:

- **Instruction specops.** `specops` takes a mnemonic, an operand list, the
  prototype symbol and a body. An optional first item is a parameter alist.
  `specops.lisp:specify-ops@1e53ae7` processes it, and
  `specops.lisp:clause-processor@1e53ae7` wraps the body into a lexicon
  function of `(program-api . operands)` with `of-program` bound locally.
- **Architecture wrappers.** Modules define short macros so that each
  specification reads like the manual. `specop-68k` and `specops-sh` wrap
  `specops`, supplying the prototype and the module's `:process-forms`
  function. `system-z/base.lisp:specop-z@1e53ae7` instead expands an `mqbase`
  format with an opcode and registers the resulting encoder and decoder
  itself.
- **Parameters** extend a specification without new macros: `:combine`
  generates mnemonic families, `:tabular` lays out an operation table
  (`specops.lisp:process-clause-matrix@1e53ae7`), `:for-types` restricts an
  instruction to ISA variants, `:type-matcher` adds a variant test, `:duplex`
  adds decoders.
- **Architecture-specific expansion.** A module whose encodings do not fit the
  default overrides `specify-ops`, `clause-processor` or `extend-clauses`. x86
  replaces the body with `:provisions` (named encoding parts, as
  `symbol-macrolet`) and a list of clauses tested in `:priority` order
  (`x86/base.lisp:specify-ops@1e53ae7`, with the tests built by
  `x86/base.lisp:qualify-ops@1e53ae7`). Z80 extends each table with shadow
  forms (`z80/base.lisp:extend-clauses@1e53ae7`).

## Operand and storage design

- **Registers** are keywords grouped by type in the assembler's storage, and
  resolved to indices by the module's qualify and derive functions.
- **Immediates** are Lisp integers; their permitted width is part of the
  operand type (`(imm 16)`).
- **Memory operands** are memory access schemes: classes built from the core
  mixins `mas-based`, `mas-indexed`, `mas-displaced`, `mas-scaling-displaced`
  and `mas-absolute` (`specops.lisp:memory-access-scheme@1e53ae7`). Each
  module defines its own `@` constructor family in its package and its own
  operand types over them. Disassemblers emit `(@ …)` forms, so their output
  can be assembled again.
- **Validation** is the `determine` form of SPECOPS-DRAFT-specops-D6.
- **Symbolic registers** come from the pool of SPECOPS-DRAFT-specops-D10.

## Encoding and serialization design

- **Bit fields** use `masque` (SPECOPS-DRAFT-specops-D5). Instruction formats
  shared by many instructions are defined once with `mqbase`, which yields a
  macro per format producing encoder and decoder.
- **Byte-variable encodings** (x86) compose a list of optional parts (prefixes,
  REX, opcode, ModRM, SIB, immediates), with `nil` for absent parts.
- **Context** reaches an encoder through the program API: `:exmode` for the
  execution mode, `:label` for branch targets, `:assembler-type` for ISA
  variants. `specops.lisp:compose@1e53ae7` and `program.lisp:%make-code-api@1e53ae7`
  provide it, and patch forward label references after the pass.
- **Serialization** follows SPECOPS-DRAFT-specops-D9. Byte order across units
  is handled per output: `program.lisp:asm-endianness@1e53ae7` when a program
  is emitted, swap modes in the manifest layer, and in one case by the encoder
  itself (`6502/base.lisp:enc-displ@1e53ae7`); see
  SPECOPS-DRAFT-specops-O3.

## Disassembly design

`specops.lisp:interpret@1e53ae7` walks a code vector of the assembler's unit
type. At each point it reads candidate patterns of the lengths in the
assembler's disassembly segment and calls `interpret-element`. That looks the
pattern up in the decoder, then tries the battery (SPECOPS-DRAFT-specops-D7).
A matching entry may read further units through a reader closure to complete
the instruction. The result is a list of instruction forms in the input syntax
of SPECOPS-DRAFT-specops-D2. Input vectors must have the correct element type.
A vector typed in at the REPL is a simple vector, and will not disassemble.

## Program and object-format design

`program.lisp` realizes SPECOPS-DRAFT-specops-D11. The `program` macro takes
`params` (name, entry, store, execution mode) and section forms (`code`,
`data`, `rodata`, `bss`), and matches its keywords by name, so that it works
from any architecture package. Code segments are assembled in two passes:
labels, symbols and relocations are recorded in the first pass, and forward
references are resolved in the second. Offsets are counted in breadth units
and converted to octets with the architecture's endianness on output.
`emit-program` has methods for `:elf` (`elf.lisp`, an ELF64 executable
writer), `:goff` (`goff.lisp`, z/OS object records) and `:wasm`
(`wasm/program.lisp`, which also provides a `wasm-program` macro for module
structure). Each writer is now a format module, `specops.format.elf`,
`specops.format.goff` and `specops.format.wasm`, kept apart from the
architecture modules because one instruction set may be written in several
formats: s390x code goes into ELF on Linux and GOFF on z/OS
(SPECOPS-DRAFT-restructure-D4). The ELF and GOFF writers have packages of
their own. WebAssembly is split in two: its instruction set is an
architecture module, and its binary module writer a format module that
depends on it and is still in the architecture's package.

## Binary-format design

Object files and containers are binary records whose layout depends on their
own contents (lengths, offsets, checksums). The manifest layer describes such
a record once, with `defmanifest`, and generates a writer (`marshal`) and a
reader (`unmarshal`). Manifests contain no logic, regions nest, and
sub-manifests inline into their parent. Its decisions are recorded as
SPECOPS-DRAFT-manifest-D1 through SPECOPS-DRAFT-manifest-D14 in the
[Manifest development log](Log.Manifest.md). Character codecs for string
fields are code tables defined with `defcodetable`, in the map modules
`specops.map.cp437` and `specops.map.ebcdic` (CP437, EBCDIC code pages 037 and
310; SPECOPS-DRAFT-restructure-D5). The GOFF END record is the
first object-format record written through a manifest; the design intends the
format writers to move onto manifests generally. The PNG manifests in
`test/manifest.lisp` are a test case for the layer, not a format module
(SPECOPS-DRAFT-restructure-D7).

## Module design

Every system under `mod/` is a module: an ASDF system in
`mod/<kind>/<name>/`, named `specops.<kind>.<name>`, whose package has the
same name (SPECOPS-DRAFT-restructure-D1, SPECOPS-DRAFT-restructure-D2). The
one exception is the WebAssembly format module, whose file is still in the
package of its architecture module. There are three kinds:

- **Architecture modules** (`mod/arch/`, `specops.arch.<isa>`) each implement
  one instruction set. They are named for the instruction set, not for a
  machine or product family: the System Z module is `s390x`
  (SPECOPS-DRAFT-restructure-D3). Variants and older modes of an instruction
  set belong to its module, as execution modes or ISA types.
- **Format modules** (`mod/format/`, `specops.format.<format>`) each write one
  object or container format through `emit-program` methods, and in time
  through manifests (SPECOPS-DRAFT-restructure-D4). They are for the formats
  that carry programs. Image and other media formats are outside SpecOps's
  scope; native readers for them belong in projects of their own that depend
  on SpecOps (SPECOPS-DRAFT-restructure-D7).
- **Map modules** (`mod/map/`, `specops.map.<name>`) each hold a two-way
  mapping between value spaces that belongs to no single architecture or
  format, such as a code table (SPECOPS-DRAFT-restructure-D5). An opcode map
  is not a map module; it belongs to its architecture.

Dependencies run one way. Map modules depend only on the core. Format modules
depend on the core and on map modules, and on an architecture module only
when the format exists for that one instruction set, as WebAssembly's module
format does. Architecture modules depend on the core and on map modules, never
on format modules; they give formats the facts they need through generic
functions of the core's program protocol, such as `asm-endianness` and
`asm-elf-machine`.

An architecture module depends on `specops` and has its own package using
`specops`. It must provide:

1. an assembler subclass of `assembler-encoding`, `assembler-masking` or both,
   with class-allocated instruction tables (lexicon, decoder or battery),
   disassembly segment and breadth, and its storage and program model.
   Per-program state, namely the register pool and reservations, belongs to
   each instance (SPECOPS-DRAFT-specops-D10);
2. a prototype variable `*assembler-prototype-<arch>*`, bound before any
   specification is compiled;
3. operand support: qualify, derive and verbalize functions for `determine`,
   register predicates and types, memory access scheme classes and `@`
   constructors;
4. specification macros, where the core `specops` is not used directly, and
   the instruction specifications themselves;
5. disassembly, through duplex specifications or `readops`;
6. for object output, methods on `asm-endianness` and, for ELF,
   `asm-elf-machine`.

The table below is evidence of the state at `1e53ae7`, not part of the
design; it will go stale, and belongs in a Ref once one exists. Its systems
have since been renamed `specops.arch.<isa>`, with `specops.system-z` now
`specops.arch.s390x`, and `wasm/program.lisp` is now the format module
`specops.format.wasm`.

Table: Architecture modules at commit 1e53ae7: assembler class, how
instructions are specified, and how they are disassembled.

| Module | Assembler class | Specification | Disassembly |
|---|---|---|---|
| `specops.m68k` | encoding and masking | `specop-68k` with `determine`, `masque` | decoder and battery (`readop-68k`) |
| `specops.x86` | plain `assembler` | `specops` with `:provisions` clauses | none yet |
| `specops.system-z` | masking | `specop-z` over `mqbase` formats | battery, duplex |
| `specops.superh` | encoding and masking | `specops-sh` with `:for-types` | decoder and battery (`readops-sh`) |
| `specops.z80` | encoding | operation tables, shadow forms | decoder, duplex |
| `specops.6502` | encoding | one operation table | decoder, duplex |
| `specops.wasm` | encoding | operation tables in `ops.lisp` and the `wasm-program` macro in `wasm/program.lisp`, neither yet in the system | none yet |

## Open Questions

### SPECOPS-DRAFT-specops-O1 — Where do text-syntax front ends live?

SPECOPS-DRAFT-specops-D2 leaves conventional text syntax to front ends that
produce list forms. Are these per-architecture parsers in each module, a
shared parsing toolkit in the core, or separate systems? Should they map
width-named registers onto the explicit width operands of
SPECOPS-DRAFT-specops-D3?

### SPECOPS-DRAFT-specops-O2 — What unit breadth do byte-oriented instruction sets use?

The README describes Z80, 6502 and x86 output as vectors of 8-bit elements,
but `specops.lisp:assembler@1e53ae7` gives `%breadth` a class-allocated
default of 16, and none of those modules overrides it. As a result, a prefixed
Z80 instruction comes out as a single 16-bit element. Should every
byte-oriented module declare a breadth of 8? Does `assemble` always return
the instruction set's natural unit, or does `program`/`emit-program` alone
decide the octet layout?

### SPECOPS-DRAFT-specops-O3 — Who owns byte order?

Byte order is handled in three places: by `asm-endianness` when a program is
emitted, by swap modes in manifests, and inside individual encoders (the 6502
writes little-endian addresses through `6502/base.lisp:enc-displ@1e53ae7`,
while `serialize` splits values most-significant first). Should endianness be
a property of the assembler that `serialize` applies, leaving encoders to
produce plain integers?

### SPECOPS-DRAFT-specops-O4 — What reader protocol does disassembly offer?

`specops.lisp:interpret@1e53ae7` reads the disassembly segment
(`asm-msk-segment`) for every assembler, a slot that encoding-only
assemblers such as Z80 and 6502 lack. Their decoders also call
`(of-program :next-bytes …)`, which the reader closure does not provide. The
loop over candidate lengths never runs when the disassembly segment has a
single entry (M68K's `(1)`), and the outer loop stops one unit before the end
of the vector. A System Z round trip decodes later instructions wrongly. The
contract between `interpret` and decoder or battery entries (how many units
are read, how entries request more, how prefixes chain) needs to be fixed in
one place, with tests for round trips.

### SPECOPS-DRAFT-specops-O5 — How is a module's conformance verified?

The module contract under Module design is not checked anywhere. At
`1e53ae7`, in a clean image:

- M68K does not load: `*m68k-layout*` is referenced but never defined
  (`m68k/base.lisp:assembler-m68k@1e53ae7`).
- SuperH does not load: its prototype passes an initarg no class declares.
- WebAssembly does not load: its system declares no dependency on `specops`,
  and `*assembler-prototype-wasm*` is never bound. Since `50e231d` the
  system declares the dependency and loads, but the prototype, which
  `ops.lisp` names, is still unbound, and `ops.lisp` is not in the system.
  M68K and SuperH still fail to load as described above.
- x86 loads with warnings, but the README example fails at run time because
  `*x86-layout*` is never defined (`x86/base.lisp:gprix@1e53ae7`).

The test scripts load source files directly and cover System Z,
`program.lisp`, the format writers and WebAssembly output only. What is the
minimum per-module suite (README examples assembling to their stated vectors,
and disassembling back), and should it be the module's ASDF `test-op`?

### SPECOPS-DRAFT-specops-O6 — What is the supported public API?

`package.lisp` exports names that are not defined (`join`, `label-delta`,
`process-operands`, `make-array-writer`, `asm-promodel`, `derive-promodel`,
`asm-enc-breadth`). It does not export others that modules and users need
(`serialize`, `defmanifest`, `defenum`, `unmarshal`, and `asm-pmodel`).
Because the module packages simply use `specops`, an unexported name in a
module is a different symbol from the core's. A module's `%pmodel` slot and
`asm-pmodel` accessor are therefore invisible to the core's `locate` and
pool builder, and `:store` finds no registers. Which names form the stable
interface for module authors and for users, and should they be split into
separate packages?

## Roadmap

1. **Restore load health.** Every module system loads in a clean image, and
   the README examples assemble to the vectors it shows
   (SPECOPS-DRAFT-specops-O5).
2. **Test suites.** Per-module and core suites behind ASDF `test-op`,
   absorbing the test scripts and the manifest demos in `test/`.
3. **Settle units and byte order** (SPECOPS-DRAFT-specops-O2,
   SPECOPS-DRAFT-specops-O3), and fix the disassembly reader protocol
   (SPECOPS-DRAFT-specops-O4) so that every duplex module disassembles.
4. **Package the format layer.** Put `elf.lisp`, `goff.lisp` and the
   per-architecture program helpers into ASDF systems, and move record
   layouts onto manifests as the manifest layer matures. The writers are now
   the format modules of SPECOPS-DRAFT-restructure-D4. The s390x module's
   `program.lisp`, with its methods of the program protocol, is now part of
   its system, but still holds the GOFF attribute constants beside z/OS
   layout helpers; the constants are to move to the GOFF module.
5. **Define the public API** (SPECOPS-DRAFT-specops-O6), then document it in
   a Ref.
6. **Complete the large instruction sets.** Finish x86 (remaining
   instructions, disassembly), add ARM, and finish the WebAssembly tables.
7. **Text front ends** (SPECOPS-DRAFT-specops-O1).

## Appendices

### Worked scenario: one M68K instruction

`(assemble *assembler-prototype-m68k* (:addi :b 10 :d1))` passes through the
design as follows. This is the README's example, and describes the intended
path. At `1e53ae7` the M68K module does not load
(SPECOPS-DRAFT-specops-O5).

1. At load time, `specop-68k addi` expanded through `specops`. The module's
   `form-process` rewrote its `determine` form into `determine-in-context`,
   and `clause-processor` stored a function under `:addi` in the lexicon.
2. `assemble` expands, through `%assemble`, into a call to `compose` with the
   instruction list `(list :addi :b 10 :d1)`, its operands evaluated.
3. `compose` looks up `:addi` in the lexicon and calls the encoder with the
   program API and the operands.
4. The `determine` code checks that `:b` is a width, `10` an immediate of at
   most 32 bits, and `:d1` a register or memory operand. It binds the encoded
   width (0), addressing mode (0) and register index (1).
5. `masque "00000110.SSMMMXXX"` folds those into `#x0601`. The encoder returns
   it with the immediate word, joined as `#(1537 10)`.
6. `compose` serializes the result into a 16-bit vector and returns it.

`(interpret *assembler-prototype-m68k* nil #(1537 10))`, on a vector of type
`(unsigned-byte 16)`, is designed to reverse steps 4 and 5 through the decoder
and battery and return `((:addi :b 10 :d1))`. Even with the module loading,
this needs the reader protocol of SPECOPS-DRAFT-specops-O4.

### Project structure

The layout at `50e231d`, after the restructure of SPECOPS-DRAFT-restructure:

```text
specops.asd            systems specops (src/) and specops/demo.png (test/)
src/
  package.lisp         the specops package and its exports
  checksum.lisp        CRC-32 and Adler-32
  serial.lisp          serialization, defcodetable, masque and unmasque
  manifest/            the manifest layer: scan.lisp (stop rules and value
                       assemblers), manifest.lisp (defenum, defmanifest,
                       marshal, unmarshal)
  assembler.lisp       mqbase, operand and storage classes, assembler classes,
                       specification macros, assemble, interpret
  program.lisp         program model, build-program, emit-program protocol
  manifest-original/   a copy of the manifest layer for a coming stage of
                       work; not loaded by any system
mod/
  arch/                architecture modules specops.arch.<isa>:
                       6502 m68k s390x superh wasm x86 z80
  format/              format modules specops.format.<format>:
                       elf goff wasm
  map/                 map modules specops.map.<name>: cp437 ebcdic
test/                  manifest.lisp (PNG manifests and manifest suites);
                       program, ELF, GOFF and WebAssembly test scripts
doc/                   Compass documents
```

Description: The tree lists the repository's directories and the core's
files, with a one-line purpose for each. The core system `specops` is built
from the files in `src/`, in the order shown, except `manifest-original/`.
Every module is a subdirectory of `mod/arch/`, `mod/format/` or `mod/map/`
with its own `.asd` file, named for its system. Tests are in `test/`.
