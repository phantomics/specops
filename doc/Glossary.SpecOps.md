---
id:            SPECOPS-DRAFT-glossary
title:         SpecOps Terms
genre:         Glossary
scope:         project
project:       SpecOps
language:      en
status:        Draft
provenance:
  assistant:   claude-opus (OpenCode)
  session:     Compass adoption
relates-to:
  - SPECOPS-DRAFT-specops
  - SPECOPS-DRAFT-manifest
  - SPECOPS-DRAFT-restructure
---

# SpecOps: Glossary

This glossary fixes the vocabulary of the SpecOps assembler framework for the
SpecOps corpus: the terms used for assemblers and their tables, instruction
specifications, operands and storage, encoding and disassembly, programs and
object formats, the manifest layer for binary records, and the kinds of module
the repository is divided into. Other documents name it in their `glossary:`
field. The Notes column names the Lisp symbol behind a term, pinned to commit
`1e53ae7` where it is a definition, or as a bare accessor name; terms added
later are pinned to the commit of their source. Terms are grouped by subsystem
rather than sorted alphabetically.

## Canonical Terms

Table: Assemblers, specifications and operands, with their definitions and
sources.

| Term | Definition | Notes / Source |
|---|---|---|
| assembler | A CLOS instance that holds everything SpecOps knows about one instruction set: its lexicon, its disassembly tables, its storage and program model, its unit breadth and its execution modes. | `specops.lisp:assembler@1e53ae7` |
| assembler prototype | The assembler instance for an architecture, bound to a special variable named `*assembler-prototype-<arch>*`. Specification macros and `assemble` take the variable's *symbol* and read its value at macroexpansion time to choose methods. | e.g. `m68k/base.lisp:*assembler-prototype-m68k*@1e53ae7` |
| encoding assembler | An assembler (`assembler-encoding`) whose instructions can mostly be disassembled by looking up a whole encoded value in a decoder. | `specops.lisp:assembler-encoding@1e53ae7`; Z80, 6502 |
| masking assembler | An assembler (`assembler-masking`) whose instructions are disassembled by testing encoded values against bit masks in a battery. | `specops.lisp:assembler-masking@1e53ae7`; System Z |
| breadth | The width in bits of one output code unit for an assembler; the element type of the vectors `assemble` returns. | `asm-breadth`, default 16 |
| lexicon | The table mapping each mnemonic keyword to its encoder: a constant code, or a function of the program API and the operands. | `specops.lisp:of-lexicon@1e53ae7` |
| decoder | The table mapping a whole encoded value (with any prefix) to its disassembled form, or to a function that completes it. | `specops.lisp:of-decoder@1e53ae7` |
| battery | The table of unmasker functions tried in turn against an encoded value; each returns a disassembled form or `nil`. Also *test battery*. | `specops.lisp:of-battery@1e53ae7` |
| duplex | Describes a specification from which both the encoder and the decoder are generated, as with `mqbase` formats and `:duplex` operation tables. | README "Duplex Operation" |
| specop | One instruction specification, which registers an encoder in the lexicon: a `specops` form, a wrapper around it such as `specop-68k` or `specops-sh`, or a System Z `specop-z` line pairing a mnemonic with a format and an opcode. | `specops.lisp:specops@1e53ae7`, `system-z/base.lisp:specop-z@1e53ae7` |
| readop | One disassembler specification, made with `readops` or a wrapper such as `readops-sh`, which registers a decoder entry or a battery unmasker. | `specops.lisp:readops@1e53ae7` |
| operation table | A `specops` form with `(:tabular :cross-adding …)` that lays out a whole opcode page as a grid; each cell's code is its row plus its column plus the table's prefix. | Z80, 6502, WebAssembly |
| shadow form | A Z80 instruction derived automatically from a main-table instruction by substituting `IX` or `IY` for `HL` under the `#xDD` or `#xFD` prefix. | `z80/base.lisp:extend-clauses@1e53ae7` |
| combined mnemonic | A family of mnemonics generated from one specop by `:combine`, such as the condition-coded branch variants of an instruction. | `specify-ops` `:combine` |
| `determine` | A form in an instruction body that checks each operand against its permitted types and binds the derived encodings. It expands into `determine-in-context` with the architecture's qualify, derive and verbalize functions, and signals an error that describes the instruction's valid operands. | `specops.lisp:determine-in-context@1e53ae7` |
| operand type | A keyword or form naming an acceptable kind of operand (`gpr`, `(imm 16)`, `mas-bd12`), interpreted by an architecture's qualify function. | |
| ISA type | A variant or extension tag of an instruction set (`:sh2a`, `:sh4`). A specop's `:for-types` limits it to assemblers carrying one of its tags. | `asm-extns` |
| execution mode | A processor mode that changes encodings, such as x86's 64-bit long mode `:l64`. The first of an assembler's modes is the default. | `asm-exmodes` |
| storage | An assembler's register names grouped by type keyword, used to resolve register operands to indices. | `specops.lisp:of-storage@1e53ae7` |
| program model | The register files an assembler makes available to programs, from which the register pool is built. | `asm-pmodel` |
| register file | A named set of registers of one kind; reduced, displaced and blocked variants describe derived or reserved files. | `specops.lisp:register-file@1e53ae7` |
| register pool | The registers available for symbolic allocation, excluding those in blocked files. Built per instance; not yet consulted by `locate` at `1e53ae7`. | `asm-pool` |
| store | The `:store` parameter of `assemble` or `program`, which binds symbols to free registers of a type, optionally constrained with `:bind`. | `specops.lisp:locate@1e53ae7` |
| memory access scheme (MAS) | An object describing one way an instruction addresses memory, built from the mixins `mas-based`, `mas-indexed`, `mas-displaced`, `mas-scaling-displaced` and `mas-absolute`. | `specops.lisp:memory-access-scheme@1e53ae7` |
| `@` function | The convention for constructing memory access schemes: each architecture package defines `@` and variants (`@+`, `-@`, `@%`, `@/` …), and disassemblers emit `(@ …)` forms. | e.g. `system-z/base.lisp:@@1e53ae7` |
| program API | The closure passed to every encoder and decoder as `program-api` and called through the local function `of-program`. It answers queries such as `:exmode`, `:label` and `:read`. | `asm-program-api`, `asm-program-api-access` |

Table: Encoding, serialization and disassembly, with their definitions and
sources.

| Term | Definition | Notes / Source |
|---|---|---|
| mask string | The bit-field notation used by `masque` and its relatives, such as `"00000110.SSMMMXXX"`. Digits are constant bits, letters name fields, `.` is a spacer, and an optional `b:`, `o:`, `d:` or `h:` prefix selects the radix. | `specops.lisp:quantify-mask-string@1e53ae7` |
| masque | The macro that composes an integer from a mask string and field values at compile time. | `specops.lisp:masque@1e53ae7` |
| unmasque | The inverse macro: it tests an integer against a mask string's constant bits and binds its fields, returning `nil` on mismatch. | `specops.lisp:unmasque@1e53ae7` |
| mqbase | A currying macro that defines an instruction *format* as a macro producing a paired encoder (from `masque`) and decoder (from `unmasque`). | `specops.lisp:mqbase@1e53ae7` |
| instruction format | A named bit layout shared by many instructions, together with the meaning of its fields, as in System Z's `zformat-rr` or `zformat-rx-a`. | `system-z/base.lisp:specop-z@1e53ae7` |
| width pair | A cons `(width . value)` asking a serializer to write `value` in exactly `width` units, keeping leading zeros. Produced by x86's `w`. | |
| serialize | Splitting each encoder result (an integer, width pair or vector) into breadth-wide units and appending them to the output. Its callers drop `nil` parts first. | `specops.lisp:serialize@1e53ae7`, `serializer-for` |
| swap mode | The segment-reversal applied for byte order: 0 none, 1 bytes, 2 16-bit words (PDP-11 middle-endian). | `specops.lisp:swap-segments@1e53ae7` |
| interpret | Disassembly: reading a code vector and returning the list of instruction forms it encodes. | `specops.lisp:interpret@1e53ae7` |
| disassembly segment | For masking assemblers, the list of instruction lengths (in units) tried when reading. Not related to a program *segment*. | `asm-msk-segment` |

Table: Programs, object formats and the manifest layer, with their definitions
and sources.

| Term | Definition | Notes / Source |
|---|---|---|
| program | A structured, unassembled representation of an assembly program (units, segments, symbols, relocations), built with the `program` macro and serializable to several object formats. | `program.lisp:program@1e53ae7` |
| program unit | A separately named part of a program, corresponding to a GOFF section definition. | `program.lisp:program-unit@1e53ae7` |
| segment | A program section of one kind (`:code`, `:data`, `:rodata`, `:bss`) holding items, corresponding to an ELF or WebAssembly section or a GOFF element. | `program.lisp:segment@1e53ae7` |
| item | One element of a segment: instructions, raw bytes, label definitions and references, alignment, functions and data words. | `program.lisp:segment-item@1e53ae7` |
| relocation | A record that a field at an offset must be patched with a symbol's address by a linker, of an architecture-specific type. | `program.lisp:relocation@1e53ae7` |
| emit | Writing an assembled program in an object format, selected by a keyword (`:elf`, `:goff`, `:wasm`). | `program.lisp:emit-program@1e53ae7` |
| code table | A character-to-code mapping, defined with `defcodetable`, that works in both directions; used as a string codec. Code tables are packaged as map modules. | `specops.lisp:defcodetable@1e53ae7`; CP437, EBCDIC |
| codec | A per-character function used by a manifest string: a character to its code, and a code to its character. | Log SPECOPS-DRAFT-manifest-D6 |
| manifest | A declarative description of a binary record, made with `defmanifest`, from which `marshal` and `unmarshal` generate a writer and a reader. | [Manifest log](Log.Manifest.md) |
| field | A named, typed clause of a manifest that takes or yields a value. | |
| directive | A manifest clause led by a SpecOps symbol (`masque`, `pad`, `str`, `span`, `manifest`) that describes structure rather than a value. | Log SPECOPS-DRAFT-manifest-D4 |
| span | A directive that wraps fields in a named region, so that a length, offset or checksum can describe it. | Log SPECOPS-DRAFT-manifest-D8 |
| slot | A field whose value is computed from a region: `:length-of`, `:offset-of` or `:checksum-of`. | Log SPECOPS-DRAFT-manifest-D8 |
| sub-manifest | A manifest included in another with the `manifest` directive and inlined into its parent's code. | Log SPECOPS-DRAFT-manifest-D10 |
| sub-lexicon | The parent's generated bindings, passed to an inlined sub-manifest so that it shares the cursor, region map and serializers. | Log SPECOPS-DRAFT-manifest-D10 |
| enum | A keyword-to-integer map defined with `defenum` and applied to a manifest field. | `specops.lisp:defenum@1e53ae7` |
| marshal / unmarshal | Writing values into a manifest's structure, and reading them back out. | `specops.lisp:marshal@1e53ae7`, `specops.lisp:unmarshal@1e53ae7` |

Table: The repository's core and kinds of module, with their definitions and
sources.

| Term | Definition | Notes / Source |
|---|---|---|
| core | The system `specops`, built from `src/`: the package, checksums, serialization, the manifest layer, the assembler classes and specification macros, and the program model. Every module depends on it. | `specops.asd:specops@13b5887` |
| module | An ASDF system outside the core, in `mod/<kind>/<name>/`, named `specops.<kind>.<name>`, with a package of the same name. Its kind is architecture, format or map. | Log SPECOPS-DRAFT-restructure-D1, SPECOPS-DRAFT-restructure-D2 |
| architecture module | A module that implements one instruction set: its assembler class and prototype, operand support, and instruction specifications. It is named for the instruction set, not a machine (`s390x`, not System Z), and never depends on a format module. | Log SPECOPS-DRAFT-restructure-D3; `mod/arch/s390x/package.lisp:specops.arch.s390x@ada2d19` |
| format module | A module that writes one object or container format for programs, through `emit-program` methods. It depends on an architecture module only when the format exists for that one instruction set, as WebAssembly's does. Image and other media formats are not format modules. | Log SPECOPS-DRAFT-restructure-D4; `mod/format/goff/goff.lisp:emit-program@ada2d19` |
| map module | A module that holds a two-way mapping between value spaces belonging to no single architecture or format, such as a code table. It depends only on the core. | Log SPECOPS-DRAFT-restructure-D5; `mod/map/ebcdic/ebcdic.lisp:ebcdic-code-cp037@ada2d19` |
| opcode map | A manufacturer's grid of an instruction set's opcodes, transcribed in SpecOps as an operation table. It belongs to its architecture module, not to a map module. | Z80, 6502 |

## Backronym Registry

Table: Backronyms and their expansions.

| Acronym | Expansion | Gloss |
|---|---|---|
| SpecOps | To Specify Operations | The framework's purpose: specifying the operations of instruction sets in Lisp |
