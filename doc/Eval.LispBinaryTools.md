---
id:            SPECOPS-DRAFT-lisp-binary-tools
title:         Lisp Binary-Format Libraries Compared with the Manifest Layer
genre:         Eval
subtype:       prior-art
scope:         component
project:       SpecOps
component:     Manifest
language:      en
status:        Draft
provenance:
  assistant:   claude-opus-5-5 (OpenCode)
  session:     Analysis of Lisp binary-format libraries
relates-to:
  - SPECOPS-DRAFT-manifest
  - SPECOPS-DRAFT-manifest-complete
  - SPECOPS-DRAFT-specops
glossary:      SPECOPS-DRAFT-glossary
open-questions:
  - SPECOPS-DRAFT-lisp-binary-tools-O1
  - SPECOPS-DRAFT-lisp-binary-tools-O2
  - SPECOPS-DRAFT-lisp-binary-tools-O3
  - SPECOPS-DRAFT-lisp-binary-tools-O4
  - SPECOPS-DRAFT-lisp-binary-tools-O5
  - SPECOPS-DRAFT-lisp-binary-tools-O6
  - SPECOPS-DRAFT-lisp-binary-tools-O7
cites:
  - title:    lisp-binary (Jeremy Phelps), Quicklisp release lisp-binary-20221106-git
    locator:  binary-2.lisp (defbinary and its docstring), types.lisp (pointer, region-tag, eval, counted and terminated types), binary-1.lisp (enums, fixed-length strings), exif.lisp
    external: true
  - title:    binary-structures (Yukari Hafner), codeberg.org/shinmera/binary-structures
    locator:  commit 19ff73e; protocol.lisp (io-type classes, define-io-structure), octet-vector.lisp, stream.lisp, foreign-pointer.lisp, README.mess
    external: true
  - title:    Practical Common Lisp (Peter Seibel), chapter 24, Practical — Parsing Binary Files
    locator:  define-binary-class, define-binary-type
    external: true
  - title:    Binary-Types (Frode Vatvedt Fjeld), github.com/frodef/binary-types
    locator:  README
    external: true
  - title:    Gloss (Clojure byte-format library)
    locator:  frames, codecs, headers
    external: true
  - title:    Racket bitsyntax package
    locator:  port of Erlang bit syntax
    external: true
  - title:    Erlang Reference Manual
    locator:  Bit Syntax Expressions
    external: true
  - title:    Kaitai Struct Serialization Guide
    locator:  doc.kaitai.io/serialization.html — writing since v0.11, _check()
    external: true
  - title:    Data Format Description Language (DFDL) v1.0, Open Grid Forum
    locator:  dfdl:outputValueCalc; unparsing
    external: true
  - title:    "Nail: A Practical Tool for Parsing and Generating Data Formats (Bangert and Zeldovich, OSDI 2014)"
    locator:  dependent fields and transformations
    external: true
  - title:    Construct (Python declarative binary parsing and building library)
    locator:  construct.readthedocs.io — Rebuild, Checksum, Pointer, compile
    external: true
  - title:    binrw (Rust binary reading and writing library)
    locator:  docs.rs/binrw — count, calc and magic directives
    external: true
---

# Lisp Binary-Format Libraries: Prior-Art Evaluation

This evaluation compares SpecOps's manifest layer, the `defmanifest`,
`marshal` and `unmarshal` macros chronicled in
[SPECOPS-DRAFT-manifest](Log.Manifest.md) and planned in
[SPECOPS-DRAFT-manifest-complete](Plan.ManifestComplete.md), with the two
Common Lisp libraries closest to it: lisp-binary and binary-structures. It
asks what each does that the manifest layer does not, what the manifest layer
does that neither does, and what is worth taking from them. It places the
three among other Lisp tools and the wider field of binary-format systems, as
background only. The architecture it serves is
[SPECOPS-DRAFT-specops](Arch.SpecOps.md). The candidates it finds are recorded
as open questions, for the steward to accept into the plan or reject.

## Method

- **Sources.** lisp-binary was read in its Quicklisp release
  `lisp-binary-20221106-git`, whose files match those of its GitHub master by
  name; their contents were not compared. binary-structures was read from its
  Codeberg repository at commit `19ff73e` (2026-10-11), where it moved from
  GitHub in August 2025. The manifest layer was read at commit `cb931c8`, and
  its documents at `2c78df9`. All three were read at source level, together
  with lisp-binary's long `defbinary` docstring, its EXIF example, and
  binary-structures' README.
- **Nothing was run.** No library was loaded, no format was written in either
  library, and nothing was measured. Statements about generated code describe
  its shape as read from the expanders, not its speed. Statements about how a
  library would handle a scenario follow from its source, and some could be
  wrong in detail.
- **Unit of comparison.** Each library is scored against the rubric's
  criteria, then against the scenario's formats, and closes with what to take
  and what to leave.
- **Other systems.** The tools in "Other Lisp tools and the wider field" were
  not read at source level. They are described from documentation and general
  knowledge, checked against their documentation only where noted, and serve
  as context.
- **Licences.** lisp-binary is GPLv3, binary-structures zlib, SpecOps a
  BSD-style licence. Ideas may be taken from either library; code may be taken
  only from binary-structures, with its notice.

## The Shared Scenario

Each system is judged by how it would describe the records SpecOps writes, or
plans to:

- **S1, PNG.** A signature, then chunks up to IEND. Each chunk's length counts
  only its data, which follows a 4-byte type, and its CRC covers the type and
  the data. Ancillary chunks may appear in any number. This is the manifest
  layer's own test format.
- **S2, GOFF.** Fixed 80-byte records of EBCDIC text and big-endian integers,
  names padded to their fields, read one after another from a stream.
- **S3, ELF64.** A header whose `EI_DATA` byte sets the byte order of
  everything after it, so one description should serve big-endian s390x and
  little-endian x86-64; a section header table found through the header's
  `e_shoff`, and names found through offsets into a string table.
- **S4, WOFF.** A header and a table directory holding, for each table, its
  offset, its length and a checksum: the plan's next milestone
  (SPECOPS-DRAFT-manifest-complete-D6).
- **S5, wider units.** A record counted in 16-bit units, such as an M68K or
  s390x code segment, which SpecOps's assemblers produce.

## The Rubric

Table: The criteria each system is scored against.

| # | Criterion | Holds up when |
|---|---|---|
| G1 | Two directions from one description | Every field kind is both read and written |
| G2 | Logic kept out of the description | The description is data that can be analysed, with no arbitrary forms |
| G3 | Derived fields on write | Lengths, offsets, counts and checksums are computed, not supplied by the caller |
| G4 | Checked reads | Checksums, lengths and constants are checked, with a way to go on past a failure |
| G5 | Variable structure | Tag dispatch, repetition by count or condition, terminated and self-delimiting fields |
| G6 | Layout out of order | Data reached through stored offsets can be read, and written with the offsets backpatched |
| G7 | Byte order and units | Byte order per field, or chosen at run time; units other than octets |
| G8 | Value kinds | Integers of any width, floats, enums, bit fields, encoded text |
| G9 | Generated code | Specialised and inlined at expansion, with typed primitives and few checks per record |
| G10 | Sources and destinations | Vectors, streams and other storage, with overrun reported |
| G11 | In-memory form and round trips | Records are easy to build and edit, and a read record writes back the same bytes |
| G12 | Footprint | Few dependencies, a permissive licence, portable Common Lisp |

## lisp-binary

`defbinary` declares a record as a list of fields, each with a name, a default
and a type, and expands into a `defstruct` with `read-binary` and
`write-binary` methods on streams. Types are Common Lisp types such as
`(unsigned-byte 32)`, named records, and virtual types such as
`counted-string`, `terminated-buffer`, `magic`, `pointer` and `bit-field`.
Expressions anywhere in a type, such as an array's length, are evaluated with
the earlier fields bound.

- **G1, two directions:** strong. Every type has a reader and a writer.
- **G2, logic:** weak by design. Lengths are expressions, an `eval` type
  computes a field's type when the code runs, and `custom` fields take reader
  and writer functions. An `eval` whose form is a `case` of constant types is
  expanded into code at expansion; any other is expanded and `eval`led when
  the code runs.
- **G3, derived fields:** partial. A `counted-array`, `counted-string` or
  `counted-buffer` writes its own length prefix, and a `pointer` writes a
  placeholder that a later `region-tag` field backpatches. A length elsewhere
  than immediately before its data, and any checksum, is the caller's to
  compute.
- **G4, checked reads:** magic values only, signalling `bad-magic-value` with
  a `continue` restart. A code an enum lacks signals `bad-enum-value`.
- **G5, variable structure:** strong, through logic. Counted and terminated
  strings and buffers, arrays whose length is any expression, and `eval` for
  tag dispatch. A terminator is one integer of a given width written in the
  field's byte order, so CR LF is one 2-byte terminator. There is no
  repetition until a condition except by `eval` or `custom`.
- **G6, layout out of order:** the strongest of the three. A `pointer` field
  reads the data at a stored offset, absolute or relative to a
  `base-pointer` field. On write it queues the data under a `region-tag`; the
  field of that tag writes the queued data and backpatches the pointers. The
  stream must be open for both directions, and pointers may point only
  forward.
- **G7, byte order and units:** byte order per field or per record, or
  `:dynamic`, read from the special variable `*byte-order*` when the code
  runs, which a field read early can set, as the EXIF example does for TIFF's
  `II`/`MM` mark. Fields of any bit width are packed into whole bytes, or read
  through a bit stream when they cannot be. The unit of storage is the octet.
- **G8, value kinds:** the widest. Signed integers in two's or one's
  complement; IEEE floats from half to octuple precision, through CFFI for
  single and double; enums; bit fields; text in any flexi-streams external
  format, which has no EBCDIC.
- **G9, generated code:** reads and writes call generic primitives such as
  `read-integer` with keyword arguments, and a nested record is read through
  the generic function `read-binary`, not inlined.
- **G10, sources and destinations:** streams, with in-memory streams for
  vectors. Alignment and pointers use `file-position`.
- **G11, in-memory form:** always a struct. `read-binary` returns the struct
  and the number of bytes read. A `fixed-length-string` too long for its field
  signals `input-string-too-long`, with a `truncate` restart, and a short one
  is padded with a padding character; as read from the source, the reader
  returns the whole field decoded, padding included.
- **G12, footprint:** GPLv3; depends on alexandria, closer-mop, moptilities,
  flexi-streams, quasiquote-2.0 and, unless a feature excludes it, CFFI.

Against the scenario:

- **S1:** a chunk's type sits between its length and its data, so the length
  cannot be a `counted-buffer` prefix; the caller sets it, and computes the
  CRC by hand or in a `custom` writer. The chunk sequence up to IEND needs
  `eval` or `custom` code.
- **S2:** fixed records read in sequence from a stream suit it, but EBCDIC
  would need a `custom` type, since flexi-streams lacks it.
- **S3:** `:dynamic` byte order and pointers with a base cover ELF's needs,
  and are what the EXIF example uses for TIFF.
- **S4:** pointers and region tags can write the tables after the directory
  and backpatch their offsets; lengths and checksums are the caller's.
- **S5:** no; the unit is the octet.

**Steal:** pointers and region tags as the declarative model for deferred
data; byte order chosen when the code runs; the `truncate` restart for text
too long for its field; floats of every IEEE width; EXIF as a test format.
**Reject:** `eval` and `custom` types, expressions in lengths, the special
variable for byte order, flexi-streams, and a mandatory struct.

## binary-structures

`define-io-structure` declares a record as fields of named I/O types, and
expands into a `defstruct`, an I/O type registered at compile time, and
reader and writer functions for each storage backend: streams, octet vectors
and foreign pointers. Types are classes with generic functions that return
code (`read-form`, `write-form`, `octet-size`), so a backend or a type is
added with methods.

- **G1, two directions:** strong for what it covers.
- **G2, logic:** a list that names no I/O type is taken as a Lisp form, and
  `(slot name)` reads an earlier field, so counts and `case` keys can be any
  expression.
- **G3, derived fields:** a vector whose count is a type writes its own count
  prefix from the value's length. Nothing else is computed.
- **G4, checked reads:** magic strings and constants, by `assert`. A `case`
  value outside its branches signals `unknown-value`.
- **G5, variable structure:** `case` reads a tag and chooses a type, or a
  constant, which serves as an enum; `typecase` dispatches on a form. A
  vector's count is a number, a type read as a prefix, a form, or, for a
  bounds-checked backend, the rest of the storage divided by a fixed element
  size. Strings are null-terminated or of fixed size. There is no repetition
  until a condition and no other terminator.
- **G6, layout out of order:** a field may be placed at a fixed `:offset`
  within its structure, with `:align` and `:pad`. Stored offsets are not
  followed.
- **G7, byte order and units:** byte order per type (`uint32-be`), fixed at
  expansion. The unit is the octet, and all data must be octet-aligned, as the
  README states.
- **G8, value kinds:** integers of 1 to 16 octets, booleans, timestamps with
  an epoch and a resolution, and text in any babel encoding, which includes
  EBCDIC 037. Floats of 32 and 64 bits on streams and vectors; the 16- and
  128-bit float types are handled only by the foreign-pointer backend.
- **G9, generated code:** the best of the three. Each backend's methods emit
  inline calls to nibbles' typed accessors inside functions with declared
  types. The backend tracks the offset at expansion, so fields at known
  offsets need no seeking, and a bounds-checked backend checks the space for a
  structure's leading fixed-size fields once. Nested structures are inlined by
  default (`*io-structure-inline*`). `describe-object` prints a structure's
  layout: address, size, type and name of each field.
- **G10, sources and destinations:** streams, pathnames, octet vectors with
  start and end, and foreign memory, with backends open to extension;
  `end-of-storage` reports overruns. A call dispatches on the storage's type.
- **G11, in-memory form:** always a struct, with `:include` for structures
  that share a prefix. On write, a `case` field chooses its tag from the
  value's Lisp type.
- **G12, footprint:** zlib; depends on CFFI (except on Mezzano), babel,
  nibbles, form-fiddle, trivial-extensible-sequences and
  documentation-utils.

Against the scenario:

- **S1:** the length and CRC are the caller's, and no repetition reads
  chunks up to IEND.
- **S2:** babel gives EBCDIC; as read from the source, a fixed-size string is
  written as its encoded octets with no padding to the field's size.
- **S3:** byte order fixed at expansion, and no following of `e_shoff`.
- **S4:** offsets, lengths, counts and checksums are the caller's.
- **S5:** no; octet-aligned only.

**Steal:** code specialised by backend and by statically known offset, with
typed primitives and bounds checked once per run of fixed fields; backends as
a protocol; the size of a value before writing; a printed layout table.
**Reject:** forms in the description, choosing a `case` tag from the value's
Lisp type, a mandatory struct, and its dependencies.

## Other Lisp tools and the wider field

These tools were not examined at source level; they place the two libraries.

- **Older Lisp work.** Peter Seibel's `define-binary-class` in *Practical
  Common Lisp* is the teaching example of the idea: a macro generating
  `read-value` and `write-value` methods. Frode Fjeld's Binary-Types is the
  ancestor lisp-binary names. Gloss, in Clojure, builds two-way codecs from
  frames, and Racket's bitsyntax ports Erlang's bit syntax.
- **Outside Lisp.** Systems of this kind take one of four routes. A separate
  description language compiled to code: Kaitai Struct, which writes as well
  as reads only since v0.11, and has the caller set even constant fields before
  `_check()` and writing; DFDL, whose `dfdl:outputValueCalc` computes fields on
  write; and
  Nail, whose dependent fields compute lengths, offsets and checksums on
  write. Run-time combinators: Construct in Python, whose `Rebuild` and
  `Checksum` compute fields on write. Macros or their equivalents in other
  languages, such as Rust's binrw. Bit syntax built into a language, as in
  Erlang.
- **What the Lisp libraries share.** Like binrw, both libraries define a
  format as a type with code attached, and let host-language expressions into
  the description. The manifest layer is closer in spirit to Nail and DFDL:
  derived fields from a fixed vocabulary, no expressions. Unlike them, it is
  compiled in the Lisp image by macros, with no separate language.

## Comparison

Table: The manifest layer at `cb931c8` against the two libraries, criterion by criterion.

| # | Manifest layer | lisp-binary | binary-structures |
|---|---|---|---|
| G1 | Every field kind both ways | Every type both ways | Every type both ways |
| G2 | None (SPECOPS-DRAFT-manifest-D2) | Expressions, `eval`, `custom` | Forms, `(slot name)` |
| G3 | `:length-of`, `:offset-of`, `:count-of`, `:checksum-of` over spans, backpatched | Count prefixes; pointers | Count prefixes |
| G4 | All slots and constants, with `continue` and `use-value` (SPECOPS-DRAFT-manifest-D42) | Magic, with `continue` | Magic, by `assert` |
| G5 | `:case` over types and sub-manifests; repeats by count, field, `:until`, end, length; terminators of several elements; tar-style buffers; LEB128; user stop rules | Counted, terminated and fixed text; arrays by expression; `eval` | `case`, `typecase`; vectors by count, form or rest of storage; null-terminated text |
| G6 | `:offset-of` from the record's start; following offsets deferred | Pointers, base pointers, region tags | Fixed offsets within a structure |
| G7 | Per field or manifest, fixed at expansion; swap modes; any unit breadth | Per field, or `:dynamic`; sub-byte fields and bit streams | Per type; octets only |
| G8 | Integers, enums, masques, codecs; floats planned | Integers, one's complement, floats half to octuple, enums, bit fields, flexi-streams text | Integers, floats (32, 64), booleans, timestamps, babel text |
| G9 | Specialised structure and inlined sub-manifests; primitives through closures | Generic primitives; nested records by generic call | Typed, inlined primitives; static offsets; one check per run; inlined nesting |
| G10 | Vectors, streams, functions out, buffered when slots need it; vectors, integers, streams in; `manifest-overrun` | Streams | Streams, vectors, foreign pointers; extensible; `end-of-storage` |
| G11 | Plists, values, or a record class on request; run-time records; canonical round trips | Struct always | Struct always, with `:include` |
| G12 | No dependencies; BSD-style | Six dependencies; GPLv3 | Six dependencies; zlib |

## Synthesis

1. **Derived fields and checked reads are where the manifest layer stands
   apart.** Neither library can place a length before a region of several
   fields, compute a checksum, or count elements it has not just written, and
   neither checks more than magic values on read. Every PNG chunk shows the
   difference: its length and CRC are the caller's work in both libraries.
   Repair on read (`use-value` with the recomputed checksum) has no
   counterpart in either.
2. **Keeping logic out pays for analysis.** Both libraries let forms into
   descriptions; the manifest layer refuses them (SPECOPS-DRAFT-manifest-D2),
   and the analyses built since rely on it: `simplify-plan`, the region table
   (SPECOPS-DRAFT-manifest-D43), and selective reads that pass over fields
   through skip effectors (SPECOPS-DRAFT-manifest-D53,
   SPECOPS-DRAFT-manifest-D54). Neither library could compute what a key
   depends on.
3. **The variable-length model is broader.** Terminators of several elements
   matched as patterns, fixed buffers ended by a terminator, LEB128 scalars and
   vectors, repeats until a condition, and user stop rules that move from
   run-time calls to inline code have no counterpart in binary-structures, and
   only partial ones in lisp-binary, through expressions.
4. **Units other than octets are unique to it.** Neither library can
   describe S5. This matters to SpecOps more than to either library's users.
5. **Optional records fit an image that changes.** Both libraries always
   define a struct, which SBCL cannot redefine with other slots; the manifest
   layer defines a class only on request (SPECOPS-DRAFT-manifest-D32).
6. **lisp-binary leads on layout out of order and on byte order.** Pointers,
   base pointers and region tags, and byte order chosen when the code runs,
   are what S3 needs, and the manifest layer has neither yet.
7. **binary-structures leads on generated code.** Its primitives are typed and
   inlined, offsets are tracked at expansion, and bounds are checked once per
   run of fixed fields. The manifest layer specialises the structure of a
   record but writes each unit through an `enter` closure and a
   `serializer-for` closure (`src/manifest/manifest.lisp:marshal@cb931c8`,
   `src/serial.lisp:serializer-for@cb931c8`), and checks a vector written in
   place unit by unit (SPECOPS-DRAFT-manifest-D48). How much this costs has
   not been measured.
8. **Inlining nested descriptions is not unique to the manifest layer.**
   binary-structures also keeps a compile-time registry and inlines nested
   structures. What the manifest layer adds is analysis across the nesting:
   regions, slots, selections and byte orders gathered by one walker
   (`src/manifest/manifest.lisp:walk-manifest@cb931c8`).
9. **Footprint.** The manifest layer depends on nothing, under a permissive
   licence; lisp-binary's GPLv3 rules out taking its code.

The candidates worth taking are the open questions below. Of the plan's
remaining milestones, WOFF stands to gain from the offset bases of
SPECOPS-DRAFT-lisp-binary-tools-O1, floats from
SPECOPS-DRAFT-lisp-binary-tools-O7, and the deferred following of offsets from
SPECOPS-DRAFT-lisp-binary-tools-O1 and SPECOPS-DRAFT-lisp-binary-tools-O2.

## Open Questions

### SPECOPS-DRAFT-lisp-binary-tools-O1 — Should following offsets adopt pointers, region tags and offset bases?

Following offsets on read is deferred to a later plan (the Scope and
Non-Goals of SPECOPS-DRAFT-manifest-complete). lisp-binary's model is
declarative and fits the manifest layer's backpatching: a pointer field
writes a placeholder and queues its data under a named region; the region,
where it is declared, writes the queued data and the pointers are patched; a
base field makes offsets relative. An `:offset-of` slot today gives a region's
position from the start of the top-level record, with no other base, while
OpenType and TIFF offsets count from a table's start. Should the deferred
work take this shape, with a base for `:offset-of`, and should lisp-binary's
EXIF example, TIFF with byte order and offsets both chosen by the data, be its
test format?

Answered in part in [SPECOPS-DRAFT-manifest](Log.Manifest.md) by
SPECOPS-DRAFT-manifest-D56 and SPECOPS-DRAFT-manifest-D57: placed fields take
the shape of pointers and region tags, written where a `place` directive
stands and read at their `:offset-of` slots' values, for WOFF. Offsets from a
base other than the top-level record's start, structures followed by offset
without being placed, and EXIF as a test format remain open.

### SPECOPS-DRAFT-lisp-binary-tools-O2 — Should byte order be selectable by an earlier field?

ELF's `EI_DATA` and TIFF's `II`/`MM` set the byte order of what follows, so
one ELF description could serve s390x and x86-64 (S3). lisp-binary reads it
from a special variable when the code runs. The manifest way would be a byte
order selected by an earlier field, compiled into both branches as `:case`
fields are (SPECOPS-DRAFT-manifest-D19, SPECOPS-DRAFT-manifest-D20). Should
the manifest layer offer it, and how does it bear on the architecture's
question of who owns byte order (SPECOPS-DRAFT-specops-O3)?

### SPECOPS-DRAFT-lisp-binary-tools-O3 — Should generated code use typed primitives and checks hoisted over fixed runs?

binary-structures emits typed, inlined accessors, tracks the offset at
expansion, and checks bounds once for a run of fixed-size fields. The manifest
layer already knows fixed extents (`src/manifest/scan.lisp:known-extent@cb931c8`)
but writes each unit through closures and checks each unit written in place.
Should `marshal` and `unmarshal` specialise their primitives by destination
and source type, and is a benchmark against binary-structures needed first to
show the gain?

### SPECOPS-DRAFT-lisp-binary-tools-O4 — Should sources and destinations be a protocol?

The manifest layer chooses among vectors, streams and functions in fixed
code inside `marshal` and `unmarshal`; binary-structures defines backends as
classes with code-returning methods, foreign memory among them. A protocol
would let a module add storage without changing the macros, and a backend for
raw memory would let an assembler write code straight into executable memory.
That backend would depend on CFFI or on implementation internals, so under
SPECOPS-DRAFT-specops-D1 it would live behind a conditional or outside the
core. Should destinations and sources become a protocol, and how would it
meet the reader protocol that disassembly needs (SPECOPS-DRAFT-specops-O4)?

### SPECOPS-DRAFT-lisp-binary-tools-O5 — Should a record's size be computable before it is written?

binary-structures generates the size of a value without writing it. The
manifest layer buffers a record with slots and copies it out
(SPECOPS-DRAFT-manifest-D46). A size computed first would allow writing
directly to a stream or function with the size known, allocating a vector of
exactly the right size, and placing records whose offsets depend on the sizes
of those before them, as ELF's sections do. Is it worth generating, given
that buffering already works?

### SPECOPS-DRAFT-lisp-binary-tools-O6 — Should a manifest's layout be printable?

binary-structures' `describe-object` prints each field's address, size, type
and name. The region table (SPECOPS-DRAFT-manifest-D43) holds most of what a
manifest's table would need, with variable-length fields marked as such. A
table to compare with a format's manual would help format authors, and is
cheap. Should the manifest layer offer a function that prints one for a named
manifest?

### SPECOPS-DRAFT-lisp-binary-tools-O7 — Should text that does not fit, and floats, follow lisp-binary?

Two of the plan's open items have answers in lisp-binary. For a fixed-length
field given text of another length (SPECOPS-DRAFT-manifest-complete-O5), it
signals a condition with a `truncate` restart for text too long and pads
short text with a declared character, though its reader keeps the padding;
a round trip in the manifest layer would need it stripped on read. For the planned
`:f` class, it covers IEEE half to octuple precision, encoding the widest
arithmetically and decoding them to rationals. Should the manifest layer take
both: a restart in the style of SPECOPS-DRAFT-manifest-D42, and every IEEE
width with a portable arithmetic path and implementation fast paths behind
conditionals?

Answered for floats in [SPECOPS-DRAFT-manifest](Log.Manifest.md) by
SPECOPS-DRAFT-manifest-D64 and SPECOPS-DRAFT-manifest-D65: IEEE floats of 16,
32, 64 and 128 bits, with a portable arithmetic path and SBCL's floats behind a
conditional; octuple precision was not taken. Text that does not fit its
field, SPECOPS-DRAFT-manifest-complete-O5, remains open.

## Appendix — Source Map

Table: Sections of this evaluation and the sources each draws on.

| Section | Primary sources |
|---|---|
| lisp-binary | lisp-binary 20221106: `binary-2.lisp` (`defbinary` and its docstring), `types.lisp`, `binary-1.lisp`, `exif.lisp`, `lisp-binary.asd` |
| binary-structures | binary-structures at `19ff73e`: `protocol.lisp`, `octet-vector.lisp`, `stream.lisp`, `foreign-pointer.lisp`, `standard-types.lisp`, `README.mess`, `binary-structures.asd`; babel's EBCDIC encodings |
| Other Lisp tools and the wider field | Practical Common Lisp ch. 24; Binary-Types; Gloss; Racket bitsyntax; Erlang bit syntax; Kaitai Struct Serialization Guide; DFDL; Nail; Construct; binrw |
| Comparison and Synthesis | The above, and the manifest layer at `cb931c8` with [SPECOPS-DRAFT-manifest](Log.Manifest.md) and [SPECOPS-DRAFT-manifest-complete](Plan.ManifestComplete.md) at `2c78df9` |
