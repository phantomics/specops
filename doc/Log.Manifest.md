---
id:            SPECOPS-DRAFT-manifest
title:         Manifest Development Log — defmanifest, marshal and unmarshal
genre:         Log
scope:         component
project:       SpecOps
component:     Manifest
language:      en
status:        Draft
provenance:
  assistant:   claude-opus (OpenCode)
  session:     Marshal
decisions:
  - SPECOPS-D1
  - SPECOPS-D2
  - SPECOPS-D3
  - SPECOPS-D4
  - SPECOPS-D5
  - SPECOPS-D6
  - SPECOPS-D7
  - SPECOPS-D8
  - SPECOPS-D9
  - SPECOPS-D10
  - SPECOPS-D11
  - SPECOPS-D12
  - SPECOPS-D13
  - SPECOPS-D14
cites:
  - title:    lisp-binary (Common Lisp binary format library) wiki
    locator:  github.com/j3pic/lisp-binary/wiki — DEFBINARY structs; EVAL type specifier
    external: true
  - title:    PNG (Portable Network Graphics) Specification
    locator:  chunk layout, CRC-32 over chunk type and data
    external: true
  - title:    WOFF File Format
    locator:  table directory (offset tables)
    external: true
---

# Manifest: Development Log

This document chronicles the design and construction of SpecOps's declarative
binary-format layer: `defmanifest`, which declares the structure of a binary
record, `marshal`, which writes values into that structure, and `unmarshal`,
which reads them back out. It covers the path from a freeform `marshal` macro to
manifest-driven writing and reading, and the PNG test format used to exercise
it. The chronicled state is commit `10b1568`.

## Problem

SpecOps assembles instructions by composing integers into vectors through
`serialize` and `masque`, but the object-file and container formats around that
code (GOFF, WebAssembly modules, PNG-style chunked files) were being written
imperatively: long sequences of `write-u16-be`/`write-u32-le` calls, or
offset-addressed pokes into a zeroed record buffer (`%goff-set-u32 rec 8 …`).
That code was verbose, error-prone about offsets, could not be read back without
a second hand-written parser, and had no way to express data whose layout
depends on its own contents (length prefixes, checksums, offset tables).

The goal was a single declarative description of a record from which both a
writer and a reader are generated, keeping logic out of the description, and
fitting SpecOps's existing idioms (`masque` bit fields, macro-time code
generation).

## Starting point

The first version was a freeform `marshal` macro: a destination, a parameter
list and a sequence of items (integers, `(width . value)` pairs, vectors,
`(masque …)` forms, `(:pad …)` directives) serialized in order through
`serializer-for`. Rewriting `%goff-write-end` with it exposed the weaknesses of
a purely sequential writer: fixed-offset gaps were silently lost, `nil` values
collided with the `(width . value)` syntax, and there was no path to reading,
backpatching or variable-length data. A review of the prior-art library
lisp-binary (see `cites:`) confirmed the feature set a format layer needs
(length-prefixed and terminated fields, enums, bit fields, runtime type
selection, pointer resolution) and highlighted the one thing a data-driven
manifest could do that lisp-binary cannot: generate selective readers that skip
to requested fields.

## Design Decisions

Decision identifiers are provisional; final numbers come from the namespace
registry at acceptance.

### SPECOPS-D1 — Manifest-driven marshal replaces the freeform item list

**Status:** Accepted
**Context:** The freeform `marshal` serialized an ad hoc item list. It could
not be inverted into a reader and every call site restated the layout.
**Decision:** A named manifest declares the layout once; `(marshal name
destination :key value …)` supplies values for its named fields. The freeform
mode was removed rather than reconciled.
**Alternatives:** Keeping freeform `marshal` alongside a manifest mode
(rejected: two vocabularies for one job); a struct-only interface (deferred;
keyword input came first).

### SPECOPS-D2 — Manifests describe structure only; logic stays at the call site

**Status:** Accepted
**Context:** Early drafts added `:compute` forms, binding blocks and inverse
`:unmasque` expressions to fields. Each made the read direction harder, since
arbitrary write-side expressions cannot be inverted.
**Decision:** A manifest contains no computation. Conditionals such as
`(if entry-esdid 1 0)` are written in the `marshal` call. Every masque group is
either a static value or a direct keyword input, so reading is always direct.
**Alternatives:** `:compute`/`:default` derivations in the manifest (rejected);
binding blocks with explicit inverse clauses (rejected as unnecessary once
logic left the manifest).

### SPECOPS-D3 — Compile-time manifest registry with inlining macros

**Status:** Accepted
**Context:** A runtime composer-function design could not introduce lexical
bindings for a reader and paid plist overhead per call.
**Decision:** `defmanifest` stores a normalized field spec in `*manifests*`
inside `eval-when (:compile-toplevel :load-toplevel :execute)`; `marshal` and
`unmarshal` look the spec up at macroexpansion and inline specialized code.
Enums use a parallel `*enums*` table populated by `defenum`.
**Alternatives:** Generated composer functions under interned `⍁`-prefixed
names (rejected: prefix mismatches, call-time macroexpansion, no binding-body
reader); a hybrid of the two (rejected as more moving parts).

### SPECOPS-D4 — Keywords are fields, specops symbols are directives

**Status:** Accepted
**Context:** Field declarations and structural directives share one list.
**Decision:** A clause led by a keyword is an input field
(`(:width (:u 4) :default 0)`); a clause led by a specops symbol is a
directive (`masque`, `pad`, `str`, `span`, `manifest`), dispatched by symbol
identity. Inside a `masque` the same rule applies per group: a keyword is an
input field, anything else a static value.
**Alternatives:** Dispatch by symbol name with an open directive registry
(rejected in favor of fixed, package-identified directives).

### SPECOPS-D5 — Unit-counted width specs with modifier sets

**Status:** Accepted
**Context:** Bit-named keywords (`:u32`) cannot express 24-bit or other odd
widths and mean different things at different output units.
**Decision:** The primary type form is `(class width . modifiers)`, with
`width` counted in output units and `class` one of `:u`/`:s` (with `:f`
reserved for floats). Modifiers form a set: `:vec` marks an array, `:leb` a
self-delimiting encoding, and `(:case field …)` selects a width from an earlier
field's value. Bit-named keywords remain as sugar for 8-bit output only.
**Alternatives:** Keeping only bit-named keywords (rejected); a single
modifier slot (rejected: vectors of LEB-encoded elements need two).

### SPECOPS-D6 — Per-character string codecs, Latin-1 by default

**Status:** Accepted
**Context:** Text in binary formats uses many encodings (ASCII, EBCDIC code
pages, retro multi-byte schemes); endianness of text units is
encoding-specific.
**Decision:** A `(str "LITERAL" :codec #'fn :end-by n)` directive emits a
constant string through a per-character codec, the same shape as a
`defcodetable` function (character to code, code to character). With no codec,
characters are written as Latin-1 codes. Strings always use the swap-0
serializer; any byte order belongs to the codec. `unmarshal` validates the
literal byte by byte.
**Alternatives:** Whole-string `:encode-by`/`:decode-by` functions (deferred
until a variable-width encoding such as UTF-8 or Shift-JIS is needed).

### SPECOPS-D7 — Backpatching through destination `tell`/`write` with absolute positions

**Status:** Accepted
**Context:** Length, offset and checksum fields often precede the data they
describe, which a linear writer cannot know yet.
**Decision:** The destination dispatch provides `enter` (append a unit),
`tell` (current absolute position) and `write` (serialize a value at a
position without moving the cursor). Vectors patch with `aref`, seekable
streams with `file-position`. Positions are recorded absolutely when a slot is
emitted. `serializer-for` is untouched: patching reuses it with a different
collector.
**Alternatives:** Threading tell/patch flags through the serializer call
(rejected: mixes destination concerns into decomposition); always buffering
the whole record (deferred: an auto-buffer for non-seekable destinations is
planned only for manifests that need it).

### SPECOPS-D8 — Nesting-only spans, region-close slot resolution over a runtime map

**Status:** Accepted
**Context:** A checksum may cover several fields; a length may describe one
field inside that range (PNG's CRC covers type and data, its length only the
data).
**Decision:** A region is declared by wrapping fields in `(span name …)`.
Spans nest but cannot overlap. During marshalling each field, span and
sub-manifest records `(name start end)` in a runtime `map`. A pre-pass builds
`slots-of` (target region to slot fields with their widths and byte orders).
When a region closes, `resolve-region` writes `(- end start)` for
`:length-of` or `start` for `:offset-of` at each slot's recorded position.
A slot placed after its region (a trailing checksum) is computed inline.
**Alternatives:** Inline `span-start`/`span-end` markers allowing overlap
(rejected: no known format needs overlapping regions, and unbalanced markers
become possible); a static table of region offsets (rejected: variable-length
fields make offsets unknowable at expansion).

### SPECOPS-D9 — Pluggable checksums over output spans

**Status:** Accepted
**Context:** Checksum algorithms vary by format; SpecOps should stay free of
external dependencies.
**Decision:** `(:crc (:u 4) :slot (:checksum-of region :by #'fn))` calls
`(fn buffer start end)` over the bytes of the named region. CRC-32 and Adler-32
are implemented natively in `checksum.lisp`; other algorithms are supplied as
functions.
**Alternatives:** Depending on Ironclad (rejected: external dependency);
computing checksums from a single field's input value (the first
implementation; replaced because it cannot cover multiple fields).

### SPECOPS-D10 — Sub-manifests inline into the parent through a sub-lexicon

**Status:** Accepted
**Context:** Formats compose records from records (a PNG file of chunks, a
chunk containing an IHDR body). Expanding a nested `marshal` as an independent
call gave it its own cursor and accumulator.
**Decision:** `(manifest :field sub-manifest)` inlines the child's field code
into the parent. The parent passes a sub-lexicon of its gensyms
(`:-+sub-lexicon+-` in the pairs for `marshal`, in `params` for `unmarshal`);
the child reuses them and emits no binding form of its own, so cursor, map and
serializers are shared. The caller supplies the child's values as a literal
nested plist: `:ihdr (:width 1 :height 1)`. The sub-manifest body is recorded
as a region in `map`.
**Alternatives:** Static values baked into the manifest clause (rejected:
callers could not vary them); flat namespaced keys such as `:ihdr.width`
(rejected as verbose).

### SPECOPS-D11 — Per-instance name prefixes; spans do not prefix

**Status:** Accepted
**Context:** Sibling sub-manifest instances (IHDR, IDAT and IEND chunks)
repeat field and region names in the shared `map`.
**Decision:** Each inlined instance receives a prefix (`:pr-sym`) equal to the
qualified field name, so its regions become `:ihdr/length`,
`:ihdr/png-ihdr/width` and so on. Every shared-map reference uses the qualified
name; caller value lookups (`getf pairs`) use the local name. Spans contribute
nothing to the prefix, since names are already unique within one manifest.
**Alternatives:** Prefixing by span context (implemented first; broke slot
targeting and did not address sibling instances); requiring unique names across
nesting (rejected as restrictive).

### SPECOPS-D12 — `deserializer-for` as the inverse of `serializer-for`

**Status:** Accepted
**Context:** Reading needs the exact inverse of the write path, including
byte order and sign.
**Decision:** `deserializer-for` returns `(lambda (index length signed
source))`. It composes `length` units starting at `index`, most significant
first, applies `swap-segments` again (it is its own inverse) to undo byte
order, then sign-extends when `signed`. Sources may be vectors, bare integers,
or `(units . integer)` pairs, the last preserving leading zero units that
`find-width` would drop.
**Alternatives:** Extra width parameter on the lambda (rejected in favor of the
cons form, mirroring `serializer-for`'s `(width . value)`).

### SPECOPS-D13 — unmarshal return shapes: values when keyed, plist otherwise

**Status:** Accepted
**Context:** Callers either bind specific fields or want to see a record's
whole structure.
**Decision:** `(unmarshal name source params &rest keys)` returns multiple
values in key order when keys are given. With no keys it returns a plist of
every named field, with sub-manifests as nested plists. Span children appear at
the span's level. Every named scalar is read regardless of the request, so
length and count fields are available to later fields.
**Alternatives:** Interleaved key/value multiple values for the no-key case
(rejected: needs `multiple-value-list` to `getf` and hits the portable
20-value limit); a binding-body form (superseded); nesting span children under
the span name (rejected: spans are regions, not namespaces).

### SPECOPS-D14 — Masque reads by direct group extraction

**Status:** Accepted
**Context:** `unmasque` binds group characters as variables (impossible for
groups like `t`) and returns `nil` on constant mismatch.
**Decision:** `unmarshal` reads a masque as one unsigned integer of the
masque's `:length` units, then uses `quantify-mask-string` at expansion to emit
`ldb` extractions per group. It signals errors for mismatched constant digits
and static groups, and binds keyword groups as fields with sign extension and
enum decoding. On write, the masque value is serialized as
`(cons length value)` so leading zero bytes are kept.
**Alternatives:** Expanding into `unmasque` (rejected for the reasons above);
recording a masque byte count in `masque`'s expansion (made unnecessary by the
stored `:length`).

## Implementation

### `specops.lisp`

- **Serialization primitives.** `specops.lisp:find-width@10b1568` now sizes
  negative values in two's complement instead of looping forever.
  `specops.lisp:serializer-for@10b1568` gained an overflow guard on
  `(width . value)` pairs and zero padding for `(n)` pairs.
  `specops.lisp:deserializer-for@10b1568` implements SPECOPS-D12, with a shared
  `finish` step for byte order and sign.
- **Registries and enums.** `*manifests*` and `*enums*` are defined under
  `eval-when`; `specops.lisp:defenum@10b1568` registers keyword-to-integer
  maps and `specops.lisp:enumerated@10b1568` decodes them on read.
- **LEB128.** `specops.lisp:encode-leb128-unsigned@10b1568` and
  `specops.lisp:encode-leb128-signed@10b1568` back the `:leb` modifier on
  write.
- **`defmanifest`.** `specops.lisp:defmanifest@10b1568` normalizes clauses
  through `process-field` (width specs, signedness, endianness, slots, enums,
  `:case` conditions, `:count`) and `process-entry` (the `span`, `pad`, `str`,
  `masque` and `manifest` directives). `pad` supports a count, `:to` an index
  and `:align`; a trailing `:length` pads the record out. The spec is a list
  headed by a `:config` entry carrying `:unit-spec`.
- **`marshal`.** `specops.lisp:marshal@10b1568` imports or creates the
  sub-lexicon, runs `preprocess-spec` to collect byte orders, regions and
  `slots-of`, builds the `enter`/`tell`/`write` destination dispatch, and emits
  code per field through `generate`: scalars, `:vec`, `:leb`, `:case` widths,
  enums, masques, constant strings, spans, sub-manifests and slot resolution via
  `resolve-region`. `qualify` applies per-instance prefixes. Nested expansions
  emit a bare `progn` that shares the parent's bindings.
- **`unmarshal`.** `specops.lisp:unmarshal@10b1568` walks the spec with a
  runtime cursor through `process-item`: scalars through a per-byte-order
  deserializer vector, `:vec` with counts from `:count`, an earlier field, or a
  reverse `:length-of` lookup (`vec-count-form`), spans walked in place,
  sub-manifests expanded with the shared cursor and their own deserializers,
  masques per SPECOPS-D14, and constant strings validated per SPECOPS-D6.

Fixes made along the way, each found by the PNG exercise or by review:

- A manifest with only sub-manifest fields had no byte orders, so
  `(reduce #'max nil)` failed; `:initial-value 0` added.
- The nested `marshal` branch never propagated the sub-lexicon, so a
  sub-manifest two levels down escaped shared mode and wrote from index 0.
- A span's region was created lazily by its first child and persisted across
  sibling sub-manifests, so IDAT's CRC used IHDR's start; spans now push a
  fresh region on entry.
- Span children were stored in reverse order; the `:items` list is now
  reversed like the top-level field list.
- Slot widths were taken from the marshal `length` gensym instead of the slot
  field's declared length.

### `checksum.lisp`

`checksum.lisp:crc@10b1568` computes CRC-32 over `[start, end)` of a byte
vector using a table-driven update; `checksum.lisp:adler32-checksum@10b1568`
provides Adler-32 for zlib streams. The CRC table is now a `defvar`, so
recompiling the core system in a running image no longer signals
`defconstant-uneql`.

### `goff.lisp`

`goff.lisp:%goff-write-end@10b1568` writes the GOFF END record through
`marshal` and the `goff-end` manifest (a masque PTV header, scalars, pads and
an 80-byte `:length`), replacing offset-addressed buffer pokes.

### `manifest.lisp`

`manifest.lisp` is the demonstration and test file. It defines PNG as
manifests (`png-ihdr`, `png-ihdr-chunk`, `png-idat-chunk`, `png-iend-chunk`,
`png-file`), builds a valid 1×1 RGB image with
`manifest.lisp:make-1x1-png@10b1568` (IDAT from a stored-block zlib stream via
`manifest.lisp:zlib-store@10b1568`), and checks it with an independent
structural reader, `manifest.lisp:verify-png@10b1568`. Probe manifests cover
signedness and byte order, nesting, masques and strings.

### `specops.asd`

The `specops/demo.png` system loads `manifest.lisp` and depends on
`specops/format.ebcdic` for the EBCDIC string checks.

## Design Properties

- **One description, two directions.** Every field kind written by `marshal`
  except the caller-supplied string field has a matching `unmarshal` reader.
- **No logic in manifests.** Reads never need to invert an expression.
- **Shared environment under nesting.** A sub-manifest at any depth uses the
  top-level cursor, map and serializers; only `unmarshal` children bind their
  own deserializer vectors.
- **Regions never overlap.** Spans nest, so every region has a single
  well-defined start and end.

## Verification

Run via:

```
sbcl --non-interactive --eval '(asdf:load-system :specops/demo.png)' \
  --eval '(specops::run-png-demo)' --eval '(specops::run-unmarshal-demo)' \
  --eval '(specops::run-chunk-unmarshal-demo)' --eval '(specops::run-masque-demo)' \
  --eval '(specops::run-string-demo)'
```

Results at `10b1568`:

Table: Test suites in `manifest.lisp` and their results at commit `10b1568`.

| Suite | Function | Checks | Result |
|---|---|---|---|
| PNG write and structural verify | `run-png-demo` | 1 (72-byte file) | Pass |
| Flat unmarshal | `run-unmarshal-demo` | 8 | 8 pass |
| Chunks, spans, nesting | `run-chunk-unmarshal-demo` | 9 | 9 pass |
| Masque fields | `run-masque-demo` | 10 | 10 pass |
| Constant strings and codecs | `run-string-demo` | 9 | 9 pass |

The marshalled PNG was also checked outside Lisp: `file` reports it as a
1×1 8-bit RGB PNG, and Python's PIL decodes it to the pixel (255, 0, 0). The
demo system is not yet wired into a Square-style `test-op`; the suites are
called by hand.

## Files

Table: Files created or changed by the manifest work, from the freeform
`marshal` through commit `10b1568`.

| File | Action | Description |
|------|--------|-------------|
| `specops.lisp` | Modified | `defenum`, `defmanifest`, `marshal`, `unmarshal`, `deserializer-for`, LEB128 encoders, serializer fixes |
| `checksum.lisp` | **New** | CRC-32 and Adler-32 implementations |
| `manifest.lisp` | **New** | PNG manifests, 1×1 PNG builder, structural verifier, probe manifests and test suites |
| `goff.lisp` | Modified | GOFF END record written through the `goff-end` manifest |
| `specops.asd` | Modified | `checksum` component; `specops/demo.png` system with EBCDIC dependency |
| `package.lisp` | Modified | exports `marshal` |
| `doc/Log.Manifest.md` | **New** | this log |

## Metrics

- Test checks: 37 automated checks plus 1 structural PNG verification.
- Regressions at `10b1568`: 0.
- New source files: 2 (`checksum.lisp`, `manifest.lisp`).

## Outstanding Work

- **Caller-supplied string fields.** The `:str` field type still marshals as a
  1-unit scalar and fails; only the constant `str` directive works. A generic
  chunk manifest needs it.
- **Validation inside `unmarshal`.** Checksums and `:length-of` values are
  checked by the tests, not by `unmarshal`. This needs a read-side region map
  threaded through the sub-lexicon, and a policy for mismatches.
- **Selective reads.** `unmarshal` reads every named scalar and every
  sub-manifest in full. Dependency closure and skipping, including a skip mode
  for sub-manifests, remain.
- **Variable structure.** Arrays of sub-manifests (repeated chunks, "read
  until IEND") and `:case` that selects a sub-manifest by tag, with indexed
  prefixes for repeated instances.
- **Struct input and output.** `defmanifest` does not generate a struct, so
  neither `marshal` nor `unmarshal` offers the struct path, and `unmarshal`'s
  runtime plist cannot yet be fed back into `marshal`, which takes nested
  values only as literal plists at macroexpansion.
- **Runtime enum values on write.** Enum mapping in `marshal` resolves only
  literal keywords; a runtime form holding a keyword is not mapped.
- **Masque range checks and registration.** Values too wide for a masque
  group are silently truncated, and masque groups are not registered in
  `values`/`map`, so a `:case` or `:count` cannot refer to them on write.
- **`pad :align`.** The count is computed as `(mod index n)`, the remainder
  already used, rather than the padding needed.
- **Auto-buffering and stream checksums.** Checksums read the destination
  vector directly; non-seekable streams need the planned auto-buffer path.
- **Byte-order union for nesting.** A sub-manifest using a byte order its
  parent lacks would index past the parent's serializer array; an `unmarshal`
  manifest whose strings need swap 0 but has no swap-0 field would call a nil
  deserializer.
- **Exports and packaging.** `defmanifest`, `defenum` and `unmarshal` are not
  exported, `goff.lisp` is not in any ASDF system, and the demo suites are not
  wired to `test-op`.
- **Decimal masques and floats.** `d:` masque strings are not read, and the
  reserved `:f` class is unimplemented.
- **Offset-table exercise.** WOFF, chosen to test `:offset-of` position
  backpatching, has not been written.
