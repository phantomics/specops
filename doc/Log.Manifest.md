---
id:            SPECOPS-0006
title:         Manifest Development Log — defmanifest, marshal and unmarshal
genre:         Log
scope:         component
project:       SpecOps
component:     Manifest
language:      en
status:        Implemented
authors:
  - Andrew Sengul
approved-by:   Andrew Sengul
reviewed:      2026-11-01
provenance:
  assistant:   claude-opus (OpenCode)
  session:     Marshal
decisions:
  - SPECOPS-D28
  - SPECOPS-D29
  - SPECOPS-D30
  - SPECOPS-D31
  - SPECOPS-D32
  - SPECOPS-D33
  - SPECOPS-D34
  - SPECOPS-D35
  - SPECOPS-D36
  - SPECOPS-D37
  - SPECOPS-D38
  - SPECOPS-D39
  - SPECOPS-D40
  - SPECOPS-D41
  - SPECOPS-D42
  - SPECOPS-D43
  - SPECOPS-D44
  - SPECOPS-D45
  - SPECOPS-D46
  - SPECOPS-D47
  - SPECOPS-D48
  - SPECOPS-D49
  - SPECOPS-D50
  - SPECOPS-D51
  - SPECOPS-D52
  - SPECOPS-D53
  - SPECOPS-D54
  - SPECOPS-D55
  - SPECOPS-D56
  - SPECOPS-D57
  - SPECOPS-D58
  - SPECOPS-D59
  - SPECOPS-D60
  - SPECOPS-D61
  - SPECOPS-D62
  - SPECOPS-D63
  - SPECOPS-D64
  - SPECOPS-D65
  - SPECOPS-D66
  - SPECOPS-D67
  - SPECOPS-D68
  - SPECOPS-D69
  - SPECOPS-D70
  - SPECOPS-D71
  - SPECOPS-D72
  - SPECOPS-D73
  - SPECOPS-D74
  - SPECOPS-D75
  - SPECOPS-D76
  - SPECOPS-D77
  - SPECOPS-D78
  - SPECOPS-D79
  - SPECOPS-D80
  - SPECOPS-D81
  - SPECOPS-D82
  - SPECOPS-D83
  - SPECOPS-D84
  - SPECOPS-D85
  - SPECOPS-D86
  - SPECOPS-D87
  - SPECOPS-D88
  - SPECOPS-D89
  - SPECOPS-D90
  - SPECOPS-D91
  - SPECOPS-D92
  - SPECOPS-D93
  - SPECOPS-D94
  - SPECOPS-DRAFT-manifest-D68
  - SPECOPS-DRAFT-manifest-D69
  - SPECOPS-DRAFT-manifest-D70
  - SPECOPS-DRAFT-manifest-D71
  - SPECOPS-DRAFT-manifest-D72
  - SPECOPS-DRAFT-manifest-D73
relates-to:
  - SPECOPS-0003
  - SPECOPS-0007
  - SPECOPS-0005
  - SPECOPS-0004
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
  - title:    Square — Software QUality Assurance Regimen & Ethos (Common Lisp testing standard)
    locator:  Square.md — layout (§4), test package (§5), suites (§6), isolation (§7), test bodies (§8), runners and test-op (§11), non-conforming patterns (§14)
    external: true
  - title:    Faster APL with Lazy Extensions (Andrew Sengul, 2023)
    locator:  generators and their per-element dispatch (§3.4); effectors assembled into kernels (§5.1); allocator extensions (§6.1); layered fallback (§8)
    external: true
---

# Manifest: Development Log

This document chronicles the design and construction of SpecOps's declarative
binary-format layer: `defmanifest`, which declares the structure of a binary
record, `marshal`, which writes values into that structure, and `unmarshal`,
which reads them back out. It covers the path from a freeform `marshal` macro to
manifest-driven writing and reading, and the PNG test format used to exercise
it. The chronicled state is commit `10b1568`; the updates at the end of this
log carry the record forward: that of 2026-10-30 to commit `653345f`, three
of 2026-10-31, to commits `738105a`, `d030f5c` and `2e1af03`, ten of
2026-11-01, to commits `4f9c6d4`, `d7ba27f`, `1cc7600`, `b63b7d2`, `239b2f7`,
`cb931c8`, `52b81e5`, `e340210`, `5862d3a` and `e4a1f9a`, that of 2026-11-02
to commit `de25ae7`, tagged `v0.1.0`, and four of 2026-11-11, to commits
`adc24c0`, `17bac3c`, `f85a370` and `cba66c2`, of which `adc24c0`, `17bac3c`
and `cba66c2` are tagged `v0.1.1`, `v0.1.2` and `v0.1.3`. The repository restructure that followed
moved the files this log names (see
[SPECOPS-0005](Log.Restructure.md)); code references stay pinned
to the commits that held the work.

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

### SPECOPS-D28 — Manifest-driven marshal replaces the freeform item list

**Status:** Accepted
**Context:** The freeform `marshal` serialized an ad hoc item list. It could
not be inverted into a reader and every call site restated the layout.
**Decision:** A named manifest declares the layout once; `(marshal name
destination :key value …)` supplies values for its named fields. The freeform
mode was removed rather than reconciled.
**Alternatives:** Keeping freeform `marshal` alongside a manifest mode
(rejected: two vocabularies for one job); a struct-only interface (deferred;
keyword input came first).

### SPECOPS-D29 — Manifests describe structure only; logic stays at the call site

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

### SPECOPS-D30 — Compile-time manifest registry with inlining macros

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

### SPECOPS-D31 — Keywords are fields, specops symbols are directives

**Status:** Accepted
**Context:** Field declarations and structural directives share one list.
**Decision:** A clause led by a keyword is an input field
(`(:width (:u 4) :default 0)`); a clause led by a specops symbol is a
directive (`masque`, `pad`, `str`, `span`, `manifest`), dispatched by symbol
identity. Inside a `masque` the same rule applies per group: a keyword is an
input field, anything else a static value.
**Alternatives:** Dispatch by symbol name with an open directive registry
(rejected in favor of fixed, package-identified directives).

### SPECOPS-D32 — Unit-counted width specs with modifier sets

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

### SPECOPS-D33 — Per-character string codecs, Latin-1 by default

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

### SPECOPS-D34 — Backpatching through destination `tell`/`write` with absolute positions

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

### SPECOPS-D35 — Nesting-only spans, region-close slot resolution over a runtime map

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

### SPECOPS-D36 — Pluggable checksums over output spans

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

### SPECOPS-D37 — Sub-manifests inline into the parent through a sub-lexicon

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

### SPECOPS-D38 — Per-instance name prefixes; spans do not prefix

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

### SPECOPS-D39 — `deserializer-for` as the inverse of `serializer-for`

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

### SPECOPS-D40 — unmarshal return shapes: values when keyed, plist otherwise

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

### SPECOPS-D41 — Masque reads by direct group extraction

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
  `specops.lisp:deserializer-for@10b1568` implements
  SPECOPS-D39, with a shared `finish` step for byte order and
  sign.
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
  masques per SPECOPS-D41, and constant strings validated per
  SPECOPS-D33.

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

- Test checks: 36 automated checks plus 1 structural PNG verification.
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

## Update 2026-10-30 — Pads, codecs, terminated fields, scan effectors and :case

This installment covers commits `1e53ae7` through `653345f`. Pads were
reworked to be sized at run time. Codecs were extended from constant strings to
caller-supplied vector fields, and fields gained `:end-by` terminators.
Variable-length fields were then given a single model: stop rules and
assemblers whose effector methods emit fused code at macroexpansion. LEB128
fields now work in both directions on that model. Finally, `:case` fields,
whose width is chosen by an earlier field, were rebuilt as a choice between
complete field plans and now work in both directions. The test suites grew
from 36 checks to 117.

### SPECOPS-D42 — Pads are sized at run time, `:to` from the record's start and `:align` from the output's

**Status:** Implemented
**Context:** `defmanifest` computed each pad's size from a static `index` as it
parsed the clauses. Once a variable-length field preceded a pad, that index
was wrong. `pad :align` also computed the remainder already used rather than
the padding still needed. A sub-manifest instance needs to pad to its own
length, wherever it starts in the output.
**Decision:** `(pad with n)` writes `n` units. `(pad with :to n)` pads up to
`n` units from the start of the current manifest instance, whose base
position is recorded per instance and shared through the sub-lexicon.
`(pad with :align n)` pads to a multiple of `n` counted from the start of the
output. A manifest's trailing `:length` is a `:to` pad. The count is computed
when the generated code runs, by `specops.lisp:pad-count-form@653345f`, and a
`:to` target already passed signals an error. `unmarshal` moves its cursor by
the same count.
**Alternatives:** Static counts at `defmanifest` (the earlier design; wrong
after any variable-length field); `:to` counted from the start of the output
(rejected: a sub-manifest instance could not pad to its own length); a
separate `skip` directive for reading (rejected: reading a pad already skips
it).

### SPECOPS-D43 — Codecs apply to `:vec` fields, and unmapped input is an error

**Status:** Implemented
**Context:** SPECOPS-D33 applied codecs only to the constant
`str` directive. Caller-supplied text, such as an EBCDIC name, had no path. A
`defcodetable` function returned `nil` for a character or code it did not map,
and the `nil` reached the serializer.
**Decision:** A `:vec` field takes a `:codec`, given as `#'fn` or `'fn`.
`marshal` encodes a string value character by character through the codec,
and `unmarshal` decodes the elements back into a string. With no codec,
characters are written as their Latin-1 codes. A string default goes through
the codec; a numeric default is already a code, and is repeated `:count`
times without it. `specops.lisp:codec-for@653345f` wraps a codec so that an
unmapped character or code signals an error naming the input and the codec,
on write and on read. `specops.lisp:defcodetable@653345f` marks unassigned
entries with -1 in its `(signed-byte 16)` code table and `nil` in its reverse
table. Caller text is therefore a `:vec` field with a codec, not the `:str`
field type that SPECOPS-D12 proposed.
**Alternatives:** A dedicated `:str` field type (not built: the `:vec` field
with a codec covers it with no new type, and
SPECOPS-D12 may be reworded); passing unmapped input
through as `nil` or skipping it (rejected: corrupt output with no error).

### SPECOPS-D44 — A character terminator goes through the codec; an integer terminator is a raw code

**Status:** Implemented
**Context:** Many fields end at a terminator rather than at a known count: C
strings end at NUL, some EBCDIC text at a `$`. A terminator written as a
character should match the data's encoding, but an integer passed to a
`defcodetable` function means "decode this code", and CP037 maps none of its
control characters, so NUL cannot be encoded through it.
**Decision:** `:end-by` on a `:vec` field takes a character, encoded like the
data (through the codec, or as `char-code` without one), or an integer, written
as is. The terminator is one element of the field's width and byte order.
Data containing its own terminator signals an error. A field with no value
writes the terminator alone. The terminator counts as part of the field's
region, so a `:length-of` or checksum over the field includes it. On read, the
scan compares raw unsigned codes, before sign extension or decoding, and the
cursor moves past the terminator; a source that ends first signals an error.
`:count` together with `:end-by` signals an error for now.
**Alternatives:** An explicit flag choosing whether the terminator is encoded
(rejected: the type of the terminator already says it); always encoding it
through the codec (rejected: NUL-terminated EBCDIC could not be written);
allowing data that contains the terminator (rejected: a reader would stop
early). Multi-element terminators and fixed buffers holding a terminator are
planned in SPECOPS-D19 and
SPECOPS-D18.

### SPECOPS-D45 — Variable-length fields are built from stop rules and assemblers whose effectors emit fused code at expansion

**Status:** Implemented
**Context:** Counts, terminators and LEB128's continuation bit were each a
separate branch in `marshal` and `unmarshal`. The `:leb` write branch was
broken and there was no `:leb` read; the terminator read scanned every element
twice. A runtime scanner protocol, fed one element at a time, would unify
them but pays a generic call per element and buffers the elements. The
paper "Faster APL with Lazy Extensions" (see `cites:`) builds one typed kernel
from code fragments returned by effector methods on the classes involved, with
slower layers as fallbacks. A manifest's structure is known at macroexpansion,
so that assembly can happen there, with no run-time compilation.
**Decision:** The variable-length part of a field is a stop rule
(`count-rule`, `terminator-rule`, `continuation-rule`) and an assembler
(`vector-assembler`, `leb-assembler`), built at expansion from the field's
keys. On read, `stop-effector` and `assemble-effector` return code fragments
(bindings, declarations, a test before each read, a test after it, whether the
stopping element is data, the collection step and the result), and
`scan.lisp:emit-scan@653345f` combines them into one loop that reads each
element once and advances the cursor. On write, `frame-effector` and
`disassemble-effector` do the same (a check on each element, a mark such as a
continuation bit, a trailer, and the splitting of a value into elements)
through `scan.lisp:emit-frame@653345f`. Elements are read as raw unsigned
codes; sign extension and enum decoding belong to the assembler. Analysis
methods such as `scan.lisp:known-count@653345f` give facts about a field before
any code is emitted. Rule classes without effector methods are to run through a
runtime protocol (SPECOPS-D20).
**Alternatives:** A branch per field kind (the earlier design); the runtime
scanner protocol as the only path (rejected as the primary path: dispatch and a
buffer per element; kept as the planned fallback); closures returned by
factories, like `serializer-for` (rejected: harder for users to extend);
run-time code generation as in April (unnecessary: the facts it waits for are
known at expansion).

### SPECOPS-D46 — A `:case` field is a choice between complete field plans

**Status:** Implemented
**Context:** `(:case field …)` (SPECOPS-D32) stored its branches
unparsed. `marshal` looked up a width at run time from a plist of the values
the caller passed, so each branch was reduced to a width, its signedness and
modifiers were lost, and a selector left at its default had no value.
`unmarshal` ignored `:case` and read a one-unit scalar.
**Decision:** `defmanifest` parses each branch, `(keys . type)` with `type` a
width spec or a bit-named keyword, into a plist with its length, signedness
and modifiers, through the same type parser fields use. At expansion,
`scan.lisp:field-plan@653345f` turns a stored field into a `scalar-plan`, a
`scan-plan` (a stop rule and an assembler) or a `case-plan` whose branches are
plans in turn. `read-effector` and `write-effector` emit each plan; a case plan
emits one `case` on the selector, whose clauses are the branches' code with
constant widths, so the run-time dispatch happens once.
`scan.lisp:simplify-plan@653345f` reduces a case plan whose selector is a
constant to its branch at expansion, and `scan.lisp:known-extent@653345f`
gives the width when every branch agrees.
**Alternatives:** Computing a width at run time and passing it to generic code
(the earlier write path; loses per-branch types); storing whole field
descriptors as objects in `defmanifest` (deferred: plans are built from the
stored plists at expansion, and the stored form is unchanged).

### SPECOPS-D47 — `:case` keys compare integer codes, and `otherwise` is the default branch

**Status:** Implemented
**Context:** A selector field may be enumerated, so a caller can give it as a
keyword or an integer, and `unmarshal` returns it decoded. Branch keys need
one domain to compare in. `marshal` also needs the selector's value when the
`:case` field is written, including a default the caller did not pass.
**Decision:** Branch keys are compared as integer codes. Keyword keys are
translated through the selector's enum at expansion by
`scan.lisp:translate-keys@653345f`; a keyword outside the enum is an expansion
error. A key may be a single value or a list, and `otherwise` names the default
branch; with no default, an unmatched selector signals an error, at expansion
when the selector is a constant. The selector must come before the `:case`
field, on both sides, or expansion signals an error.
`specops.lisp:case-selectors@653345f` finds the selectors. `marshal` binds a
variable for each, set to the code actually written (the value or the default,
after enum mapping), and uses a constant instead when the call gives one or
omits the selector. These variables replace the run-time `values` plist, whose
pushes evaluated every value form a second time. `unmarshal` binds a raw-code
variable beside the decoded one for an enumerated selector.
**Alternatives:** Comparing decoded values (rejected: a run-time selector
given as an integer would miss keyword keys, and reads would need to re-encode);
accepting `t` as a default as in `cl:case` (rejected: `otherwise` alone keeps
`t` usable as a key, written `(t)`); a separate `:default-type` key on the
field (rejected: the default reads more clearly as a branch).

### SPECOPS-D48 — Directives dispatch by symbol name, and `_` names an unnamed field

**Status:** Implemented
**Context:** SPECOPS-D31 dispatched directives by symbol
identity. A manifest written in a package that does not import SpecOps's
internal `span`, `pad`, `str` and `manifest` lost those clauses without a
message, as it did a clause led by `_` and a misspelt directive.
**Decision:** `defmanifest` dispatches a clause led by a non-keyword symbol on
the symbol's name, so directives work from any package. A clause led by `_`
from any package, or by `:_`, is an unnamed field: it is written with its
default and skipped on read. An unknown directive signals an error. This
amends SPECOPS-D31, which rejected dispatch by name.
**Alternatives:** Keeping identity and exporting the directive symbols
(rejected: `_` would have to be exported, and misspellings would still be
dropped); accepting only `:_` for unnamed fields (rejected: `_` is the
conventional placeholder).

### Implementation

- **`specops.lisp`.**
  - `specops.lisp:pad-count-form@653345f` sizes pads per
    SPECOPS-D42.
  - `specops.lisp:codec-for@653345f` and `specops.lisp:defcodetable@653345f`
    implement the codec errors of SPECOPS-D43.
  - `specops.lisp:defmanifest@653345f` parses types through a local
    `parse-type` shared by fields and `:case` branches, and dispatches
    directives by name.
  - `specops.lisp:marshal@653345f` writes `:vec` fields with codecs and
    terminators, `:leb` fields through `emit-frame`, and `:case` fields
    through `write-effector` with selector variables. Run-time enum values
    are mapped, and each value form is evaluated once.
  - `specops.lisp:unmarshal@653345f` reads `:vec` fields with codecs and
    terminators (a local `term-code-form` computes the terminator's code, and
    `vec-count-form` scans for it), `:leb` fields through `emit-scan`, and
    `:case` fields through `read-effector` with raw-code variables for
    enumerated selectors.
  - `defmanifest`, `defenum` and `unmarshal` are exported from
    `package.lisp` since `371f0b2`.
- **`scan.lisp`** (new). The stop-rule, assembler and field-plan classes; the
  read and write effectors; `scan.lisp:emit-scan@653345f` and
  `scan.lisp:emit-frame@653345f`; `scan.lisp:field-plan@653345f`,
  `scan.lisp:translate-keys@653345f` and `scan.lisp:simplify-plan@653345f`.
- **`manifest.lisp`.** Four suites were added:
  `manifest.lisp:run-pad-demo@653345f`,
  `manifest.lisp:run-terminated-demo@653345f` (codecs and terminators),
  `manifest.lisp:run-leb-demo@653345f` and
  `manifest.lisp:run-case-demo@653345f`.
- **`specops.asd`.** The `scan` component loads before `specops`.

Fixes made along the way:

- `defcodetable` read an unbound gensym in its encoder, filled its reverse
  table with NUL characters so unassigned codes decoded silently, and for a
  time passed a malformed argument list to `make-array`.
- `unmarshal`'s `vec-count-form` was called with the terminator arguments
  before its lambda list accepted them.
- The old `:leb` write passed a `(width . value)` pair to an encoder that takes
  an integer, and chose signed or unsigned when the code ran.
- A clause led by `_` was dropped without a message.
- A defaulted `:case` selector failed on write, and `unmarshal` ignored
  `:case`.
- `marshal` evaluated every value form twice: once to write it, once to record
  it in `values`.

### Verification

Run via:

```
sbcl --non-interactive --eval '(asdf:load-system :specops/demo.png)' \
  --eval '(specops::run-png-demo)' --eval '(specops::run-unmarshal-demo)' \
  --eval '(specops::run-chunk-unmarshal-demo)' --eval '(specops::run-masque-demo)' \
  --eval '(specops::run-string-demo)' --eval '(specops::run-pad-demo)' \
  --eval '(specops::run-terminated-demo)' --eval '(specops::run-leb-demo)' \
  --eval '(specops::run-case-demo)'
```

Table: Test suites in `manifest.lisp` and their results at commit `653345f`.

| Suite | Function | Checks | Result |
|---|---|---|---|
| PNG write and structural verify | `run-png-demo` | 1 (72-byte file) | Pass |
| Flat unmarshal | `run-unmarshal-demo` | 8 | 8 pass |
| Chunks, spans, nesting | `run-chunk-unmarshal-demo` | 9 | 9 pass |
| Masque fields | `run-masque-demo` | 10 | 10 pass |
| Constant strings and codecs | `run-string-demo` | 9 | 9 pass |
| Pads | `run-pad-demo` | 18 | 18 pass |
| Codecs on vectors, terminated fields | `run-terminated-demo` | 19 | 19 pass |
| LEB128 | `run-leb-demo` | 17 | 17 pass |
| `:case` | `run-case-demo` | 27 | 27 pass |

The LEB128 checks compare the new writer against the earlier
`encode-leb128-*` functions on values up to 2⁷⁰ and round-trip each value. The
`:case` checks include a sub-manifest with a run-time selector, a constant
selector that leaves no `case` in the expansion, and a check that each value
form is evaluated once.

### Files

Table: Files created or changed from commit `1e53ae7` through `653345f`.

| File | Action | Description |
|------|--------|-------------|
| `scan.lisp` | **New** | Stop rules, assemblers, field plans, their effectors, `emit-scan` and `emit-frame` |
| `specops.lisp` | Modified | Pads sized at run time, codec errors, terminators, `parse-type`, dispatch by name, `:leb` and `:case` on both sides |
| `manifest.lisp` | Modified | Pad, codec and terminator, LEB128 and `:case` suites |
| `specops.asd` | Modified | `scan` component |
| `package.lisp` | Modified | Exports `defmanifest`, `defenum` and `unmarshal` |
| `doc/Log.Manifest.md` | Modified | This update |

### Metrics

- Test checks: 117 automated checks plus 1 structural PNG verification, up
  from 36.
- Regressions at `653345f`: 0.
- New source files: 1 (`scan.lisp`).

### Outstanding Work

Items from the outstanding work above that are now done: run-time enum values
on write, `pad :align`, caller-supplied text (as `:vec` fields with a codec,
SPECOPS-D43), and `:case` widths. The exports are done, but
`goff.lisp` is still in no ASDF system and the suites are not behind `test-op`.
The other earlier items remain open.

New items:

- **`:vec` on the scan model.** `:vec` fields still use their own branches; the
  terminator scan reads each element twice.
- **Agreed but not built.** Fixed buffers holding a terminator
  (SPECOPS-D18), multi-element terminators
  (SPECOPS-D19), and user-defined stop rules with the
  runtime fallback (SPECOPS-D20).
- **Vectors of LEB elements**, which the modifier set allows.
- **Keyword defaults on enumerated fields.** A `:default` given as a keyword is
  not mapped through the enum, so it fails to marshal.
- **Unknown enum codes.** `unmarshal` decodes a code missing from the enum as
  `nil`.
- **Error wording.** A selector placed after its `:case` field is reported
  against the manifest's name rather than the field's.
- **Dead code.** `encode-leb128-unsigned` and `encode-leb128-signed` have no
  callers.

## Update 2026-10-31 — Vector fields on the scan model

This installment covers commits `0b03854` and `738105a`. Vector fields joined
LEB128 and `:case` fields on the scan model of SPECOPS-D45, so
that every variable-length field is now built from a field plan, and each
macro handles them in one clause. A terminated vector is read in one pass. Two
rules were settled on the way: how vector elements are encoded on write, and
where enum decoding happens. The new suite brings the checks from 117 to 135.

Soon after `738105a`, the repository restructure
([SPECOPS-0005](Log.Restructure.md)) moved the code this
installment names: `scan.lisp` to `src/manifest/scan.lisp`, the manifest
layer of `specops.lisp` to `src/manifest/manifest.lisp`, and `manifest.lisp`
to `test/manifest.lisp`. It changed no behavior. The references below are
pinned to `738105a`, where the files had their old paths.

### SPECOPS-D49 — Vector elements follow the terminator rule, and signed elements are written as raw codes

**Status:** Implemented
**Context:** On write, an element of a `:vec` field with a codec went through
the codec whatever its type, and a `defcodetable` function given an integer
decodes it, so integers in a text field failed. Keyword elements of an
enumerated vector were not mapped. A negative element of a signed field was
compared with the terminator as a negative number, while the terminator is a
raw code: `-1` in an `(:s 1 :vec)` field ended by `#xFF` passed the check,
was written as `FF`, and would end the field early for a reader.
**Decision:** Each element of a vector is encoded by the rule
SPECOPS-D44 set for terminators. A character goes through the
codec, or `char-code` without one; a keyword goes through the field's enum,
and signals an error if the enum lacks it; an integer is a raw code. A
negative integer in a signed field is checked against the field's width and
masked to it, so the terminator check and the serializer both see the raw
code. Data, defaults and terminators thus share one rule, implemented by
`scan.lisp:disassemble-effector@738105a` on the vector assembler.
**Alternatives:** Accepting only characters in a codec field (rejected by the
steward: integers in a text field are legitimate codes, and one rule is
simpler); comparing signed values with the terminator (rejected: the case of
`-1` above).

### SPECOPS-D50 — Each field plan decodes its own enum

**Status:** Implemented
**Context:** `unmarshal` applied enum decoding to a field's value after
reading it. For a vector the value is a whole array, whose elements each need
decoding, and a `:case` field may have a scalar branch and a vector branch,
so one decoding step outside the plan cannot be right for both.
**Decision:** The scalar plan, the LEB128 assembler and the vector assembler
each decode their own enum, through `scan.lisp:enum-decode-form@738105a`;
the vector assembler decodes each element. `field-plan` passes the field's
`:enumerate-by` to every plan it builds, and `unmarshal` applies no decoding
of its own to a field built from a plan. On write, scalar, LEB and `:case`
values still go through `marshal`'s enum mapping before the plan sees them,
so that a literal keyword becomes a constant at expansion, which
`simplify-plan` needs; vector elements are mapped one by one
(SPECOPS-D49).
**Alternatives:** Decoding vector elements in the assembler and other values
outside it (rejected by the steward: wrong for a `:case` mixing scalar and
vector branches, and two places to look).

### SPECOPS-D51 — Vector, LEB and `:case` fields are built from field plans in one clause per macro

**Status:** Implemented
**Context:** After SPECOPS-D46, `:case` and LEB fields went
through field plans, but vectors kept hand-written branches in both macros.
The read branch scanned a terminated field once to count it and again to
read it.
**Decision:** `scan.lisp:field-plan@738105a` builds a scan plan for a `:vec`
field, with the element width of the field: a terminator rule from
`:end-by`, encoded by `scan.lisp:terminator-code-form@738105a`, or a count
rule whose count comes from `:count`, a count field or a `:length-of` slot.
The vector assembler carries the element's bits, signedness, enum and codec,
and builds its own decoding and encoding forms. A `:case` branch is built from
the branch's keys followed by the field's, so a vector branch takes the
field's `:count`, `:end-by` and `:codec`. In `specops.lisp:marshal@738105a`
one clause writes all three kinds through `write-effector`; a local
`vec-source-form` chooses a vector's value: the caller's, a vector or string
default, an empty string for a terminated field, or a numeric default
repeated `:count` times, built at expansion as a literal vector. In
`specops.lisp:unmarshal@738105a` one clause reads all three through
`read-effector`, and a local `record-read` binds the result. A terminated
field is read once, element by element.
**Alternatives:** Keeping the vector branches (the earlier code; two passes on
read and a second encoding path on write); repeating a numeric default with a
loop at run time (rejected: the vector is constant).

### Implementation

- **`scan.lisp`.** `scan-plan` gained an element width, used on both sides.
  The vector assembler's slots became `bits`, `signed`, `enum` and `codec`,
  and its write side, `scan.lisp:disassemble-effector@738105a`, was added.
  The scalar plan and the LEB assembler gained an `enum` slot.
  `scan.lisp:codec-form@738105a` builds a codec wrapper that reports unmapped
  input, and `terminator-code-form` moved here from `unmarshal`, to serve
  both macros. The check that a LEB field is one 8-bit unit wide moved into
  `field-plan`.
- **`specops.lisp`.** The hand-written `:vec` branches and the separate LEB
  branch of `marshal` were removed, and `encode-leb128-unsigned` and
  `encode-leb128-signed` commented out. `marshal`'s local
  `scalar-value-form` gives the value of a scalar, LEB or `:case` field and
  sets its selector variable (SPECOPS-D47).
- **`manifest.lisp`.** `manifest.lisp:run-vec-scan-demo@738105a` is the new
  suite.

In review before `0b03854`, the first version of the change did not compile.
The value helpers in `marshal` were defined outside `generate` and used its
variables, and returned code that wrote the field rather than its value; the
scan plan read and wrote each element as one unit; the LEB assembler's
`with-slots` had no instance; and `record-read` was missing. All were fixed
before the commit.

### Verification

Run via:

```
sbcl --non-interactive --eval '(asdf:load-system :specops/demo.png :force t)' \
  --eval '(specops::run-png-demo)' --eval '(specops::run-unmarshal-demo)' \
  --eval '(specops::run-chunk-unmarshal-demo)' --eval '(specops::run-masque-demo)' \
  --eval '(specops::run-string-demo)' --eval '(specops::run-pad-demo)' \
  --eval '(specops::run-terminated-demo)' --eval '(specops::run-leb-demo)' \
  --eval '(specops::run-case-demo)' --eval '(specops::run-vec-scan-demo)'
```

Table: The suite added in this installment, and the totals at commit `738105a`.

| Suite | Function | Checks | Result |
|---|---|---|---|
| Vector fields on the scan model | `run-vec-scan-demo` | 18 | 18 pass |
| All ten suites | from `run-png-demo` to `run-vec-scan-demo` | 135 and the PNG verification | All pass |

The new suite checks that a terminated field's read expands to one
deserializer call per field; integers, and characters mixed with integers, in
a CP037 field; signed elements with a `#xFF` terminator, including `-1` and an
out-of-range `-129`; enumerated elements written from keywords and read back,
with a keyword outside the enum and one in a field without an enum; an unnamed
terminated field; a `:case` field whose vector branch inherits `:end-by`, and
one whose enum decodes a scalar branch and a vector branch; and two-byte
little-endian elements. The ten suites were run on checkouts of `738105a` and
of `6660c7f`, the restructured tree, with ASDF limited to each checkout, and
gave the same results.

### Files

Table: Files changed in commits `0b03854` and `738105a`.

| File | Action | Description |
|------|--------|-------------|
| `scan.lisp` | Modified | Vector assembler on both sides, element width, enums in every plan, `codec-form`, `terminator-code-form` |
| `specops.lisp` | Modified | One clause per macro for vector, LEB and `:case` fields; old branches removed |
| `manifest.lisp` | Modified | The vector scan suite |
| `doc/Plan.ManifestComplete.md` | Modified | Progress installment of 2026-10-30 |
| `doc/Log.Manifest.md` | Modified | This update |

### Metrics

- Test checks: 135 automated checks plus 1 structural PNG verification, up
  from 117.
- Regressions at `738105a`: 0.

### Outstanding Work

`:vec` on the scan model, the first new item of the update of 2026-10-30, is
done. Remaining:

- **Dead code.** `unmarshal`'s `term-code-form` and the `:end-by` branch and
  optional arguments of its `vec-count-form` are no longer called, and the
  LEB128 encoders remain as comments.
- **Agreed but not built:** fixed buffers holding a terminator
  (SPECOPS-D18), multi-element terminators
  (SPECOPS-D19), and user-defined stop rules with the
  runtime fallback (SPECOPS-D20); each adds a rule or
  an assembler to the scan model.
- **Vectors of LEB elements.**
- **Keyword defaults on enumerated fields**, still not mapped through the enum.
- **Unknown enum codes**, still read as `nil`.
- **Error wording.** A selector placed after its `:case` field is still
  reported against the manifest's name.

## Update 2026-10-31 — The scan model completed: fixed buffers, multi-element terminators, LEB vectors and user stop rules

This installment covers commits `f7713c5` through `d030f5c`, which complete
the scan model of SPECOPS-D45 as the Manifest Completion Plan
laid out. It builds three of that plan's decisions:
SPECOPS-D18 (fixed buffers holding a terminator),
SPECOPS-D19 (terminators of several elements) and
SPECOPS-D20 (user-defined stop rules with a runtime
fallback). It also adds vectors of LEB128 elements. The groundwork for all
four came first, in a commit that changed no behavior. Four suites bring the
checks from 135 to 202. The code is now in `src/manifest/`, and references
are pinned to `d030f5c`.

Between this update and the last, commits `5e0976e` and `d524c3c` taught
`masque` and `unmasque` decimal mask strings:
`src/serial.lisp:quantify-mask-string@d524c3c` now also returns the
decimal factor. The manifest layer's two callers were adapted, but
`unmarshal` still rejects decimal masques.

### SPECOPS-D52 — A scan keeps a position, and rules report their extent apart from their count

**Status:** Implemented
**Context:** `emit-scan` computed each element's offset as the cursor plus
the element index times a fixed width, and moved the cursor by the number of
elements read. Vectors of LEB elements have no fixed width. A fixed buffer
holding a terminator consumes its whole size while yielding fewer elements.
A terminator of several elements leaves its first elements among the data,
and a user rule decides at run time whether the element that stops it is
data.
**Decision:** `src/manifest/scan.lisp:emit-scan@d030f5c` keeps a position
variable and takes a function that reads one element at it and advances it.
Rules report two facts: `known-count`, the data elements, and
`src/manifest/scan.lisp:known-extent@d030f5c`, the elements consumed when that
is fixed. When the extent is known and the elements have a fixed width, the
source is checked once before the loop and the cursor moves by the whole
extent; otherwise each read is checked and the cursor ends at the position. A
rule may return a `:trailer` form, the number of collected elements to drop
at the end, and `:keep :dynamic`, for an `:after` form that returns `nil`,
`:keep` or `:drop`. `src/manifest/scan.lisp:emit-frame@d030f5c` counts the
elements it writes and passes the counter to rules, which may also add a
prelude.
**Alternatives:** A separate expander for each kind of field (rejected: the
reason for SPECOPS-D45); collecting every element into a buffer
and trimming afterwards (rejected: the trailer drops elements only when a
rule asks).

### SPECOPS-D53 — Element plans nest, and a vector of LEB elements is a scan over an LEB scan

**Status:** Implemented
**Context:** The modifier set allows `(:u 1 :vec :leb)`
(SPECOPS-D32). Each element is itself a variable-length
field, so it needs its own stop rule and assembler.
**Decision:** A scan plan may hold an element plan. Reading, each element is
read by the element plan with the outer scan's position as its cursor;
writing, each element is written by the element plan. For a vector of LEB
elements, `src/manifest/scan.lisp:field-plan@d030f5c` builds an inner LEB scan
that carries the sign and an outer vector with no bit width, which compares
element values with a terminator, decodes the enum and builds an integer
vector. Two forms are expansion errors for now. A vector of LEB elements
cannot be a fixed buffer, since skipping the rest of the buffer means reading
elements of unknown width. And `unmarshal` cannot count such a vector from a
`:length-of` slot, which gives bytes rather than elements; `marshal` still
fills that slot with the byte length.
**Alternatives:** A dedicated LEB-vector assembler (rejected: nesting plans
serves any element kind); counting a `:length-of` vector by reading until the
bytes are consumed (deferred: a units rule, noted under Outstanding Work).

### SPECOPS-D54 — A fixed buffer must leave room for a whole terminator

**Status:** Implemented
**Context:** SPECOPS-D18 pads a short buffer with the
terminator repeated, and SPECOPS-D19 allows terminators
of several elements. Data may leave fewer places in a buffer than such a
terminator needs.
**Decision:** On write, data that neither fills the buffer exactly nor leaves
room for the whole terminator signals an error. Otherwise the terminator is
written once and the rest of the buffer is filled by repeating it, cut off at
the buffer's end. On read, the pattern is matched by
`src/manifest/scan.lisp:pattern-matcher@d030f5c`, a ring buffer of the last
elements read, and the elements of a matched terminator collected as data are
dropped; a buffer that ends at its count without a match keeps all its
elements. On write, the data and then the terminator are fed through the same
matcher, so a pattern that straddles the two is caught as well as one within
the data. A buffer can be written only with an integer `:count` for now;
reading a buffer sized by an earlier field works.
**Alternatives:** Writing a partial terminator (rejected by the steward: a
reader would take it as data); requiring a terminator in every buffer
(rejected in SPECOPS-D18).

### SPECOPS-D55 — User stop rules run through a five-function protocol until they define effectors

**Status:** Implemented
**Context:** SPECOPS-D20 lets a field name a user's
stop rule with `:stop-by`, to run through a runtime protocol until the rule
has effector methods. The protocol's shape, and how a rule gets its
parameters and state, remained to be settled.
**Decision:** The protocol is five generic functions, each with a default on
`stop-rule`: `src/manifest/scan.lisp:done-p@d030f5c`, asked before each read;
`src/manifest/scan.lisp:feed@d030f5c`, which returns `nil`, `:keep` or
`:drop` after each; `trailer`; and, for writing, `frame-element`, given each
element and whether it is the last, and `frame-trailer`, the codes to append.
`:stop-by` names a class or a class with constant initargs.
`src/manifest/scan.lisp:user-rule@d030f5c` makes an instance at expansion,
for dispatch, so the class, and any effector methods, must be defined at
compile time. The default effectors make a fresh instance per field read or
written, through `runtime-rule-form`, and call the protocol on it. A class
that defines `stop-effector` or `frame-effector` gets inline code instead,
with no change to its manifests. `:stop-by` applies to `:vec` fields, not
with `:end-by` or `:count`; `defmanifest` rejects it on other fields. The
protocol does not check written data against the rule; that is the rule's
own concern.
**Alternatives:** A read-only fallback (rejected by the steward: a rule with
only runtime methods should work both ways); a closure-based protocol like
`serializer-for` (rejected in SPECOPS-D45); one instance per
manifest (rejected: two fields with the same rule would share state).

### Implementation

- **Groundwork, `f7713c5`.** `emit-scan`, `emit-frame` and `known-extent` per
  SPECOPS-D52; the vector assembler trims its fill pointer by a
  trailer. Dead code went: `unmarshal`'s `term-code-form` and the `:end-by`
  branch of its `vec-count-form`, and the commented-out LEB128 encoders, old
  `field-plan` and old vector assembler. All ten suites passed unchanged.
- **Fixed buffers, `b7d4f13`.** `bounded-terminator-rule`, a subclass of
  `terminator-rule` with a size: it stops before a read at its size, after one
  at the terminator, and on write checks the size and pads.
  `src/manifest/scan.lisp:field-plan@d030f5c` builds it for `:count` with
  `:end-by`, where it signalled "not supported yet". `marshal` passes the size
  when `:count` is an integer.
- **Multi-element terminators, `f60ca9f`.** The terminator rule holds a list
  of codes, from `src/manifest/scan.lisp:terminator-code-forms@d030f5c`.
  Reading and writing share `pattern-matcher` and
  `src/manifest/scan.lisp:terminator-frame@d030f5c`; a one-element terminator
  keeps its plain comparison. The fixed buffer reuses the terminator rule's
  read fragments through `call-next-method`.
- **LEB vectors, `96b4e8b`.** The scan plan's element slot, the outer
  vector's integer element type, and `check-leb-width`, now shared by scalar
  LEBs and LEB vectors. `unmarshal` rejects a LEB vector with neither
  `:count` nor `:end-by`.
- **User stop rules, `d030f5c`.** The protocol, the default effectors and
  `user-rule`. `stop-rule` remembers its constant initargs.
  `src/manifest/manifest.lisp:defmanifest@d030f5c` stores `:stop-by` and
  rejects it on fields that are neither `:vec` nor `:case`.
  `src/manifest/manifest.lisp:marshal@d030f5c` now accepts stored keys it
  does not read, which a new key had made fail.

### Verification

Run via:

```
sbcl --non-interactive --eval '(asdf:load-system :specops/demo.png :force t)' \
  --eval '(specops::run-png-demo)' --eval '(specops::run-unmarshal-demo)' \
  --eval '(specops::run-chunk-unmarshal-demo)' --eval '(specops::run-masque-demo)' \
  --eval '(specops::run-string-demo)' --eval '(specops::run-pad-demo)' \
  --eval '(specops::run-terminated-demo)' --eval '(specops::run-leb-demo)' \
  --eval '(specops::run-case-demo)' --eval '(specops::run-vec-scan-demo)' \
  --eval '(specops::run-bounded-demo)' --eval '(specops::run-multi-terminator-demo)' \
  --eval '(specops::run-leb-vector-demo)' --eval '(specops::run-stop-by-demo)'
```

Table: The suites added in this installment, and the totals at commit `d030f5c`.

| Suite | Function | Checks | Result |
|---|---|---|---|
| Fixed buffers holding a terminator | `run-bounded-demo` | 19 | 19 pass |
| Terminators of several elements | `run-multi-terminator-demo` | 19 | 19 pass |
| Vectors of LEB elements | `run-leb-vector-demo` | 16 | 16 pass |
| User stop rules | `run-stop-by-demo` | 13 | 13 pass |
| All fourteen suites | from `run-png-demo` to `run-stop-by-demo` | 202 and the PNG verification | All pass |

The new suites check, among other things:

- **Fixed buffers:** short, full and empty buffers; bytes after the terminator
  skipped on read; a codec and two-byte elements; a length slot over a
  buffer; and the facts `known-count` and `known-extent`.
- **Terminators of several elements:** CR LF as a vector and as a string; a
  lone CR and data ending in CR kept as data; the straddling case of `xA`
  with `AA`; and buffers ended by CR LF, including one without room for it.
- **LEB vectors:** counted, signed, counted from an earlier field,
  terminated, enumerated and in a `:case` branch, with a bignum element.
- **User stop rules:** a QUIC-style length rule using `done-p`, with fresh
  state per field; rules using `:keep` with `frame-element`, and `:drop` with
  `trailer` and `frame-trailer`; initargs; and an upgraded rule whose
  expansions call no protocol function but give the same bytes.

Each step was run against all suites before the next, and the text of each
expected error was checked to be the intended one. The style warnings in the
manifest layer number 26 before and after.

### Files

Table: Files changed in commits `f7713c5` through `d030f5c`.

| File | Action | Description |
|------|--------|-------------|
| `src/manifest/scan.lisp` | Modified | Scan groundwork, fixed buffers, multi-element terminators, element plans, the runtime protocol |
| `src/manifest/manifest.lisp` | Modified | `:stop-by` stored and checked; sizes passed on write; dead code removed |
| `test/manifest.lisp` | Modified | Four suites |
| `doc/Log.Manifest.md` | Modified | This update |

### Metrics

- Test checks: 202 automated checks plus 1 structural PNG verification, up
  from 135.
- Regressions at `d030f5c`: 0.

### Outstanding Work

The dead code and the three agreed decisions listed in the previous update are
done, as are vectors of LEB elements. Remaining from that list: keyword
defaults on enumerated fields, unknown enum codes read as `nil`, and the
wording of the selector-order error. New:

- **Exports.** A stop rule defined outside the `specops` package needs
  `stop-rule`, the five protocol functions, and `stop-effector` and
  `frame-effector` exported from `src/package.lisp`.
- **Buffers sized by another field, on write.** Writing a fixed buffer needs
  an integer `:count`; the earlier field's value could come through variables
  like the selector variables of SPECOPS-D47.
- **A units rule.** A rule that stops when the bytes read reach a length,
  and signals an error on overrun, would let `unmarshal` read a vector of LEB
  elements counted by a `:length-of` slot, and later read fields that fill
  the rest of a region.
- **Fixed buffers of LEB elements**, which need the units rule or a way to
  skip unread elements.
- **Decimal masques in `unmarshal`**, now that `unmasque` reads them.

## Update 2026-10-31 — Corrections after the scan model

This installment covers commit `2e1af03`, which makes the corrections the
Manifest Completion Plan lists under its first milestone, now that the scan
model is complete. Probing them turned up one more fault, larger than the
rest: a manifest's `:endian` had no effect on its fields. The checks for each
correction went into the suite of the feature it corrects, which brings the
checks from 202 to 217.

### SPECOPS-D56 — A field without a byte order takes its manifest's; strings are in stream order

**Status:** Implemented
**Context:** `process-field` defaulted a field's byte order to big-endian, so
the manifest's `:endian` never reached a field that gave none, and a manifest
declared little-endian wrote its fields big-endian. Every probe set `:endian`
per field, so no suite saw it. Once fixed, a child manifest with a byte order
its parent lacked would index past the parent's serializers, which nested
expansions share. A constant string stored its manifest's byte order, while
both macros read and write strings through the first serializer and
deserializer.
**Decision:** A field's byte order is its own `:endian`, else its manifest's,
else big-endian. `marshal` sizes its serializers from the byte orders of
every field it writes, inside spans and nested manifests, collected by
`src/manifest/manifest.lisp:manifest-swap-specs@2e1af03`. A constant string
stores byte order 0: it is written and read in stream order, and any byte
order belongs to its codec (SPECOPS-D33).
**Alternatives:** Requiring `:endian` on every field (rejected: the manifest's
`:endian` exists to state it once); giving each nested manifest serializers
of its own (rejected: nested expansions share one environment by design,
SPECOPS-D37). Who owns byte order across the system remains
the architecture's open question SPECOPS-O3; this settles it
within a manifest.

### SPECOPS-D57 — A code an enum lacks is read as its integer

**Status:** Implemented
**Context:** `unmarshal` decoded an enumerated field's code to `nil` when the
enum had no keyword for it, so the value was lost and the record could not be
written back.
**Decision:** `src/manifest/manifest.lisp:enumerated@2e1af03` returns the code
itself when the enum names no such code. Known codes still become keywords.
`marshal` already writes integers as they are, so a record holding a code the
enum lacks round-trips. This holds for every field kind, since scalars, LEB
values, vector elements, `:case` values and masque groups all decode through
`enumerated`.
**Alternatives:** Signalling an error (rejected by the steward: files with
newer or vendor codes could not be read at all); keeping `nil` (rejected: the
value is lost).

### SPECOPS-D58 — A correction is tested in the suite of the feature it corrects

**Status:** Implemented
**Context:** The corrections could have been tested by a suite of their own.
Such a suite gathers checks by when their faults were found rather than by
what they cover, and must change whenever any of those features does.
**Decision:** Each correction's checks join the suite of the field kind or
directive it corrects, and the log records how many each suite gained.
**Alternatives:** A corrections suite (rejected by the steward, for the
reasons above).

### Implementation

- **Byte orders, SPECOPS-D56.** In
  `src/manifest/manifest.lisp:defmanifest@2e1af03`, a field's `:endian`
  defaults to `nil`, the manifest's byte order to big-endian, and a `str`
  directive stores byte order 0. In `src/manifest/manifest.lisp:marshal@2e1af03`,
  `manifest-swap-specs` replaces the byte-order collection in
  `preprocess-spec`.
- **Keyword defaults.** A default goes through the enum like a given value,
  in `marshal`'s `scalar-value-form` and its masque binder. The scalar clause
  of `generate`, which repeated `scalar-value-form`, now calls it.
- **Unknown codes, SPECOPS-D57**, in `enumerated`.
- **Error wording.** `field-plan` passes a field's name to `selector-form`,
  and `unmarshal`'s `var-of` names both fields: "unmarshal: field :V needs
  field :KIND, which isn't read before it." `marshal`'s message names both as
  well.
- **Unused variables.** `defmanifest` accepts `:index`, `:vextend-by` and
  `:count-from` and declares them ignored. Removed: the static `index` counter
  and the `unincrementing` argument of `process-field` that served it, the
  unused `:length` key of `process-field`, `olength` in `marshal`, the unused
  keys of `generate`'s destructuring, and `count` in `unmarshal`'s
  `process-item`. The manifest layer compiles with no style warnings, where it
  had 26.
- **Exports.** `src/package.lisp@2e1af03` exports `stop-rule`, the five
  protocol functions of SPECOPS-D55, `stop-effector` and
  `frame-effector`, so that a stop rule can be defined in another package. No
  module that uses `specops` defines any of these names.

### Verification

The run command is that of the previous update.

Table: Checks added to existing suites in commit `2e1af03`.

| Suite | Checks | Added |
|---|---|---|
| `run-unmarshal-demo` | 8 → 12 | A manifest-level `:endian :little` with one field overriding it, both ways; a keyword default; a code the enum lacks, read and written back |
| `run-chunk-unmarshal-demo` | 9 → 13 | A little-endian child in a big-endian parent, and the reverse, both ways |
| `run-masque-demo` | 10 → 11 | A keyword default in a masque group |
| `run-string-demo` | 9 → 11 | A constant string in a little-endian manifest, both ways |
| `run-leb-demo` | 17 → 18 | A keyword default on a LEB |
| `run-case-demo` | 27 → 28 | A keyword default for a `:case` value; the selector default back to `:word`; both selector-order messages name the field and its selector |
| `run-vec-scan-demo` | 18 → 19 | A code the enum lacks, in a vector, read and written back |
| `run-stop-by-demo` | 13 → 14 | A stop rule defined in another package through the exported names |
| All fourteen suites | 202 → 217 | All pass, with the PNG verification |

Two checks were confirmed to fail without their fix. Without the nested
byte-order collection, or without the exports, `test/manifest.lisp` does not
compile. The architecture systems that loaded before, `specops.arch.6502`,
`.s390x`, `.x86`, `.z80` and `.wasm`, and the three format systems load as
before; x86's 14 full warnings are unchanged. The five test scripts in `test/`
pass, among them the GOFF writer's, which uses manifests.

### Files

Table: Files changed in commit `2e1af03`.

| File | Action | Description |
|------|--------|-------------|
| `src/manifest/manifest.lisp` | Modified | Byte orders, keyword defaults, unknown codes, error wording, unused variables |
| `src/manifest/scan.lisp` | Modified | `field-plan` passes the field's name to `selector-form` |
| `src/package.lisp` | Modified | Exports for user stop rules |
| `test/manifest.lisp` | Modified | Checks in eight suites |
| `doc/Log.Manifest.md` | Modified | This update |

### Metrics

- Test checks: 217 automated checks plus 1 structural PNG verification, up
  from 202.
- Regressions at `2e1af03`: 0.
- Style warnings in the manifest layer: 0, down from 26.

### Outstanding Work

From the previous update, the keyword defaults, unknown enum codes, error
wording and exports are done. Remaining: writing a buffer sized by another
field, the units rule, fixed buffers of LEB elements, and decimal masques in
`unmarshal`. The Manifest Completion Plan's other milestones are unchanged.

## Update 2026-11-01 — Records and round trips

This installment covers commits `fed7f35` through `4f9c6d4`, which give the
manifest layer records: `marshal` takes field values from a record when its
code runs, `defmanifest` can define a record class, `unmarshal` can return an
instance of it, and a record read from a file writes the file back. They
answer the Manifest Completion Plan's open questions
SPECOPS-O8 and SPECOPS-O9, and
build its SPECOPS-D15 with a class in place of the
struct it named. A new suite brings the checks from 217 to 258.

### SPECOPS-D59 — A manifest's record is a CLOS class, defined on request, with its names in the class's package

**Status:** Implemented
**Context:** SPECOPS-D15 proposed that `defmanifest`
always define a struct. SBCL treats redefining a struct with different slots
as an incompatible redefinition, which stops with an error until the image is
restarted, and a manifest's fields change often while a format is being
written. Defining a type for every manifest could also clash with names
already in use.
**Decision:** `defmanifest` defines a record class when its parameters give
`:record`: `t` names the class after the manifest, and a symbol names it
explicitly. `src/manifest/manifest.lisp:define-manifest-record@4f9c6d4`
defines the class, with a slot per named field, an initarg of the field's
keyword and an accessor named after the class and the field; a constructor,
`make-` and the class name; a `field-value` method; and a `print-object`
method that shows the fields. The slots are the fields that `unmarshal`'s
plist has, both being taken from
`src/manifest/manifest.lisp:manifest-field-names@4f9c6d4`: span children at
the span's level, masque keyword groups, sub-manifest fields and slot fields,
but not unnamed fields. Derived names are interned in the package of the
class's name. Because the class is defined from the stored fields,
`defmanifest` expands into the registration followed by
`define-manifest-record`, which reads them once the registration has run at
compile time.
**Alternatives:** A struct, as SPECOPS-D15 proposed
(rejected by the steward: the redefinition problem above); a record for every
manifest (rejected by the steward: needless types and possible clashes);
derived names in the package current at the `defmanifest` form (rejected by
the steward in favor of keeping them beside the class).

### SPECOPS-D60 — `marshal` takes a run-time record through an options list

**Status:** Implemented
**Context:** `marshal` took each field's value from its literal arguments at
expansion, so a sub-manifest's values had to be a literal plist in the call,
and a record built or read at run time could not be written
(SPECOPS-O9). A reserved keyword argument such as
`:from` would clash with a format that has a field of that name.
**Decision:** `marshal`'s arguments may begin with a list of options, which
cannot be taken for a field since fields begin with keywords:
`(marshal png-ihdr buf (:from record) :height 600)`. A field's value is its
literal argument, else `(field-value record field)` when the code runs, else
its default. `src/manifest/manifest.lisp:field-value@4f9c6d4` is a generic
function with a method for plists, to which each record class adds its own,
so plists and records share one generated path. A field with no value in the
record and no default signals an error naming the field and the manifest when
the code runs. Slot fields are never read from a record: lengths, offsets and
checksums are recomputed, so an edited record is written consistently. A
sub-manifest field's record value is bound once and passed to the child's
expansion as its own `:from`, at any depth, and literal values for the child
still override it. An unknown option is an expansion error.
**Alternatives:** A reserved key (rejected by the steward: the clash above);
fetching values with `getf` only (rejected: records would need converting);
separate generated paths for plists and records (rejected: one generic
function serves both at the cost of a dispatch per field).

### SPECOPS-D61 — `unmarshal` takes its options in its params, `(:as :record)` among them

**Status:** Implemented
**Context:** `unmarshal` returned values for given keys and a plist otherwise
(SPECOPS-D40). Records needed a way to ask for them, and
SPECOPS-O8 asked where `unmarshal`'s options belong.
**Decision:** Options go in `unmarshal`'s `params` argument, which already
carried the internal sub-lexicon: `(unmarshal png-file source (:as :record))`
returns an instance of the manifest's record class, and `:as :plist` is the
default. Nested sub-manifests are read as records when they have a record
class, and as plists inside the parent's record when they do not. An unknown
option, an `:as` other than `:plist` or `:record`, `:as :record` with keys,
and `:as :record` for a manifest without a record class are expansion errors.
**Alternatives:** Options after the manifest's name, as in
`(unmarshal (png-file :record) …)` (rejected in
SPECOPS-O8: the name should stay a plain symbol);
always returning a record when the manifest has one (rejected: the plist stays
the default, as callers already expect).

### SPECOPS-D62 — Round trips are canonical

**Status:** Implemented
**Context:** A record read by `unmarshal` and written by `marshal` should
reproduce its source, but a record holds only named fields.
**Decision:** A round trip reproduces a canonical source exactly, and writes
any other source in canonical form. Pad bytes are written with the pad's fill
value, unnamed fields with their defaults, and the elements after a
terminator in a fixed buffer as terminators; slot fields are recomputed, so
they match a source only if it was consistent. The limits are tested, not
just stated.
**Alternatives:** Keeping pad bytes, unnamed fields and buffer tails in
hidden slots so that any source is reproduced (rejected by the steward: much
more work for sources that are malformed or unusual).

### Implementation

- **Run-time records, `fed7f35`.** In
  `src/manifest/manifest.lisp:marshal@4f9c6d4`, the options list is taken from
  the arguments before anything reads them, and its record is bound once. A
  local `value-source` chooses each field's value for the masque binder,
  `selector-form`, `scalar-value-form` and `vec-source-form`; for a vector,
  the fallback is its vector default, an empty string for a terminated field,
  or its repeated numeric default. `field-value` is exported.
- **Record classes, `360acc0`.** `defmanifest` accepts `:record` and stores
  the class's name in the manifest's configuration entry;
  `define-manifest-record` and `manifest-field-names` are new, the latter
  lifted out of `unmarshal`.
- **Records out, `1e8938d`.** In `src/manifest/manifest.lisp:unmarshal@4f9c6d4`,
  the `:as` option, its checks, the result built with `make-instance`, and
  `:as :record` passed to nested expansions.
- **Round-trip tests, `4f9c6d4`.** The PNG manifests of the test file gained
  `:record t`; their plist behavior is unchanged.

### Verification

The run command is that of the update of 2026-10-31 for the scan model, with
`(specops::run-record-demo)` added.

Table: The suite added in this installment, and the totals at commit `4f9c6d4`.

| Suite | Function | Checks | Result |
|---|---|---|---|
| Records and round trips | `run-record-demo` | 41 | 41 pass |
| All fifteen suites | from `run-png-demo` to `run-record-demo` | 258 and the PNG verification | All pass |

The new suite checks, among other things:

- **Run-time records:** a compiled function, `test/manifest.lisp:write-ihdr-from@4f9c6d4`,
  writing two different plists; defaults, enum values given three ways,
  overrides and a missing field; nested records whose length and CRC are
  recomputed; `:case`, vector and masque fields from records.
- **Record classes:** named after the manifest and explicitly, with accessors
  in another package; editing through accessors; redefining a manifest with a
  new field.
- **Records out:** a record holding a record, a record holding a plist, and
  the option errors.
- **Round trips:** the 1×1 PNG through a plist and through records; an edit
  to its IHDR (Adam7 interlacing), accepted by the independent PNG verifier;
  an edit to its IDAT, which gives exactly the bytes of a PNG built with that
  pixel; and the 80-byte GOFF END probe. Outside the suite, both edited files
  were decoded by Python's PIL, which read the interlaced image and the green
  pixel.
- **Limits:** pad bytes, buffer tails and an unnamed field written back in
  canonical form.

The manifest layer has no style warnings, and the five test scripts in
`test/` pass.

### Files

Table: Files changed in commits `fed7f35` through `4f9c6d4`.

| File | Action | Description |
|------|--------|-------------|
| `src/manifest/manifest.lisp` | Modified | `field-value`, the options of `marshal` and `unmarshal`, record classes |
| `src/package.lisp` | Modified | Exports `field-value` |
| `test/manifest.lisp` | Modified | The records suite; records for the PNG manifests |
| `doc/Log.Manifest.md` | Modified | This update |

### Metrics

- Test checks: 258 automated checks plus 1 structural PNG verification, up
  from 217.
- Regressions at `4f9c6d4`: 0.

### Outstanding Work

- **Destinations that cannot be patched.** A manifest with a `:length-of`
  slot cannot be written to a function, record or not; buffering is the
  plan's SPECOPS-D13.
- **Exact round trips** of sources that are not canonical, should a format
  need them (SPECOPS-D62).
- The items left from the previous update remain.

## Update 2026-11-01 — Variable structure

This installment covers commits `a12b394` through `d7ba27f`, which build the
Manifest Completion Plan's SPECOPS-D16 and answer its
SPECOPS-O10. A sub-manifest became a field plan, so
that a `:case` field can choose one and a manifest can repeat one: by a
count, by a count field, until an instance matches a condition, to the end of
the source, or until a length slot's bytes are used. Count fields can be
computed on write. One manifest now reads and writes any sequence of PNG
chunks. The checks went from 258 to 292.

### SPECOPS-D63 — A sub-manifest is a field plan, expanded by the macro that holds its parent

**Status:** Implemented
**Context:** Both macros expanded a sub-manifest in a clause of their own,
which kept the region map and the shared environment. A sub-manifest could
therefore appear only as a field by itself, not as a `:case` branch or as the
element of a scan.
**Decision:** `src/manifest/scan.lisp:manifest-plan@d7ba27f` is a field plan
holding a sub-manifest's name. Its effectors call functions the macros pass
in their context, `read-manifest` and `write-manifest`, which expand the
child at a given cursor, or from given literal values and a record, sharing
the parent's environment. The region bookkeeping for the field stays with the
code that writes the field. The first commit made this change alone, and the
expansions of nested manifests were compared before and after: identical once
gensym names were normalized.
**Alternatives:** A sub-manifest kind of plan that expands the child itself
(rejected: the child's code must share the macros' lexical environment, which
only they hold); a separate mechanism for `:case` branches and repeats
(rejected: one plan serves both, and nests).

### SPECOPS-D64 — A `:case` branch may be a sub-manifest, `(key (manifest name))`

**Status:** Implemented
**Context:** Chunked formats choose a record's layout by a tag, as PNG does
by a chunk's type. SPECOPS-D16 asked for `:case` to
choose a sub-manifest, with a default branch.
**Decision:** A branch written `(key (manifest name))` is stored like the
`manifest` directive and read and written through a manifest plan. Keys stay
codes through the selector's enum (SPECOPS-D47), so a tag the
enum lacks takes the `otherwise` branch, which may read raw bytes counted by a
length slot. `marshal` gives each kind of branch its value: a sub-manifest
branch takes literal nested values, or a record form, from the given value or
a `:from` record; other branches take a value form. Nested values for a
branch that is not a sub-manifest are an expansion error when a constant
selector chooses it, and a run-time error otherwise. Since every branch is
compiled when the selector is known only at run time, a vector field now
accepts any sequence, coerced to a vector once.
**Alternatives:** A bare symbol naming the manifest, `(key name)` (rejected by
the steward: `(manifest name)` mirrors the directive and cannot be taken for
a width spec); string keys compared with `equalp` (rejected: an enum of the
tags' codes keeps keys as integers).

### SPECOPS-D65 — A repeated sub-manifest is a scan whose elements are instances

**Status:** Implemented
**Context:** SPECOPS-D16 asked for sub-manifests
repeated by a count, by a count or length field, or until an instance
matching a condition on its fields, read as a list. A PNG file is a signature
and chunks up to IEND.
**Decision:** Options after the child's name in the `manifest` directive make
it a repeat: `:count n`, `:count :field`, `:until (field value …)`,
`:until :end`, or `:until :length` (SPECOPS-D67). In
`src/manifest/scan.lisp:field-plan@d7ba27f` a repeat is a scan plan whose
element is the manifest plan, whose assembler,
`src/manifest/scan.lisp:list-assembler@d7ba27f`, collects a list on read and
walks any sequence on write, and whose stop rule is a count rule,
`src/manifest/scan.lisp:end-rule@d7ba27f`, the units rule, or
`src/manifest/scan.lisp:until-rule@d7ba27f`. The until rule keeps the
instance that matches. It reads fields with `field-value`, so plists and
records serve alike, and compares codes through the enum of the child's
field, so a value given as a keyword or as a code matches either way. On
write it checks, as for terminators, that only the last instance matches,
that it does, and that there is at least one. A repeat given no value, or
`nil`, has no instances, since a record read from a file with none holds
`nil`. A scan of instances checks before each that the source has not run
out.
**Alternatives:** A separate `repeat` directive (rejected by the steward:
options on `manifest` keep one directive for sub-manifests); conditions
written as code (rejected: no logic in manifests,
SPECOPS-D29); an empty repeat as an error (rejected: it must
round-trip).

### SPECOPS-D66 — Slots inside a repeated instance resolve within that instance

**Status:** Implemented
**Context:** Regions are named by keywords fixed at expansion, made unique per
sub-manifest field by prefixes (SPECOPS-D38). A repeat writes
its child's code once and runs it for each instance, so every instance has
the same region names. SPECOPS-O10 asked how repeated
instances are named and addressed.
**Decision:** Each instance pushes fresh regions onto the map as it is
written, as spans already did, so a slot inside an instance, such as a chunk's
length or CRC, resolves within that instance. A slot outside a repeat can
cover the whole repeat, but not one instance of it.
**Alternatives:** Names built at run time, such as `:chunks/3/crc`, so that a
slot outside a repeat could address one instance (rejected by the steward as
more machinery than chunked formats need; possible later).

### SPECOPS-D67 — A length slot bounds elements of varying width in units

**Status:** Implemented
**Context:** A `:length-of` slot gave a vector its element count by dividing
the length by the element width. The elements of a vector of LEBs, and the
instances of a repeat, have no fixed width, so `unmarshal` could not read
them by a length.
**Decision:** `src/manifest/scan.lisp:units-rule@d7ba27f` reads elements until
the length a `:length-of` slot gives is used, and signals an error when an
element crosses its end. `unmarshal` passes `field-plan` a function giving the
length field's variable. The rule serves a LEB vector with no `:count` and a
repeat written `(manifest :items item :until :length)`; the option is needed
because a length slot can also cover a single sub-manifest. On write the rule
adds nothing: the slot is computed from what is written.
**Alternatives:** Inferring a repeat from a length slot over a `manifest`
field (rejected: a single sub-manifest may have one, as PNG's IHDR chunk
does); a units rule for fixed-width vectors too (unneeded: division already
gives their count, and lets the vector be allocated in advance).

### SPECOPS-D68 — A `:count-of` slot is backpatched with the elements written

**Status:** Implemented
**Context:** Formats such as WOFF hold a count field before the entries it
counts. Callers supplied count fields themselves.
**Decision:** A field declared `(:n :u16 :slot (:count-of :entries))` is
written as a placeholder and backpatched, like a `:length-of` slot, with the
number of elements written to the field it covers, which must be a vector or
a repeated sub-manifest. `src/manifest/scan.lisp:emit-frame@d7ba27f` returns
that number, and `marshal` records it in a variable shared with nested
expansions, beside the region map. On read the field is an ordinary count
field, which the covered field names with `:count :n`. As for other slot
fields, a value given for it, or held in a record, is ignored.
**Alternatives:** Computing the count before the covered field is written
(rejected: the sequence would be evaluated twice, or held apart from the
field that writes it); a count slot also used by reads to infer the count
(rejected: `:count :n` already states it).

### Implementation

- **Manifest plans, `a12b394`.** `manifest-plan` and its two delegating
  effectors; `read-manifest` in `src/manifest/manifest.lisp:unmarshal@d7ba27f`
  and `write-manifest` in `src/manifest/manifest.lisp:marshal@d7ba27f`. The
  `manifest` directive's trailing values, once passed to the child as static
  values and used nowhere, are no longer passed; they now hold a repeat's
  options.
- **`:case` with sub-manifests, `17d30fd`.**
  `src/manifest/manifest.lisp:manifest-branch-p@d7ba27f` recognizes the
  branch form in `defmanifest`; `marshal`'s `case-value-context` gives each
  kind of branch its value; `unmarshal` passes `read-manifest` to plans. The
  vector assembler coerces its value to a vector.
- **Repeats, `d4b1f5a`.** The until and end rules, the list assembler,
  `src/manifest/scan.lisp:until-conditions@d7ba27f` and
  `src/manifest/scan.lisp:until-test-form@d7ba27f`, the repeat branch of
  `field-plan`, and the `manifest` clauses of both macros. `scan.lisp`
  declares the registries special, since `manifest.lisp` defines them and is
  loaded after it. `emit-scan` passes the source to rules and checks before
  each element of varying width that the source has not run out.
- **The units rule, `2caa4e2`.** The rule, and `unmarshal`'s `length-form`.
- **`:count-of`, `d7ba27f`.** `emit-frame`'s count, `marshal`'s `counts`
  variable and `counted-form`, the `:count-of` case of `resolve-region`, and
  the check in `preprocess-spec` that a `:count-of` slot covers a vector or a
  repeat. A repeat's missing value became no instances, and an empty `:until`
  repeat an error on write.

### Verification

The run command is that of the update of 2026-10-31 for the scan model, with
`(specops::run-record-demo)` and `(specops::run-variable-demo)` added.

Table: Suites added or extended in commits `a12b394` through `d7ba27f`, and the totals at `d7ba27f`.

| Suite | Checks | Added |
|---|---|---|
| `run-variable-demo` | 32 | New. The generic `png-chunk` with an IHDR branch and a raw branch, both ways, with a run-time selector, as records, and with a type the enum lacks; `png-any` reading and writing the 1×1 PNG through plists and records, and a PNG with a `tEXt` chunk and two IDATs; the `:until` checks; counted, fixed, to-the-end and length-bounded repeats; `:count-of` over a repeat and a vector; the expansion errors |
| `run-vec-scan-demo` | 19 → 20 | A list as a vector field's value |
| `run-leb-vector-demo` | 16 → 17 | A LEB vector read by its length slot, and an element crossing its end; this replaces the check that it was an error |
| All sixteen suites | 258 → 292 | All pass, with the PNG verification |

The test file's `test/manifest.lisp:verify-png@d7ba27f` now accepts ancillary
chunks and consecutive IDATs between IHDR and IEND. Outside the suite, the PNG
with a `tEXt` chunk and two IDATs, built by
`test/manifest.lisp:png-with-ancillary-chunks@d7ba27f`, was decoded by Python's
PIL, which read its title and its pixel. The manifest layer has no style
warnings, and the five test scripts in `test/` pass.

### Files

Table: Files changed in commits `a12b394` through `d7ba27f`.

| File | Action | Description |
|------|--------|-------------|
| `src/manifest/scan.lisp` | Modified | Manifest plans, the until, end and units rules, the list assembler, repeats in `field-plan`, `emit-frame`'s count |
| `src/manifest/manifest.lisp` | Modified | Sub-manifest branches, both macros' sub-manifest expansion, repeats, `:count-of` |
| `test/manifest.lisp` | Modified | The variable structure suite; `verify-png` for ancillary chunks |
| `doc/Log.Manifest.md` | Modified | This update |

### Metrics

- Test checks: 292 automated checks plus 1 structural PNG verification, up
  from 258.
- Regressions at `d7ba27f`: 0.

### Outstanding Work

The units rule, listed as outstanding since the scan model was completed, is
done, and with it reading a vector of LEB elements by a length slot. New:

- **Instances addressed from outside a repeat**, by names built at run time,
  should a format need a slot outside a repeat that covers one instance
  (SPECOPS-D66).
- **Trailing `:count-of` and `:length-of` slots.** A slot placed after the
  field it covers is resolved before its own position is known, and writing
  fails with a type error rather than a clear message; only checksum slots are
  computed inline. The value could be written in place, as checksums are, or
  the placement rejected at expansion.
- **Fixed buffers of LEB elements**, which still need a way to skip unread
  elements.
- The other items of the previous updates remain.

## Update 2026-11-01 — Checked reads

This installment covers commit `1cc7600`, which makes `unmarshal` check what it
reads. Checksums, length, offset and count slots are compared with the regions
they cover, including inside repeated instances, and constants are checked as
before. A failed check signals a condition that the caller can answer with a
restart. The work answers the Manifest Completion Plan's open question
SPECOPS-O7. A new suite brings the checks from 292 to
315.

### SPECOPS-D69 — A failed check signals a condition with restarts, and `(:check nil)` leaves the checks out

**Status:** Implemented
**Context:** `unmarshal` read checksum, length and offset slots as plain values,
so a corrupt file passed silently, while a constant that did not match
signalled a plain error, which ended the read.
SPECOPS-O7 asked what a failed check should do. A
caller reading trusted data wants no cost; a caller reading a damaged file
wants to see its contents anyway; most callers want corruption reported.
**Decision:** A failed check signals `manifest-check-failed`, a subclass of
`simple-error`, through
`src/manifest/manifest.lisp:check-failed@1cc7600`. The condition holds the
manifest, the field, the kind of check (`:checksum`, `:length`, `:offset`,
`:count` or `:constant`), the expected and actual values and the position, with
exported readers. A `continue` restart accepts the value read and reads on. For
a slot field, a `use-value` restart substitutes another value for the field, so
a caller can take the recomputed checksum and get a consistent record. A
constant has no field to substitute, so it offers `continue` only; its message
is unchanged. Checks are on by default. `(:check nil)` in `unmarshal`'s params
leaves every check out of the expansion, including those of nested manifests,
since a restart cannot spare the cost of computing a checksum. After a length
fails, `continue` reads on from where the field ended.
**Alternatives:** A plain error with an option to turn checks off, as
SPECOPS-O7 suggested (rejected by the steward: a
damaged file could be read only in a separate mode, and the record could not be
repaired as it is read); returning a status with the values (rejected: a caller
that ignores it lets corrupt data pass); restarts with no option (rejected by
the steward: the checks' work could not be skipped).

### SPECOPS-D70 — Checks are placed at expansion from a region table, with variables only for the regions they need

**Status:** Implemented
**Context:** `marshal` records regions in a list built as its code runs,
pushing an entry for every region of every instance
(SPECOPS-D35). Read the same way, a file of many chunks would
allocate for each of them. Slots inside a repeated instance must resolve
within it (SPECOPS-D66), and selective reads
(SPECOPS-D14) will need to know which fields and spans
lie where.
**Decision:** `src/manifest/manifest.lisp:manifest-regions@1cc7600` builds a
region table at expansion: each named field and span, with the span holding it
and the slots that cover it, and each slot, with its kind, its region and its
checksum function. In `src/manifest/manifest.lisp:unmarshal@1cc7600` the table
decides which variables to bind: a start and an end for each region a check
needs, and a position for each checked slot field. Nothing else is emitted.
Each check is emitted once its slot field has been read and its region closed,
so a slot may come before or after its region. A length is compared with the
region's end less its start, an offset with its start, a checksum with the
slot's function over the source from start to end, and a count with the number
of elements in the field it covers. A count check is left out when the covered
field is counted by the slot's own field, since the two then agree. Nested
expansions bind their own variables, so each repeated instance is checked
against its own regions with no allocation. A slot over a region the manifest
lacks, or a checksum without `:by`, is an expansion error. Note that the
implementation currently only supports writes where a checksum comes after
its region.
**Alternatives:** A list of regions built at run time, as `marshal` does
(rejected: it allocates for every region of every instance); a table holding
only what checks need (rejected by the steward: the general table costs nothing
when the code runs, and selective reads can extend it).

### SPECOPS-D71 — An integer source is checksummed through a buffer of its units, made once

**Status:** Implemented
**Context:** `unmarshal` reads integers and `(units . integer)` pairs as well
as vectors (SPECOPS-D39), but a checksum function reads a
buffer. Integer sources are mostly packed values, rarely checksummed. Whether
the source is an integer is known only when the code runs.
**Decision:** When the source is not a vector, a checksum check reads
`src/manifest/manifest.lisp:source-units@1cc7600`, a vector of the source's
units numbered from the most significant, as `deserializer-for` numbers them.
The buffer is made at most once per top-level `unmarshal` and shared with
nested expansions through the sub-lexicon. The variable that holds it is bound
only when the manifest or a manifest nested in it, as a field, a repeat or a
`:case` branch, has a checksum and checks aren't toggled off by `(:check nil)`,
which `src/manifest/manifest.lisp:manifest-checksums-p@1cc7600` decides at
expansion. A bare integer has as many units as its value needs, so leading zero
units are lost, as they are for reading.
**Alternatives:** Skipping checksum checks for integer sources, or rejecting
them (rejected by the steward: the check would be lost, or a source the reader
accepts refused); a fresh buffer for each check (rejected: repeated work in
each instance of a repeat).

### SPECOPS-D72 — Every checksum function takes a buffer, a start and an end

**Status:** Implemented
**Context:** A `:checksum-of` slot calls its function as `(fn buffer start
end)` (SPECOPS-D36), which `checksum.lisp:crc@10b1568` follows.
`adler32-checksum` takes its bounds as keyword arguments, so it would fail
as a `:by` function if called by a manifest, and checked reads call these
functions on every read for manifests with a checksum slot.
**Decision:** `src/checksum.lisp:adler32@1cc7600` wraps `adler32-checksum`
with the slot signature, beside `crc`.
**Alternatives:** Changing `adler32-checksum` (rejected by the steward: it
keeps the interface of the library it came from, and the PNG test's
`zlib-store` calls it); calling functions with keyword bounds from the macros
(rejected: two conventions for one slot).

### Implementation

- **`src/manifest/manifest.lisp`.**
  - The condition `manifest-check-failed` and
    `src/manifest/manifest.lisp:check-failed@1cc7600`, per
    SPECOPS-D69.
  - `manifest-regions`, `manifest-checksums-p` and `source-units`, per
    SPECOPS-D70 and SPECOPS-D71.
  - In `src/manifest/manifest.lisp:unmarshal@1cc7600`, the `:check` option,
    passed to nested expansions with `:as`; the buffer's variable in the
    sub-lexicon; a local `process-item` that records where a region begins and
    ends around `read-item`, the former body; `check-form`, which builds a
    slot's check, and `emit-ready-checks`, which places it; and
    `constant-failed`, which the constant checks of strings, terminators and
    masques now call. A string reports its first differing unit only.
- **`src/checksum.lisp`.** `adler32`, per SPECOPS-D72.
- **`src/package.lisp`.** Exports `manifest-check-failed` and its readers
  `check-manifest`, `check-field`, `check-kind`, `check-expected`,
  `check-actual` and `check-position`.
- **`test/manifest.lisp`.** `test/manifest.lisp:run-checked-read-demo@1cc7600`
  and its helper `test/manifest.lisp:failed-checks@1cc7600`, which answers each
  failure with a restart and collects what it reported.

### Verification

The run command is that of the update of 2026-10-31 for the scan model, with
`(specops::run-record-demo)`, `(specops::run-variable-demo)` and
`(specops::run-checked-read-demo)` added.

Table: The suite added in this installment, and the totals at commit `1cc7600`.

| Suite | Function | Checks | Result |
|---|---|---|---|
| Checked reads | `run-checked-read-demo` | 23 | 23 pass |
| All seventeen suites | from `run-png-demo` to `run-checked-read-demo` | 315 and the PNG verification | All pass |

The new suite checks, among other things:

- **Sound files:** the 1×1 PNG, through `png-file` and `png-any`, and the PNG
  with ancillary chunks, read with no failed check.
- **Checksums:** a corrupt IDAT CRC, with the condition's kind, field,
  manifest, position and message; `continue` keeping the stored CRC;
  `use-value` giving the computed one, in a plist and in a record; a changed
  byte in the second of two IDATs, failing that chunk alone; and Adler-32
  through the wrapper.
- **Lengths, offsets and counts:** an IHDR length of 14 over its 13-byte body;
  a length of 3 over two-byte elements; `:offset-of` written and read back, its
  first test in either direction, and a wrong offset; a `:count-of` slot over a
  repeat to the end of the source, and no check where the count cannot disagree.
- **Integer sources:** a sound and a corrupt checksum, a `(units . integer)`
  pair with a leading zero unit, and the numbering of `source-units`.
- **Constants:** a changed chunk type failing as a constant and then its CRC
  failing, with `use-value` not offered for the constant; a missing terminator
  and a wrong masque group, each read on with `continue`.
- **The option and the errors:** `(:check nil)` reading each of these with no
  failure and leaving every check out of the expansion; a slot over a region
  the manifest lacks, a checksum without `:by`, and an unknown option.

The manifest layer has no style warnings, and the five test scripts in `test/`
pass. The architecture systems `specops.arch.6502`, `.s390x`, `.x86`, `.z80`
and `.wasm`, and the three format systems, load with the new exports.

### Files

Table: Files changed in commit `1cc7600`, and the documents updated in commits
`1b6cafe` and `68285f1`.

| File | Action | Description |
|------|--------|-------------|
| `src/manifest/manifest.lisp` | Modified | The condition and its restarts, the region table, checked reads in `unmarshal`, integer sources |
| `src/checksum.lisp` | Modified | `adler32`, with the slot signature |
| `src/package.lisp` | Modified | Exports the condition and its readers |
| `test/manifest.lisp` | Modified | The checked reads suite |
| `doc/Log.Manifest.md` | Modified | This update |
| `doc/Plan.ManifestComplete.md` | Modified | Progress installment for checked reads; SPECOPS-O7 answered |

### Metrics

- Test checks: 315 automated checks plus 1 structural PNG verification, up
  from 292.
- Regressions at `1cc7600`: 0.

### Outstanding Work

Validation inside `unmarshal`, outstanding since this log began, is done. New:

- **Byte orders of `:case` branches.**
  `src/manifest/manifest.lisp:manifest-swap-specs@1cc7600` collects the byte
  orders of nested manifests but not of sub-manifests chosen by `:case`, so a
  branch whose manifest uses a byte order its parent lacks would index past
  `marshal`'s serializers.
- **Checksum functions are not exported.** A manifest written in another
  package names them as `specops::crc` and `specops::adler32`.
- **Checksums on stream sources** wait on stream sources: a stream cannot be
  read back, so the bytes of a checksummed region must be kept, perhaps in a
  buffer reused across reads.
- The other items of the previous updates remain.

## Update 2026-11-01 — Destinations

This installment covers commit `b63b7d2`, which builds the Manifest Completion
Plan's sixth milestone, destinations. A manifest with slots or checksums can
now be written to a function, a stream or any vector, since `marshal` writes
it through a buffer wherever it cannot be patched in place. The buffer may be
one the caller keeps across calls, which answers the plan's open question
SPECOPS-O12 for writing. A vector too small signals a
condition of its own. One walker over nested manifests now serves the code
that needs them all, which fixes the byte orders of sub-manifests chosen by
`:case`. A fault in writing to streams was found and removed on the way. The
checks went from 315 to 335.

### SPECOPS-D73 — A manifest with slots is written through a buffer to any destination it cannot patch in place

**Status:** Implemented
**Context:** Slots are backpatched through the destination's `write`, and
checksums read the destination itself (SPECOPS-D34,
SPECOPS-D36). In `src/manifest/manifest.lisp:marshal@1cc7600` a
function had no `write`, so any slot failed by calling `nil`, and a checksum
was handed the function or stream it was to read. A stream was patched by
seeking to a recorded position, which counts from where `marshal` began,
while `file-position` counts from the start of the file, and after each patch
the position was set to the stream's absolute one. A record written after
anything else in a stream therefore had its slots written over earlier bytes:
a length slot written after a 2-byte header gave `#(3 7 0 1 0 5 9)` for
`#(7 7 3 1 0 5 9)`. The GOFF writer writes records partway into a stream, and
was spared only because its END record has no slots.
SPECOPS-D13 planned a buffer for destinations that
cannot be patched and read.
**Decision:** At expansion,
`src/manifest/manifest.lisp:manifest-slots-p@b63b7d2` decides whether the
manifest, or a manifest nested in it, has a slot of any kind. A manifest with
none is written directly, as before. A manifest with one is written in place
only to a simple array of its unit type, which checksum functions such as
`crc` can read. A function, a stream, or any other vector is written into a
buffer, a simple array grown by doubling, in which slots are resolved and
checksums computed; when the record is complete the buffer is passed on, to a
stream by one `write-sequence`, to a function by one call per unit, and to a
vector by `aref`. Streams are never sought or read back. This amends
SPECOPS-D34, whose streams were patched by seeking.
**Alternatives:** Patching streams that can seek, after fixing the positions
(rejected by the steward: deciding at run time whether a stream can seek has
many pitfalls, and pipes and sockets cannot); buffering every manifest
(rejected in SPECOPS-D13: manifests without slots would
pay for it); patching every vector in place (rejected: `crc` declares a simple
byte array, so a checksum failed on a vector with a fill pointer or of general
elements, as the new suite showed).

### SPECOPS-D74 — A caller may keep the buffer across calls in a `manifest-buffer`

**Status:** Implemented
**Context:** A buffer made for every call puts pressure on the garbage
collector when many records are written. A buffer kept across calls needs an
owner, and SPECOPS-O12 asked who owns it, how large it
grows, and whether writing and reading share one.
**Decision:** By default `marshal` makes a fresh buffer of 256 units for each
call that needs one. Given `(:buffer b)` in its options, it takes the array
that `b`, a `src/manifest/manifest.lisp:manifest-buffer@b63b7d2` made by
`make-manifest-buffer`, holds, when its elements are of the manifest's unit
type, and stores the array back when the record is complete, grown to the
largest record written through it. The caller owns the buffer, so no special
variable is involved and reentrancy and threads are the caller's concern, as
for any object it passes. Stream sources, when built, are to accept the same
object, so that one type serves both directions; whether one instance serves
both is the caller's choice.
**Alternatives:** A fresh buffer only (rejected by the steward: the pressure
above); a special variable holding a buffer (rejected: a nested or concurrent
call would share it); passing the array itself (rejected: an array replaced as
it grows would be lost to the caller).

### SPECOPS-D75 — A vector too small signals `manifest-overrun`, and a fill pointer is neither read nor set

**Status:** Implemented
**Context:** A vector destination too small for its record failed inside
`aref`, with a message that named neither the manifest nor the vector's size.
Adjustable vectors and vectors with fill pointers raised the question of
whether `marshal` should grow them.
**Decision:** `src/manifest/manifest.lisp:manifest-overrun@b63b7d2`, a subclass
of `error`, holds the manifest, the vector's size and the position of the
unit that did not fit, with exported readers. A vector is bounded by its
allocated size: a vector written in place checks each unit and signals at the
first that does not fit, leaving the units before it written; a vector
written through a buffer is checked before the copy, so nothing is written. A
fill pointer is neither read nor set. Growing a vector is the caller's
concern, which the condition's position serves. There is no restart, since a
record cannot be resumed in another vector.
**Alternatives:** A plain error with a clear message (rejected by the steward:
a caller could not catch it apart from other errors and retry); growing an
adjustable vector with `vector-push-extend` (rejected by the steward: the
caller's concern, and such a vector would still need the buffer for
checksums); bounding a vector by its fill pointer (rejected: it would differ
from `aref`, and a vector with a fill pointer of 0 could hold nothing).

### SPECOPS-D76 — One walker visits every manifest nested in another, `:case` branches included

**Status:** Implemented
**Context:** `marshal` sizes its serializers from the byte orders of every
manifest it expands (SPECOPS-D56), but
`src/manifest/manifest.lisp:manifest-swap-specs@1cc7600` walked spans and
sub-manifest fields and not sub-manifests chosen by `:case`. A branch whose
manifest used a byte order its parent lacked indexed past the serializers.
`manifest-checksums-p` had its own walk, and buffering needed a third.
**Decision:** `src/manifest/manifest.lisp:walk-manifest@b63b7d2` calls a
function on each stored field of a manifest and of every manifest nested in
it, inside spans, as fields and repeats, and as `:case` branches, each once.
`manifest-swap-specs` and `manifest-slots-p` are built on it; the latter,
given `:checksum-of`, replaces `manifest-checksums-p`.
**Alternatives:** Adding `:case` branches to `manifest-swap-specs` alone
(rejected: three walks would drift apart again).

### Implementation

- **`src/manifest/manifest.lisp`.**
  - `walk-manifest`, `manifest-swap-specs` and `manifest-slots-p`, per
    SPECOPS-D76.
  - `manifest-overrun` and `signal-overrun`, per SPECOPS-D75;
    `manifest-buffer`, `reusable-buffer` and
    `src/manifest/manifest.lisp:grow-buffer@b63b7d2`, per
    SPECOPS-D73 and SPECOPS-D74.
  - In `src/manifest/manifest.lisp:marshal@b63b7d2`: the `:buffer` option; a
    local `in-place` function that sets the vector's checked `enter` and its
    `write`; the dispatch on the destination, chosen at expansion by
    `manifest-slots-p`; the copy of the buffer to its destination at the end;
    and an error for a destination that is not a vector, a stream or a
    function. The stream `write` that sought, and the `tell` function that
    nothing called, are gone.
  - `src/manifest/manifest.lisp:unmarshal@b63b7d2` asks `manifest-slots-p`
    for checksums.
- **`src/package.lisp`.** Exports `make-manifest-buffer`, `manifest-buffer`,
  `manifest-overrun` and its readers `overrun-manifest`, `overrun-size` and
  `overrun-position`.
- **`test/manifest.lisp`.** `test/manifest.lisp:run-destination-demo@b63b7d2`
  and its helper `test/manifest.lisp:collect-units@b63b7d2`; the probe
  `test/manifest.lisp:case-le-branch-probe@b63b7d2`.

### Verification

The run command is that of the update of 2026-10-31 for the scan model, with
`(specops::run-record-demo)`, `(specops::run-variable-demo)`,
`(specops::run-checked-read-demo)` and `(specops::run-destination-demo)`
added.

Table: Suites added or extended in commit `b63b7d2`, and the totals there.

| Suite | Checks | Added |
|---|---|---|
| `run-destination-demo` | 19 | New. Functions, file streams, buffers and overruns, as listed below |
| `run-case-demo` | 28 → 29 | A little-endian sub-manifest chosen by `:case` in a big-endian parent, written and read back |
| All eighteen suites | 315 → 335 | All pass, with the PNG verification |

The new suite checks, among other things:

- **Functions:** `png-file`, `png-any` with ancillary chunks, a `:count-of`
  slot and an `:offset-of` slot, each giving the bytes written to a vector; a
  312-byte IDAT chunk, which grows the buffer past its first 256 units.
- **Streams:** files opened for output only, at their start and after a
  header, for a manifest with checksums and for one with a length slot alone;
  the PNG with ancillary chunks read back from such a file with no failed
  check; a manifest without slots written directly.
- **Expansion:** only manifests with a slot, here or nested, are written
  through a buffer.
- **Reusable buffers:** a `manifest-buffer` keeping its array across two
  calls, keeping the array it grew, and left alone by a vector destination.
- **Vectors:** `manifest-overrun` with its size and position, for a manifest
  with slots and one without; a vector with a fill pointer and a vector of
  general elements receiving a PNG through the buffer, the fill pointer left
  alone; an overrun through the buffer writing nothing; a vector with a fill
  pointer written in place by a manifest without slots.
- **Errors:** a destination that is not a vector, a stream or a function, and
  an unknown option.

The faults these checks guard against were reproduced by hand on a checkout of
`1cc7600`: a length slot written to a stream after a header, `png-file` written
to a function, `png-file` written to a 50-byte vector, and the `:case` branch,
each failing as the contexts of SPECOPS-D73,
SPECOPS-D75 and SPECOPS-D76 describe. The manifest layer has no style warnings, and
the five test scripts in `test/` pass, among them the GOFF writer's, which
writes to a stream through `marshal`. The architecture systems
`specops.arch.6502`, `.s390x`, `.x86`, `.z80` and `.wasm`, and the three
format systems, load with the new exports.

### Files

Table: Files changed in commit `b63b7d2`, and the documents updated after it.

| File | Action | Description |
|------|--------|-------------|
| `src/manifest/manifest.lisp` | Modified | The walker, buffering, the reusable buffer, overruns |
| `src/package.lisp` | Modified | Exports the buffer and the overrun condition |
| `test/manifest.lisp` | Modified | The destinations suite; the `:case` byte-order check |
| `doc/Log.Manifest.md` | Modified | This update |
| `doc/Plan.ManifestComplete.md` | Modified | Progress installment for destinations; SPECOPS-O12 answered for writing; a note under SPECOPS-D13 |

### Metrics

- Test checks: 335 automated checks plus 1 structural PNG verification, up
  from 315.
- Regressions at `b63b7d2`: 0.

### Outstanding Work

Auto-buffering and stream checksums, outstanding since this log began, are
done, as are the byte orders of `:case` branches. New:

- **Checksums over sources that are not simple byte arrays.** `unmarshal`
  hands a vector source to the checksum function as it is, so `crc` fails on
  a vector with a fill pointer or of general elements; `source-units`
  converts only integers.
- **Writing a buffer out in pieces**, once nothing before a point can still be
  patched, should a record grow too large to hold whole.
- The other items of the previous updates remain, among them the checksum
  functions, which are not exported.

## Update 2026-11-01 — Stream sources

This installment covers commit `239b2f7`, the first half of the Manifest
Completion Plan's seventh milestone: `unmarshal` reads from streams. A stream
is read only as far as each read needs, its units are kept so that checksums
can read them again, and a read takes exactly one record, so that records can
be read from one stream in sequence. The buffer the units are kept in may be
the caller's, which answers the reading side of the plan's
SPECOPS-O12. The second half of the milestone,
selective reads, follows in its own installment. A new suite brings the checks
from 335 to 348.

### SPECOPS-D77 — A stream is a source of `deserializer-for`, read as far as each read needs and kept

**Status:** Implemented
**Context:** `unmarshal` read vectors, integers and `(units . integer)` pairs
(SPECOPS-D39), at any index. Every field is read at or after
the cursor, but a checksum reads its region again (SPECOPS-D70),
so a stream's units must be kept at least until the record's checksums are
done. Three places used the source other than through the deserializers: the
end rule took its `length`, `emit-scan` checked vectors for overruns, and
checksums read it. Each nested expansion builds its own deserializers, so a
wrapper around those of the top-level expansion would not reach the
sub-manifests.
**Decision:** `src/serial.lisp:stream-source@239b2f7` holds a stream and the
units read from it so far, in a simple array numbered from 0 where reading
began, as positions are on the write side. It is a fourth kind of source for
`src/serial.lisp:deserializer-for@239b2f7`, tested after vectors, which pay
nothing for it. A read first calls
`src/serial.lisp:fill-stream-source@239b2f7`, which reads with
`read-sequence` up to the last unit the read needs and no further, and
signals an error when the stream ends first. The stream is never sought, as
destinations are not (SPECOPS-D73), and every unit of the
record is kept. `src/serial.lisp:source-end-p@239b2f7` tells the end rule
whether a vector or a stream has a unit at a position, reading one unit ahead
of a stream only then. The top-level `unmarshal` wraps a stream in a
`stream-source`; nested expansions share it, and checksums read its array.
**Alternatives:** Wrapping the deserializers in `unmarshal` (rejected: nested
expansions build their own); seeking a file stream to skip or to read a region
again (rejected by the steward for the reasons of
SPECOPS-D73); reading a stream in blocks ahead of need
(rejected: the stream would be left past the end of the record); dropping
units no check still needs (deferred: the record is kept whole, as it is when
written through a buffer).

### SPECOPS-D78 — A read from a stream takes exactly one record

**Status:** Implemented
**Context:** A trailing pad, the rest of a fixed buffer after its terminator,
and fields of known size move the cursor without reading units. A stream read
only as far as reads need would be left inside such a record, and formats such
as GOFF hold records one after another in one stream.
**Decision:** When the top-level `unmarshal` has read a record from a stream,
it reads on to the record's final cursor, and no further, so that the stream
is left at the start of whatever follows. Selective reads of streams will also
take the whole record, which the steward has decided for the second half of
this milestone.
**Alternatives:** Leaving the stream where the last read stopped (rejected:
the next read would begin inside the record's padding or buffer).

### SPECOPS-D79 — `unmarshal` keeps a stream's units in the `manifest-buffer` that `marshal` uses

**Status:** Implemented
**Context:** SPECOPS-D74 let a caller keep `marshal`'s buffer
across calls and intended stream sources to accept the same object, which
SPECOPS-O12 asked about for reading.
**Decision:** `(:buffer b)` in `unmarshal`'s params gives a stream source the
array a `manifest-buffer` holds, when its elements are of the manifest's unit
type, and the grown array is stored back when the record is read. One type
serves both directions, and one instance may serve both, since each call
leaves the buffer free when it returns.
**Alternatives:** A separate buffer type for reading (rejected: SPECOPS-D74
intended one); passing the array itself (rejected there: a grown array would be
lost to the caller).

### Implementation

- **`src/serial.lisp`.** `stream-source`, `fill-stream-source` and
  `source-end-p`, per SPECOPS-D77, placed before
  `deserializer-for`, whose vector case became a local `from-vector` that the
  stream case shares. `src/serial.lisp:grow-buffer@239b2f7` moved here from
  `manifest.lisp`, since both directions use it.
- **`src/manifest/manifest.lisp`.** In
  `src/manifest/manifest.lisp:unmarshal@239b2f7`: the `:buffer` option; the
  top-level binding of the source, which wraps a stream; the checksum's buffer,
  chosen by the kind of source; and the read to the record's end, which also
  stores the grown array back, per SPECOPS-D78 and
  SPECOPS-D79.
- **`src/manifest/scan.lisp`.** The end rule,
  `src/manifest/scan.lisp:stop-effector@239b2f7` on `end-rule`, asks
  `source-end-p`.
- **`test/manifest.lisp`.** `test/manifest.lisp:run-stream-source-demo@239b2f7`
  and its helper `test/manifest.lisp:with-source-file@239b2f7`; the probe
  `stream-buffer-probe`, a record ending in a fixed buffer.

### Verification

The run command is that of the update of 2026-10-31 for the scan model, with
`(specops::run-record-demo)`, `(specops::run-variable-demo)`,
`(specops::run-checked-read-demo)`, `(specops::run-destination-demo)` and
`(specops::run-stream-source-demo)` added.

Table: The suite added in this installment, and the totals at commit `239b2f7`.

| Suite | Function | Checks | Result |
|---|---|---|---|
| Stream sources | `run-stream-source-demo` | 13 | 13 pass |
| All nineteen suites | from `run-png-demo` to `run-stream-source-demo` | 348 and the PNG verification | All pass |

The new suite checks, among other things:

- **Whole files:** the 1×1 PNG read from a file stream as from a vector,
  leaving the stream at its end; the PNG with ancillary chunks read after a
  3-byte header, and read as records and written back.
- **Records in sequence:** two `item-probe` records and a PNG from one stream;
  two 80-byte GOFF END records, each read with its trailing pad, and the unit
  after them; records ending in a pad and in a fixed buffer, read to their
  ends.
- **The end of the stream:** a repeat to the end of the stream; a stream that
  ends before IEND, which signals an error; a terminated LEB vector read
  element by element.
- **Checks:** a corrupt IDAT CRC caught on a stream, at the unit it is caught
  at on a vector.
- **Buffers:** a `manifest-buffer` keeping its array across two reads and a
  write between them, and growing past its first 256 units.
- **Expansion:** only the top-level expansion wraps the stream.

The manifest layer has no style warnings, and the five test scripts in `test/`
pass. The architecture systems `specops.arch.6502`, `.s390x`, `.x86`, `.z80`
and `.wasm`, and the three format systems, load.

### Files

Table: Files changed in commit `239b2f7`, and the documents updated after it.

| File | Action | Description |
|------|--------|-------------|
| `src/serial.lisp` | Modified | The stream source as a source of `deserializer-for`; `grow-buffer` |
| `src/manifest/manifest.lisp` | Modified | Streams in `unmarshal`: the wrapper, the `:buffer` option, the read to the record's end |
| `src/manifest/scan.lisp` | Modified | The end rule asks `source-end-p` |
| `test/manifest.lisp` | Modified | The stream sources suite |
| `doc/Log.Manifest.md` | Modified | This update |
| `doc/Plan.ManifestComplete.md` | Modified | Progress installment for stream sources; SPECOPS-O12 answered for reading; key paths deferred |

### Metrics

- Test checks: 348 automated checks plus 1 structural PNG verification, up
  from 335.
- Regressions at `239b2f7`: 0.

### Outstanding Work

Stream sources, outstanding since this log began, are done. New:

- **Selective reads**, the second half of the milestone, next. Key paths into
  sub-manifests, such as `(:ihdr :width)`, are deferred by the steward: a key
  that names a sub-manifest field returns the whole sub-record.
- **Units kept for the whole record.** A record read from a stream is held
  whole, as SPECOPS-D77 decided; dropping units no check still
  needs would serve records too large to hold.
- **A stream's element type** must match the manifest's unit; a character
  stream cannot be read.
- **The reader protocol of the architecture.** The stream source is a
  candidate base for the protocol that SPECOPS-O4 asks
  disassembly to offer; that question remains open.
- The other items of the previous updates remain.

## Update 2026-11-01 — Selective reads

This installment covers commit `cb931c8`, the second half of the Manifest
Completion Plan's seventh milestone. Given keys, `unmarshal` now decodes only
those fields and the fields that locate them, and passes over the rest,
reading no more of a field than finding its end needs. A read from a vector
stops after its last key; a read from a stream still takes the whole record.
Only the slots in the part read are checked. This builds the plan's
SPECOPS-D14 and amends SPECOPS-D40. Key
paths into sub-manifests are deferred by the steward. A new suite brings the
checks from 348 to 360.

### SPECOPS-D80 — A keyed read decodes its keys and the fields that locate them

**Status:** Implemented
**Context:** `unmarshal` read every named field whatever keys it was given, so
that counts and lengths were available to later fields
(SPECOPS-D40). Reading one field of a large file decoded all of
it. SPECOPS-D14 planned a closure of the requested keys
computed at expansion, and the region table of SPECOPS-D70 was
built in a general form for it.
**Decision:** At expansion, `src/manifest/manifest.lisp:manifest-selection@cb931c8`
gives each field of the manifest, spans' fields included, a mode: read, which
decodes it, or skip, which passes over it. Working back from the last field,
a field is read when it is a key or another field needs it, and
`src/manifest/manifest.lisp:field-dependencies@cb931c8` names what each needs:
the field a `:count` names, a `:case` selector, and the field of a length slot
when the length counts the field's elements or bounds a repeat. A field passed
over that a length slot read before it covers needs only that slot's field,
and is passed over by the length. A key the manifest lacks is an expansion
error. A key that names a sub-manifest field gives its whole record: key paths
into sub-manifests, such as `(:ihdr :width)`, are deferred by the steward.
Reads without keys decode every field, as before. This amends
SPECOPS-D40.
**Alternatives:** Decoding every field, as before (kept for reads without
keys); a parser that decodes fields lazily when the code runs (rejected in
SPECOPS-D14: it loses the specialization expansion
gives); key paths now (deferred by the steward).

### SPECOPS-D81 — A field is passed over through its plan's skip effector

**Status:** Implemented
**Context:** Passing over a field must find its end. For some kinds that is
known before reading; for others, such as terminated or LEB fields, the
elements must be read, though not decoded, enum-mapped or collected. Field
plans and their effectors (SPECOPS-D45,
SPECOPS-D46) already know how each kind ends.
**Decision:** `src/manifest/scan.lisp:skip-effector@cb931c8` emits the code
that moves the cursor past a plan's field. A scalar, a field of fixed size, or
one whose element count is known before reading moves the cursor at once. A
`:case` field dispatches once on its selector. A sub-manifest is expanded to
read only what finding its own end needs. Other scans run with
`src/manifest/scan.lisp:null-assembler@cb931c8`, which collects nothing,
reading their elements only when the stop rule decides by their values, as
`src/manifest/scan.lisp:rule-reads-elements-p@cb931c8` tells; a repeat ended by
`:until` reads in each instance only the fields its condition tests, through
a nested read of those fields alone. Pads, strings, scalars and masques move
the cursor by their width. Streams are read past, never sought, as the steward
decided for them (SPECOPS-D77).
**Alternatives:** Reading and discarding each field (rejected: it decodes,
maps enums and allocates for nothing); seeking file streams (rejected by the
steward); computing each sub-manifest's fixed size at expansion (deferred: a
skipped child of fixed fields already moves the cursor with no reads).

### SPECOPS-D82 — A selective read stops after its last key, but takes a stream's whole record, and checks only the part read

**Status:** Implemented
**Context:** Fields after the last key need not be reached, but a read from a
stream takes exactly one record (SPECOPS-D78). Checks
(SPECOPS-D69, SPECOPS-D70) need their slot
fields read and their regions located, so checking everything would turn a
selective read into a full one.
**Decision:** The boundary is the top-level item that holds the last key.
From a vector or an integer, reading stops there. The items after it are
passed over only when the source is a stream, under a test made when the code
runs, so that the stream is left where a full read would leave it. A field
before the boundary that only an item after it needs is read from every
source. A slot is checked when its field and its region both lie in the items
up to the boundary, and its field is then read; constants passed over are not
checked, and nothing after the boundary is.
**Alternatives:** Every check pulling its fields into the read (rejected by
the steward: a selective read would become a full one); no checks with keys
(rejected by the steward: what is read past should still be checked);
stopping a stream after the last key (rejected by the steward: the next
record could not be read).

### Implementation

- **`src/manifest/manifest.lisp`.**
  - `manifest-leaves`, `leaf-names`, `field-dependencies` and
    `manifest-selection`, per SPECOPS-D80 and
    SPECOPS-D82.
  - In `src/manifest/manifest.lisp:unmarshal@cb931c8`: the selection, given
    by the keys at the top level or by an internal `:-+select+-` param for a
    nested manifest, `nil` passing over it; the modes and boundary, with the
    checked slots taken from the selection; local `skip-item`, `skip-manifest`
    and `select-manifest`; `item-plan` and `item-ctx`, which reading and
    skipping share, replacing the two copies of plan building in `read-item`;
    the guard around the items after the boundary; and the plist a nested
    selection returns. A key that names nothing is reported.
- **`src/manifest/scan.lisp`.** `skip-effector` and its methods,
  `null-assembler` and `rule-reads-elements-p`, per
  SPECOPS-D81. `emit-scan` declares the element ignorable, since
  a skip collects nothing.
- **`test/manifest.lisp`.** `test/manifest.lisp:run-selective-read-demo@cb931c8`,
  with the macro `test/manifest.lisp:each-key-agrees@cb931c8` and
  `test/manifest.lisp:reads-outside-guard@cb931c8`, which counts the reads in an
  expansion outside a stream's tail; the probe `until-then-probe`.

### Verification

The run command is that of the update of 2026-10-31 for the scan model, with
`(specops::run-record-demo)`, `(specops::run-variable-demo)`,
`(specops::run-checked-read-demo)`, `(specops::run-destination-demo)`,
`(specops::run-stream-source-demo)` and `(specops::run-selective-read-demo)`
added.

Table: The suite added in this installment, and the totals at commit `cb931c8`.

| Suite | Function | Checks | Result |
|---|---|---|---|
| Selective reads | `run-selective-read-demo` | 12 | 12 pass |
| All twenty suites | from `run-png-demo` to `run-selective-read-demo` | 360 and the PNG verification | All pass |

Every keyed read in the earlier suites is now a selective read, and they pass
unchanged. The new suite checks, among other things:

- **The same values:** each key read alone gives what a full read gives, over
  the three PNG manifests and 20 probes covering terminated, multi-element, user-rule, LEB,
  `:case`, masque, fixed-buffer, repeated, nested, padded and slot fields;
  several keys come back in the order asked.
- **No decoding of what is passed over:** the reads in expansions outside a
  stream's tail, such as 1 for `nest-probe`'s `:trailer` against 9 for a full
  read, and 2 for a PNG chunk's `:type`, whose data is passed over by its
  length; vectors cut short after the keys still read them.
- **Checks:** a corrupt CRC unchecked when reading `:type` and caught when
  reading `:crc`; a changed chunk type passed over, not checked as a constant,
  while the CRC over it is.
- **Streams:** a selective read of a PNG taking its whole record, its CRCs
  unchecked, so that the next record reads whole; a repeat ended by `:until`
  passed over on a stream.

The expansion of a repeat ended by `:until` was also inspected: each instance
reads only its `:tag` and moves past its `:value`. The manifest layer has no
style warnings, the five test scripts in `test/` pass, and the architecture
systems `specops.arch.6502`, `.s390x`, `.x86`, `.z80` and `.wasm`, and the
three format systems, load.

### Files

Table: Files changed in commit `cb931c8`, and the documents updated after it.

| File | Action | Description |
|------|--------|-------------|
| `src/manifest/manifest.lisp` | Modified | The selection, skipping and the stream's tail in `unmarshal`; shared plan building |
| `src/manifest/scan.lisp` | Modified | `skip-effector`, `null-assembler`, `rule-reads-elements-p` |
| `test/manifest.lisp` | Modified | The selective reads suite |
| `doc/Log.Manifest.md` | Modified | This update |
| `doc/Plan.ManifestComplete.md` | Modified | Progress installment for selective reads; a note under SPECOPS-D14 |

### Metrics

- Test checks: 360 automated checks plus 1 structural PNG verification, up
  from 348.
- Regressions at `cb931c8`: 0.

### Outstanding Work

Selective reads, outstanding since this log began, are done. New:

- **Key paths into sub-manifests**, such as `(:ihdr :width)`, deferred by the
  steward (SPECOPS-D80).
- **Fields read only for the tail.** A field before the boundary that only an
  item after it needs is read from vectors too, where the tail is not reached
  (SPECOPS-D82).
- The other items of the previous updates remain.

## Update 2026-11-01 — Placed fields and WOFF

This installment covers commit `52b81e5`, which builds the Manifest Completion
Plan's eighth milestone, WOFF (SPECOPS-D17). A WOFF
file's table directory holds, for each table, its offset, length and checksum,
while the tables follow the whole directory, in an order of their own. That
needs what SPECOPS-D66 left open: a slot in one instance of a
repeat covering data written outside it. Placed fields provide it, after
lisp-binary's pointers and region tags, which the prior-art evaluation
[SPECOPS-0004](Eval.LispBinaryTools.md) proposed in its
SPECOPS-O13. A WOFF and an sfnt manifest in the tests
read real fonts and WOFF files, and fontTools accepts what they write. A new
suite brings the checks from 360 to 377.

### SPECOPS-D83 — A field may be placed: written where a place directive for its tag stands, its slots backpatched there

**Status:** Implemented
**Context:** A WOFF directory entry describes a table that is written after
all the entries. Slots resolve within their instance
(SPECOPS-D66), and instances are not addressed from outside a
repeat. The WOFF specification also stores tables in the source font's order,
which need not be the directory's: in all seven rustdoc WOFFs and both sfnt
fonts examined, the orders differ.
**Decision:** A field declared `:place tag`, or a sub-manifest with `:place
tag` among its options, is written where a directive `(place tag :align n)`
stands in the same manifest or one enclosing it. In
`src/manifest/manifest.lisp:marshal@52b81e5`, the field's writing code is
queued under its tag, with the positions of the slots over it captured from
its instance. The directive writes each queued field in turn, padding before
it and after the last to the alignment, and backpatches its `:offset-of`,
`:length-of` and `:checksum-of` slots. A checksum slot over a placed field
writes a placeholder where it stands. In memory, a placed field belongs to its
instance, as a WOFF entry holds its table's data. Expansion errors: a tag no
directive declares; a slot over a placed field that comes after it, or that is
not an offset, length or checksum slot; and a placed field inside a span. A
field queued after its tag's last directive is an error when the code runs.
`src/manifest/manifest.lisp:manifest-places@52b81e5` finds the tags and
directives across nested manifests.
**Alternatives:** Paired repeats, instance *i* of one describing instance *i*
of another (rejected by the steward: a table's data would be split from its
entry, and reading would still assume directory order); instance names built
at run time, such as `:tables/3/data`, as SPECOPS-D66 deferred
(rejected: heavier, and no help with the order); offsets computed by the
caller (rejected: the purpose of SPECOPS-D17).

### SPECOPS-D84 — A placed field is read at the offset its `:offset-of` slot gives

**Status:** Implemented
**Context:** Reading tables in sequence fails for real files, whose tables
are not in directory order. Following offsets on read was deferred by the
plan's Scope and Non-Goals. A stream source keeps the units read
(SPECOPS-D77), so an offset behind the cursor can be read
again, and one ahead is read to.
**Decision:** In `src/manifest/manifest.lisp:unmarshal@52b81e5`, a placed
field is read at the value of its `:offset-of` slot field, which must be read
before it; the cursor then returns. A placed field with no `:offset-of` slot
is an expansion error. The directive moves the cursor past the furthest unit
read by its tag's fields and aligns it, so a stream is still taken one record
at a time (SPECOPS-D78). Checks work as for any field, and a
selective read passing over a placed field reads its offset and length fields
only. The steward brought this forward for placed fields only; following
offsets to structures that are not placed stays deferred.
**Alternatives:** Reading placed data in queue order (rejected: wrong for
every real file examined); general pointers now (deferred by the steward).

### SPECOPS-D85 — Fields that describe data the manifest does not hold are given, not derived

**Status:** Implemented
**Context:** A WOFF table may be zlib-compressed. Its original length and
checksum, and the header's `totalSfntSize`, describe the uncompressed font,
which is not in the file, and SpecOps has no deflate compressor. An sfnt's
`head` table checksum is computed with the table's `checkSumAdjustment` taken
as zero, which no slot over its bytes reproduces.
**Decision:** In the test manifests, `orig-length`, `orig-checksum`,
`total-sfnt-size` and an sfnt entry's `checksum` are ordinary fields, given by
the caller or carried in a record; the layout fields, offsets, compressed
lengths, the file's length and the table count, are slots. Compressed files
then read with no failed check and write back exactly, and
`src/checksum.lisp:sfnt-checksum@52b81e5`, with the slot signature of
SPECOPS-D72, serves callers computing checksums.
**Alternatives:** Slots for the original length and checksum (rejected by the
steward: valid only for uncompressed tables, so every real file examined would
fail its checks); an option to treat a field as zero during a checksum, as
SPECOPS-D17 noted for TrueType (deferred).

### SPECOPS-D86 — Placed fields are written in the order they were queued

**Status:** Implemented
**Context:** A record read from a file whose tables are out of directory
order holds its entries in directory order. Writing them back in the file's
order would need an order other than the record's.
**Decision:** The directive writes its fields in the order they were queued,
which for a repeat is the order of its instances. A file stored in another
order is written back valid but reordered, a canonical round trip in the sense
of SPECOPS-D62.
**Alternatives:** An option ordering placed data by the offsets read
(rejected by the steward for now: exact round trips of such files are not
needed).

### Implementation

- **`src/manifest/manifest.lisp`.**
  - `src/manifest/manifest.lisp:defmanifest@52b81e5` stores `:place` on fields
    and on sub-manifests, taking it out of a sub-manifest's options, and
    parses the `place` directive.
  - `manifest-places`, per SPECOPS-D83.
  - In `marshal`, local `placed-item`, `placed-field` and `place-directive`;
    the queues, a plist by tag, bound at the top level and shared through the
    sub-lexicon; the check, when the code ends, that every queue was written.
  - In `unmarshal`, `offset-sources`; the shared plist of the furthest unit
    read by each tag's fields; the jump to a placed field's offset and back in
    `process-item`, around the recording of its region; and `place-form`, for
    the directive, read or passed over.
  - `src/manifest/manifest.lisp:manifest-selection@52b81e5` adds a placed
    field's offset field to what it needs.
- **`src/checksum.lisp`.** `sfnt-checksum`, per SPECOPS-D85.
- **`test/manifest.lisp`.** The manifests
  `test/manifest.lisp:woff-file@52b81e5`, `woff-entry`,
  `test/manifest.lisp:sfnt-file@52b81e5` and `sfnt-entry`, and probes of placed
  fields; `test/manifest.lisp:verify-woff@52b81e5`, an independent hand
  verifier like `verify-png`; `test/manifest.lisp:hand-woff@52b81e5`, which
  builds WOFF bytes without manifests, in any table order;
  `test/manifest.lisp:table-checksum@52b81e5`, with `head`'s rule;
  `test/manifest.lisp:read-woff@52b81e5` and `write-woff`; and
  `test/manifest.lisp:run-woff-demo@52b81e5`.

The first draft of the suite inlined some thirty `woff-file` expansions in one
function, and SBCL exhausted its default heap compiling it, though each
expansion is under 1,700 conses. Reads and writes that need no options now go
through `read-woff` and `write-woff`. The first draft of `verify-woff` also
rejected the `head` checksum of every real WOFF; it now applies `head`'s rule.

### Verification

The run command is that of the update of 2026-10-31 for the scan model, with
`(specops::run-record-demo)`, `(specops::run-variable-demo)`,
`(specops::run-checked-read-demo)`, `(specops::run-destination-demo)`,
`(specops::run-stream-source-demo)`, `(specops::run-selective-read-demo)` and
`(specops::run-woff-demo)` added.

Table: The suite added in this installment, and the totals at commit `52b81e5`.

| Suite | Function | Checks | Result |
|---|---|---|---|
| Placed fields and WOFF | `run-woff-demo` | 17 | 17 pass |
| All twenty-one suites | from `run-png-demo` to `run-woff-demo` | 377 and the PNG verification | All pass |

The new suite checks, among other things:

- **Placed fields:** sub-manifests placed after their entries, 2-aligned,
  written to a vector and a function and read back; placed vectors with
  offset, length and checksum slots, and a changed byte caught by the
  checksum; the four expansion errors.
- **WOFF written:** four tables passing `verify-woff`, and byte for byte the
  WOFF `hand-woff` builds; read back, and written back through plists and
  records.
- **Order and compression:** a WOFF stored out of directory order, read with
  no failed check and written back valid, in directory order; a compressed
  table, with its given length and checksum, carried through unchanged.
- **Streams and selective reads:** two WOFFs read in sequence from a stream,
  offsets read backwards and forwards; the flavor, the table count and the
  entries read alone. Because `woff-file` wraps every field in one span, the
  whole record is the part read (SPECOPS-D82): reading
  `:flavor` reads the file's length, the table count and each entry's offset
  and length, 5 reads against 18 for a full read, and no table data.
- **Checks:** a wrong file length, and a wrong signature as a constant.
- **sfnt:** `sfnt-checksum`, and an sfnt written, read back and packed as a
  WOFF.

Outside the suite, real fonts were read and written, the fonts left where they
are and the outputs written to a scratch directory. `av05-logotype.otf`, an
OpenType font with CFF outlines and 10 tables, and Qt's `icons.ttf`, a
TrueType font with 16, were read from streams through `sfnt-file`, their
tables at offsets out of directory order, packed as WOFFs, and written back as
sfnts of their original sizes. The seven WOFFs in rustc 1.48's rustdoc, whose
tables are mostly compressed and stored out of directory order, read with no
failed check and were written back at their sizes. `verify-woff` passed every
original and every output, and fontTools 4.60.2 opened all eleven outputs with
checksum checking set to raise, found each table equal to its source's, and
parsed every table it knows.

The manifest layer has no style warnings, the five test scripts in `test/`
pass, and the architecture systems `specops.arch.6502`, `.s390x`, `.x86`,
`.z80` and `.wasm`, and the three format systems, load.

### Files

Table: Files changed in commit `52b81e5`, and the documents updated after it.

| File | Action | Description |
|------|--------|-------------|
| `src/manifest/manifest.lisp` | Modified | Placed fields: the `:place` key and `place` directive, writing, reading at offsets, selection |
| `src/checksum.lisp` | Modified | `sfnt-checksum` |
| `test/manifest.lisp` | Modified | WOFF and sfnt manifests, `verify-woff`, `hand-woff`, the placed fields and WOFF suite |
| `doc/Log.Manifest.md` | Modified | This update |
| `doc/Plan.ManifestComplete.md` | Modified | Progress installment for WOFF; a note under SPECOPS-D17; Scope and Non-Goals |
| `doc/Eval.LispBinaryTools.md` | Modified | SPECOPS-O13 answered in part |

### Metrics

- Test checks: 377 automated checks plus 1 structural PNG verification, up
  from 360.
- Regressions at `52b81e5`: 0.

### Outstanding Work

The offset-table exercise, outstanding since this log began, is done with
WOFF, and instances addressed from outside a repeat are no longer needed for
it. New:

- **General pointers and offset bases.** Following an offset to a structure
  that is not placed, and offsets counted from a base other than the
  top-level record's start, as TIFF and OpenType tables need, remain
  deferred (SPECOPS-O13).
- **Limits of placed fields.** A placed field cannot be inside a span or
  covered by a `:count-of` slot, and the slots over it must come before it.
- **Treating a field as zero during a checksum**, for an sfnt's `head`
  (SPECOPS-D85).
- **Out of scope for WOFF:** compression, the metadata and private blocks, and
  checking a file for overlapping or extra data on read.
- **Large test functions.** Many expansions inlined in one function can
  exhaust SBCL's default heap at compilation; helper functions keep the
  suites small.
- The other items of the previous updates remain.

## Update 2026-11-01 — Masque completion

This installment covers commit `e340210`, the masque half of the Manifest
Completion Plan's ninth milestone. Decimal masks, which commits `5e0976e` and
`d524c3c` began, now compose and decompose correctly in `masque` and
`unmasque`; `masque` takes an `:overflow` parameter choosing what a value too
wide for its group does; and the manifest layer reads decimal masques, signals
an error for a group value that does not fit, and lets a masque group select a
`:case` branch. Binary and hexadecimal masks, on which every architecture
module relies, are unchanged. The checks went from 377 to 382.

### SPECOPS-D87 — Decimal mask positions count digits from the right, as binary positions count bits

**Status:** Implemented
**Context:** `src/serial.lisp:quantify-mask-string@d524c3c` gave a decimal
mask's group positions as powers of ten of their offsets from the left end.
That is right only where the mask is symmetric. Evaluated at `266d471`,
`(masque "d:1a2bb" (a 7) (b 45))` gave a number of some 300 digits for 17245;
`unmasque` extracted one digit of each group, giving `(2 4)` for `d:aabb` over
1234; and it never compared a decimal mask's constant digits, so `d:1a2bb`
matched 97245. The plan's updates since 2026-10-31 listed decimal masques in
`masque` and `unmasque` as done; they were only partly so. No module uses a
decimal mask, while binary and hexadecimal masks appear in every architecture
module.
**Decision:** `src/serial.lisp:quantify-mask-string@e340210` gives every
mask's positions in one form: each group's lowest position from the right
end, and the top of the last, in bits, or in digits for a decimal mask, whose
factor is then the base they are powers of. `src/serial.lisp:masque@e340210`
scales a decimal group by ten to the power of its position, and
`src/serial.lisp:unmasque@e340210` extracts the digits from its position to
the next, and returns `nil` when the value less its groups is not the mask's
constant digits, as binary and hexadecimal masks return `nil` when their
constant bits differ. The binary and hexadecimal paths are not changed: all
786 mask strings in the repository's Lisp files give the same values from
`quantify-mask-string` before and after, and `masque`'s expansion of them is
the same.
**Alternatives:** Keeping powers of ten as the decimal positions (rejected:
wrong away from a symmetric mask); a separate function for decimal masks
(rejected: one form of positions serves every base).

### SPECOPS-D88 — `masque` takes an `:overflow` parameter, `:mask` by default

**Status:** Implemented
**Context:** `masque` masks each value to its group with `logand`, so a value
too wide is cut off silently. The steward considered making masking opt-in,
since an assembler's known-good code pays for it on every instruction. But a
negative value, a signed immediate or displacement, depends on the mask to
become two's complement within its group; unmasked, it sets every bit above
the group and makes the whole word negative. A constant value's mask can be
folded by the compiler; this was not measured.
**Decision:** `masque`'s parameter alist, which already takes
`:with-processor`, takes `(:overflow . mode)`. `:mask`, the default, keeps
today's behaviour: `logand` to the group's width, or the remainder modulo ten
to the width of a decimal group. `:error` signals an error unless the value
fits the group as an unsigned or a two's complement number, then masks it; a
literal that does not fit is an error at expansion. `nil` leaves values as
they are, for code known to give values that fit. The `:with-processor` branch,
whose masking splits a value into units, is not affected, and `unmasque` takes
no such parameter.
**Alternatives:** Masking off by default (rejected by the steward: unmasked
negative values would corrupt whole words, and every call site would need an
audit first); separate parameters for masking and for errors (rejected: they
are three behaviours of one choice, and the steward chose one parameter with
three modes); range checks in the
manifest layer alone (rejected by the steward: the check belongs to `masque`,
where any caller can ask for it).

### SPECOPS-D89 — A manifest masque is written with `:overflow :error`, read in decimal, and sized by its largest value

**Status:** Implemented
**Context:** The plan asked for decimal masques in `unmarshal`, which rejected
them, and for an error when a group's value does not fit, which `marshal`
truncated. `defmanifest` sized a masque by the bit count `quantify-mask-string`
returns, which for a decimal mask counts digits, so `d:9999` would get one
byte.
**Decision:** `src/manifest/manifest.lisp:marshal@e340210` writes every
masque with `(:overflow . :error)`, so a group value too wide is an error,
while a negative value is written as two's complement.
`src/manifest/manifest.lisp:defmanifest@e340210` sizes a decimal masque by
the bits of its largest value: `d:1a2bb` takes 17 bits, so 3 bytes.
`src/manifest/manifest.lisp:unmarshal@e340210` reads a decimal masque's groups
by powers of ten and checks its constant digits, and its static groups, as
`:constant` checks (SPECOPS-D69). A signed decimal group is an
expansion error.
**Alternatives:** Checking each group's range by its signedness in the
manifest layer (not taken: `:overflow :error` accepts either reading, so an
unsigned group given -1 or a signed one given 200 in 8 bits is written; noted
under Outstanding Work).

### SPECOPS-D90 — A masque group may select a `:case` branch

**Status:** Implemented
**Context:** The plan asked for masque keyword groups to be registered so that
`:case` and `:count` could refer to them on write. `marshal` found selectors
only among fields, not masque groups. Since SPECOPS-D47,
selectors are read through variables rather than a `values` plist. On write, a
vector's own length is written, so `:count` needs nothing.
**Decision:** `src/manifest/manifest.lisp:find-field@e340210` finds a stored
field by name inside spans and masque groups, and both macros look selectors
and their enums up through it. On write, a masque group that is a selector
sets its selector variable, as a field does. On read, an enumerated group
that is a selector also keeps its raw code, since branch keys are compared as
codes. Selective reads read the masque that holds a selector the key needs.
**Alternatives:** Registering groups in a run-time plist, as the plan first
proposed (superseded by the selector variables of SPECOPS-D47).

### Implementation

- **`src/serial.lisp`.** `quantify-mask-string`, `masque` and `unmasque`, per
  SPECOPS-D87 and SPECOPS-D88. The unused
  `mask-overflow` binding the steward had begun in `masque` became the
  `:overflow` parameter.
- **`src/manifest/manifest.lisp`.** In `defmanifest`, a decimal masque's
  width; in `marshal`, `(:overflow . :error)` and the masque binder setting a
  selector variable; in `unmarshal`, the masque branch rewritten around local
  `extract` and `fit` functions that serve binary and decimal masks alike, and
  raw codes for enumerated selector groups; `find-field`, per
  SPECOPS-D89 and SPECOPS-D90.
- **`test/manifest.lisp`.** The probes
  `test/manifest.lisp:decimal-masque-probe@e340210` and
  `test/manifest.lisp:masque-selector-probe@e340210`, and five checks in
  `test/manifest.lisp:run-masque-demo@e340210`, the suite of the feature they
  complete (SPECOPS-D58).

### Verification

The run command is that of the update of 2026-11-01 for placed fields and
WOFF.

Table: Checks added in commit `e340210`, and the totals there.

| Suite | Checks | Added |
|---|---|---|
| `run-masque-demo` | 11 → 16 | Decimal `masque` and `unmasque`, constant digits included; the three `:overflow` modes, an error both when the code runs and at expansion; a decimal masque in 3 units, written, read, and with a wrong constant digit; group values too wide on write, and a negative one as two's complement; an enumerated masque group selecting a `:case` branch, written, read and read selectively |
| All twenty-one suites | 377 → 382 | All pass, with the PNG verification |

The mask strings of every Lisp file in the repository, 786 of them, were
collected and passed to `quantify-mask-string` before and after the change,
with identical results, and `masque`'s default expansion of a hexadecimal mask
is unchanged form for form. The five test scripts in `test/` pass, the
manifest layer has no style warnings, and the architecture systems
`specops.arch.6502`, `.s390x`, `.x86`, `.z80` and `.wasm`, and the three format
systems, load, rebuilt from source.

### Files

Table: Files changed in commit `e340210`, and the documents updated after it.

| File | Action | Description |
|------|--------|-------------|
| `src/serial.lisp` | Modified | Decimal positions, groups and constant digits; the `:overflow` parameter |
| `src/manifest/manifest.lisp` | Modified | Decimal masques in manifests, range errors on write, masque groups as selectors |
| `test/manifest.lisp` | Modified | Two probes; five checks in the masque suite |
| `doc/Log.Manifest.md` | Modified | This update |
| `doc/Plan.ManifestComplete.md` | Modified | Progress installment for masque completion; a note under its masque items |

### Metrics

- Test checks: 382 automated checks plus 1 structural PNG verification, up
  from 377.
- Regressions at `e340210`: 0.

### Outstanding Work

Decimal masques, masque range checks and registration, outstanding since this
log began, are done. New:

- **Floats**, the other half of the milestone, next.
- **Signedness in range checks.** `:overflow :error` accepts a value that fits
  its group as unsigned or as two's complement, so it does not tell a signed
  group from an unsigned one (SPECOPS-D89).
- **Architecture modules that do not load.** `specops.arch.m68k`, with an
  unbound `*m68k-layout*`, and `specops.arch.superh`, with an invalid
  initialization argument, fail to load at `266d471` as well as after this
  change; they are outside this work.
- **Relative offsets.** `src/assembler.lisp:locate-relative@e340210` writes a
  negative offset as a sign bit and a magnitude, where a branch displacement is
  usually two's complement; this may be a fault, outside this work.
- The other items of the previous updates remain.

## Update 2026-11-01 — Floats

This installment covers commit `5862d3a`, which completes the Manifest
Completion Plan's ninth milestone with the `:f` class reserved since
SPECOPS-D32: IEEE binary floats of 16, 32, 64 and 128 bits. It
follows the prior-art evaluation's SPECOPS-O19, which
pointed to lisp-binary's floats of every IEEE width, as the steward settled it.
A new suite brings the checks from 382 to 396.

### SPECOPS-D91 — A float field holds an IEEE binary float of 16, 32, 64 or 128 bits as its bit pattern

**Status:** Implemented
**Context:** The `:f` class was reserved but not built. Formats store floats
in IEEE binary formats, of 32 and 64 bits most often, and of 16 bits in
graphics and machine learning; the evaluation found lisp-binary covering half
to octuple precision. Common Lisp's float types do not portably include half
or quad precision.
**Decision:** `(:f width)` declares a float, and `:f16`, `:f32`, `:f64` and
`:f128` are keywords for it in 8-bit units, beside `:u8` and the rest.
`src/manifest/manifest.lisp:defmanifest@5862d3a` marks the field's subtypes,
and `src/manifest/scan.lisp:field-plan@5862d3a` builds its plan with the
format's width in bits, checked by `src/manifest/scan.lisp:float-bits@5862d3a`:
a width other than 16, 32, 64 or 128 bits, or a float that is also a LEB, is an
expansion error. A float is written and read as the integer of its bit
pattern, so byte order per field, vectors, `:case` branches, records and
selective reads work as for integers; the scalar plan and the vector
assembler convert each value. Half floats read as single floats, which hold
them exactly; single and double floats as such; quad floats as exact
rationals, so that they round-trip. A value is rounded to the nearest the
format holds, ties to even, and a finite value too large for it is an error.
**Alternatives:** 32 and 64 bits only (rejected by the steward in favour of the
four formats); quad floats read as double floats (rejected: they would not
round-trip); octuple precision (not taken: no format in view needs it).

### SPECOPS-D92 — Floats are converted arithmetically, and through SBCL's own floats where it has them

**Status:** Implemented
**Context:** Common Lisp has no portable way to make an infinity or a NaN, nor
to reach a float's bits; SBCL has both for single and double floats. Reading a
NaN as a keyword loses its payload, which some formats use. SBCL 2.3.4's
`coerce` of a rational to a single float is not correctly rounded for some
values near and below the least normal single float: comparing the two paths
on 100,000 random rationals in that range, while the work was built, found a
third rounded differently, and exact
rounding computed in Python agreed with the arithmetic path in every case
examined, `#x444` where SBCL gave `#x443` for one. Common Lisp leaves that
rounding to the implementation.
**Decision:** `src/serial.lisp:portable-encode-float@5862d3a` and
`src/serial.lisp:portable-decode-float@5862d3a` convert between numbers and
bit patterns arithmetically for every width, exactly rounded, with subnormals
and negative zero; they read infinities and NaN as `:infinity`, `:-infinity`
and `:nan`, and take those keywords on write. `src/serial.lisp:encode-float-bits@5862d3a`
and `src/serial.lisp:decode-float-bits@5862d3a`, which the manifest layer
calls, use SBCL's float bit accessors for 32 and 64 bits, behind `#+sbcl`, so
there an infinity or a NaN, its payload included, reads as an SBCL float and
writes back bit for bit. On write, only a value that is already a float takes
SBCL's path, whose conversions between single and double are the hardware's
and correctly rounded; a rational always takes the arithmetic one. Elsewhere,
and for 16 and 128 bits everywhere, the arithmetic path serves. A quad
negative zero reads as 0, which a rational cannot sign.
**Alternatives:** An error for infinities and NaN everywhere (rejected:
formats store them; the steward chose keywords and SBCL's floats); keywords on
SBCL too (rejected: NaN payloads would be lost); SBCL's `coerce` for rationals
(rejected: the
rounding above); `decode-float` and `encode-float` as names (not possible:
`decode-float` is a Common Lisp function).

### Implementation

- **`src/serial.lisp`.** `float-format`, the exponent and fraction widths of
  each format; `float-special-p`; the portable pair and the pair the manifest
  layer calls, per SPECOPS-D92.
- **`src/manifest/manifest.lisp`.** `defmanifest`'s type parser marks `:f`
  fields and the four keywords with a `:float` subtype; `marshal` and
  `unmarshal` send float fields through field plans, with vector, LEB and
  `:case` fields.
- **`src/manifest/scan.lisp`.** A `float` slot on `scalar-plan` and
  `vector-assembler`, holding the format's width in bits, whose read and write
  effectors decode and encode; `float-bits`; a float vector's elements are of
  type `t`, since they may read as keywords or rationals.
- **`test/manifest.lisp`.** Six probes and
  `test/manifest.lisp:run-float-demo@5862d3a`.

### Verification

The run command is that of the update of 2026-11-01 for placed fields and
WOFF, with `(specops::run-float-demo)` added.

Table: The suite added in this installment, and the totals at commit `5862d3a`.

| Suite | Function | Checks | Result |
|---|---|---|---|
| IEEE floats | `run-float-demo` | 14 | 14 pass |
| All twenty-two suites | from `run-png-demo` to `run-float-demo` | 396 and the PNG verification | All pass |

The new suite checks the four formats with their Lisp types and a record
round trip; byte order and 16-bit units; rounding of integers, rationals and
narrowed doubles; the least subnormal of each format; quad precision's 1/3 as
an exact rational; errors for values too large and a width of 24 bits;
keywords for infinities and NaN, and negative zero; on SBCL, infinities and a
NaN payload read and written back bit for bit, and the arithmetic path
agreeing with SBCL's floats on 20,000 random patterns of 32 and 64 bits; a
float vector counted by a `:count-of` slot; and a float `:case` branch.

Outside the suite, 29,997 doubles of every magnitude were encoded in half,
single and double precision and matched Python's `struct.pack` in every case,
its overflow errors included, and all 65,536 half patterns and 100,000 random
single patterns, subnormals, infinities and NaN among them, decoded as
`struct.unpack` decodes them. The manifest layer has no style warnings, the
five test scripts in `test/` pass, and the architecture systems
`specops.arch.6502`, `.s390x`, `.x86`, `.z80` and `.wasm`, and the three format
systems, load.

### Files

Table: Files changed in commit `5862d3a`, and the documents updated after it.

| File | Action | Description |
|------|--------|-------------|
| `src/serial.lisp` | Modified | IEEE floats as bit patterns, portable and through SBCL's floats |
| `src/manifest/manifest.lisp` | Modified | The `:f` class and float keywords; float fields through field plans |
| `src/manifest/scan.lisp` | Modified | Floats in the scalar plan and the vector assembler; `float-bits` |
| `test/manifest.lisp` | Modified | Float probes and the float suite |
| `doc/Log.Manifest.md` | Modified | This update |
| `doc/Plan.ManifestComplete.md` | Modified | Progress installment for floats; a note under its Floats item |
| `doc/Eval.LispBinaryTools.md` | Modified | SPECOPS-O19 answered for floats |

### Metrics

- Test checks: 396 automated checks plus 1 structural PNG verification, up
  from 382.
- Regressions at `5862d3a`: 0.

### Outstanding Work

Floats, outstanding since this log began, are done. New:

- **Widths count bytes, not units.** SPECOPS-D32 says a width
  counts output units, but `defmanifest` stores `(ash width (- unit-spec))`, so
  widths count bytes: in a 16-bit manifest `(:u 2)` takes one unit and
  `(:u 4)` two, and a 32-bit float there is `(:f 4)`. A width of less than a
  unit, such as `(:u 1)` in 16-bit units, would take none. The record or the
  code needs reconciling, for the steward to decide.
- **A quad negative zero** reads as 0 (SPECOPS-D92).
- **The float functions are not exported**, like the checksum functions.
- The other items of the previous updates remain.

## Update 2026-11-01 — Tests under Square

This installment covers commit `e4a1f9a`, which completes the first part of
the Manifest Completion Plan's tenth milestone: the manifest suites run behind
ASDF's `test-op`, rewritten to the Square testing standard, as the steward
asked. `(asdf:test-system :specops)` runs 397 tests, one for each of the 396
checks and the PNG verification that the suites held at `5862d3a`, with 566
assertions; all pass. The architecture's SPECOPS-D1 is amended
in the same update to allow a test system its libraries.

### SPECOPS-D93 — The manifest tests are a Square test system, `specops/test`, behind `test-op`

**Status:** Implemented
**Context:** At `5862d3a` the tests were one file of 3,824 lines,
`test/manifest.lisp`, in package `specops`, loaded by the system
`specops/demo.png` and run by hand: 22 functions from
`test/manifest.lisp:run-png-demo@5862d3a` to
`test/manifest.lisp:run-float-demo@5862d3a`, each with a local `check`
function that printed a line and pushed a boolean. The checks of a suite
shared its bindings, so a check could depend on what an earlier one built,
and the stream checks wrote to fixed paths in `/tmp`. Nothing ran them
through ASDF, which the plan's packaging milestone asks for, and a failure
showed only as a printed FAIL. Square rejects homegrown assertion functions
and manual pass counters, and prescribes FiveAM and cl-hamcrest, a test
package, a suite hierarchy, isolated tests and the `test-op` contract.
**Decision:** `specops.asd:specops/test@e4a1f9a`, a secondary system that
replaces `specops/demo.png`, depends on `specops`, `specops.map.ebcdic`,
`fiveam` and `hamcrest/fiveam`, and its `test-op` calls
`test/helpers.lisp:run-all-tests@e4a1f9a`; `specops` reaches it through
`:in-order-to`. Its package, `specops-tests`
(`test/package.lisp:specops-tests@e4a1f9a`), uses `specops`, imports the
FiveAM and cl-hamcrest names it uses, and imports the internals the tests
reach as an explicit list: the checksum and float functions, stream sources,
the field plan and the manifest registry, and the symbols expansion checks
look for. `test/helpers.lisp` holds the suites, the two runners, and the
manifests and builders more than one file uses. The root suite `specops` has
two children: `serial`, for serialization without a manifest, and
`manifest`, divided into `manifest-basics`, `-fields`, `-records`,
`-variable`, `-reads`, `-destinations` and `-formats`. Each suite has one
file, `test-serial.lisp` or `test-manifest-<name>.lisp`, holding the probes
only it uses. Each old check is one `def-test`, whose docstring states what
it proves and which builds its own data; a check that compared a list of
values became an assertion for each. Expected errors are tested with
`signals`, a condition's report by containment, and records read from bytes
with `assert-that` and `has-slots`. Files are temporary and deleted
afterwards.
**Alternatives:** A sibling system `specops-tests.asd` (not taken: the
steward chose the secondary system, which replaces `specops/demo.png`); the
tests left in package `specops` (not taken: the steward chose an explicit
import list, which Square's package rule asks for); the `check` functions
kept beside FiveAM (rejected: Square names them as a non-conforming pattern).

### SPECOPS-D94 — The program and format scripts move to `scripts/`, outside the test system

**Status:** Implemented
**Context:** `test/` also held five scripts, `test-build.lisp`,
`test-emit.lisp`, `test-goff.lisp`, `test-program.lisp` and
`test-wasm-macro.lisp`, which load source files directly and print their
results. They test the program builder and the ELF, GOFF and WebAssembly
writers, which the architecture's SPECOPS-O5 leaves without a
suite. Their names follow Square's `test-*` pattern, but Square's layout
reserves `test/` for the test system's files.
**Decision:** The scripts move unchanged to `scripts/`, apart from the path
in each header, and are run by hand from the repository root, as before.
Suites for the program builder and the format writers wait for
SPECOPS-O5 and the revision of those writers.
**Alternatives:** Converting the scripts now into `program` and `formats`
suites (rejected by the steward: he will revise the GOFF, ELF and other
tools, and wants a more rigorous test discipline applied to them then than
these scripts carry).

### Implementation

- **`specops.asd`.** `specops/test` in place of `specops/demo.png`; `specops`
  gains `:in-order-to` for `test-op`.
- **`test/package.lisp`, `test/helpers.lisp`.** The package and its import
  list; the suite hierarchy; `run-all-tests`, whose `:skip-slow` omits
  nothing, since no suite is slow, and `run-suite`, which takes a keyword, a
  string or a symbol; the PNG manifests, builder and verifier, the probes and
  stop rules shared between files, the rules package
  `specops.test.rules`, and the readers and collectors, among them
  `test/helpers.lisp:with-source-file@e4a1f9a`, which now writes a temporary
  file.
- **The eight `test-*.lisp` files.** The tests, with the probes each uses
  alone. `test/test-manifest-destinations.lisp:file-units-written@e4a1f9a`
  replaces the fixed output path, and
  `test/test-manifest-reads.lisp:keys-read-apart@e4a1f9a` names the keys whose
  selective read differs from the full read, where the old helper gave a
  boolean for each.
- **`scripts/`.** The five scripts, per SPECOPS-D94.
- **`test/manifest.lisp`.** Removed.

The tests were converted from `test/manifest.lisp` mechanically, each with
the setup its check had used, unused bindings dropped, and then reviewed by
hand: about eighty docstrings that named only the field or probe now state
the behaviour, three tests that assumed package `specops` were corrected, and
helpers that tested for an error with a boolean gave way to `signals`.

### Verification

Run with `sbcl --non-interactive --eval '(asdf:test-system :specops)'` from
the repository root, or `(specops-tests:run-suite :manifest-reads)` for one
suite.

Table: The suites at commit `e4a1f9a`, with their tests and assertions.

| Suite | File | Tests | Assertions | Result |
|---|---|---|---|---|
| `serial` | `test-serial.lisp` | 10 | 48 | All pass |
| `manifest-basics` | `test-manifest-basics.lisp` | 68 | 80 | All pass |
| `manifest-fields` | `test-manifest-fields.lisp` | 163 | 192 | All pass |
| `manifest-records` | `test-manifest-records.lisp` | 41 | 47 | All pass |
| `manifest-variable` | `test-manifest-variable.lisp` | 32 | 41 | All pass |
| `manifest-reads` | `test-manifest-reads.lisp` | 47 | 90 | All pass |
| `manifest-destinations` | `test-manifest-destinations.lisp` | 19 | 22 | All pass |
| `manifest-formats` | `test-manifest-formats.lisp` | 17 | 46 | All pass |
| `specops`, the root | all eight | 397 | 566 | All pass |

The conversion lost no check: there is one test for each of the 396 checks
and the PNG verification, and the 566 assertions FiveAM reports match a count
of the assertion forms in the files, so every test ran. Each test was also run
alone, in three shuffled orders, with no failure, so none depends on another.
The test files compile with no warnings of their own. `run-suite` was run with
a keyword, a string and a symbol, and a failing `assert-that`, with a nested
matcher, was confirmed to fail its test. The five scripts pass from
`scripts/`, and the architecture systems `specops.arch.6502`, `.s390x`,
`.x86`, `.z80` and `.wasm`, and the three format systems, load.

### Files

Table: Files changed in commit `e4a1f9a`, and the documents updated after it.

| File | Action | Description |
|------|--------|-------------|
| `specops.asd` | Modified | `specops/test` replaces `specops/demo.png`; `test-op` for `specops` |
| `test/package.lisp` | Added | The `specops-tests` package |
| `test/helpers.lisp` | Added | Suites, runners, shared manifests and helpers |
| `test/test-serial.lisp` | Added | The `serial` suite |
| `test/test-manifest-*.lisp` | Added | Seven files, one for each `manifest` suite |
| `test/manifest.lisp` | Deleted | Replaced by the files above |
| `scripts/test-*.lisp` | Renamed | The five scripts, moved from `test/` |
| `doc/Eval.LispBinaryTools.md` | Modified | Committed with this change; recorded in the update for floats |
| `doc/Log.Manifest.md` | Modified | This update |
| `doc/Plan.ManifestComplete.md` | Modified | Progress installment for the tests; a note under Packaging and tests |
| `doc/Arch.SpecOps.md` | Modified | SPECOPS-D1 amended for test systems; the project structure and SPECOPS-O5 brought up to date |
| `AGENTS.md` | Modified | Square's block, routing test work to the `square-*` skills |

### Metrics

- Tests: 397, with 566 assertions, from 396 checks and 1 structural PNG
  verification. The added assertions are checks of several values split
  apart, not new coverage.
- Regressions at `e4a1f9a`: 0.

### Outstanding Work

Suites behind `test-op`, outstanding since the plan began, are done. New:

- **Style warnings in expansions.** `marshal` and `unmarshal` expansions bind
  variables they do not use, which draws about 1,560 style warnings when the
  tests compile; the old test file drew about 1,370, and the tests now expand
  more often, each building its own data. The expansions should declare or
  drop them.
- **Bundled assertions.** About fifty assertions still compare a list of
  several results, such as a value and a boolean; they fail readably, but
  could be split.
- **A slot read by name.** One test reads a scan plan's `rule` slot with
  `slot-value`, since the class has no reader for it; the symbol is in the
  import list.
- **The scripts** in `scripts/` are not under Square (SPECOPS-D94).
- **Square's matcher list** names `has-accessors` and `instance-of`, which
  cl-hamcrest does not have; a matter for Square, outside this project.
- The other items of the previous updates remain.

## Update 2026-11-02 — Release 0.1.0 and the Manifest Reference

This installment covers commit `de25ae7`, tagged `v0.1.0`, which completes the
Manifest Completion Plan's tenth milestone, and the documents written after it:
the [Manifest Reference](Ref.Manifest.md), `SPECOPS-0007`, and a
manifest section in the README. Before the interface could be documented, two
questions had to be settled, as the plan's previous update said: widths now
count units, as SPECOPS-D32 records, and the functions a manifest names are
exported. The Reference's examples are a test suite of their own.

### SPECOPS-DRAFT-manifest-D68 — Widths count units, as SPECOPS-D32 records

**Status:** Proposed
**Context:** SPECOPS-D32 says a field's width counts output units, but
`src/manifest/manifest.lisp:defmanifest@e4a1f9a` stored `(ash width (-
unit-spec))`, so widths counted bytes: in a 16-bit manifest `(:u 2)` took one
unit, a 32-bit float was `(:f 4)`, and `(:u 1)` took none. Positions,
lengths, counts and offsets all count units, and the Reference had to state
one rule.
**Decision:** `src/manifest/manifest.lisp:defmanifest@de25ae7` stores a list
type's width as given, in units, for fields and `:case` branches. Keyword
types such as `:u16`, allowed only in 8-bit manifests, are unchanged, since a
byte is a unit there. Floats already took their bits from the length in units
(`src/manifest/scan.lisp:float-bits@de25ae7`). The one manifest of wider units
in the suites, the float probe of two 16-bit units, is now `(:f 2)`.
**Alternatives:** Keeping bytes, and superseding SPECOPS-D32 with a new
record (not taken: the steward chose units, as the record has them).

### SPECOPS-DRAFT-manifest-D69 — The functions manifests and their users call are exported from `specops`

**Status:** Proposed
**Context:** A manifest names its checksum function in a slot, `(:checksum-of
:body :by #'crc)`, but `crc`, `adler32`, `adler32-checksum` and
`sfnt-checksum` were internal, so a manifest in a user's package had to write
`specops::crc`. The float functions were internal too, and `serialize`, which
the architecture's SPECOPS-O6 lists among the names users need. SPECOPS-O6
also asks whether the interface should be split into packages.
**Decision:** `src/package.lisp:specops@de25ae7` exports `serialize`, the
four checksum functions, `encode-float-bits` and `decode-float-bits`. No
module defines a symbol of the same name, so none clashes. The portable float
functions and the stream-source functions stay internal. The manifest layer's
interface stays in the package `specops`, which answers SPECOPS-O6 for this
layer only; the rest of the question remains open.
**Alternatives:** A package of the manifest layer's own (not taken: the
steward kept everything in `specops`); leaving the functions internal
(rejected: manifests name them, so users would reach into the package).

### SPECOPS-DRAFT-manifest-D70 — The Reference's examples are tests, and the Reference describes a tagged release

**Status:** Proposed
**Context:** A reference whose examples are not run drifts from the code, and
a reference pinned to a moving commit describes no version a user can name.
**Decision:** Every example in `SPECOPS-0007` is a test of the
suite `manifest-ref`, in `test/test-manifest-ref.lisp`, which uses only the
names `specops` exports and a four-line `octets` helper the Reference defines.
The Reference names the test beside each example. `specops` and
`specops/test` are versioned 0.1.0, the steward tagged the commit `v0.1.0`,
and the Reference's `api-version` and code references pin to that tag, to be
advanced with each release.
**Alternatives:** Examples written for the document alone (not taken: the
steward chose a suite); code references pinned to the commit (not taken: the
steward chose a version and a tag).

### Implementation

- **`src/manifest/manifest.lisp`.** Widths as given, per
  SPECOPS-DRAFT-manifest-D68.
- **`src/package.lisp`.** The seven exports of SPECOPS-DRAFT-manifest-D69.
- **`specops.asd`.** Version 0.1.0 for both systems; `test-manifest-ref` in
  `specops/test`.
- **`test/test-manifest-ref.lisp`.** The suite `manifest-ref`: 29 tests of
  the Reference's examples, from
  `test/test-manifest-ref.lisp:ref-shape-written-and-read@de25ae7` to
  `test/test-manifest-ref.lisp:ref-float-bits@de25ae7`.
- **`test/test-manifest-basics.lisp`.** A 16-bit probe and
  `test/test-manifest-basics.lisp:word-probe-widths-count-16-bit-units@de25ae7`.
- **`test/test-manifest-fields.lisp`, `test/package.lisp`,
  `test/helpers.lisp`.** The float probe in units; the exported functions out
  of the import list; the suite `manifest-ref`.
- **`doc/Ref.Manifest.md`.** The Reference, written after the commit: the
  declaration forms, `marshal`, `unmarshal`, records, conditions, the stop
  rule protocol and the serialization primitives, with the limits of 0.1.0.
- **`README.md`.** A section, "Binary Formats: Manifests", with an example and
  a link to the Reference.

### Verification

Run with `sbcl --non-interactive --eval '(asdf:test-system :specops)'` from
the repository root.

Table: The suites at commit `de25ae7`, with their tests and assertions.

| Suite | Tests | Assertions | Result |
|---|---|---|---|
| `serial` | 10 | 48 | All pass |
| `manifest-basics` | 69 | 84 | All pass |
| `manifest-fields` | 163 | 192 | All pass |
| `manifest-records` | 41 | 47 | All pass |
| `manifest-variable` | 32 | 41 | All pass |
| `manifest-reads` | 47 | 90 | All pass |
| `manifest-destinations` | 19 | 22 | All pass |
| `manifest-formats` | 17 | 46 | All pass |
| `manifest-ref` | 29 | 80 | All pass |
| `specops`, the root | 427 | 650 | All pass |

Each test was run alone in three shuffled orders with no failure. The test
files compile with no warnings of their own. The five scripts pass, and the
architecture systems `specops.arch.6502`, `.s390x`, `.x86`, `.z80` and
`.wasm`, and the three format systems, load. The README's example was run as
written, in a package of its own that uses `specops`, and gives the results it
shows. Claims of the Reference that no example covers were tried in a scratch
image before they were written: that a value too wide for an integer field is
an error, that a slot's value given to `marshal` is ignored and recomputed,
and the limits below.

### Files

Table: Files changed in commit `de25ae7`, and the documents written after it.

| File | Action | Description |
|------|--------|-------------|
| `src/manifest/manifest.lisp` | Modified | Widths count units |
| `src/package.lisp` | Modified | `serialize`, the checksum functions and the float functions exported |
| `specops.asd` | Modified | Version 0.1.0; the suite `manifest-ref` |
| `test/test-manifest-ref.lisp` | Added | The Reference's examples |
| `test/test-manifest-basics.lisp` | Modified | Widths in 16-bit units |
| `test/test-manifest-fields.lisp` | Modified | The float probe in units |
| `test/helpers.lisp` | Modified | The suite `manifest-ref` |
| `test/package.lisp` | Modified | Exported functions out of the import list |
| `doc/Ref.Manifest.md` | Added | The Manifest Reference |
| `README.md` | Modified | The manifest section |
| `doc/Log.Manifest.md` | Modified | This update |
| `doc/Plan.ManifestComplete.md` | Modified | Progress installment for the release and the Reference |

### Metrics

- Tests: 427, with 650 assertions, up from 397 and 566: 29 tests of the
  Reference's examples and one of widths in units.
- Regressions at `de25ae7`: 0.

### Outstanding Work

The Reference and the exports, the last of the plan's milestones, are done,
and the decision on widths is made. New, found while writing the Reference's
examples, and listed there under its limits:

- **Byte orders in wider units.** In a manifest of units wider than 8 bits,
  `:little` and `:middle` reorder a field's bytes, not its units: `(:u 1)`
  holding 1 in a little-endian 16-bit manifest is written `#x0100`. The
  serializers swap at byte granularity whatever the unit.
- **Source vectors.** `unmarshal` reads only a vector whose element type is
  the manifest's unit, and reports a general vector as "Incompatible type for
  collection array", which does not say what to give.
- **Counts on write.** `marshal` does not check a vector's value against its
  `:count`, so a short value moves the fields after it.
- **Run-time sub-manifest values.** A sub-manifest's value in `marshal`'s
  pairs must be a literal list of pairs; a variable is an expansion error,
  reported as "The value P is not of type LIST". Such values go through
  `(:from record)`.
- **Record names.** A record class named by a `common-lisp` symbol, such as
  `directory`, cannot be defined, because its accessors would be interned in
  that locked package.
- **Reserved options.** `str`'s `:encode-by` is stored and never used, and its
  `:length` has no test.
- The other items of the previous updates remain.

## Update 2026-11-11 — Release 0.1.1: masque group types

This installment covers commit `adc24c0`, tagged `v0.1.1`, and the update of
the [Manifest Reference](Ref.Manifest.md) after it. A masque group may now be
typed `:u` or `:s`, its width taken from the mask, and a type keyword that
names no type is an expansion error. The steward found the question while
reading the recursive-manifests Survey, whose sketches typed a one-bit group
`:u8`. The commit also carries the sketches of the arrays plan and of that
Survey, brought up to the new form.

### SPECOPS-DRAFT-manifest-D71 — A masque group's type gives only its signedness, written `:u` or `:s`

**Status:** Proposed
**Context:** A masque group's width and position are those of its letters in
the mask: `src/manifest/manifest.lisp:unmarshal@v0.1.0` extracts each group
with `ldb` over the width and offset it finds in the string, and `masque`
composes it the same way. The binding's type, a field's type, was used only
for its signedness, so `(p :constructed :u8)` on a single letter was a
one-bit unsigned group that read as eight bits. The keyword `:s`, which no
type defined, passed through as unsigned without an error, so a group meant
to be signed was read wrong. In a manifest of 16-bit units, a group typed
`:u8` was an error, although its width was never used. The Reference did not
say any of this.
**Decision:** A group's type may be `:u` or `:s`, for unsigned and signed,
with the width the mask gives; `src/manifest/manifest.lisp:defmanifest@adc24c0`
reads them as `(:u 1)` and `(:s 1)`, whose width a group ignores, so they
serve in manifests of any unit. A field's type keyword that names no type is
now an expansion error: `:u` or `:s` alone, with a message saying that a
field's type is `(:u width)` and that the keyword alone is a group's, and any
other unknown keyword. The Reference says that a group's type gives only its
signedness, and that a field type such as `:u8` is accepted in 8-bit
manifests with its width ignored. The version is 0.1.1.
**Alternatives:** Documenting the existing behaviour alone (not taken: the
steward chose to add `:u` and `:s` as well, and documentation alone would
have left `:s` silently unsigned); a width that disagrees with its group made
an expansion error (not taken for now: it would break manifests that use
`:u8` loosely, and decimal masks count digits, not bits); a boolean `:flag`
type for one-bit groups, read as `t` and `nil` (deferred by the steward to a
later release, as more natural for attributes such as `:wide` and `:blink`;
it changes what a record holds, and what a `:case` on the group keys on).

### Implementation

- **`src/manifest/manifest.lisp`.** In `defmanifest`, `group-field` gives a
  binding typed `:u` or `:s` the type `(:u 1)` or `(:s 1)`; the type parser
  signals an error for `:u` and `:s` alone in a field and for an unknown
  keyword, where it left the width unset.
- **`test/test-manifest-basics.lisp`.** Two probes, `group-type-probe`, of a
  one-bit, a four-bit signed and a three-bit group in 8-bit units, and
  `word-group-probe`, of two signed and unsigned groups in a 16-bit unit; and
  three tests, from
  `test/test-manifest-basics.lisp:group-types-u-and-s-take-their-widths-from-the-mask@adc24c0`
  to
  `test/test-manifest-basics.lisp:a-field-typed-u-or-s-alone-or-an-unknown-keyword-is-an-expansion-error@adc24c0`.
- **`test/test-manifest-ref.lisp`.** The Reference's masque example types its
  group `:u`.
- **`specops.asd`.** Version 0.1.1 for both systems.
- **`doc/Plan.OverArrays.md`, `doc/Survey.RecursiveManifests.md`.** Their
  sketches type groups `:u`; the plan's Lexter cell writes its format byte as
  the constant `(masque "h:00")`.
- **`doc/Ref.Manifest.md`.** Written after the commit: version 0.1.1, its code
  references pinned to `v0.1.1`, group types under the masque directive, and
  the expansion error for a type keyword that names no type, under field types.

### Verification

Run with `sbcl --non-interactive --eval '(asdf:test-system :specops)'`. At
`adc24c0`, `manifest-basics` has 72 tests with 91 assertions, and the root
suite 430 tests with 657 assertions; all pass. No existing test used a type
keyword that the parser now refuses.

### Files

Table: Files changed in commit `adc24c0`, and the documents updated after it.

| File | Action | Description |
|------|--------|-------------|
| `src/manifest/manifest.lisp` | Modified | Group types `:u` and `:s`; unknown type keywords refused |
| `test/test-manifest-basics.lisp` | Modified | Group types, and the expansion errors |
| `test/test-manifest-ref.lisp` | Modified | The masque example typed `:u` |
| `specops.asd` | Modified | Version 0.1.1 |
| `doc/Plan.OverArrays.md` | Modified | Sketches with group types `:u` |
| `doc/Survey.RecursiveManifests.md` | Modified | Sketches with group types `:u`, and the steward's inline constant digit |
| `doc/Ref.Manifest.md` | Modified | Release 0.1.1 and group types, after the commit |
| `doc/Log.Manifest.md` | Modified | This update |

### Metrics

- Tests: 430, with 657 assertions, up from 427 and 650.
- Regressions at `adc24c0`: 0.

### Outstanding Work

New:

- **A `:flag` type** for one-bit groups, deferred by the steward, and
  **widths checked against groups**, not taken for now
  (SPECOPS-DRAFT-manifest-D71).
- **Bare numbers as constants,** such as `0` for a zero byte, in place of
  `(masque "h:00")` or `(pad 0 1)`, which the steward has asked to weigh.
- The other items of the previous updates remain.

## Update 2026-11-11 — Release 0.1.2: constant numbers and `vec`

This installment covers commit `17bac3c`, tagged `v0.1.2`, and the update of
the [Manifest Reference](Ref.Manifest.md) after it. A manifest may now hold a
constant as a bare number, or as `(width . value)` for a constant of a given
width, and a run of constant units as `(vec unit...)`. The steward asked for
them as more elegant than `(masque "h:00")` or `(pad 0 1)` for a constant, and
decided their form after weighing it with the assistant. The commit also
carries the documents of release 0.1.1.

### SPECOPS-DRAFT-manifest-D72 — A constant is a bare number, or `(width . value)`, in the manifest's byte order, and `(vec unit...)` writes units in stream order

**Status:** Proposed
**Context:** A format's constants, such as a zero byte, a version or a magic
number, were written as a masque of constant digits, `(masque "h:00")`, which
is checked on read, or as a pad, `(pad 0 1)`, which is not. Both say less
plainly what they hold than the number itself. A bare number's width cannot
come from its digits, since the reader keeps no leading zeros: `#x00010000`,
sfnt's version, reads as `#x10000`, which needs three bytes. Some constants
are values, such as WebAssembly's version, 1 in four little-endian bytes;
others are sequences of units in stream order, such as WebAssembly's magic
number, `00 61 73 6D`, which a byte order would reverse.
**Decision:** In `src/manifest/manifest.lisp:defmanifest@17bac3c`:

- **A bare number,** a non-negative integer, is a constant in as few units as
  it holds, zero in one: `#x010203` is three bytes, or two units of 16 bits.
- **`(width . value)`** is a constant of WIDTH units, as `serialize` takes a
  pair; an entry whose first element is an integer is no other form.
- **Both are written in the manifest's byte order,** as a field or a masque
  is: `(4 . 1)` is `01 00 00 00` in a little-endian manifest.
- **`(vec unit...)`** writes its units in the order given, whatever the byte
  order, each an integer that fits one unit.
- **Constants are checked on read** as `:constant`, at the unit where the
  constant begins, with a report naming the constant and the manifest. They
  hold no field, count in the spans and slots that cover them, and are passed
  over by selective reads.
- **Expansion errors:** a negative number, a value wider than its width, a
  width that is not a positive integer, a pair whose value is not an integer,
  and a `vec` element that does not fit one unit.

A constant is stored as a masque of constant digits, so it is written,
checked and passed over by the masque's code.
**Alternatives:** A pad before a bare number to give its width (not taken:
the steward chose the pair; a pad is not checked on read, and it divides one
constant between two entries); bare numbers in stream order, most significant
unit first, as `serialize` and `str` write (not taken: the steward chose the
manifest's byte order, which fields and masques follow, so that a constant
value is written as the field it stands for would be); a bare vector,
`#(...)`, for units in stream order (not taken: the steward chose `(vec ...)`,
which matches `str` in form); a bare string for `(str ...)` (not taken by the
steward: `(str ...)` is not cumbersome, it names the type of what it holds,
and it and `vec` complement each other in appearance and structure).

### Implementation

- **`src/manifest/manifest.lisp`.** In `defmanifest`, `constant-entry` makes
  the masque of a constant, with its width checked and its number kept for the
  report; `process-entry` takes bare numbers and pairs, and passes other
  entries to `process-form`, which holds the forms of 0.1.1 and the new `vec`
  directive. `unmarshal` reports a failed constant by its number and manifest.
- **`test/test-manifest-basics.lisp`.** A section, "Constants: bare numbers and
  vec", with four probes, among them the WebAssembly header, and six tests,
  from
  `test/test-manifest-basics.lisp:bare-numbers-are-constants-in-as-few-units-as-they-need@17bac3c`
  to
  `test/test-manifest-basics.lisp:constants-that-cannot-be-written-are-expansion-errors@17bac3c`.
- **`test/test-manifest-ref.lisp`.** The Reference's example,
  `test/test-manifest-ref.lisp:ref-constants@17bac3c`.
- **`specops.asd`.** Version 0.1.2.
- **`doc/Ref.Manifest.md`.** Written after the commit: version 0.1.2, its code
  references pinned to `v0.1.2`, the constant forms among a manifest's
  entries, and a section, "Constants, masques and pads", with the example.

### Verification

Run with `sbcl --non-interactive --eval '(asdf:test-system :specops)'`. At
`17bac3c` the root suite has 437 tests with 682 assertions; all pass, and each
test passes alone in three shuffled orders. `manifest.lisp` compiles with no
warnings. The format systems `specops.format.goff`, `.elf` and `.wasm` and the
architecture system `specops.arch.wasm` load, and the GOFF script passes.

### Files

Table: Files changed in commit `17bac3c`, and the documents updated after it.

| File | Action | Description |
|------|--------|-------------|
| `src/manifest/manifest.lisp` | Modified | Constant numbers, `(width . value)` and `vec` |
| `test/test-manifest-basics.lisp` | Modified | The constants' section |
| `test/test-manifest-ref.lisp` | Modified | The Reference's example of constants |
| `specops.asd` | Modified | Version 0.1.2 |
| `doc/Ref.Manifest.md` | Modified | Release 0.1.1 in the commit; release 0.1.2 and constants after it |
| `doc/Log.Manifest.md` | Modified | The update for 0.1.1 in the commit; this update after it |

### Metrics

- Tests: 437, with 682 assertions, up from 430 and 657.
- Regressions at `17bac3c`: 0.

### Outstanding Work

Bare numbers as constants, outstanding since the previous update, are done.
The other items of the previous updates remain.

## Update 2026-11-11 — Lexter's cell model as a test format

This installment covers commit `f85a370`, the first step of the
[plan for manifests over arrays of records](Plan.OverArrays.md): a prototype
of Lexter's cell model as a test format, as PNG and WOFF are, with a
measurement of what writing and reading a cell costs. The record is that of
Lexter's Cell Model Architecture at its commit `76741bc`: eight little-endian
32-bit words in an array of bytes, its format in the first byte. No feature
of the manifest layer was needed beyond release 0.1.2, so this installment
records no decision.

### Implementation

- **`test/test-manifest-arrays.lisp`.** A suite, `manifest-arrays`, with one
  flat manifest for each of the plain, inline-colour and tile formats, since
  a glyph word's form is chosen by a type bit in byte 1 and a sub-manifest
  cannot read its parent's fields; glyph words as `glyph-layer` and
  `glyph-mask`, chosen by `:case` on the type bits; `tile-ref`, a 16-bit
  masque of a tile's index and flips; and `cell-common`, a view of the first
  eight bytes. The format byte is a bare constant, `0`, `2` or `4`, so that a
  cell read through another format's manifest fails at unit 0.
  `test/test-manifest-arrays.lisp:read-cell@f85a370` dispatches on that byte.
  Fourteen tests, from
  `test/test-manifest-arrays.lisp:a-record-of-zeros-is-a-blank-plain-cell@f85a370`
  to `test/test-manifest-arrays.lisp:a-cell-written-as-read-is-reproduced@f85a370`,
  check the worked records of Lexter's architecture: the record of zeros; g
  underlined, reversed, beneath a red underline with a strikethrough, and
  blinking; a scroll bar's thumb; a mask reaching two layers; inline colours;
  a chess piece on its square; and shore water mirrored beneath a boat.
- **`scripts/bench-cell.lisp`.** The measurement, run by hand.
- **`test/helpers.lisp`, `specops.asd`.** The suite and its file.
- **`doc/Plan.OverArrays.md`.** The steward's edit to the plan's sketch, its
  constant bytes written as bare `0`.

### Verification

Run with `sbcl --non-interactive --eval '(asdf:test-system :specops)'`. At
`f85a370`, `manifest-arrays` has 14 tests with 28 assertions, and the root
suite 451 tests with 710 assertions; all pass. The prototype settled the
plan's questions about the format:

- **A 16-bit masque in a little-endian manifest** is written least
  significant byte first: a tile index of 20 with its horizontal flip,
  `#x4014`, is `14 40`.
- **Type bits choose a glyph word's form**, at expansion when they are given
  as constants and when the code runs otherwise, with literal nested values
  for either branch.
- **A record of zeros** reads as a plain cell with no glyphs, and a plain cell
  written with every field defaulted is all zeros.

Table: The cost of one plain cell, by `scripts/bench-cell.lisp` at `f85a370`,
a million calls each, with SBCL 2.3.4.

| Operation | Time per cell | Bytes allocated per cell |
|---|---|---|
| `marshal`, the full cell, values known when the code runs | about 2,500 ns | about 1,340 |
| The same bytes packed by hand | about 8 ns | 0 |
| `unmarshal`, `:blink` alone | about 210 ns | 144 |
| The blink bit read by hand | about 4 ns | 0 |

A full write costs about three hundred times what packing by hand does, and
allocates on every call; a screen of 200 by 60 cells, written in full, would
take some 30 ms and allocate some 16 MB.

### Files

Table: Files changed in commit `f85a370`, and the documents updated after it.

| File | Action | Description |
|------|--------|-------------|
| `test/test-manifest-arrays.lisp` | Added | Lexter's cell model as a test format |
| `scripts/bench-cell.lisp` | Added | The cost of a cell, against packing by hand |
| `test/helpers.lisp` | Modified | The suite `manifest-arrays` |
| `specops.asd` | Modified | The file in `specops/test` |
| `doc/Plan.OverArrays.md` | Modified | The steward's edit in the commit; a progress update after it |
| `doc/Log.Manifest.md` | Modified | This update |

### Metrics

- Tests: 451, with 710 assertions, up from 437 and 682.
- Regressions at `f85a370`: 0.

### Outstanding Work

New:

- **The cost of a call.** `marshal` and `unmarshal` allocate on every call and
  write through closures, unit by unit, which the measurement above puts at
  hundreds of times the cost of packing by hand. Where the time and the
  allocation go is to be found first; the fast path of SPECOPS-O15 is the
  likely remedy for records of fixed size, such as Lexter's cells.
- The other items of the previous updates remain.

## Update 2026-11-11 — Release 0.1.3: records at an index

This installment covers commit `cba66c2`, tagged `v0.1.3`, the second step of
the [plan for manifests over arrays of records](Plan.OverArrays.md), and the
update of the [Manifest Reference](Ref.Manifest.md) after it. `marshal` and
`unmarshal` take `(:at index)`, so that a record is written or read at an
index of a vector, as the cells of Lexter's screen lie in one array. The
plan's SPECOPS-DRAFT-over-arrays-D1 and SPECOPS-DRAFT-over-arrays-D2 decide
the option and how positions count; this installment records how the layer
carries them out.

### SPECOPS-DRAFT-manifest-D73 — A record's start is kept in one variable that every nested manifest shares, and its offset slots count from it

**Status:** Proposed
**Context:** `marshal` kept its write position in a variable that began at 0,
or at the undocumented `:offset` parameter of `defmanifest`, and a second
variable, the base, marking where the manifest being expanded begins, against
which `:to` pads count (`src/manifest/manifest.lisp:marshal@v0.1.2`). A
sub-manifest rebinds the base to its own start, so the base cannot give the
record's start, from which a format's offsets count: a WOFF entry's offset
slot, in a sub-manifest, holds an offset from the start of the file.
`:offset-of` slots held positions in the destination, which were offsets from
the record's start only while the record began at 0; `unmarshal` checked
them, and read placed fields, the same way.
**Decision:** In `src/manifest/manifest.lisp:marshal@cba66c2` and
`src/manifest/manifest.lisp:unmarshal@cba66c2`, the top-level expansion binds
a variable, the origin, to the record's start: the index `(:at index)` gives,
checked to be a non-negative integer, or 0. The write position or the cursor,
and the base, begin there. The origin is passed to every nested expansion in
the lexicon that the two macros share with their sub-manifests, so that an
`:offset-of` slot written anywhere in the record holds the position of its
field less the origin, a check of one compares the stored offset with the
same difference, and a placed field is read at the origin plus its offset.
`:align` pads, checksum ranges, reported positions and `marshal`'s result keep
counting from the start of the vector, as SPECOPS-DRAFT-over-arrays-D2
requires. A destination that is a function or a stream, or a source that is an
integer or a stream, is an error under `(:at index)` when the code runs, and an
expansion error where the form is a `function` or `lambda` form, an integer, or
a quoted pair.
**Alternatives:** The base as the record's start (rejected: sub-manifests
rebind it, so offsets in a WOFF entry would count from the entry); the
`:offset` parameter of `defmanifest` as the index (rejected: it is fixed for
every call, where a record's index is a value of each call); offsets left as
positions in the destination (rejected by SPECOPS-DRAFT-over-arrays-D2: a
record copied elsewhere would be corrupt).

### Implementation

- **`src/manifest/manifest.lisp`.** In `marshal`: the option `:at`, the
  origin and its place in the lexicon, offsets less the origin in
  `resolve-region` and in placed fields, the index's check, and the errors for
  destinations without positions, at expansion and in the run-time dispatch
  on the destination. In `unmarshal`: the parameter `:at`, the origin, the
  cursor and base beginning there, a vector source required, offset checks and
  placed reads by the origin. Both docstrings describe the option.
- **`test/test-manifest-arrays.lisp`.** A section, "Records at an index", and a
  placed-field archive, `indexed-archive`, with seven tests, from
  `test/test-manifest-arrays.lisp:a-cell-written-at-an-index-of-a-grid@cba66c2`
  to `test/test-manifest-arrays.lisp:at-an-index-needs-a-vector-and-an-index@cba66c2`:
  a cell at an index and cells written in sequence; reads at an index, full
  and selective; a PNG at an index, its CRCs over the units where they lie and
  a failure reported at its position in the vector; offsets from the record's
  start, in placed fields and an offset probe; a `:to` pad from the record and
  an `:align` pad from the vector; an overrun past the vector's end; a record
  with slots in a vector with a fill pointer, through a buffer; and the errors.
- **`test/test-manifest-ref.lisp`.** The Reference's example,
  `test/test-manifest-ref.lisp:ref-records-at-an-index@cba66c2`.
- **`specops.asd`.** Version 0.1.3.
- **`doc/Ref.Manifest.md`.** Written after the commit: version 0.1.3, its code
  references pinned to `v0.1.3`, `(:at index)` among the options of `marshal`
  and the params of `unmarshal`, and a section, "Records at an index", with the
  table of SPECOPS-DRAFT-over-arrays-D2 and the example.

### Verification

Run with `sbcl --non-interactive --eval '(asdf:test-system :specops)'`. At
`cba66c2`, `manifest-arrays` has 22 tests with 61 assertions, `manifest-ref`
31 with 88, and the root suite 460 tests with 747 assertions; all pass, and
each test passes alone in three shuffled orders. With no `(:at index)`, every
earlier test passes unchanged, so records at 0 are written and read as before.
`src/manifest/manifest.lisp` compiles with no warnings; the format systems
`specops.format.goff` and `.wasm` load, and the GOFF script passes.

### Files

Table: Files changed in commit `cba66c2`, and the documents updated after it.

| File | Action | Description |
|------|--------|-------------|
| `src/manifest/manifest.lisp` | Modified | `(:at index)` in `marshal` and `unmarshal`; the origin |
| `test/test-manifest-arrays.lisp` | Modified | Records at an index |
| `test/test-manifest-ref.lisp` | Modified | The Reference's example of records at an index |
| `specops.asd` | Modified | Version 0.1.3 |
| `doc/Ref.Manifest.md` | Modified | Release 0.1.3 and records at an index, after the commit |
| `doc/Log.Manifest.md` | Modified | This update |
| `doc/Plan.OverArrays.md` | Modified | Progress update for the step, after the commit |

### Metrics

- Tests: 460, with 747 assertions, up from 451 and 710.
- Regressions at `cba66c2`: 0.

### Outstanding Work

New:

- **A record at an index of a vector that is not simple,** with slots, is
  written through a buffer as large as the index plus the record, since the
  buffer's positions are the vector's; it could begin at the index.
- **The `:offset` parameter of `defmanifest`,** which sets where every write
  of a manifest begins, is accepted, undocumented and untested; `(:at index)`
  serves its purpose per call, and it could be retired.
- The other items of the previous updates remain, the cost of a call first.
