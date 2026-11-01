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
  - SPECOPS-DRAFT-manifest-D1
  - SPECOPS-DRAFT-manifest-D2
  - SPECOPS-DRAFT-manifest-D3
  - SPECOPS-DRAFT-manifest-D4
  - SPECOPS-DRAFT-manifest-D5
  - SPECOPS-DRAFT-manifest-D6
  - SPECOPS-DRAFT-manifest-D7
  - SPECOPS-DRAFT-manifest-D8
  - SPECOPS-DRAFT-manifest-D9
  - SPECOPS-DRAFT-manifest-D10
  - SPECOPS-DRAFT-manifest-D11
  - SPECOPS-DRAFT-manifest-D12
  - SPECOPS-DRAFT-manifest-D13
  - SPECOPS-DRAFT-manifest-D14
  - SPECOPS-DRAFT-manifest-D15
  - SPECOPS-DRAFT-manifest-D16
  - SPECOPS-DRAFT-manifest-D17
  - SPECOPS-DRAFT-manifest-D18
  - SPECOPS-DRAFT-manifest-D19
  - SPECOPS-DRAFT-manifest-D20
  - SPECOPS-DRAFT-manifest-D21
  - SPECOPS-DRAFT-manifest-D22
  - SPECOPS-DRAFT-manifest-D23
  - SPECOPS-DRAFT-manifest-D24
  - SPECOPS-DRAFT-manifest-D25
  - SPECOPS-DRAFT-manifest-D26
  - SPECOPS-DRAFT-manifest-D27
  - SPECOPS-DRAFT-manifest-D28
  - SPECOPS-DRAFT-manifest-D29
  - SPECOPS-DRAFT-manifest-D30
  - SPECOPS-DRAFT-manifest-D31
  - SPECOPS-DRAFT-manifest-D32
  - SPECOPS-DRAFT-manifest-D33
  - SPECOPS-DRAFT-manifest-D34
  - SPECOPS-DRAFT-manifest-D35
  - SPECOPS-DRAFT-manifest-D36
  - SPECOPS-DRAFT-manifest-D37
  - SPECOPS-DRAFT-manifest-D38
  - SPECOPS-DRAFT-manifest-D39
  - SPECOPS-DRAFT-manifest-D40
  - SPECOPS-DRAFT-manifest-D41
  - SPECOPS-DRAFT-manifest-D42
  - SPECOPS-DRAFT-manifest-D43
  - SPECOPS-DRAFT-manifest-D44
  - SPECOPS-DRAFT-manifest-D45
  - SPECOPS-DRAFT-manifest-D46
  - SPECOPS-DRAFT-manifest-D47
  - SPECOPS-DRAFT-manifest-D48
  - SPECOPS-DRAFT-manifest-D49
  - SPECOPS-DRAFT-manifest-D50
  - SPECOPS-DRAFT-manifest-D51
  - SPECOPS-DRAFT-manifest-D52
relates-to:
  - SPECOPS-DRAFT-manifest-complete
  - SPECOPS-DRAFT-restructure
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
of 2026-10-31, to commits `738105a`, `d030f5c` and `2e1af03`, and five of
2026-11-01, to commits `4f9c6d4`, `d7ba27f`, `1cc7600`, `b63b7d2` and
`239b2f7`. The repository restructure that followed
moved the files this log names (see
[SPECOPS-DRAFT-restructure](Log.Restructure.md)); code references stay pinned
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

### SPECOPS-DRAFT-manifest-D1 — Manifest-driven marshal replaces the freeform item list

**Status:** Accepted
**Context:** The freeform `marshal` serialized an ad hoc item list. It could
not be inverted into a reader and every call site restated the layout.
**Decision:** A named manifest declares the layout once; `(marshal name
destination :key value …)` supplies values for its named fields. The freeform
mode was removed rather than reconciled.
**Alternatives:** Keeping freeform `marshal` alongside a manifest mode
(rejected: two vocabularies for one job); a struct-only interface (deferred;
keyword input came first).

### SPECOPS-DRAFT-manifest-D2 — Manifests describe structure only; logic stays at the call site

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

### SPECOPS-DRAFT-manifest-D3 — Compile-time manifest registry with inlining macros

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

### SPECOPS-DRAFT-manifest-D4 — Keywords are fields, specops symbols are directives

**Status:** Accepted
**Context:** Field declarations and structural directives share one list.
**Decision:** A clause led by a keyword is an input field
(`(:width (:u 4) :default 0)`); a clause led by a specops symbol is a
directive (`masque`, `pad`, `str`, `span`, `manifest`), dispatched by symbol
identity. Inside a `masque` the same rule applies per group: a keyword is an
input field, anything else a static value.
**Alternatives:** Dispatch by symbol name with an open directive registry
(rejected in favor of fixed, package-identified directives).

### SPECOPS-DRAFT-manifest-D5 — Unit-counted width specs with modifier sets

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

### SPECOPS-DRAFT-manifest-D6 — Per-character string codecs, Latin-1 by default

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

### SPECOPS-DRAFT-manifest-D7 — Backpatching through destination `tell`/`write` with absolute positions

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

### SPECOPS-DRAFT-manifest-D8 — Nesting-only spans, region-close slot resolution over a runtime map

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

### SPECOPS-DRAFT-manifest-D9 — Pluggable checksums over output spans

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

### SPECOPS-DRAFT-manifest-D10 — Sub-manifests inline into the parent through a sub-lexicon

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

### SPECOPS-DRAFT-manifest-D11 — Per-instance name prefixes; spans do not prefix

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

### SPECOPS-DRAFT-manifest-D12 — `deserializer-for` as the inverse of `serializer-for`

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

### SPECOPS-DRAFT-manifest-D13 — unmarshal return shapes: values when keyed, plist otherwise

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

### SPECOPS-DRAFT-manifest-D14 — Masque reads by direct group extraction

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
  SPECOPS-DRAFT-manifest-D12, with a shared `finish` step for byte order and
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
  masques per SPECOPS-DRAFT-manifest-D14, and constant strings validated per
  SPECOPS-DRAFT-manifest-D6.

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

### SPECOPS-DRAFT-manifest-D15 — Pads are sized at run time, `:to` from the record's start and `:align` from the output's

**Status:** Proposed
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

### SPECOPS-DRAFT-manifest-D16 — Codecs apply to `:vec` fields, and unmapped input is an error

**Status:** Proposed
**Context:** SPECOPS-DRAFT-manifest-D6 applied codecs only to the constant
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
field type that SPECOPS-DRAFT-manifest-complete-D1 proposed.
**Alternatives:** A dedicated `:str` field type (not built: the `:vec` field
with a codec covers it with no new type, and
SPECOPS-DRAFT-manifest-complete-D1 may be reworded); passing unmapped input
through as `nil` or skipping it (rejected: corrupt output with no error).

### SPECOPS-DRAFT-manifest-D17 — A character terminator goes through the codec; an integer terminator is a raw code

**Status:** Proposed
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
planned in SPECOPS-DRAFT-manifest-complete-D8 and
SPECOPS-DRAFT-manifest-complete-D7.

### SPECOPS-DRAFT-manifest-D18 — Variable-length fields are built from stop rules and assemblers whose effectors emit fused code at expansion

**Status:** Proposed
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
runtime protocol (SPECOPS-DRAFT-manifest-complete-D9).
**Alternatives:** A branch per field kind (the earlier design); the runtime
scanner protocol as the only path (rejected as the primary path: dispatch and a
buffer per element; kept as the planned fallback); closures returned by
factories, like `serializer-for` (rejected: harder for users to extend);
run-time code generation as in April (unnecessary: the facts it waits for are
known at expansion).

### SPECOPS-DRAFT-manifest-D19 — A `:case` field is a choice between complete field plans

**Status:** Proposed
**Context:** `(:case field …)` (SPECOPS-DRAFT-manifest-D5) stored its branches
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

### SPECOPS-DRAFT-manifest-D20 — `:case` keys compare integer codes, and `otherwise` is the default branch

**Status:** Proposed
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

### SPECOPS-DRAFT-manifest-D21 — Directives dispatch by symbol name, and `_` names an unnamed field

**Status:** Proposed
**Context:** SPECOPS-DRAFT-manifest-D4 dispatched directives by symbol
identity. A manifest written in a package that does not import SpecOps's
internal `span`, `pad`, `str` and `manifest` lost those clauses without a
message, as it did a clause led by `_` and a misspelt directive.
**Decision:** `defmanifest` dispatches a clause led by a non-keyword symbol on
the symbol's name, so directives work from any package. A clause led by `_`
from any package, or by `:_`, is an unnamed field: it is written with its
default and skipped on read. An unknown directive signals an error. This
amends SPECOPS-DRAFT-manifest-D4, which rejected dispatch by name.
**Alternatives:** Keeping identity and exporting the directive symbols
(rejected: `_` would have to be exported, and misspellings would still be
dropped); accepting only `:_` for unnamed fields (rejected: `_` is the
conventional placeholder).

### Implementation

- **`specops.lisp`.**
  - `specops.lisp:pad-count-form@653345f` sizes pads per
    SPECOPS-DRAFT-manifest-D15.
  - `specops.lisp:codec-for@653345f` and `specops.lisp:defcodetable@653345f`
    implement the codec errors of SPECOPS-DRAFT-manifest-D16.
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
SPECOPS-DRAFT-manifest-D16), and `:case` widths. The exports are done, but
`goff.lisp` is still in no ASDF system and the suites are not behind `test-op`.
The other earlier items remain open.

New items:

- **`:vec` on the scan model.** `:vec` fields still use their own branches; the
  terminator scan reads each element twice.
- **Agreed but not built.** Fixed buffers holding a terminator
  (SPECOPS-DRAFT-manifest-complete-D7), multi-element terminators
  (SPECOPS-DRAFT-manifest-complete-D8), and user-defined stop rules with the
  runtime fallback (SPECOPS-DRAFT-manifest-complete-D9).
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
LEB128 and `:case` fields on the scan model of SPECOPS-DRAFT-manifest-D18, so
that every variable-length field is now built from a field plan, and each
macro handles them in one clause. A terminated vector is read in one pass. Two
rules were settled on the way: how vector elements are encoded on write, and
where enum decoding happens. The new suite brings the checks from 117 to 135.

Soon after `738105a`, the repository restructure
([SPECOPS-DRAFT-restructure](Log.Restructure.md)) moved the code this
installment names: `scan.lisp` to `src/manifest/scan.lisp`, the manifest
layer of `specops.lisp` to `src/manifest/manifest.lisp`, and `manifest.lisp`
to `test/manifest.lisp`. It changed no behavior. The references below are
pinned to `738105a`, where the files had their old paths.

### SPECOPS-DRAFT-manifest-D22 — Vector elements follow the terminator rule, and signed elements are written as raw codes

**Status:** Proposed
**Context:** On write, an element of a `:vec` field with a codec went through
the codec whatever its type, and a `defcodetable` function given an integer
decodes it, so integers in a text field failed. Keyword elements of an
enumerated vector were not mapped. A negative element of a signed field was
compared with the terminator as a negative number, while the terminator is a
raw code: `-1` in an `(:s 1 :vec)` field ended by `#xFF` passed the check,
was written as `FF`, and would end the field early for a reader.
**Decision:** Each element of a vector is encoded by the rule
SPECOPS-DRAFT-manifest-D17 set for terminators. A character goes through the
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

### SPECOPS-DRAFT-manifest-D23 — Each field plan decodes its own enum

**Status:** Proposed
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
(SPECOPS-DRAFT-manifest-D22).
**Alternatives:** Decoding vector elements in the assembler and other values
outside it (rejected by the steward: wrong for a `:case` mixing scalar and
vector branches, and two places to look).

### SPECOPS-DRAFT-manifest-D24 — Vector, LEB and `:case` fields are built from field plans in one clause per macro

**Status:** Proposed
**Context:** After SPECOPS-DRAFT-manifest-D19, `:case` and LEB fields went
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
  sets its selector variable (SPECOPS-DRAFT-manifest-D20).
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
  (SPECOPS-DRAFT-manifest-complete-D7), multi-element terminators
  (SPECOPS-DRAFT-manifest-complete-D8), and user-defined stop rules with the
  runtime fallback (SPECOPS-DRAFT-manifest-complete-D9); each adds a rule or
  an assembler to the scan model.
- **Vectors of LEB elements.**
- **Keyword defaults on enumerated fields**, still not mapped through the enum.
- **Unknown enum codes**, still read as `nil`.
- **Error wording.** A selector placed after its `:case` field is still
  reported against the manifest's name.

## Update 2026-10-31 — The scan model completed: fixed buffers, multi-element terminators, LEB vectors and user stop rules

This installment covers commits `f7713c5` through `d030f5c`, which complete
the scan model of SPECOPS-DRAFT-manifest-D18 as the Manifest Completion Plan
laid out. It builds three of that plan's decisions:
SPECOPS-DRAFT-manifest-complete-D7 (fixed buffers holding a terminator),
SPECOPS-DRAFT-manifest-complete-D8 (terminators of several elements) and
SPECOPS-DRAFT-manifest-complete-D9 (user-defined stop rules with a runtime
fallback). It also adds vectors of LEB128 elements. The groundwork for all
four came first, in a commit that changed no behavior. Four suites bring the
checks from 135 to 202. The code is now in `src/manifest/`, and references
are pinned to `d030f5c`.

Between this update and the last, commits `5e0976e` and `d524c3c` taught
`masque` and `unmasque` decimal mask strings:
`src/serial.lisp:quantify-mask-string@d524c3c` now also returns the
decimal factor. The manifest layer's two callers were adapted, but
`unmarshal` still rejects decimal masques.

### SPECOPS-DRAFT-manifest-D25 — A scan keeps a position, and rules report their extent apart from their count

**Status:** Proposed
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
reason for SPECOPS-DRAFT-manifest-D18); collecting every element into a buffer
and trimming afterwards (rejected: the trailer drops elements only when a
rule asks).

### SPECOPS-DRAFT-manifest-D26 — Element plans nest, and a vector of LEB elements is a scan over an LEB scan

**Status:** Proposed
**Context:** The modifier set allows `(:u 1 :vec :leb)`
(SPECOPS-DRAFT-manifest-D5). Each element is itself a variable-length
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

### SPECOPS-DRAFT-manifest-D27 — A fixed buffer must leave room for a whole terminator

**Status:** Proposed
**Context:** SPECOPS-DRAFT-manifest-complete-D7 pads a short buffer with the
terminator repeated, and SPECOPS-DRAFT-manifest-complete-D8 allows terminators
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
(rejected in SPECOPS-DRAFT-manifest-complete-D7).

### SPECOPS-DRAFT-manifest-D28 — User stop rules run through a five-function protocol until they define effectors

**Status:** Proposed
**Context:** SPECOPS-DRAFT-manifest-complete-D9 lets a field name a user's
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
`serializer-for` (rejected in SPECOPS-DRAFT-manifest-D18); one instance per
manifest (rejected: two fields with the same rule would share state).

### Implementation

- **Groundwork, `f7713c5`.** `emit-scan`, `emit-frame` and `known-extent` per
  SPECOPS-DRAFT-manifest-D25; the vector assembler trims its fill pointer by a
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
  like the selector variables of SPECOPS-DRAFT-manifest-D20.
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

### SPECOPS-DRAFT-manifest-D29 — A field without a byte order takes its manifest's; strings are in stream order

**Status:** Proposed
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
order belongs to its codec (SPECOPS-DRAFT-manifest-D6).
**Alternatives:** Requiring `:endian` on every field (rejected: the manifest's
`:endian` exists to state it once); giving each nested manifest serializers
of its own (rejected: nested expansions share one environment by design,
SPECOPS-DRAFT-manifest-D10). Who owns byte order across the system remains
the architecture's open question SPECOPS-DRAFT-specops-O3; this settles it
within a manifest.

### SPECOPS-DRAFT-manifest-D30 — A code an enum lacks is read as its integer

**Status:** Proposed
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

### SPECOPS-DRAFT-manifest-D31 — A correction is tested in the suite of the feature it corrects

**Status:** Proposed
**Context:** The corrections could have been tested by a suite of their own.
Such a suite gathers checks by when their faults were found rather than by
what they cover, and must change whenever any of those features does.
**Decision:** Each correction's checks join the suite of the field kind or
directive it corrects, and the log records how many each suite gained.
**Alternatives:** A corrections suite (rejected by the steward, for the
reasons above).

### Implementation

- **Byte orders, SPECOPS-DRAFT-manifest-D29.** In
  `src/manifest/manifest.lisp:defmanifest@2e1af03`, a field's `:endian`
  defaults to `nil`, the manifest's byte order to big-endian, and a `str`
  directive stores byte order 0. In `src/manifest/manifest.lisp:marshal@2e1af03`,
  `manifest-swap-specs` replaces the byte-order collection in
  `preprocess-spec`.
- **Keyword defaults.** A default goes through the enum like a given value,
  in `marshal`'s `scalar-value-form` and its masque binder. The scalar clause
  of `generate`, which repeated `scalar-value-form`, now calls it.
- **Unknown codes, SPECOPS-DRAFT-manifest-D30**, in `enumerated`.
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
  protocol functions of SPECOPS-DRAFT-manifest-D28, `stop-effector` and
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
SPECOPS-DRAFT-manifest-complete-O2 and SPECOPS-DRAFT-manifest-complete-O3, and
build its SPECOPS-DRAFT-manifest-complete-D4 with a class in place of the
struct it named. A new suite brings the checks from 217 to 258.

### SPECOPS-DRAFT-manifest-D32 — A manifest's record is a CLOS class, defined on request, with its names in the class's package

**Status:** Proposed
**Context:** SPECOPS-DRAFT-manifest-complete-D4 proposed that `defmanifest`
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
**Alternatives:** A struct, as SPECOPS-DRAFT-manifest-complete-D4 proposed
(rejected by the steward: the redefinition problem above); a record for every
manifest (rejected by the steward: needless types and possible clashes);
derived names in the package current at the `defmanifest` form (rejected by
the steward in favor of keeping them beside the class).

### SPECOPS-DRAFT-manifest-D33 — `marshal` takes a run-time record through an options list

**Status:** Proposed
**Context:** `marshal` took each field's value from its literal arguments at
expansion, so a sub-manifest's values had to be a literal plist in the call,
and a record built or read at run time could not be written
(SPECOPS-DRAFT-manifest-complete-O3). A reserved keyword argument such as
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

### SPECOPS-DRAFT-manifest-D34 — `unmarshal` takes its options in its params, `(:as :record)` among them

**Status:** Proposed
**Context:** `unmarshal` returned values for given keys and a plist otherwise
(SPECOPS-DRAFT-manifest-D13). Records needed a way to ask for them, and
SPECOPS-DRAFT-manifest-complete-O2 asked where `unmarshal`'s options belong.
**Decision:** Options go in `unmarshal`'s `params` argument, which already
carried the internal sub-lexicon: `(unmarshal png-file source (:as :record))`
returns an instance of the manifest's record class, and `:as :plist` is the
default. Nested sub-manifests are read as records when they have a record
class, and as plists inside the parent's record when they do not. An unknown
option, an `:as` other than `:plist` or `:record`, `:as :record` with keys,
and `:as :record` for a manifest without a record class are expansion errors.
**Alternatives:** Options after the manifest's name, as in
`(unmarshal (png-file :record) …)` (rejected in
SPECOPS-DRAFT-manifest-complete-O2: the name should stay a plain symbol);
always returning a record when the manifest has one (rejected: the plist stays
the default, as callers already expect).

### SPECOPS-DRAFT-manifest-D35 — Round trips are canonical

**Status:** Proposed
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
  plan's SPECOPS-DRAFT-manifest-complete-D2.
- **Exact round trips** of sources that are not canonical, should a format
  need them (SPECOPS-DRAFT-manifest-D35).
- The items left from the previous update remain.

## Update 2026-11-01 — Variable structure

This installment covers commits `a12b394` through `d7ba27f`, which build the
Manifest Completion Plan's SPECOPS-DRAFT-manifest-complete-D5 and answer its
SPECOPS-DRAFT-manifest-complete-O4. A sub-manifest became a field plan, so
that a `:case` field can choose one and a manifest can repeat one: by a
count, by a count field, until an instance matches a condition, to the end of
the source, or until a length slot's bytes are used. Count fields can be
computed on write. One manifest now reads and writes any sequence of PNG
chunks. The checks went from 258 to 292.

### SPECOPS-DRAFT-manifest-D36 — A sub-manifest is a field plan, expanded by the macro that holds its parent

**Status:** Proposed
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

### SPECOPS-DRAFT-manifest-D37 — A `:case` branch may be a sub-manifest, `(key (manifest name))`

**Status:** Proposed
**Context:** Chunked formats choose a record's layout by a tag, as PNG does
by a chunk's type. SPECOPS-DRAFT-manifest-complete-D5 asked for `:case` to
choose a sub-manifest, with a default branch.
**Decision:** A branch written `(key (manifest name))` is stored like the
`manifest` directive and read and written through a manifest plan. Keys stay
codes through the selector's enum (SPECOPS-DRAFT-manifest-D20), so a tag the
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

### SPECOPS-DRAFT-manifest-D38 — A repeated sub-manifest is a scan whose elements are instances

**Status:** Proposed
**Context:** SPECOPS-DRAFT-manifest-complete-D5 asked for sub-manifests
repeated by a count, by a count or length field, or until an instance
matching a condition on its fields, read as a list. A PNG file is a signature
and chunks up to IEND.
**Decision:** Options after the child's name in the `manifest` directive make
it a repeat: `:count n`, `:count :field`, `:until (field value …)`,
`:until :end`, or `:until :length` (SPECOPS-DRAFT-manifest-D40). In
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
SPECOPS-DRAFT-manifest-D2); an empty repeat as an error (rejected: it must
round-trip).

### SPECOPS-DRAFT-manifest-D39 — Slots inside a repeated instance resolve within that instance

**Status:** Proposed
**Context:** Regions are named by keywords fixed at expansion, made unique per
sub-manifest field by prefixes (SPECOPS-DRAFT-manifest-D11). A repeat writes
its child's code once and runs it for each instance, so every instance has
the same region names. SPECOPS-DRAFT-manifest-complete-O4 asked how repeated
instances are named and addressed.
**Decision:** Each instance pushes fresh regions onto the map as it is
written, as spans already did, so a slot inside an instance, such as a chunk's
length or CRC, resolves within that instance. A slot outside a repeat can
cover the whole repeat, but not one instance of it.
**Alternatives:** Names built at run time, such as `:chunks/3/crc`, so that a
slot outside a repeat could address one instance (rejected by the steward as
more machinery than chunked formats need; possible later).

### SPECOPS-DRAFT-manifest-D40 — A length slot bounds elements of varying width in units

**Status:** Proposed
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

### SPECOPS-DRAFT-manifest-D41 — A `:count-of` slot is backpatched with the elements written

**Status:** Proposed
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
  (SPECOPS-DRAFT-manifest-D39).
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
SPECOPS-DRAFT-manifest-complete-O1. A new suite brings the checks from 292 to
315.

### SPECOPS-DRAFT-manifest-D42 — A failed check signals a condition with restarts, and `(:check nil)` leaves the checks out

**Status:** Proposed
**Context:** `unmarshal` read checksum, length and offset slots as plain values,
so a corrupt file passed silently, while a constant that did not match
signalled a plain error, which ended the read.
SPECOPS-DRAFT-manifest-complete-O1 asked what a failed check should do. A
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
SPECOPS-DRAFT-manifest-complete-O1 suggested (rejected by the steward: a
damaged file could be read only in a separate mode, and the record could not be
repaired as it is read); returning a status with the values (rejected: a caller
that ignores it lets corrupt data pass); restarts with no option (rejected by
the steward: the checks' work could not be skipped).

### SPECOPS-DRAFT-manifest-D43 — Checks are placed at expansion from a region table, with variables only for the regions they need

**Status:** Proposed
**Context:** `marshal` records regions in a list built as its code runs,
pushing an entry for every region of every instance
(SPECOPS-DRAFT-manifest-D8). Read the same way, a file of many chunks would
allocate for each of them. Slots inside a repeated instance must resolve
within it (SPECOPS-DRAFT-manifest-D39), and selective reads
(SPECOPS-DRAFT-manifest-complete-D3) will need to know which fields and spans
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

### SPECOPS-DRAFT-manifest-D44 — An integer source is checksummed through a buffer of its units, made once

**Status:** Proposed
**Context:** `unmarshal` reads integers and `(units . integer)` pairs as well
as vectors (SPECOPS-DRAFT-manifest-D12), but a checksum function reads a
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

### SPECOPS-DRAFT-manifest-D45 — Every checksum function takes a buffer, a start and an end

**Status:** Proposed
**Context:** A `:checksum-of` slot calls its function as `(fn buffer start
end)` (SPECOPS-DRAFT-manifest-D9), which `checksum.lisp:crc@10b1568` follows.
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
    SPECOPS-DRAFT-manifest-D42.
  - `manifest-regions`, `manifest-checksums-p` and `source-units`, per
    SPECOPS-DRAFT-manifest-D43 and SPECOPS-DRAFT-manifest-D44.
  - In `src/manifest/manifest.lisp:unmarshal@1cc7600`, the `:check` option,
    passed to nested expansions with `:as`; the buffer's variable in the
    sub-lexicon; a local `process-item` that records where a region begins and
    ends around `read-item`, the former body; `check-form`, which builds a
    slot's check, and `emit-ready-checks`, which places it; and
    `constant-failed`, which the constant checks of strings, terminators and
    masques now call. A string reports its first differing unit only.
- **`src/checksum.lisp`.** `adler32`, per SPECOPS-DRAFT-manifest-D45.
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
| `doc/Plan.ManifestComplete.md` | Modified | Progress installment for checked reads; SPECOPS-DRAFT-manifest-complete-O1 answered |

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
SPECOPS-DRAFT-manifest-complete-O6 for writing. A vector too small signals a
condition of its own. One walker over nested manifests now serves the code
that needs them all, which fixes the byte orders of sub-manifests chosen by
`:case`. A fault in writing to streams was found and removed on the way. The
checks went from 315 to 335.

### SPECOPS-DRAFT-manifest-D46 — A manifest with slots is written through a buffer to any destination it cannot patch in place

**Status:** Proposed
**Context:** Slots are backpatched through the destination's `write`, and
checksums read the destination itself (SPECOPS-DRAFT-manifest-D7,
SPECOPS-DRAFT-manifest-D9). In `src/manifest/manifest.lisp:marshal@1cc7600` a
function had no `write`, so any slot failed by calling `nil`, and a checksum
was handed the function or stream it was to read. A stream was patched by
seeking to a recorded position, which counts from where `marshal` began,
while `file-position` counts from the start of the file, and after each patch
the position was set to the stream's absolute one. A record written after
anything else in a stream therefore had its slots written over earlier bytes:
a length slot written after a 2-byte header gave `#(3 7 0 1 0 5 9)` for
`#(7 7 3 1 0 5 9)`. The GOFF writer writes records partway into a stream, and
was spared only because its END record has no slots.
SPECOPS-DRAFT-manifest-complete-D2 planned a buffer for destinations that
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
SPECOPS-DRAFT-manifest-D7, whose streams were patched by seeking.
**Alternatives:** Patching streams that can seek, after fixing the positions
(rejected by the steward: deciding at run time whether a stream can seek has
many pitfalls, and pipes and sockets cannot); buffering every manifest
(rejected in SPECOPS-DRAFT-manifest-complete-D2: manifests without slots would
pay for it); patching every vector in place (rejected: `crc` declares a simple
byte array, so a checksum failed on a vector with a fill pointer or of general
elements, as the new suite showed).

### SPECOPS-DRAFT-manifest-D47 — A caller may keep the buffer across calls in a `manifest-buffer`

**Status:** Proposed
**Context:** A buffer made for every call puts pressure on the garbage
collector when many records are written. A buffer kept across calls needs an
owner, and SPECOPS-DRAFT-manifest-complete-O6 asked who owns it, how large it
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

### SPECOPS-DRAFT-manifest-D48 — A vector too small signals `manifest-overrun`, and a fill pointer is neither read nor set

**Status:** Proposed
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

### SPECOPS-DRAFT-manifest-D49 — One walker visits every manifest nested in another, `:case` branches included

**Status:** Proposed
**Context:** `marshal` sizes its serializers from the byte orders of every
manifest it expands (SPECOPS-DRAFT-manifest-D29), but
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
    SPECOPS-DRAFT-manifest-D49.
  - `manifest-overrun` and `signal-overrun`, per SPECOPS-DRAFT-manifest-D48;
    `manifest-buffer`, `reusable-buffer` and
    `src/manifest/manifest.lisp:grow-buffer@b63b7d2`, per
    SPECOPS-DRAFT-manifest-D46 and SPECOPS-DRAFT-manifest-D47.
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
each failing as the contexts of SPECOPS-DRAFT-manifest-D46,
SPECOPS-DRAFT-manifest-D48 and SPECOPS-DRAFT-manifest-D49 describe. The manifest layer has no style warnings, and
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
| `doc/Plan.ManifestComplete.md` | Modified | Progress installment for destinations; SPECOPS-DRAFT-manifest-complete-O6 answered for writing; a note under SPECOPS-DRAFT-manifest-complete-D2 |

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
SPECOPS-DRAFT-manifest-complete-O6. The second half of the milestone,
selective reads, follows in its own installment. A new suite brings the checks
from 335 to 348.

### SPECOPS-DRAFT-manifest-D50 — A stream is a source of `deserializer-for`, read as far as each read needs and kept

**Status:** Proposed
**Context:** `unmarshal` read vectors, integers and `(units . integer)` pairs
(SPECOPS-DRAFT-manifest-D12), at any index. Every field is read at or after
the cursor, but a checksum reads its region again (SPECOPS-DRAFT-manifest-D43),
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
destinations are not (SPECOPS-DRAFT-manifest-D46), and every unit of the
record is kept. `src/serial.lisp:source-end-p@239b2f7` tells the end rule
whether a vector or a stream has a unit at a position, reading one unit ahead
of a stream only then. The top-level `unmarshal` wraps a stream in a
`stream-source`; nested expansions share it, and checksums read its array.
**Alternatives:** Wrapping the deserializers in `unmarshal` (rejected: nested
expansions build their own); seeking a file stream to skip or to read a region
again (rejected by the steward for the reasons of
SPECOPS-DRAFT-manifest-D46); reading a stream in blocks ahead of need
(rejected: the stream would be left past the end of the record); dropping
units no check still needs (deferred: the record is kept whole, as it is when
written through a buffer).

### SPECOPS-DRAFT-manifest-D51 — A read from a stream takes exactly one record

**Status:** Proposed
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

### SPECOPS-DRAFT-manifest-D52 — `unmarshal` keeps a stream's units in the `manifest-buffer` that `marshal` uses

**Status:** Proposed
**Context:** SPECOPS-DRAFT-manifest-D47 let a caller keep `marshal`'s buffer
across calls and intended stream sources to accept the same object, which
SPECOPS-DRAFT-manifest-complete-O6 asked about for reading.
**Decision:** `(:buffer b)` in `unmarshal`'s params gives a stream source the
array a `manifest-buffer` holds, when its elements are of the manifest's unit
type, and the grown array is stored back when the record is read. One type
serves both directions, and one instance may serve both, since each call
leaves the buffer free when it returns.
**Alternatives:** A separate buffer type for reading (rejected: SPECOPS-DRAFT-manifest-D47
intended one); passing the array itself (rejected there: a grown array would be
lost to the caller).

### Implementation

- **`src/serial.lisp`.** `stream-source`, `fill-stream-source` and
  `source-end-p`, per SPECOPS-DRAFT-manifest-D50, placed before
  `deserializer-for`, whose vector case became a local `from-vector` that the
  stream case shares. `src/serial.lisp:grow-buffer@239b2f7` moved here from
  `manifest.lisp`, since both directions use it.
- **`src/manifest/manifest.lisp`.** In
  `src/manifest/manifest.lisp:unmarshal@239b2f7`: the `:buffer` option; the
  top-level binding of the source, which wraps a stream; the checksum's buffer,
  chosen by the kind of source; and the read to the record's end, which also
  stores the grown array back, per SPECOPS-DRAFT-manifest-D51 and
  SPECOPS-DRAFT-manifest-D52.
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
| `doc/Plan.ManifestComplete.md` | Modified | Progress installment for stream sources; SPECOPS-DRAFT-manifest-complete-O6 answered for reading; key paths deferred |

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
  whole, as SPECOPS-DRAFT-manifest-D50 decided; dropping units no check still
  needs would serve records too large to hold.
- **A stream's element type** must match the manifest's unit; a character
  stream cannot be read.
- **The reader protocol of the architecture.** The stream source is a
  candidate base for the protocol that SPECOPS-DRAFT-specops-O4 asks
  disassembly to offer; that question remains open.
- The other items of the previous updates remain.
