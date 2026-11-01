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
log carry the record forward, that of 2026-10-30 to commit `653345f` and that
of 2026-10-31 to commit `738105a`. The repository restructure that followed
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
