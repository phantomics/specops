---
id:            SPECOPS-DRAFT-manifest-complete
title:         Manifest Completion Plan
genre:         Plan
scope:         component
project:       SpecOps
component:     Manifest
language:      en
status:        Draft
provenance:
  assistant:   claude-opus (OpenCode)
  session:     Marshal
relates-to:
  - SPECOPS-DRAFT-manifest
  - SPECOPS-DRAFT-specops
  - SPECOPS-DRAFT-restructure
glossary:      SPECOPS-DRAFT-glossary
decisions:
  - SPECOPS-DRAFT-manifest-complete-D1
  - SPECOPS-DRAFT-manifest-complete-D2
  - SPECOPS-DRAFT-manifest-complete-D3
  - SPECOPS-DRAFT-manifest-complete-D4
  - SPECOPS-DRAFT-manifest-complete-D5
  - SPECOPS-DRAFT-manifest-complete-D6
  - SPECOPS-DRAFT-manifest-complete-D7
  - SPECOPS-DRAFT-manifest-complete-D8
  - SPECOPS-DRAFT-manifest-complete-D9
open-questions:
  - SPECOPS-DRAFT-manifest-complete-O1
  - SPECOPS-DRAFT-manifest-complete-O2
  - SPECOPS-DRAFT-manifest-complete-O3
  - SPECOPS-DRAFT-manifest-complete-O4
  - SPECOPS-DRAFT-manifest-complete-O5
cites:
  - title:    lisp-binary (Common Lisp binary format library) wiki
    locator:  github.com/j3pic/lisp-binary/wiki — DEFBINARY structs; EVAL type specifier
    external: true
  - title:    Kaitai Struct User Guide
    locator:  doc.kaitai.io — repetitions, switch-on types, instances with pos
    external: true
  - title:    Construct (Python declarative binary parsing and building library)
    locator:  construct.readthedocs.io — Prefixed, RepeatUntil, Switch, Checksum, Pointer
    external: true
  - title:    WOFF File Format 1.0
    locator:  W3C Recommendation — WOFF header and table directory
    external: true
---

# Manifest Completion: Development Plan

Part of SpecOps's manifest layer is written. At commit `1e53ae7`,
`defmanifest` declares binary records, and `marshal` and `unmarshal` write and
read most of what a manifest can declare: scalars, enums, vectors, masques,
constant strings, pads, spans with length and checksum slots, and nested
sub-manifests. A 1×1 PNG built from manifests decodes in an outside image
library. This plan describes the work that brings the layer to completion:
the field kinds that still work in only one direction or neither, validation
and selective reading on the read side, repeated and tag-dispatched structure,
a struct interface, destinations that cannot be patched, and packaging. It
assumes the decisions recorded in the
[Manifest development log, SPECOPS-DRAFT-manifest](Log.Manifest.md) and the
architecture in [SPECOPS-DRAFT-specops](Arch.SpecOps.md). Progress is charted
in dated updates at the end of this plan; the latest is the update of
2026-10-30, at commit `653345f`.

## Problem

The manifest layer was built by writing PNG and GOFF records and fixing what
they exposed, so it is complete where those formats reached and incomplete
elsewhere. A format author today cannot describe a file whose chunk sequence
is not known in advance, cannot pass a string field from the caller, cannot
read back a record and write it again, and cannot rely on `unmarshal` to catch
a corrupt checksum. Several features work on the write side only, one write
branch is broken, and none of the public names except `marshal` is exported.

The table below records the state at `1e53ae7`, as read from
`specops.lisp:marshal@1e53ae7` and `specops.lisp:unmarshal@1e53ae7` and
exercised by the suites in `manifest.lisp`.

Table: What `marshal` and `unmarshal` handle at commit `1e53ae7`.

| Feature | `marshal` | `unmarshal` |
|---|---|---|
| Scalars, signed and unsigned, per-field byte order | Yes | Yes |
| Enums, with literal or runtime values | Yes | Yes |
| `:vec` counted by `:count`, an earlier field, or a `:length-of` slot | Yes | Yes |
| `:leb` (self-delimiting integers) | Broken | No |
| `:case` width selected by an earlier field | Yes, untested | No |
| Terminated fields (`:end-by` on a field) | No | No |
| Masques | Yes | Yes, except decimal (`d:`) mask strings |
| Constant `str` with codec and terminator | Yes | Yes, validated |
| Caller-supplied `:str` fields | No | No |
| `pad` by count, `:to` and `:align`; trailing `:length` | Yes | Yes |
| Spans with `:length-of` and `:checksum-of` slots | Yes | Read as plain values, not checked |
| `:offset-of` slots | Yes, untested | Read as a plain value |
| Sub-manifests, nesting and per-instance prefixes | Yes | Yes |
| Struct input and output | No | No |

Two items in the log's Outstanding Work are already done at `1e53ae7`: enum
values known only at run time are now mapped by `marshal`, and `pad :align`
computes the padding still needed. The pad rework itself (`:to` counted from
the start of each manifest instance, `:align` from the start of the output,
sizes computed at run time by `specops.lisp:pad-count-form@1e53ae7`) is not
yet recorded in the log.

The `:leb` write branch passes a `(width . value)` pair to
`specops.lisp:encode-leb128-unsigned@1e53ae7`, which takes an integer, and
tests `signed` when the generated code runs rather than when it is expanded.
No test covers it.

Since then, the rows below have changed. The log's update of 2026-10-30
records the work (SPECOPS-DRAFT-manifest-D15 through
SPECOPS-DRAFT-manifest-D21).

Table: Features whose state changed between commits `1e53ae7` and `653345f`.

| Feature | `marshal` | `unmarshal` |
|---|---|---|
| `:leb` (self-delimiting integers) | Yes | Yes |
| `:case` width selected by an earlier field | Yes, including enumerated selectors and an `otherwise` branch | Yes |
| Terminated fields (`:end-by` on a `:vec` field), one-element terminator | Yes | Yes |
| Caller-supplied text, as a `:vec` field with a `:codec` | Yes | Yes, returned as a string |

## Goals

The layer is complete when all of these hold, each covered by a test that
runs under ASDF's `test-op`:

- **Duplex field kinds.** Every field kind and directive that `marshal`
  writes, `unmarshal` reads: `:leb`, `:case`, terminated fields,
  caller-supplied strings, floats and decimal masques included.
- **Checked reads.** `unmarshal` checks checksums, length slots and constants,
  and reports a mismatch according to one documented policy.
- **Variable structure.** A file whose chunk sequence is not known in advance,
  such as a PNG with ancillary chunks or several IDAT chunks, can be described
  and read with one manifest.
- **Round trips.** A record read with `unmarshal` can be written again with
  `marshal` and reproduces its bytes, through plists or structs.
- **Any destination.** Every manifest marshals to a vector, a stream or a
  function, including manifests with slots and checksums, and writing past
  the end of a fixed-size vector signals an error.
- **Selective reads.** Requesting a few fields reads only those fields and
  the fields their positions depend on.
- **A second container format.** WOFF, with an offset table, round-trips.
- **Packaging.** The public names are exported, the format writers that use
  manifests are in ASDF systems, the log records this stage, and a Ref
  documents the interface once it is settled.

## Settled Decisions

### SPECOPS-DRAFT-manifest-complete-D1 — Caller-supplied string fields use the per-character codec

**Status:** Proposed
**Context:** Many formats carry strings chosen by the writer: a generic
chunk's tag, a section or symbol name, a text keyword. The constant `str`
directive covers only literals. A field declared `:str` is parsed by
`specops.lisp:defmanifest@1e53ae7`, which sets only its width, so `marshal`
treats it as a one-unit integer and fails on a string. Codecs are
per-character functions (SPECOPS-DRAFT-manifest-D6).
**Decision:** A field declared `(:name :str :codec #'fn …)` encodes and
decodes through the same per-character codec as `str`, with Latin-1 as the
default. Its length comes from `:count`, from a `:length-of` slot, or from an
`:end-by` terminator, the same sources a `:vec` field uses. `unmarshal`
returns it as a string.
**Alternatives:** Whole-string encode and decode functions (deferred: only
variable-width encodings such as UTF-8 or Shift-JIS need them); declaring
strings as `:vec` fields of codes (rejected: callers would encode by hand and
the codec would go unused).

Note: caller text was built as a `:vec` field with a `:codec` instead
(SPECOPS-DRAFT-manifest-D16), which uses the codec, so the reason given above
for rejecting that form no longer holds. This record may be reworded or
withdrawn.

### SPECOPS-DRAFT-manifest-complete-D2 — Destinations that cannot be patched are buffered only when a manifest needs it

**Status:** Proposed
**Context:** Slots are resolved by writing at a recorded position, and
checksums read the bytes of a region back from the destination
(SPECOPS-DRAFT-manifest-D7, SPECOPS-DRAFT-manifest-D9). In
`specops.lisp:marshal@1e53ae7` only vector and stream destinations have a
`write` closure, and checksums read the destination as a vector. A function
destination, or a stream that cannot be read back, can hold neither.
**Decision:** When a manifest contains slots or checksums and its
destination cannot be patched and read, `marshal` writes into an internal
vector, resolves slots and checksums there, and then passes the bytes on to
the destination. Whether a manifest needs this is decided at expansion, so
manifests without slots keep writing directly.
**Alternatives:** Buffering every manifest (rejected: linear manifests would
pay for a feature they do not use); refusing such destinations (rejected:
writing object files to a stream is the common case); seeking a file stream to
read back checksummed bytes (rejected: needs the stream opened for reading as
well).

### SPECOPS-DRAFT-manifest-complete-D3 — Selective reads use a dependency closure computed at expansion

**Status:** Proposed
**Context:** `specops.lisp:unmarshal@1e53ae7` reads every named scalar and
every sub-manifest in full, whatever keys are requested, so that counts and
lengths are available to later fields. Reading one field from a large file
decodes all of it.
**Decision:** At expansion, `unmarshal` computes the closure of the requested
keys: the requested fields, and the fields that determine their positions or
types (counts, length slots, `:case` selectors, and the length fields of
variable-length fields before them). Fields outside the closure are skipped:
fixed-width runs by moving the cursor, variable-length fields by reading only
their length. Sources are vectors, file streams (using `file-position`), and,
as a fallback, streams read and discarded unit by unit.
**Alternatives:** Always reading in full (the current behavior, kept as the
fallback when the closure covers everything); a lazy parser that decodes on
access at run time (rejected: loses the code specialization that macro
expansion gives).

### SPECOPS-DRAFT-manifest-complete-D4 — `defmanifest` generates a struct for record input and output

**Status:** Proposed
**Context:** `marshal` accepts keyword arguments only, and `unmarshal`
returns values or a plist (SPECOPS-DRAFT-manifest-D13). Programs that build or
edit records, such as a linker updating a header, want an object to pass
around, and a round trip needs `unmarshal`'s output to feed `marshal`.
**Decision:** `defmanifest` also defines a struct whose slots are the
manifest's named fields; a sub-manifest field holds the child's struct.
`marshal` accepts an instance in place of keyword arguments, and `unmarshal`
returns one when asked. A required field missing from an instance is
reported at run time.
**Alternatives:** Plists only (rejected: no slot-name checking and no
accessors); CLOS classes (deferred: structs are enough and cheaper to
generate).

### SPECOPS-DRAFT-manifest-complete-D5 — Variable structure is declared, with no evaluation escape hatch

**Status:** Proposed
**Context:** Real files hold sequences whose length depends on their data,
such as PNG chunks up to IEND, and records whose layout is chosen by a tag.
lisp-binary handles both with an `eval` type specifier (see `cites:`).
Manifests contain no logic (SPECOPS-DRAFT-manifest-D2).
**Decision:** Add a repeated sub-manifest field, repeated by a count, by a
count or length field, or until an instance matching a condition on its
fields, and extend `:case` to choose a sub-manifest, with a default branch,
by the value of an earlier field. `unmarshal` returns a repeated field as a
list.
**Alternatives:** An `eval` type (rejected: puts logic back into the
manifest and prevents computing a closure for selective reads); loops written
by hand around `marshal` and `unmarshal` (rejected as the end state: spans and
slots could not cover the whole sequence).

### SPECOPS-DRAFT-manifest-complete-D6 — WOFF exercises offset-table backpatching

**Status:** Proposed
**Context:** `:offset-of` slots are implemented on the write side, but no
test format contains a table of absolute offsets; PNG chunks are sequential
and use only lengths.
**Decision:** Write manifests for the WOFF header and table directory, whose
entries carry `:offset-of` and `:length-of` slots for opaque table bodies, and
round-trip a minimal file.
**Alternatives:** RIFF/WAV (rejected: sequential length prefixes, which PNG
already covers); TrueType (deferred: its `checkSumAdjustment` is computed over
the whole file including itself, which needs an option to treat a field as
zero during the checksum).

### SPECOPS-DRAFT-manifest-complete-D7 — Fixed buffers with a terminator are tar-style, padded with the terminator

**Status:** Proposed
**Context:** Many formats hold a terminated string in a buffer of fixed size:
C `char[n]`, tar names, record formats. `:count` together with `:end-by`
signals an error today (SPECOPS-DRAFT-manifest-D17). This is the terminated
case of SPECOPS-DRAFT-manifest-complete-O5.
**Decision:** A `:vec` field with both `:count n` and `:end-by` holds at most
`n` elements. Reading stops at the terminator or after `n` elements, whichever
comes first, and always moves the cursor past all `n`; a full buffer with no
terminator is valid. Writing signals an error for data longer than `n`, writes
data of exactly `n` elements with no terminator, and follows shorter data with
the terminator repeated to fill the buffer. At expansion the two keys become one
bounded stop rule (SPECOPS-DRAFT-manifest-D18) whose units consumed are known
while its data count is not, so `known-count` and a separate `known-extent`
report them apart and selective reads can still skip the field.
**Alternatives:** Requiring the terminator in every buffer, as in C
`char[n]` (rejected: tar-style fields that fill the buffer could not be read);
a separate fill key (rejected for terminated fields: repeating the terminator
is the common case; space-padded fields with no terminator remain open under
SPECOPS-DRAFT-manifest-complete-O5).

### SPECOPS-DRAFT-manifest-complete-D8 — Multi-element terminators are written as a vector or a string

**Status:** Proposed
**Context:** Some fields end at a sequence rather than one element, such as
CR LF. SPECOPS-DRAFT-manifest-D17 allows one element only.
**Decision:** `:end-by` may be a vector or a string. Each element follows the
one-element rule: a character goes through the codec, an integer is a raw
code. Reading keeps a window of the last elements, stops when it matches the
pattern, and drops the pattern from the result. The write check runs the same
matcher over the data and the terminator together and requires the first
match to end on the terminator's last element, so a match that straddles the
data and the terminator is caught: with the pattern `AA`, the data `xA` would
otherwise be read back as `x`.
**Alternatives:** Vectors only (rejected: a string is the natural form for
text terminators); checking only the data for the pattern (rejected: misses
straddling matches); a precomputed matching automaton (deferred: an
optimisation when every code is a constant).

### SPECOPS-DRAFT-manifest-complete-D9 — User-defined stop rules arrive with the runtime fallback, through a `:stop-by` key

**Status:** Proposed
**Context:** Formats have their own self-delimiting encodings: QUIC's
variable-length integers, BER lengths, UTF-8 characters. Under
SPECOPS-DRAFT-manifest-D18 a stop rule needs effector methods to take part in
generated code, which is a high bar for a first definition.
**Decision:** A field's `:stop-by` key names a user stop-rule class. The
default `stop-effector` on `stop-rule` emits calls to a runtime protocol, in
which `feed` takes each element and returns whether to continue, stop keeping
the element, or stop dropping it, and `done-p` is asked before each read. A
class with only the runtime methods works at once; writing effector methods
later upgrades it to inline code without changing manifests.
**Alternatives:** Requiring effector methods for every rule (rejected: no
incremental path); adding `:stop-by` before the fallback exists (rejected:
nothing could run a rule without effectors).

## Field kinds

- **`:leb`.** Fix the write branch: pass the value as an integer, choose the
  signed or unsigned encoder at expansion, and register the field in the
  region map. Add LEB128 decoders and a read branch. Support a vector of LEB
  elements, which the modifier set allows (SPECOPS-DRAFT-manifest-D5).
- **`:case` on read.** Read the selector first (it is always read), then the
  width of the branch it selects.
- **Terminated fields.** Honor `:end-by` on `:vec` and `:str` fields on both
  sides: write the terminator after the data, and on read scan up to it.
- **Caller-supplied strings** per SPECOPS-DRAFT-manifest-complete-D1, with
  the length policy of SPECOPS-DRAFT-manifest-complete-O5.
- **Floats.** Implement the reserved `:f` class for 32-bit and 64-bit
  IEEE-754 values, with per-field byte order, by converting to and from bit
  patterns around the existing serializers.
- **Masques.** Read decimal (`d:`) mask strings; signal an error when a value
  does not fit its group instead of truncating it; register keyword groups in
  `values` and the region map so `:case` and `:count` can refer to them on
  write.

## Reading

- **Checked reads.** Thread a read-side region map through `unmarshal`'s
  sub-lexicon, recording each field, span and sub-manifest as it is read.
  When a checksum field is reached, recompute the checksum over its region and
  compare; check each `:length-of` value against the bytes its region covered.
  The response to a mismatch is SPECOPS-DRAFT-manifest-complete-O1.
- **Selective reads** per SPECOPS-DRAFT-manifest-complete-D3, including a
  skip mode for sub-manifests outside the closure.
- **Stream sources.** `unmarshal` reads only vectors and integers today. Add
  stream sources, seekable and not, through the same reading protocol, which
  should agree with the one disassembly needs
  ([SPECOPS-DRAFT-specops-O4](Arch.SpecOps.md#specops-draft-specops-o4--what-reader-protocol-does-disassembly-offer)).

## Variable structure

Repeated and tag-dispatched sub-manifests per
SPECOPS-DRAFT-manifest-complete-D5. A PNG file then becomes a signature
followed by chunks repeated until IEND, each chunk a `:case` on its tag over
IHDR, IDAT, IEND and an opaque default. Repeated instances need names in the
region map that differ per instance
(SPECOPS-DRAFT-manifest-complete-O4).

## Structs and round trips

- **Structs** per SPECOPS-DRAFT-manifest-complete-D4.
- **Nested values at run time.** `marshal` reads a sub-manifest's values with
  `(getf pairs name)` at expansion, so they must be written as a literal plist
  in the call. Passing a plist or struct built at run time needs another path
  (SPECOPS-DRAFT-manifest-complete-O3).
- **Round-trip tests.** Unmarshal the demo PNG and the GOFF END probe, marshal
  the result, and compare bytes.

## Destinations

- **Buffering** per SPECOPS-DRAFT-manifest-complete-D2, which also makes
  checksums work on streams.
- **Overrun checks.** Signal an error when a vector destination is too small,
  rather than failing in `aref`.
- **Byte orders across nesting.** `marshal` sizes its serializer array from
  the top-level manifest's byte orders, so a sub-manifest using another order
  indexes past it; collect the byte orders of every nested manifest at
  expansion. In `unmarshal`, constant strings read with deserializer 0, which
  exists only if some field uses big-endian order. Which component owns byte
  order is the architecture's open question
  [SPECOPS-DRAFT-specops-O3](Arch.SpecOps.md#specops-draft-specops-o3--who-owns-byte-order).

## Packaging and tests

- **Exports.** Export `defmanifest`, `defenum` and `unmarshal`, as part of the
  public interface decided in
  [SPECOPS-DRAFT-specops-O6](Arch.SpecOps.md#specops-draft-specops-o6--what-is-the-supported-public-api).
- **ASDF.** Put `goff.lisp` in a system so that `goff.lisp:%goff-write-end@1e53ae7`
  is loaded and tested, and move the manifest suites behind `test-op`, per the
  architecture's
  [SPECOPS-DRAFT-specops-O5](Arch.SpecOps.md#specops-draft-specops-o5--how-is-a-modules-conformance-verified).
  Six suites exist at `1e53ae7`, from `manifest.lisp:run-png-demo@1e53ae7` to
  `manifest.lisp:run-pad-demo@1e53ae7`: 54 checks and one structural PNG
  verification.
  The first part is done: `goff.lisp` is in the system
  `mod/format/goff/specops.format.goff.asd:specops.format.goff@50e231d`, a
  format module of the repository restructure
  ([SPECOPS-DRAFT-restructure](Log.Restructure.md)).
- **Dead code.** `defmanifest` still keeps a compile-time `index` that nothing
  reads since the pad rework, `process-field` prints each list type spec, and
  both macros destructure an unused `ba-sym`.
- **Documents.** Add an update to the log for the pad rework and each later
  milestone, and write a Ref once the interface is settled.

## Scope and Non-Goals

- **Following offsets on read** (reading a structure at an offset stored in
  another field, as TIFF and ELF need) is deferred to a later plan; WOFF is
  written and read sequentially.
- **Whole-string codecs** for variable-width encodings are deferred
  (SPECOPS-DRAFT-manifest-complete-D1).
- **Overlapping regions** stay excluded (SPECOPS-DRAFT-manifest-D8).
- **Cryptographic hashes** are left to libraries; checksum functions remain
  pluggable (SPECOPS-DRAFT-manifest-D9).
- **Text front ends and disassembly** belong to the architecture's roadmap,
  not this plan.

## Open Questions

### SPECOPS-DRAFT-manifest-complete-O1 — What does `unmarshal` do when a check fails?

A checksum, length slot or constant that does not match could signal an
error, return the values with a status, or be ignored when the caller asks.
Constants signal an error today. Signalling by default with a parameter that
turns checks off, such as `(:no-validate t)` in `unmarshal`'s `params`, would
keep corrupt data from passing silently while still allowing damaged files to
be inspected.

### SPECOPS-DRAFT-manifest-complete-O2 — Where do `unmarshal`'s output options go?

An earlier design put options after the manifest name, as in
`(unmarshal (goff-end :struct) …)`. `unmarshal` has since gained a `params`
argument, which carries the sub-lexicon. Putting `:struct` and similar options
in `params` would keep the name a plain symbol and give one place for every
option.

### SPECOPS-DRAFT-manifest-complete-O3 — How does `marshal` take nested values known only at run time?

Sub-manifest values are taken from the call at expansion, so the child's
field forms are spliced in directly. A plist or struct built at run time needs
the generated code to fetch each child field from a run-time object, with
`getf` or an accessor, and to check required fields when it runs. Whether
both input forms share one generated path, and how errors are reported, is
open.

### SPECOPS-DRAFT-manifest-complete-O4 — How are repeated instances named and addressed?

Region names are keywords fixed at expansion, made unique per sub-manifest
instance by prefixes (SPECOPS-DRAFT-manifest-D11). A repeated sub-manifest
has a number of instances known only at run time, so a qualified name such as
`:chunk/3/crc` would have to be built at run time, or slots inside a repeated
instance would have to resolve within that instance only. The choice affects
whether a slot outside the sequence can refer to one instance in it.

### SPECOPS-DRAFT-manifest-complete-O5 — What happens to a fixed-length string of another length?

A `:str` field with `:count n` may be given a string shorter or longer than
`n`. Many formats pad names to a fixed width, with spaces (EBCDIC `#x40` in
GOFF) or NUL bytes. The field could signal an error, pad with a declared fill
code and strip it on read, or truncate.

## Prior Art

Table: Prior systems and what this plan draws from each.

| System | What to draw from |
|---|---|
| lisp-binary | Counted and terminated buffers and strings; its `eval` type, the escape hatch SPECOPS-DRAFT-manifest-complete-D5 declines; automatic pointer resolution, for the deferred offset-following work. |
| Kaitai Struct | Repetition by count, by expression and until a condition; types switched on a field's value; instances read at a stored position. A declarative model close to SPECOPS-DRAFT-manifest-complete-D5. |
| Construct | One declaration used for both parsing and building, as in manifests; prefixed fields, repeat-until, switches, checksums and pointers as composable constructs; a reference for the round-trip goal. |

## Roadmap

1. **Corrections and records.** Fix the `:leb` write branch and the byte-order
   gaps in nesting, remove the dead code, and add the pad rework to the log.
2. **Field kinds on read.** `:leb`, `:case` and terminated fields; then
   caller-supplied strings in both directions
   (SPECOPS-DRAFT-manifest-complete-D1, SPECOPS-DRAFT-manifest-complete-O5).
3. **Checked reads** (SPECOPS-DRAFT-manifest-complete-O1).
4. **Structs and run-time nested values**, with round-trip tests
   (SPECOPS-DRAFT-manifest-complete-D4, SPECOPS-DRAFT-manifest-complete-O2,
   SPECOPS-DRAFT-manifest-complete-O3).
5. **Variable structure**, ending with a PNG of any chunk sequence read by one
   manifest (SPECOPS-DRAFT-manifest-complete-D5,
   SPECOPS-DRAFT-manifest-complete-O4).
6. **Destinations:** buffering, checksums on streams, overrun checks
   (SPECOPS-DRAFT-manifest-complete-D2).
7. **Selective reads and stream sources**
   (SPECOPS-DRAFT-manifest-complete-D3).
8. **WOFF** (SPECOPS-DRAFT-manifest-complete-D6).
9. **Floats and masque completion.**
10. **Packaging:** exports, ASDF systems and `test-op`; then a Ref for the
    manifest interface.

## Update 2026-10-30 — Progress and priorities

This installment charts the plan's progress at commit `653345f` and sets the
order of the remaining work. The work done since `1e53ae7` is recorded in the
log's update of 2026-10-30, in SPECOPS-DRAFT-manifest-D15 through
SPECOPS-DRAFT-manifest-D21. Later stages add an installment like this one with
a fresh progress table, pinned to their commit, so the record of progress
stays in order.

### Progress

Table: State of each Roadmap milestone at commit `653345f`.

| Milestone | State | Done | Remaining |
|---|---|---|---|
| 1. Corrections and records | Partly done | `:leb` write fixed (SPECOPS-DRAFT-manifest-D18); pad rework recorded (SPECOPS-DRAFT-manifest-D15); list type specs no longer printed | Byte orders across nesting; dead code (the static `index`, the unused `ba-sym`, `encode-leb128-unsigned` and `encode-leb128-signed`); keyword defaults on enumerated fields; unknown enum codes read as `nil`; the selector-order error names the manifest |
| 2. Field kinds on read | Partly done | `:leb` and `:case` on both sides; one-element terminators; caller text as `:vec` with `:codec` (SPECOPS-DRAFT-manifest-D16) | `:vec` on the scan model; SPECOPS-DRAFT-manifest-complete-D7, SPECOPS-DRAFT-manifest-complete-D8 and SPECOPS-DRAFT-manifest-complete-D9; vectors of LEB elements; SPECOPS-DRAFT-manifest-complete-O5 for fields without a terminator |
| 3. Checked reads | Open | — | Read-side region map; the policy of SPECOPS-DRAFT-manifest-complete-O1 |
| 4. Structs and run-time nested values | Open | — | SPECOPS-DRAFT-manifest-complete-D4, SPECOPS-DRAFT-manifest-complete-O2, SPECOPS-DRAFT-manifest-complete-O3; round-trip tests |
| 5. Variable structure | Open | The field-plan level that `:case` over sub-manifests needs (SPECOPS-DRAFT-manifest-D19) | Repeated sub-manifests; `:case` choosing a sub-manifest; SPECOPS-DRAFT-manifest-complete-O4 |
| 6. Destinations | Open | — | Buffering (SPECOPS-DRAFT-manifest-complete-D2); checksums on streams; overrun checks |
| 7. Selective reads and stream sources | Open | `known-count` and `known-extent` facts on rules and plans | Dependency closure and skip mode (SPECOPS-DRAFT-manifest-complete-D3); stream sources |
| 8. WOFF | Open | — | SPECOPS-DRAFT-manifest-complete-D6, the first test of `:offset-of` |
| 9. Floats and masque completion | Open | — | The `:f` class; decimal (`d:`) masques; range checks; keyword groups registered for `:case` and `:count` |
| 10. Packaging | Partly done | `defmanifest`, `defenum` and `unmarshal` exported | `goff.lisp` in an ASDF system; suites behind `test-op`; a Ref |

### Priorities

The remaining work, in order. Where it differs from the Roadmap above, this
order replaces it.

1. **`:vec` on the scan model.** Reads through `emit-scan`, with one pass for
   terminated fields, and writes through `emit-frame`. Then fixed buffers with
   a terminator (SPECOPS-DRAFT-manifest-complete-D7), multi-element
   terminators (SPECOPS-DRAFT-manifest-complete-D8), vectors of LEB elements,
   and user-defined stop rules with the runtime fallback
   (SPECOPS-DRAFT-manifest-complete-D9).
2. **Corrections.** Byte orders across nesting, the dead code, and the small
   bugs listed under milestone 1. These are small and can be taken between
   larger steps.
3. **Structs and round trips** (SPECOPS-DRAFT-manifest-complete-D4,
   SPECOPS-DRAFT-manifest-complete-O2, SPECOPS-DRAFT-manifest-complete-O3).
   Nothing in step 1 blocks this, so it can start alongside it.
4. **Variable structure** (SPECOPS-DRAFT-manifest-complete-D5,
   SPECOPS-DRAFT-manifest-complete-O4), ending with a PNG of any chunk
   sequence.
5. **Checked reads** (SPECOPS-DRAFT-manifest-complete-O1).
6. **Destinations** (SPECOPS-DRAFT-manifest-complete-D2).
7. **Selective reads and stream sources**
   (SPECOPS-DRAFT-manifest-complete-D3).
8. **WOFF** (SPECOPS-DRAFT-manifest-complete-D6).
9. **Floats and masque completion.**
10. **Packaging:** `test-op`, then a Ref. `goff.lisp` has been in an ASDF
    system since `50e231d`.

Steps 8 and 9 depend on nothing else and can be done whenever convenient.

### Dependencies

- **`:vec` on the scan model** comes before SPECOPS-DRAFT-manifest-complete-D7,
  SPECOPS-DRAFT-manifest-complete-D8, SPECOPS-DRAFT-manifest-complete-D9 and
  vectors of LEB elements, which all add rules or assemblers to it.
- **SPECOPS-DRAFT-manifest-complete-D7** separates `known-extent` from
  `known-count`, which selective reads (SPECOPS-DRAFT-manifest-complete-D3)
  use to skip fields whose data count is unknown but whose size is fixed.
- **Structs and run-time nested values** come before variable structure:
  repeated sub-manifests return lists of records, and must accept them on
  write, so the answers to SPECOPS-DRAFT-manifest-complete-O2 and
  SPECOPS-DRAFT-manifest-complete-O3 shape both. Round-trip tests need them
  too.
- **SPECOPS-DRAFT-manifest-complete-O4**, the naming of repeated instances,
  must be settled before repeated sub-manifests are built.
- **The field-plan level** (SPECOPS-DRAFT-manifest-D19) is the base for
  `:case` choosing a sub-manifest: a branch becomes a manifest plan.
- **A read-side region map** serves both checked reads and selective reads;
  building it once, for whichever comes first, serves the other.
- **Selector variables** (SPECOPS-DRAFT-manifest-D20) are the mechanism for
  registering masque keyword groups so that `:case` and `:count` can refer to
  them on write.
- **Buffering** (SPECOPS-DRAFT-manifest-complete-D2) comes before checksums on
  stream destinations.
- **Architecture questions:** byte orders across nesting depend on
  SPECOPS-DRAFT-specops-O3 (who owns byte order); stream sources should agree
  with the reader protocol of SPECOPS-DRAFT-specops-O4; `test-op` follows
  SPECOPS-DRAFT-specops-O5; the Ref waits for the public interface of
  SPECOPS-DRAFT-specops-O6.
- **Independent:** floats, decimal masques, WOFF and overrun checks.
