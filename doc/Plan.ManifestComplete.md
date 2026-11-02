---
id:            SPECOPS-0003
title:         Manifest Completion Plan
genre:         Plan
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
relates-to:
  - SPECOPS-0006
  - SPECOPS-0001
  - SPECOPS-0005
glossary:      SPECOPS-0002
decisions:
  - SPECOPS-D12
  - SPECOPS-D13
  - SPECOPS-D14
  - SPECOPS-D15
  - SPECOPS-D16
  - SPECOPS-D17
  - SPECOPS-D18
  - SPECOPS-D19
  - SPECOPS-D20
open-questions:
  - SPECOPS-O7
  - SPECOPS-O8
  - SPECOPS-O9
  - SPECOPS-O10
  - SPECOPS-O11
  - SPECOPS-O12
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
[Manifest development log, SPECOPS-0006](Log.Manifest.md) and the
architecture in [SPECOPS-0001](Arch.SpecOps.md). Progress is charted
in dated updates at the end of this plan; the latest is the tenth update of
2026-11-01, at commit `e4a1f9a`.

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
records the work (SPECOPS-D42 through
SPECOPS-D48).

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

### SPECOPS-D12 — Caller-supplied string fields use the per-character codec

**Status:** Implemented
**Context:** Many formats carry strings chosen by the writer: a generic
chunk's tag, a section or symbol name, a text keyword. The constant `str`
directive covers only literals. A field declared `:str` is parsed by
`specops.lisp:defmanifest@1e53ae7`, which sets only its width, so `marshal`
treats it as a one-unit integer and fails on a string. Codecs are
per-character functions (SPECOPS-D33).
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
(SPECOPS-D43), which uses the codec, so the reason given above
for rejecting that form no longer holds. This record may be reworded or
withdrawn.

### SPECOPS-D13 — Destinations that cannot be patched are buffered only when a manifest needs it

**Status:** Implemented
**Context:** Slots are resolved by writing at a recorded position, and
checksums read the bytes of a region back from the destination
(SPECOPS-D34, SPECOPS-D36). In
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

Note: built as SPECOPS-D73 in the log. Every stream is buffered
when a manifest has slots, whether or not it could be sought, and so is every
vector that is not a simple array of the manifest's units, which checksum
functions need. The buffer may be one the caller keeps
(SPECOPS-D74).

### SPECOPS-D14 — Selective reads use a dependency closure computed at expansion

**Status:** Implemented
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

Note: built as SPECOPS-D80 through SPECOPS-D82 in
the log. File streams are read past, never sought with `file-position`, as
every stream is; a read from a stream still takes the whole record; and only
the slots whose field and region lie in the part read are checked. Key paths
into sub-manifests are deferred (Scope and Non-Goals).

### SPECOPS-D15 — `defmanifest` generates a struct for record input and output

**Status:** Implemented
**Context:** `marshal` accepts keyword arguments only, and `unmarshal`
returns values or a plist (SPECOPS-D40). Programs that build or
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

Note: built as a CLOS class, defined only when a manifest asks for one
(SPECOPS-D59 in the log), because a struct cannot be redefined
with different slots without an error. This record may be reworded or
withdrawn.

### SPECOPS-D16 — Variable structure is declared, with no evaluation escape hatch

**Status:** Implemented
**Context:** Real files hold sequences whose length depends on their data,
such as PNG chunks up to IEND, and records whose layout is chosen by a tag.
lisp-binary handles both with an `eval` type specifier (see `cites:`).
Manifests contain no logic (SPECOPS-D29).
**Decision:** Add a repeated sub-manifest field, repeated by a count, by a
count or length field, or until an instance matching a condition on its
fields, and extend `:case` to choose a sub-manifest, with a default branch,
by the value of an earlier field. `unmarshal` returns a repeated field as a
list.
**Alternatives:** An `eval` type (rejected: puts logic back into the
manifest and prevents computing a closure for selective reads); loops written
by hand around `marshal` and `unmarshal` (rejected as the end state: spans and
slots could not cover the whole sequence).

Note: built as SPECOPS-D64 and SPECOPS-D65 in
the log, with repeats also bounded by the end of the source or a length slot
(SPECOPS-D67).

### SPECOPS-D17 — WOFF exercises offset-table backpatching

**Status:** Implemented
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

Note: built as SPECOPS-D83 through SPECOPS-D86
in the log. Table bodies are placed fields, written where a `place` directive
stands and read at their offsets, since real files store tables out of
directory order, so WOFF is not read sequentially. The fields describing the
uncompressed font are given rather than derived, and WOFFs are written in
directory order. The tests also read and write TrueType and OpenType fonts,
with table checksums given.

### SPECOPS-D18 — Fixed buffers with a terminator are tar-style, padded with the terminator

**Status:** Implemented
**Context:** Many formats hold a terminated string in a buffer of fixed size:
C `char[n]`, tar names, record formats. `:count` together with `:end-by`
signals an error today (SPECOPS-D44). This is the terminated
case of SPECOPS-O11.
**Decision:** A `:vec` field with both `:count n` and `:end-by` holds at most
`n` elements. Reading stops at the terminator or after `n` elements, whichever
comes first, and always moves the cursor past all `n`; a full buffer with no
terminator is valid. Writing signals an error for data longer than `n`, writes
data of exactly `n` elements with no terminator, and follows shorter data with
the terminator repeated to fill the buffer. At expansion the two keys become one
bounded stop rule (SPECOPS-D45) whose units consumed are known
while its data count is not, so `known-count` and a separate `known-extent`
report them apart and selective reads can still skip the field.
**Alternatives:** Requiring the terminator in every buffer, as in C
`char[n]` (rejected: tar-style fields that fill the buffer could not be read);
a separate fill key (rejected for terminated fields: repeating the terminator
is the common case; space-padded fields with no terminator remain open under
SPECOPS-O11).

### SPECOPS-D19 — Multi-element terminators are written as a vector or a string

**Status:** Implemented
**Context:** Some fields end at a sequence rather than one element, such as
CR LF. SPECOPS-D44 allows one element only.
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

### SPECOPS-D20 — User-defined stop rules arrive with the runtime fallback, through a `:stop-by` key

**Status:** Implemented
**Context:** Formats have their own self-delimiting encodings: QUIC's
variable-length integers, BER lengths, UTF-8 characters. Under
SPECOPS-D45 a stop rule needs effector methods to take part in
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
  elements, which the modifier set allows (SPECOPS-D32).
- **`:case` on read.** Read the selector first (it is always read), then the
  width of the branch it selects.
- **Terminated fields.** Honor `:end-by` on `:vec` and `:str` fields on both
  sides: write the terminator after the data, and on read scan up to it.
- **Caller-supplied strings** per SPECOPS-D12, with
  the length policy of SPECOPS-O11.
- **Floats.** Implement the reserved `:f` class for 32-bit and 64-bit
  IEEE-754 values, with per-field byte order, by converting to and from bit
  patterns around the existing serializers.

  Note: built as SPECOPS-D91 and SPECOPS-D92 in
  the log, for 16 and 128 bits as well, after the prior-art evaluation's
  SPECOPS-O19; on SBCL, single and double floats keep
  infinities and NaN payloads, and elsewhere those read as keywords.
- **Masques.** Read decimal (`d:`) mask strings; signal an error when a value
  does not fit its group instead of truncating it; register keyword groups in
  `values` and the region map so `:case` and `:count` can refer to them on
  write.

  Note: built as SPECOPS-D87 through SPECOPS-D90
  in the log. Decimal masks first needed fixing in `masque` and `unmasque`
  themselves, which the progress tables since 2026-10-31 had listed as done.
  The error for a value too wide comes from a new `:overflow` parameter of
  `masque`, whose default keeps masking. Keyword groups select `:case`
  branches through selector variables, which replaced `values`; `:count`
  needed nothing on write.

## Reading

- **Checked reads.** Thread a read-side region map through `unmarshal`'s
  sub-lexicon, recording each field, span and sub-manifest as it is read.
  When a checksum field is reached, recompute the checksum over its region and
  compare; check each `:length-of` value against the bytes its region covered.
  The response to a mismatch is SPECOPS-O7.

  Note: built without a map threaded through the sub-lexicon or recorded at
  run time. A region table built at expansion decides which positions to keep,
  in local variables bound by each expansion, so each repeated instance is
  checked against its own regions (SPECOPS-D70 in the log). A
  mismatch signals a condition with restarts (SPECOPS-D69).
- **Selective reads** per SPECOPS-D14, including a
  skip mode for sub-manifests outside the closure.
- **Stream sources.** `unmarshal` reads only vectors and integers today. Add
  stream sources, seekable and not, through the same reading protocol, which
  should agree with the one disassembly needs
  ([SPECOPS-O4](Arch.SpecOps.md#specops-o4--what-reader-protocol-does-disassembly-offer)).

  Note: built as SPECOPS-D77 in the log. Streams are read in
  order, never sought, whether or not they could be, as a fourth kind of
  source for `deserializer-for`, and a read takes exactly one record
  (SPECOPS-D78). Whether the stream source suits disassembly is
  left to SPECOPS-O4.

## Variable structure

Repeated and tag-dispatched sub-manifests per
SPECOPS-D16. A PNG file then becomes a signature
followed by chunks repeated until IEND, each chunk a `:case` on its tag over
IHDR, IDAT, IEND and an opaque default. Repeated instances need names in the
region map that differ per instance
(SPECOPS-O10).

## Structs and round trips

- **Structs** per SPECOPS-D15.
- **Nested values at run time.** `marshal` reads a sub-manifest's values with
  `(getf pairs name)` at expansion, so they must be written as a literal plist
  in the call. Passing a plist or struct built at run time needs another path
  (SPECOPS-O9).
- **Round-trip tests.** Unmarshal the demo PNG and the GOFF END probe, marshal
  the result, and compare bytes.

## Destinations

- **Buffering** per SPECOPS-D13, which also makes
  checksums work on streams.
- **Overrun checks.** Signal an error when a vector destination is too small,
  rather than failing in `aref`.
- **Byte orders across nesting.** `marshal` sizes its serializer array from
  the top-level manifest's byte orders, so a sub-manifest using another order
  indexes past it; collect the byte orders of every nested manifest at
  expansion. In `unmarshal`, constant strings read with deserializer 0, which
  exists only if some field uses big-endian order. Which component owns byte
  order is the architecture's open question
  [SPECOPS-O3](Arch.SpecOps.md#specops-o3--who-owns-byte-order).

## Packaging and tests

- **Exports.** Export `defmanifest`, `defenum` and `unmarshal`, as part of the
  public interface decided in
  [SPECOPS-O6](Arch.SpecOps.md#specops-o6--what-is-the-supported-public-api).
- **ASDF.** Put `goff.lisp` in a system so that `goff.lisp:%goff-write-end@1e53ae7`
  is loaded and tested, and move the manifest suites behind `test-op`, per the
  architecture's
  [SPECOPS-O5](Arch.SpecOps.md#specops-o5--how-is-a-modules-conformance-verified).
  Six suites exist at `1e53ae7`, from `manifest.lisp:run-png-demo@1e53ae7` to
  `manifest.lisp:run-pad-demo@1e53ae7`: 54 checks and one structural PNG
  verification.
  The first part is done: `goff.lisp` is in the system
  `mod/format/goff/specops.format.goff.asd:specops.format.goff@50e231d`, a
  format module of the repository restructure
  ([SPECOPS-0005](Log.Restructure.md)).
  The second part is done too: at `e4a1f9a` the suites are the Square test
  system `specops.asd:specops/test@e4a1f9a`, which `(asdf:test-system
  :specops)` runs (SPECOPS-D93).
- **Dead code.** `defmanifest` still keeps a compile-time `index` that nothing
  reads since the pad rework, `process-field` prints each list type spec, and
  both macros destructure an unused `ba-sym`.
- **Documents.** Add an update to the log for the pad rework and each later
  milestone, and write a Ref once the interface is settled.

## Scope and Non-Goals

- **Following offsets on read** (reading a structure at an offset stored in
  another field, as TIFF and ELF need) is deferred to a later plan, except for
  placed fields, which the steward brought forward for WOFF
  (SPECOPS-D84).
- **WOFF compression, metadata and private blocks**, and checking a WOFF for
  overlapping or extra data on read, are out of scope; WOFF is a test format.
- **Whole-string codecs** for variable-width encodings are deferred
  (SPECOPS-D12).
- **Overlapping regions** stay excluded (SPECOPS-D35).
- **Cryptographic hashes** are left to libraries; checksum functions remain
  pluggable (SPECOPS-D36).
- **Text front ends and disassembly** belong to the architecture's roadmap,
  not this plan.
- **Key paths into sub-manifests** for selective reads, such as
  `(:ihdr :width)`, are deferred by the steward: a key that names a
  sub-manifest field returns the whole sub-record.

## Open Questions

### SPECOPS-O7 — What does `unmarshal` do when a check fails?

A checksum, length slot or constant that does not match could signal an
error, return the values with a status, or be ignored when the caller asks.
Constants signal an error today. Signalling by default with a parameter that
turns checks off, such as `(:no-validate t)` in `unmarshal`'s `params`, would
keep corrupt data from passing silently while still allowing damaged files to
be inspected.

Answered in the log by SPECOPS-D69: a failed check signals
`manifest-check-failed`, with a `continue` restart and, for slot fields, a
`use-value` restart; `(:check nil)` in `params` leaves the checks out.

### SPECOPS-O8 — Where do `unmarshal`'s output options go?

An earlier design put options after the manifest name, as in
`(unmarshal (goff-end :struct) …)`. `unmarshal` has since gained a `params`
argument, which carries the sub-lexicon. Putting `:struct` and similar options
in `params` would keep the name a plain symbol and give one place for every
option.

Answered in the log by SPECOPS-D61: options go in `params`,
with `(:as :record)` the first of them.

### SPECOPS-O9 — How does `marshal` take nested values known only at run time?

Sub-manifest values are taken from the call at expansion, so the child's
field forms are spliced in directly. A plist or struct built at run time needs
the generated code to fetch each child field from a run-time object, with
`getf` or an accessor, and to check required fields when it runs. Whether
both input forms share one generated path, and how errors are reported, is
open.

Answered in the log by SPECOPS-D60: `(:from record)` in an
options list, read through the generic function `field-value` in one path for
plists and records, with a missing field reported when the code runs.

### SPECOPS-O10 — How are repeated instances named and addressed?

Region names are keywords fixed at expansion, made unique per sub-manifest
instance by prefixes (SPECOPS-D38). A repeated sub-manifest
has a number of instances known only at run time, so a qualified name such as
`:chunk/3/crc` would have to be built at run time, or slots inside a repeated
instance would have to resolve within that instance only. The choice affects
whether a slot outside the sequence can refer to one instance in it.

Answered in the log by SPECOPS-D66: slots inside an instance
resolve within it, and a slot outside a repeat covers the whole repeat.

### SPECOPS-O11 — What happens to a fixed-length string of another length?

A `:str` field with `:count n` may be given a string shorter or longer than
`n`. Many formats pad names to a fixed width, with spaces (EBCDIC `#x40` in
GOFF) or NUL bytes. The field could signal an error, pad with a declared fill
code and strip it on read, or truncate.

### SPECOPS-O12 — Do buffered destinations and checksummed stream sources share one reusable buffer?

Both directions need bytes kept that a stream cannot give back. Writing, a
destination that cannot be patched is written through an internal vector
(SPECOPS-D13). Reading a stream source, the bytes of a
checksummed region must be kept until its checksum is computed
(SPECOPS-D70). A buffer made for each call puts pressure on the
garbage collector when many records are read or written, while a durable
buffer reused across calls must be owned by something: the caller, through an
option to `marshal` and `unmarshal`, or a special variable, which raises
reentrancy and threads. Whether one buffer serves both directions, how large it
grows (the largest checksummed region, or the whole record), and who owns it
are open.

Answered for writing in the log by SPECOPS-D74: the caller owns
a `manifest-buffer`, passed as `(:buffer b)`, which grows to the largest record
written through it; stream sources are to accept the same object, and whether
one instance serves both directions is the caller's choice. Answered for
reading by SPECOPS-D79: `unmarshal` takes the same object in
its params, and keeps a stream's units for the whole record in it.

## Prior Art

Table: Prior systems and what this plan draws from each.

| System | What to draw from |
|---|---|
| lisp-binary | Counted and terminated buffers and strings; its `eval` type, the escape hatch SPECOPS-D16 declines; automatic pointer resolution, for the deferred offset-following work. |
| Kaitai Struct | Repetition by count, by expression and until a condition; types switched on a field's value; instances read at a stored position. A declarative model close to SPECOPS-D16. |
| Construct | One declaration used for both parsing and building, as in manifests; prefixed fields, repeat-until, switches, checksums and pointers as composable constructs; a reference for the round-trip goal. |

## Roadmap

1. **Corrections and records.** Fix the `:leb` write branch and the byte-order
   gaps in nesting, remove the dead code, and add the pad rework to the log.
2. **Field kinds on read.** `:leb`, `:case` and terminated fields; then
   caller-supplied strings in both directions
   (SPECOPS-D12, SPECOPS-O11).
3. **Checked reads** (SPECOPS-O7).
4. **Structs and run-time nested values**, with round-trip tests
   (SPECOPS-D15, SPECOPS-O8,
   SPECOPS-O9).
5. **Variable structure**, ending with a PNG of any chunk sequence read by one
   manifest (SPECOPS-D16,
   SPECOPS-O10).
6. **Destinations:** buffering, checksums on streams, overrun checks
   (SPECOPS-D13).
7. **Selective reads and stream sources**
   (SPECOPS-D14).
8. **WOFF** (SPECOPS-D17).
9. **Floats and masque completion.**
10. **Packaging:** exports, ASDF systems and `test-op`; then a Ref for the
    manifest interface.

## Update 2026-10-30 — Progress and priorities

This installment charts the plan's progress at commit `653345f` and sets the
order of the remaining work. The work done since `1e53ae7` is recorded in the
log's update of 2026-10-30, in SPECOPS-D42 through
SPECOPS-D48. Later stages add an installment like this one with
a fresh progress table, pinned to their commit, so the record of progress
stays in order.

### Progress

Table: State of each Roadmap milestone at commit `653345f`.

| Milestone | State | Done | Remaining |
|---|---|---|---|
| 1. Corrections and records | Partly done | `:leb` write fixed (SPECOPS-D45); pad rework recorded (SPECOPS-D42); list type specs no longer printed | Byte orders across nesting; dead code (the static `index`, the unused `ba-sym`, `encode-leb128-unsigned` and `encode-leb128-signed`); keyword defaults on enumerated fields; unknown enum codes read as `nil`; the selector-order error names the manifest |
| 2. Field kinds on read | Partly done | `:leb` and `:case` on both sides; one-element terminators; caller text as `:vec` with `:codec` (SPECOPS-D43) | `:vec` on the scan model; SPECOPS-D18, SPECOPS-D19 and SPECOPS-D20; vectors of LEB elements; SPECOPS-O11 for fields without a terminator |
| 3. Checked reads | Open | — | Read-side region map; the policy of SPECOPS-O7 |
| 4. Structs and run-time nested values | Open | — | SPECOPS-D15, SPECOPS-O8, SPECOPS-O9; round-trip tests |
| 5. Variable structure | Open | The field-plan level that `:case` over sub-manifests needs (SPECOPS-D46) | Repeated sub-manifests; `:case` choosing a sub-manifest; SPECOPS-O10 |
| 6. Destinations | Open | — | Buffering (SPECOPS-D13); checksums on streams; overrun checks |
| 7. Selective reads and stream sources | Open | `known-count` and `known-extent` facts on rules and plans | Dependency closure and skip mode (SPECOPS-D14); stream sources |
| 8. WOFF | Open | — | SPECOPS-D17, the first test of `:offset-of` |
| 9. Floats and masque completion | Open | — | The `:f` class; decimal (`d:`) masques; range checks; keyword groups registered for `:case` and `:count` |
| 10. Packaging | Partly done | `defmanifest`, `defenum` and `unmarshal` exported | `goff.lisp` in an ASDF system; suites behind `test-op`; a Ref |

### Priorities

The remaining work, in order. Where it differs from the Roadmap above, this
order replaces it.

1. **`:vec` on the scan model.** Reads through `emit-scan`, with one pass for
   terminated fields, and writes through `emit-frame`. Then fixed buffers with
   a terminator (SPECOPS-D18), multi-element
   terminators (SPECOPS-D19), vectors of LEB elements,
   and user-defined stop rules with the runtime fallback
   (SPECOPS-D20).
2. **Corrections.** Byte orders across nesting, the dead code, and the small
   bugs listed under milestone 1. These are small and can be taken between
   larger steps.
3. **Structs and round trips** (SPECOPS-D15,
   SPECOPS-O8, SPECOPS-O9).
   Nothing in step 1 blocks this, so it can start alongside it.
4. **Variable structure** (SPECOPS-D16,
   SPECOPS-O10), ending with a PNG of any chunk
   sequence.
5. **Checked reads** (SPECOPS-O7).
6. **Destinations** (SPECOPS-D13).
7. **Selective reads and stream sources**
   (SPECOPS-D14).
8. **WOFF** (SPECOPS-D17).
9. **Floats and masque completion.**
10. **Packaging:** `test-op`, then a Ref. `goff.lisp` has been in an ASDF
    system since `50e231d`.

Steps 8 and 9 depend on nothing else and can be done whenever convenient.

### Dependencies

- **`:vec` on the scan model** comes before SPECOPS-D18,
  SPECOPS-D19, SPECOPS-D20 and
  vectors of LEB elements, which all add rules or assemblers to it.
- **SPECOPS-D18** separates `known-extent` from
  `known-count`, which selective reads (SPECOPS-D14)
  use to skip fields whose data count is unknown but whose size is fixed.
- **Structs and run-time nested values** come before variable structure:
  repeated sub-manifests return lists of records, and must accept them on
  write, so the answers to SPECOPS-O8 and
  SPECOPS-O9 shape both. Round-trip tests need them
  too.
- **SPECOPS-O10**, the naming of repeated instances,
  must be settled before repeated sub-manifests are built.
- **The field-plan level** (SPECOPS-D46) is the base for
  `:case` choosing a sub-manifest: a branch becomes a manifest plan.
- **A read-side region map** serves both checked reads and selective reads;
  building it once, for whichever comes first, serves the other.
- **Selector variables** (SPECOPS-D47) are the mechanism for
  registering masque keyword groups so that `:case` and `:count` can refer to
  them on write.
- **Buffering** (SPECOPS-D13) comes before checksums on
  stream destinations.
- **Architecture questions:** byte orders across nesting depend on
  SPECOPS-O3 (who owns byte order); stream sources should agree
  with the reader protocol of SPECOPS-O4; `test-op` follows
  SPECOPS-O5; the Ref waits for the public interface of
  SPECOPS-O6.
- **Independent:** floats, decimal masques, WOFF and overrun checks.

## Update 2026-10-31 — Progress: vector fields on the scan model

This installment charts the plan's progress at commit `6660c7f`. Since the
update of 2026-10-30, the first priority is done: vector fields are built from
field plans on the scan model, recorded in the log's update of 2026-10-31
(SPECOPS-D49 through SPECOPS-D51), in commits
`0b03854` and `738105a`. The repository restructure
([SPECOPS-0005](Log.Restructure.md)) then moved the manifest
layer to `src/manifest/` and its suites to `test/manifest.lisp`, changing no
behavior, and put `goff.lisp` into an ASDF system. At `6660c7f` all ten
suites pass: 135 checks and the PNG verification.

### Progress

Table: State of each Roadmap milestone at commit `6660c7f`. Entries new since
the update of 2026-10-30 are marked "new".

| Milestone | State | Done | Remaining |
|---|---|---|---|
| 1. Corrections and records | Partly done | As before; new: the old `:vec` and LEB branches removed, and the LEB128 encoders commented out | Byte orders across nesting; dead code (the static `index`, `unmarshal`'s unused `term-code-form` and the `:end-by` branch of `vec-count-form`, the commented-out encoders); keyword defaults on enumerated fields; unknown enum codes read as `nil`; the selector-order error names the manifest |
| 2. Field kinds on read | Partly done | As before; new: `:vec` on the scan model, with one-pass terminated reads, elements encoded by the terminator rule, and enums decoded in every plan | SPECOPS-D18, SPECOPS-D19 and SPECOPS-D20; vectors of LEB elements; SPECOPS-O11 for fields without a terminator |
| 3. Checked reads | Open | — | Read-side region map; the policy of SPECOPS-O7 |
| 4. Structs and run-time nested values | Open | — | SPECOPS-D15, SPECOPS-O8, SPECOPS-O9; round-trip tests |
| 5. Variable structure | Open | The field-plan level, which now covers every variable-length field | Repeated sub-manifests; `:case` choosing a sub-manifest; SPECOPS-O10 |
| 6. Destinations | Open | — | Buffering (SPECOPS-D13); checksums on streams; overrun checks |
| 7. Selective reads and stream sources | Open | `known-count` and `known-extent` facts on rules and plans | Dependency closure and skip mode (SPECOPS-D14); stream sources |
| 8. WOFF | Open | — | SPECOPS-D17, the first test of `:offset-of` |
| 9. Floats and masque completion | Open | — | The `:f` class; decimal (`d:`) masques, begun after `6660c7f` in `masque` and `unmasque`; range checks; keyword groups registered for `:case` and `:count` |
| 10. Packaging | Partly done | As before; new: `goff.lisp` in the system `specops.format.goff`, with the other format and map modules of the restructure | Suites behind `test-op`; a Ref |

### Priorities

The order of the update of 2026-10-30 stands, with its first step reduced to
what remains of it:

1. **The rest of the scan model:** fixed buffers with a terminator
   (SPECOPS-D18), multi-element terminators
   (SPECOPS-D19), vectors of LEB elements, and
   user-defined stop rules with the runtime fallback
   (SPECOPS-D20). Each adds a rule or an assembler;
   none needs another change to the macros.
2. **Corrections**, between larger steps.
3. **Structs and round trips.**
4. **Variable structure.**
5. **Checked reads.**
6. **Destinations.**
7. **Selective reads and stream sources.**
8. **WOFF.**
9. **Floats and masque completion.** Decimal masques were taken up early,
   after `6660c7f`.
10. **Packaging:** `test-op`, then a Ref.

### Dependencies

The dependencies listed in the update of 2026-10-30 still hold. The first of
them, `:vec` on the scan model before
SPECOPS-D18, SPECOPS-D19,
SPECOPS-D20 and vectors of LEB elements, is now met.
`test-op` still follows SPECOPS-O5.

## Update 2026-10-31 — Progress: the scan model completed

This installment charts the plan's progress at commit `d030f5c`. The first
priority of the previous update is done: fixed buffers holding a terminator
(SPECOPS-D18), terminators of several elements
(SPECOPS-D19), vectors of LEB elements, and
user-defined stop rules with the runtime fallback
(SPECOPS-D20). The log's update of the same date
records the work, with SPECOPS-D52 through
SPECOPS-D55. All fourteen suites pass: 202 checks and the PNG
verification.

### Progress

Table: State of each Roadmap milestone at commit `d030f5c`. Entries new since
the previous update are marked "new".

| Milestone | State | Done | Remaining |
|---|---|---|---|
| 1. Corrections and records | Partly done | As before; new: `unmarshal`'s unused `term-code-form` and the `:end-by` branch of `vec-count-form`, the commented-out encoders and the unused `ba-sym` removed | Byte orders across nesting; the static `index` and other unused variables of `defmanifest`; keyword defaults on enumerated fields; unknown enum codes read as `nil`; the selector-order error names the manifest; new: exports for user stop rules |
| 2. Field kinds on read | Partly done | As before; new: SPECOPS-D18, SPECOPS-D19 and SPECOPS-D20; vectors of LEB elements | SPECOPS-O11 for fields without a terminator; new: writing a buffer sized by another field; a units rule for `:length-of` over variable-width elements; fixed buffers of LEB elements |
| 3. Checked reads | Open | — | Read-side region map; the policy of SPECOPS-O7 |
| 4. Structs and run-time nested values | Open | — | SPECOPS-D15, SPECOPS-O8, SPECOPS-O9; round-trip tests |
| 5. Variable structure | Open | The field-plan level; new: plans nest, as a sub-manifest branch would | Repeated sub-manifests; `:case` choosing a sub-manifest; SPECOPS-O10 |
| 6. Destinations | Open | — | Buffering (SPECOPS-D13); checksums on streams; overrun checks |
| 7. Selective reads and stream sources | Open | `known-count` and `known-extent`; new: `known-extent` for rules, separate from the count | Dependency closure and skip mode (SPECOPS-D14); stream sources |
| 8. WOFF | Open | — | SPECOPS-D17, the first test of `:offset-of` |
| 9. Floats and masque completion | Partly done | New: decimal (`d:`) masques in `masque` and `unmasque` | The `:f` class; decimal masques in `unmarshal`; range checks; keyword groups registered for `:case` and `:count` |
| 10. Packaging | Partly done | As before | Suites behind `test-op`; a Ref |

### Priorities

1. **Corrections.** With the scan model complete, the small items of
   milestone 1 come next: byte orders across nesting, the unused variables of
   `defmanifest`, keyword defaults on enumerated fields, unknown enum codes,
   the selector-order error, and the exports for user stop rules.
2. **Structs and round trips.**
3. **Variable structure.**
4. **Checked reads.**
5. **Destinations.**
6. **Selective reads and stream sources.**
7. **WOFF.**
8. **Floats and masque completion**, including decimal masques in
   `unmarshal`.
9. **Packaging:** `test-op`, then a Ref.

The remaining field-kind items of milestone 2 wait on demand: a buffer sized
by another field on write, and the units rule, which would also serve
"the rest of the region" fields in variable structure.

### Dependencies

The dependencies of the update of 2026-10-30 hold, and the scan model's are
met. Two are new: the units rule, if built, comes before fixed buffers of LEB
elements and before reading a vector of LEB elements counted by a length
slot; and writing a buffer sized by another field uses the same variables as
`:case` selectors (SPECOPS-D47).

## Update 2026-10-31 — Progress: corrections

This installment charts the plan's progress at commit `2e1af03`, which makes
the corrections that were the first priority of the previous update. The
log's update of the same date records them, with SPECOPS-D56
through SPECOPS-D58. One fault not listed here was found and
fixed on the way: a manifest's `:endian` did not reach its fields. All
fourteen suites pass: 217 checks and the PNG verification.

### Progress

Table: State of each Roadmap milestone at commit `2e1af03`. Entries new since
the previous update are marked "new".

| Milestone | State | Done | Remaining |
|---|---|---|---|
| 1. Corrections and records | Done | As before; new: a manifest's `:endian` applies to its fields; byte orders across nesting; keyword defaults on enumerated fields; codes an enum lacks read as integers; the selector-order error names both fields; unused variables removed or declared; exports for user stop rules | — |
| 2. Field kinds on read | Partly done | As before | SPECOPS-O11 for fields without a terminator; writing a buffer sized by another field; a units rule for `:length-of` over variable-width elements; fixed buffers of LEB elements |
| 3. Checked reads | Open | — | Read-side region map; the policy of SPECOPS-O7 |
| 4. Structs and run-time nested values | Open | — | SPECOPS-D15, SPECOPS-O8, SPECOPS-O9; round-trip tests |
| 5. Variable structure | Open | The field-plan level; plans nest | Repeated sub-manifests; `:case` choosing a sub-manifest; SPECOPS-O10 |
| 6. Destinations | Open | — | Buffering (SPECOPS-D13); checksums on streams; overrun checks |
| 7. Selective reads and stream sources | Open | `known-count` and `known-extent` | Dependency closure and skip mode (SPECOPS-D14); stream sources |
| 8. WOFF | Open | — | SPECOPS-D17, the first test of `:offset-of` |
| 9. Floats and masque completion | Partly done | Decimal (`d:`) masques in `masque` and `unmasque` | The `:f` class; decimal masques in `unmarshal`; range checks; keyword groups registered for `:case` and `:count` |
| 10. Packaging | Partly done | As before; new: the stop-rule protocol exported | Suites behind `test-op`; a Ref |

### Priorities

1. **Structs and round trips** (SPECOPS-D15,
   SPECOPS-O8, SPECOPS-O9).
2. **Variable structure** (SPECOPS-D16,
   SPECOPS-O10).
3. **Checked reads** (SPECOPS-O7).
4. **Destinations** (SPECOPS-D13).
5. **Selective reads and stream sources**
   (SPECOPS-D14).
6. **WOFF** (SPECOPS-D17).
7. **Floats and masque completion**, including decimal masques in
   `unmarshal`.
8. **Packaging:** `test-op`, then a Ref.

The remaining items of milestone 2 still wait on demand, as the previous
update said.

### Dependencies

Unchanged from the previous update. Milestone 1 had no dependents, so its
completion unblocks nothing new; structs and round trips are next on their
own merits, and their answers to SPECOPS-O8 and
SPECOPS-O9 shape variable structure.

## Update 2026-11-01 — Progress: records and round trips

This installment charts the plan's progress at commit `4f9c6d4`. The first
priority of the previous update, structs and round trips, is done, with a
class in place of the struct SPECOPS-D15 named, and
SPECOPS-O8 and SPECOPS-O9 are
answered. The log's update of the same date records the work, with
SPECOPS-D59 through SPECOPS-D62. All fifteen
suites pass: 258 checks and the PNG verification.

### Progress

Table: State of each Roadmap milestone at commit `4f9c6d4`. Entries new since
the previous update are marked "new".

| Milestone | State | Done | Remaining |
|---|---|---|---|
| 1. Corrections and records | Done | As before | — |
| 2. Field kinds on read | Partly done | As before | SPECOPS-O11 for fields without a terminator; writing a buffer sized by another field; a units rule; fixed buffers of LEB elements |
| 3. Checked reads | Open | — | Read-side region map; the policy of SPECOPS-O7 |
| 4. Structs and run-time nested values | Done | New: record classes on request; `(:from record)` in `marshal`; `(:as :record)` in `unmarshal`; canonical round trips of the 1×1 PNG and the GOFF END record, with edited records written consistently | — |
| 5. Variable structure | Open | The field-plan level; plans nest; new: records and run-time nested values, which repeated sub-manifests return and accept | Repeated sub-manifests; `:case` choosing a sub-manifest; SPECOPS-O10 |
| 6. Destinations | Open | — | Buffering (SPECOPS-D13), which also lets a manifest with a `:length-of` slot write to a function; checksums on streams; overrun checks |
| 7. Selective reads and stream sources | Open | `known-count` and `known-extent` | Dependency closure and skip mode (SPECOPS-D14); stream sources |
| 8. WOFF | Open | — | SPECOPS-D17, the first test of `:offset-of` |
| 9. Floats and masque completion | Partly done | Decimal masques in `masque` and `unmasque` | The `:f` class; decimal masques in `unmarshal`; range checks; keyword groups registered for `:case` and `:count` |
| 10. Packaging | Partly done | As before; new: `field-value` exported | Suites behind `test-op`; a Ref |

### Priorities

1. **Variable structure** (SPECOPS-D16,
   SPECOPS-O10), ending with a PNG of any chunk
   sequence read and written by one manifest.
2. **Checked reads** (SPECOPS-O7).
3. **Destinations** (SPECOPS-D13).
4. **Selective reads and stream sources**
   (SPECOPS-D14).
5. **WOFF** (SPECOPS-D17).
6. **Floats and masque completion.**
7. **Packaging:** `test-op`, then a Ref.

### Dependencies

Records and run-time nested values, which variable structure waited on, are
in place: a repeated sub-manifest can return a list of records or plists, and
`marshal` can take one when its code runs. SPECOPS-O10,
the naming of repeated instances, remains to be settled before repeated
sub-manifests are built.

## Update 2026-11-01 — Progress: variable structure

This installment charts the plan's progress at commit `d7ba27f`. The first
priority of the previous update, variable structure, is done: one manifest
reads and writes a PNG of any chunk sequence, and
SPECOPS-D16 is built and
SPECOPS-O10 answered. The log's update of the same
date records the work, with SPECOPS-D63 through
SPECOPS-D68. All sixteen suites pass: 292 checks and the PNG
verification.

### Progress

Table: State of each Roadmap milestone at commit `d7ba27f`. Entries new since
the previous update are marked "new".

| Milestone | State | Done | Remaining |
|---|---|---|---|
| 1. Corrections and records | Done | As before | — |
| 2. Field kinds on read | Partly done | As before; new: a units rule, so vectors of LEB elements read by a length slot; vector fields accept any sequence | SPECOPS-O11 for fields without a terminator; writing a buffer sized by another field; fixed buffers of LEB elements; new: trailing `:length-of` and `:count-of` slots |
| 3. Checked reads | Open | — | Read-side region map; the policy of SPECOPS-O7 |
| 4. Structs and run-time nested values | Done | As before | — |
| 5. Variable structure | Done | New: sub-manifests as field plans; `:case` choosing a sub-manifest; repeats by count, count field, condition, end of source and length; `:count-of` slots; instance naming settled (SPECOPS-O10); a PNG of any chunk sequence read and written by one manifest | Instances addressed from outside a repeat, should a format need it |
| 6. Destinations | Open | — | Buffering (SPECOPS-D13), which also lets a manifest with a `:length-of` or `:count-of` slot write to a function; checksums on streams; overrun checks |
| 7. Selective reads and stream sources | Open | `known-count` and `known-extent` | Dependency closure and skip mode (SPECOPS-D14); stream sources |
| 8. WOFF | Open | New: `:count-of` slots, which its table directory needs | SPECOPS-D17, the first test of `:offset-of` |
| 9. Floats and masque completion | Partly done | Decimal masques in `masque` and `unmasque` | The `:f` class; decimal masques in `unmarshal`; range checks; keyword groups registered for `:case` and `:count` |
| 10. Packaging | Partly done | As before | Suites behind `test-op`; a Ref |

### Priorities

1. **Checked reads** (SPECOPS-O7): `unmarshal`
   checks checksums and length slots, now including those inside repeated
   instances.
2. **Destinations** (SPECOPS-D13).
3. **Selective reads and stream sources**
   (SPECOPS-D14).
4. **WOFF** (SPECOPS-D17).
5. **Floats and masque completion.**
6. **Packaging:** `test-op`, then a Ref.

### Dependencies

Checked reads and selective reads both need a read-side region map, which
also has to give each repeated instance fresh regions, as the write side does
(SPECOPS-D66). WOFF's dependency on count fields computed on
write is met. The rest are as in the update of 2026-10-30.

## Update 2026-11-01 — Progress: checked reads

This installment charts the plan's progress at commit `1cc7600`. The first
priority of the previous update, checked reads, is done:
SPECOPS-O7 is answered with a condition and restarts,
and `unmarshal` checks checksums, length, offset and count slots, inside
repeated instances too. The log's update of the same date records the work,
with SPECOPS-D69 through SPECOPS-D72. All
seventeen suites pass: 315 checks and the PNG verification.

### Progress

Table: State of each Roadmap milestone at commit `1cc7600`. Entries new since
the previous update are marked "new".

| Milestone | State | Done | Remaining |
|---|---|---|---|
| 1. Corrections and records | Partly done | As before | New: the byte orders of sub-manifests chosen by `:case` |
| 2. Field kinds on read | Partly done | As before | SPECOPS-O11 for fields without a terminator; writing a buffer sized by another field; fixed buffers of LEB elements; trailing `:length-of` and `:count-of` slots |
| 3. Checked reads | Done | New: `manifest-check-failed`, with `continue` and `use-value` restarts (SPECOPS-O7); checksum, length, offset and count slots checked, within each repeated instance; constants signal the same condition; `(:check nil)`; checksums over integer sources | — |
| 4. Structs and run-time nested values | Done | As before | — |
| 5. Variable structure | Done | As before | Instances addressed from outside a repeat, should a format need it |
| 6. Destinations | Open | — | Buffering (SPECOPS-D13), which also lets a manifest with a `:length-of` or `:count-of` slot write to a function; checksums on streams; overrun checks |
| 7. Selective reads and stream sources | Open | `known-count` and `known-extent`; new: the region table built at expansion (SPECOPS-D70) | Dependency closure and skip mode (SPECOPS-D14); stream sources, with checksums over them |
| 8. WOFF | Open | `:count-of` slots; new: `:offset-of` written and read back by a probe | SPECOPS-D17 |
| 9. Floats and masque completion | Partly done | Decimal masques in `masque` and `unmasque` | The `:f` class; decimal masques in `unmarshal`; range checks; keyword groups registered for `:case` and `:count` |
| 10. Packaging | Partly done | As before; new: the condition and its readers exported | Suites behind `test-op`; a Ref; new: the checksum functions exported |

### Priorities

1. **Destinations** (SPECOPS-D13): buffering,
   checksums on stream destinations, overrun checks.
2. **Selective reads and stream sources**
   (SPECOPS-D14).
3. **WOFF** (SPECOPS-D17).
4. **Floats and masque completion.**
5. **Packaging:** `test-op`, then a Ref.

The byte orders of `:case` branches are a small correction, to be taken
between larger steps.

### Dependencies

The region table of SPECOPS-D70 is the read-side region map
that the previous update said checked reads and selective reads share; the
dependency closure of SPECOPS-D14 extends it. Checksums
over stream sources need the bytes of each checksummed region kept, as
buffering (SPECOPS-D13) keeps them for stream
destinations; whether one reusable buffer serves both is
SPECOPS-O12, which buffering, the first priority, meets
first. The rest are as in the update of 2026-10-30.

## Update 2026-11-01 — Progress: destinations

This installment charts the plan's progress at commit `b63b7d2`. The first
priority of the previous update, destinations, is done: a manifest with slots
or checksums is written to functions, streams and any vector through a buffer
(SPECOPS-D13), which a caller may keep across calls
(SPECOPS-O12, answered for writing), and a vector too
small signals a condition. The correction listed in the previous update, the
byte orders of sub-manifests chosen by `:case`, is made. The log's update of
the same date records the work, with SPECOPS-D73 through
SPECOPS-D76. All eighteen suites pass: 335 checks and the PNG
verification.

### Progress

Table: State of each Roadmap milestone at commit `b63b7d2`. Entries new since
the previous update are marked "new".

| Milestone | State | Done | Remaining |
|---|---|---|---|
| 1. Corrections and records | Partly done | As before; new: the byte orders of sub-manifests chosen by `:case` (SPECOPS-D76) | New: checksums over `unmarshal` sources that are not simple byte arrays |
| 2. Field kinds on read | Partly done | As before | SPECOPS-O11 for fields without a terminator; writing a buffer sized by another field; fixed buffers of LEB elements; trailing `:length-of` and `:count-of` slots |
| 3. Checked reads | Done | As before | — |
| 4. Structs and run-time nested values | Done | As before | — |
| 5. Variable structure | Done | As before | Instances addressed from outside a repeat, should a format need it |
| 6. Destinations | Done | New: functions, streams and vectors other than simple arrays of units written through a buffer when a manifest has slots (SPECOPS-D73); a reusable `manifest-buffer` (SPECOPS-D74); `manifest-overrun` (SPECOPS-D75); a fault in writing slots to streams removed | Writing a buffer out in pieces, should a record grow too large to hold whole |
| 7. Selective reads and stream sources | Open | `known-count` and `known-extent`; the region table built at expansion | Dependency closure and skip mode (SPECOPS-D14); stream sources, with checksums over them through a `manifest-buffer` |
| 8. WOFF | Open | `:count-of` slots; `:offset-of` written and read back by a probe | SPECOPS-D17 |
| 9. Floats and masque completion | Partly done | Decimal masques in `masque` and `unmasque` | The `:f` class; decimal masques in `unmarshal`; range checks; keyword groups registered for `:case` and `:count` |
| 10. Packaging | Partly done | As before; new: the buffer and the overrun condition exported | Suites behind `test-op`; a Ref; the checksum functions exported |

### Priorities

1. **Selective reads and stream sources**
   (SPECOPS-D14).
2. **WOFF** (SPECOPS-D17).
3. **Floats and masque completion.**
4. **Packaging:** `test-op`, then a Ref.

Checksums over sources that are not simple byte arrays are a small
correction, to be taken between larger steps; it may fall out of stream
sources, which need the same conversion.

### Dependencies

Stream sources keep the bytes of checksummed regions in a `manifest-buffer`,
as SPECOPS-D74 intends, so the read side of
SPECOPS-O12 is settled when they are built. Selective
reads extend the region table of SPECOPS-D70. The rest are as
in the update of 2026-10-30.

## Update 2026-11-01 — Progress: stream sources

This installment charts the plan's progress at commit `239b2f7`, which builds
the first half of the first priority of the previous update: `unmarshal` reads
from streams, as far as each read needs, keeping the units for checksums and
taking exactly one record. SPECOPS-O12 is now answered
for reading as well as writing. The log's update of the same date records the
work, with SPECOPS-D77 through SPECOPS-D79. All
nineteen suites pass: 348 checks and the PNG verification.

### Progress

Table: State of each Roadmap milestone at commit `239b2f7`. Entries new since
the previous update are marked "new".

| Milestone | State | Done | Remaining |
|---|---|---|---|
| 1. Corrections and records | Partly done | As before | Checksums over `unmarshal` sources that are not simple byte arrays |
| 2. Field kinds on read | Partly done | As before | SPECOPS-O11 for fields without a terminator; writing a buffer sized by another field; fixed buffers of LEB elements; trailing `:length-of` and `:count-of` slots |
| 3. Checked reads | Done | As before | — |
| 4. Structs and run-time nested values | Done | As before | — |
| 5. Variable structure | Done | As before | Instances addressed from outside a repeat, should a format need it |
| 6. Destinations | Done | As before | Writing a buffer out in pieces, should a record grow too large to hold whole |
| 7. Selective reads and stream sources | Partly done | `known-count` and `known-extent`; the region table built at expansion; new: stream sources, read in order and never sought (SPECOPS-D77); a read from a stream takes one record (SPECOPS-D78); a `manifest-buffer` for their units (SPECOPS-D79) | Dependency closure and skip mode (SPECOPS-D14); new: units kept for the whole record, should one grow too large to hold |
| 8. WOFF | Open | `:count-of` slots; `:offset-of` written and read back by a probe | SPECOPS-D17 |
| 9. Floats and masque completion | Partly done | Decimal masques in `masque` and `unmasque` | The `:f` class; decimal masques in `unmarshal`; range checks; keyword groups registered for `:case` and `:count` |
| 10. Packaging | Partly done | As before | Suites behind `test-op`; a Ref; the checksum functions exported |

### Priorities

1. **Selective reads** (SPECOPS-D14), as the steward
   has settled them: fields are read past rather than sought on streams; a
   selective read of a stream still takes the whole record
   (SPECOPS-D78); and only the slots whose field and region lie
   in the part read are checked. Key paths into sub-manifests are deferred
   (Scope and Non-Goals).
2. **WOFF** (SPECOPS-D17).
3. **Floats and masque completion.**
4. **Packaging:** `test-op`, then a Ref.

Checksums over sources that are not simple byte arrays remain a small
correction between larger steps.

### Dependencies

Selective reads skip fields on a stream by reading past them, which stream
sources now provide, and take the rest of a stream's record by the same skips,
so the skipping of SPECOPS-D14 must cover every field
kind, not only those before the last key. They extend the region table of
SPECOPS-D70 with what each field needs to be read or skipped.
The rest are as in the update of 2026-10-30.

## Update 2026-11-01 — Progress: selective reads

This installment charts the plan's progress at commit `cb931c8`, which
completes the seventh milestone: given keys, `unmarshal` decodes only those
fields and the fields that locate them, and passes over the rest
(SPECOPS-D14), as the steward settled in the previous
update. The log's update of the same date records the work, with
SPECOPS-D80 through SPECOPS-D82. All twenty
suites pass: 360 checks and the PNG verification.

### Progress

Table: State of each Roadmap milestone at commit `cb931c8`. Entries new since
the previous update are marked "new".

| Milestone | State | Done | Remaining |
|---|---|---|---|
| 1. Corrections and records | Partly done | As before | Checksums over `unmarshal` sources that are not simple byte arrays |
| 2. Field kinds on read | Partly done | As before | SPECOPS-O11 for fields without a terminator; writing a buffer sized by another field; fixed buffers of LEB elements; trailing `:length-of` and `:count-of` slots |
| 3. Checked reads | Done | As before | — |
| 4. Structs and run-time nested values | Done | As before | — |
| 5. Variable structure | Done | As before | Instances addressed from outside a repeat, should a format need it |
| 6. Destinations | Done | As before | Writing a buffer out in pieces, should a record grow too large to hold whole |
| 7. Selective reads and stream sources | Done | As before; new: fields given modes at expansion (SPECOPS-D80); fields passed over through their plans' skip effectors (SPECOPS-D81); reading stopped after the last key, a stream's record taken whole, and only the part read checked (SPECOPS-D82) | Units kept for the whole record, should one grow too large to hold |
| 8. WOFF | Open | `:count-of` slots; `:offset-of` written and read back by a probe | SPECOPS-D17 |
| 9. Floats and masque completion | Partly done | Decimal masques in `masque` and `unmasque` | The `:f` class; decimal masques in `unmarshal`; range checks; keyword groups registered for `:case` and `:count` |
| 10. Packaging | Partly done | As before | Suites behind `test-op`; a Ref; the checksum functions exported |

Key paths into sub-manifests are deferred, as Scope and Non-Goals records.

### Priorities

1. **WOFF** (SPECOPS-D17).
2. **Floats and masque completion.**
3. **Packaging:** `test-op`, then a Ref.

Checksums over sources that are not simple byte arrays remain a small
correction between larger steps.

### Dependencies

WOFF depends on nothing still open: `:count-of` and `:offset-of` slots are
written and read, and its table directory's entries can be passed over by
selective reads. The Ref waits for the public interface of
SPECOPS-O6 and `test-op` for SPECOPS-O5, as in the
update of 2026-10-30.

## Update 2026-11-01 — Progress: placed fields and WOFF

This installment charts the plan's progress at commit `52b81e5`, which
completes the eighth milestone: WOFF files are written and read through
placed fields, whose bytes go where a `place` directive stands and are read at
their offsets (SPECOPS-D17). The log's update of the
same date records the work, with SPECOPS-D83 through
SPECOPS-D86; it also answers in part the prior-art evaluation's
SPECOPS-O13. All twenty-one suites pass: 377 checks and
the PNG verification. Outside the suite, WOFFs packed from two real fonts, and
seven real WOFFs written back, pass fontTools with checksum checking.

### Progress

Table: State of each Roadmap milestone at commit `52b81e5`. Entries new since
the previous update are marked "new".

| Milestone | State | Done | Remaining |
|---|---|---|---|
| 1. Corrections and records | Partly done | As before | Checksums over `unmarshal` sources that are not simple byte arrays |
| 2. Field kinds on read | Partly done | As before | SPECOPS-O11 for fields without a terminator; writing a buffer sized by another field; fixed buffers of LEB elements; trailing `:length-of` and `:count-of` slots |
| 3. Checked reads | Done | As before | — |
| 4. Structs and run-time nested values | Done | As before | — |
| 5. Variable structure | Done | As before; new: data described from outside a repeat, through placed fields (SPECOPS-D83) | Instances addressed by name from outside a repeat, should a format need it |
| 6. Destinations | Done | As before | Writing a buffer out in pieces, should a record grow too large to hold whole |
| 7. Selective reads and stream sources | Done | As before | Units kept for the whole record, should one grow too large to hold |
| 8. WOFF | Done | New: placed fields, written where their directive stands (SPECOPS-D83) and read at their offsets (SPECOPS-D84); fields describing the uncompressed font given (SPECOPS-D85); placed data written in queue order (SPECOPS-D86); `sfnt-checksum`; WOFF and sfnt test manifests, checked against real fonts and fontTools | — |
| 9. Floats and masque completion | Partly done | Decimal masques in `masque` and `unmasque` | The `:f` class; decimal masques in `unmarshal`; range checks; keyword groups registered for `:case` and `:count` |
| 10. Packaging | Partly done | As before | Suites behind `test-op`; a Ref; the checksum functions exported |

### Priorities

1. **Floats and masque completion.**
2. **Packaging:** `test-op`, then a Ref.

Checksums over sources that are not simple byte arrays remain a small
correction between larger steps, as do the remaining field kinds of milestone
2, which wait on demand.

### Dependencies

Floats can draw on the prior-art evaluation's
SPECOPS-O19, which the steward has yet to dispose of.
The Ref waits for the public interface of SPECOPS-O6 and
`test-op` for SPECOPS-O5, as in the update of 2026-10-30. The
evaluation's other questions, SPECOPS-O14 through
SPECOPS-O18, bear on work beyond this plan.

## Update 2026-11-01 — Progress: masque completion

This installment charts the plan's progress at commit `e340210`, which
completes the masque half of the ninth milestone. Decimal masks now work in
`masque` and `unmasque`, which the earlier progress tables had wrongly listed
as done; `masque` takes an `:overflow` parameter, `:mask` by default; and
manifests read decimal masques, signal an error for a group value too wide,
and let masque groups select `:case` branches. The log's update of the same
date records the work, with SPECOPS-D87 through
SPECOPS-D90. All twenty-one suites pass: 382 checks and the PNG
verification.

### Progress

Table: State of each Roadmap milestone at commit `e340210`. Entries new since
the previous update are marked "new".

| Milestone | State | Done | Remaining |
|---|---|---|---|
| 1. Corrections and records | Partly done | As before | Checksums over `unmarshal` sources that are not simple byte arrays |
| 2. Field kinds on read | Partly done | As before | SPECOPS-O11 for fields without a terminator; writing a buffer sized by another field; fixed buffers of LEB elements; trailing `:length-of` and `:count-of` slots |
| 3. Checked reads | Done | As before | — |
| 4. Structs and run-time nested values | Done | As before | — |
| 5. Variable structure | Done | As before | Instances addressed by name from outside a repeat, should a format need it |
| 6. Destinations | Done | As before | Writing a buffer out in pieces, should a record grow too large to hold whole |
| 7. Selective reads and stream sources | Done | As before | Units kept for the whole record, should one grow too large to hold |
| 8. WOFF | Done | As before | — |
| 9. Floats and masque completion | Partly done | New: decimal masks fixed in `masque` and `unmasque` (SPECOPS-D87); the `:overflow` parameter (SPECOPS-D88); decimal masques in manifests and range errors on write (SPECOPS-D89); masque groups as `:case` selectors (SPECOPS-D90) | The `:f` class; new: range checks that tell signed groups from unsigned |
| 10. Packaging | Partly done | As before | Suites behind `test-op`; a Ref; the checksum functions exported |

### Priorities

1. **Floats:** the `:f` class, as the steward has settled it from the
   prior-art evaluation's SPECOPS-O19. Widths of 16,
   32, 64 and 128 bits: single and double floats as such, half floats decoded
   to single floats, which hold them exactly, and quad floats to exact
   rationals. SBCL's float bit accessors where available, behind a
   conditional, and a portable arithmetic path elsewhere, which reads and
   writes infinities and NaN as `:infinity`, `:-infinity` and `:nan`; quad
   floats use those keywords everywhere. A value too large for its format is
   an error.
2. **Packaging:** `test-op`, then a Ref.

Checksums over sources that are not simple byte arrays remain a small
correction between larger steps.

### Dependencies

Floats depend on nothing open: a float is written and read as the integer of
its bit pattern, through the scalar and vector plans as they stand. The Ref
waits for the public interface of SPECOPS-O6 and `test-op` for
SPECOPS-O5, as in the update of 2026-10-30.

## Update 2026-11-01 — Progress: floats

This installment charts the plan's progress at commit `5862d3a`, which
completes the ninth milestone: the `:f` class holds IEEE binary floats of 16,
32, 64 and 128 bits, as the steward settled them from the prior-art
evaluation's SPECOPS-O19. The log's update of the same
date records the work, with SPECOPS-D91 and
SPECOPS-D92. All twenty-two suites pass: 396 checks and the PNG
verification. Outside the suite, encodings and decodings matched Python's
`struct` on some 195,000 values and patterns.

### Progress

Table: State of each Roadmap milestone at commit `5862d3a`. Entries new since
the previous update are marked "new".

| Milestone | State | Done | Remaining |
|---|---|---|---|
| 1. Corrections and records | Partly done | As before | Checksums over `unmarshal` sources that are not simple byte arrays; new: widths that count bytes where SPECOPS-D32 says units |
| 2. Field kinds on read | Partly done | As before | SPECOPS-O11 for fields without a terminator; writing a buffer sized by another field; fixed buffers of LEB elements; trailing `:length-of` and `:count-of` slots |
| 3. Checked reads | Done | As before | — |
| 4. Structs and run-time nested values | Done | As before | — |
| 5. Variable structure | Done | As before | Instances addressed by name from outside a repeat, should a format need it |
| 6. Destinations | Done | As before | Writing a buffer out in pieces, should a record grow too large to hold whole |
| 7. Selective reads and stream sources | Done | As before | Units kept for the whole record, should one grow too large to hold |
| 8. WOFF | Done | As before | — |
| 9. Floats and masque completion | Done | As before; new: the `:f` class for 16, 32, 64 and 128 bits (SPECOPS-D91), converted arithmetically and through SBCL's floats (SPECOPS-D92) | Range checks that tell signed groups from unsigned, should a format need them |
| 10. Packaging | Partly done | As before | Suites behind `test-op`; a Ref; the checksum and float functions exported |

### Priorities

1. **Packaging:** `test-op`, then a Ref.

Two smaller matters remain between larger steps: checksums over sources that
are not simple byte arrays, and a decision for the steward on widths. Widths
count bytes in the code and units in SPECOPS-D32, and one of the
two should change before the Ref documents either.

### Dependencies

The Ref waits for the public interface of SPECOPS-O6, and should
follow the decision on widths, which the interface exposes. `test-op` follows
SPECOPS-O5, as in the update of 2026-10-30.

## Update 2026-11-01 — Progress: tests under Square

This installment charts the plan's progress at commit `e4a1f9a`, which
completes the first part of the tenth milestone: the manifest suites run
behind `test-op`, as the test system `specops/test`, rewritten to the Square
testing standard. The log's update of the same date records the work, with
SPECOPS-D93 for the test system and SPECOPS-D94
for the program and format scripts, which move to `scripts/`. The 396 checks
and the PNG verification are now 397 tests with 566 assertions, and all
pass. The architecture's SPECOPS-D1 now allows a test system
its libraries, FiveAM and cl-hamcrest, while `specops` and the modules stay
free of them.

### Progress

Table: State of each Roadmap milestone at commit `e4a1f9a`. Entries new since
the previous update are marked "new".

| Milestone | State | Done | Remaining |
|---|---|---|---|
| 1. Corrections and records | Partly done | As before | Checksums over `unmarshal` sources that are not simple byte arrays; widths that count bytes where SPECOPS-D32 says units; new: unused variables in `marshal` and `unmarshal` expansions, which draw style warnings |
| 2. Field kinds on read | Partly done | As before | SPECOPS-O11 for fields without a terminator; writing a buffer sized by another field; fixed buffers of LEB elements; trailing `:length-of` and `:count-of` slots |
| 3. Checked reads | Done | As before | — |
| 4. Structs and run-time nested values | Done | As before | — |
| 5. Variable structure | Done | As before | Instances addressed by name from outside a repeat, should a format need it |
| 6. Destinations | Done | As before | Writing a buffer out in pieces, should a record grow too large to hold whole |
| 7. Selective reads and stream sources | Done | As before | Units kept for the whole record, should one grow too large to hold |
| 8. WOFF | Done | As before | — |
| 9. Floats and masque completion | Done | As before | Range checks that tell signed groups from unsigned, should a format need them |
| 10. Packaging | Partly done | As before; new: the suites behind `test-op`, as the Square test system `specops/test` (SPECOPS-D93) | A Ref; the checksum and float functions exported |

### Priorities

1. **The Ref:** to be planned with the steward, as a step of its own. It
   documents the public interface, so it follows the exports and the decision
   on widths.
2. **Exports:** the checksum and float functions, with the rest of the
   public interface of SPECOPS-O6.

Checksums over sources that are not simple byte arrays, and the unused
variables in expansions, remain small corrections between larger steps.

### Dependencies

The Ref waits for the public interface of SPECOPS-O6 and the
decision on widths, as in the previous update. Suites for the program builder
and the format writers wait for SPECOPS-O5 and the steward's
revision of those writers (SPECOPS-D94); the manifest suites
no longer do.
