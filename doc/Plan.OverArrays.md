---
id:            SPECOPS-0008
title:         Manifests over Arrays of Records Plan
genre:         Plan
scope:         component
project:       SpecOps
component:     Manifest
language:      en
status:        Accepted
authors:
  - Andrew Sengul
approved-by:   Andrew Sengul
reviewed:      2026-11-12
provenance:
  assistant:   claude-opus (OpenCode)
  session:     Manifests over arrays
relates-to:
  - SPECOPS-0007
  - SPECOPS-0006
  - SPECOPS-0003
  - SPECOPS-0004
  - SPECOPS-0009
decisions:
  - SPECOPS-D95
  - SPECOPS-D96
  - SPECOPS-D97
  - SPECOPS-D98
  - SPECOPS-D99
  - SPECOPS-D100
  - SPECOPS-D101
  - SPECOPS-D102
  - SPECOPS-D103
  - SPECOPS-D104
  - SPECOPS-D105
  - SPECOPS-D106
  - SPECOPS-DRAFT-over-arrays-D13
  - SPECOPS-DRAFT-over-arrays-D14
  - SPECOPS-DRAFT-over-arrays-D15
  - SPECOPS-DRAFT-over-arrays-D16
  - SPECOPS-DRAFT-over-arrays-D17
open-questions:
  - SPECOPS-O20
  - SPECOPS-O21
  - SPECOPS-O22
  - SPECOPS-O23
  - SPECOPS-O24
  - SPECOPS-O36
cites:
  - title:    The Cell Model Architecture (Lexter)
    locator:  Lexter repository, doc/Arch.CellModel.md — the record by word (LEXTER-D81 to LEXTER-D95); The Record; Invariants of a record
    external: true
  - title:    Python struct module
    locator:  docs.python.org/3/library/struct.html — pack_into, unpack_from
    external: true
  - title:    Python ctypes module
    locator:  docs.python.org/3/library/ctypes.html — _CData.from_buffer
    external: true
  - title:    NumPy structured arrays
    locator:  numpy.org/doc/stable/user/basics.rec.html — indexing and assignment of fields
    external: true
  - title:    Common Lisp HyperSpec
    locator:  ldb, dpb, deposit-field; the :start arguments of replace, read-sequence and write-sequence
    external: true
---

# Manifests over Arrays of Records: Development Plan

The manifest layer of release 0.1.0 writes and reads a record at the start of
its destination or source, and writes every field of it
([Manifest Reference, SPECOPS-0007](Ref.Manifest.md)). Many programs keep
records side by side in one array and change them a field at a time. Lexter,
a terminal emulator, keeps the cells of its screen as 32-byte records in the
array it uploads to the GPU, and changes a cell's blink flag, palette slot or
an ink slot without rewriting the rest. This plan adds the two things such
programs need: a record that begins at an index of a vector, and a write that
patches some fields of a record already there and leaves its other bits as
they are. It also lets a selective read or a patch name a field inside a
sub-manifest or a `:case` branch. Lexter's cell model, as its
own corpus specifies it in *The Cell Model Architecture (Lexter)*, is the test case,
as PNG and WOFF were for the [Manifest Completion Plan, SPECOPS-0003](Plan.ManifestComplete.md). The design assumes the decisions of the
[Manifest Development Log, SPECOPS-0006](Log.Manifest.md).

## Problem

`marshal` writes from position 0 of a vector, and `unmarshal` reads from
position 0. A record at cell *i* of an array, at index 32*i*, can be reached
only through a displaced array made for each call, or by copying it out and
back. Both cost allocation or copying on every cell written, and a screen is
redrawn a row of cells at a time.

`marshal` also writes every field. To change one field, a writer must supply
all the others, or read the record first and write it back, so changing a
cell's blink flag means decoding and re-encoding its 32 bytes. Lexter's design
reads and changes the flag with a byte store, and its writers are meant to use
generated accessors that write one field and mask the rest of its word
(LEXTER-D84, LEXTER-D104).

A selective read can name only a field at the top level of a manifest, or a
sub-manifest whole (SPECOPS-D80). Lexter's glyph words are sub-manifests
chosen by a `:case` on a type bit, so the reach of a mask, or the ink slot of a
layer, cannot be read or written alone.

## Goals

- A record is written or read at any index of a vector, and what it holds
  means the same at any index as at 0.
- `marshal` can write only the fields it is given, into a record already
  there, with the values, encodings and checks of a full write.
- A field inside a sub-manifest or a `:case` branch can be named, for a
  selective read and for a patch.
- Lexter's cell model is described by manifests, tested in the suites, and
  its cells are written and read in place.
- Each step is released with the Reference updated and tagged.

## Settled Decisions

### SPECOPS-D95 — A record may begin at an index of a vector, given by `(:at index)`

**Status:** Accepted
**Context:** Arrays of records are the common case this plan addresses, and a
record must be reachable where it lies without a displaced array or a copy.
`marshal` already keeps its write position in a variable that begins at 0,
and a second variable marking the record's start, against which `:to` pads
count (`src/manifest/manifest.lisp:marshal@v0.1.0`); `unmarshal`'s cursor is
the same.
**Decision:** `marshal`'s options and `unmarshal`'s parameters take
`(:at index)`, a form evaluated when the code runs: the record begins at
INDEX of the vector, a destination or source. Both positions begin there.
`(:at index)` combines with every other option, among them `(:from record)`,
`(:as :record)`, `(:check nil)` and `(:patch t)`
(SPECOPS-D97). It applies to vectors only: with a stream, a
function or an integer source it is an error when the code runs, and an
expansion error where the source or destination is a constant. Without it, a
record begins at 0, as in 0.1.0. A selective read at an index reads one field
of one record: `(unmarshal plain-cell grid (:at (* 32 i)) :background)`.
**Alternatives:** A displaced array for each call (not taken: an allocation
for every record, and a displaced array is not a simple array, so a manifest
with slots would be written through a buffer, SPECOPS-D73); the keyword
`:start`, after Common Lisp's sequence functions (not taken: there it pairs
with `:end` to bound a range, and a record's end is its own); sources and
destinations as a protocol, with an index as one backend's state (deferred to
SPECOPS-O16: an index needs no protocol, and a protocol would not remove the
need for it).

### SPECOPS-D96 — Positions inside a record count from its start; positions reported outside it count from the vector's

**Status:** Implemented
**Context:** With records at an index, each position the layer computes or
reports could count from the record's start or from the vector's. An offset a
format stores, such as a WOFF table's, is an offset within its file or record,
so it must not depend on where the record lies. A position reported in a
condition, or returned to chain records, is used to find a unit in the vector.
**Decision:** Positions are counted as follows.

Table: What each position counts from, with `(:at index)`.

| Position | Counts from |
|---|---|
| A `:to` pad's target | The record's start, as now (SPECOPS-D42) |
| An `:align` pad's boundary | The vector's start, as now (SPECOPS-D42); for records at multiples of the alignment the two agree |
| The value of an `:offset-of` slot, written and read | The record's start |
| A placed field's position on read | The record's start, plus the offset its slot gives |
| The range given to a `:checksum-of` slot's function | Indices into the vector, the region's units where they lie |
| `check-position` and `overrun-position` | The vector's start |
| `marshal`'s result | The vector's start: the position after the record, so that `(setf i (marshal cell grid (:at i) ...))` writes records one after another |

A stream source is unchanged: its positions count from where the read began,
which is the record's start (SPECOPS-D78).
**Alternatives:** Every position from the vector's start (rejected: an
offset slot would depend on where its record lies, so a record copied
elsewhere would be corrupt); every position from the record's start (not
taken: a reported position must locate the fault in the vector, and a result
relative to the record cannot chain).

### SPECOPS-D97 — `marshal` given `(:patch t)` writes only the fields it is given, into the record already there

**Status:** Accepted
**Context:** A writer that changes one field of a record should neither
supply the others nor decode them. `unmarshal` reads part of a record when it
is given keys, without a macro of its own (SPECOPS-D61, SPECOPS-D80); writing
part of a record is the same idea in the other direction.
**Decision:** `marshal`'s options take `(:patch t)`. A patching write writes
the fields given in the pairs, and with `(:from record)` those the record
gives a value for, the pairs overriding it; every other unit and bit of the
record is left as it is. The record must already be in the vector:
`(marshal plain-cell grid (:at (* 32 i) :patch t) :blink 1)`.

- **Values** are encoded as a full write encodes them: through enumerations
  and codecs, with range errors, floats and masque overflow as there.
- **Masque groups** are written by reading the masque's units, clearing the
  group's bits, setting the new value and writing the units back. Groups of
  one masque given in one call are combined into one such write. A masque's
  static groups and constant digits are not rewritten.
- **What is never written:** fields not given, unnamed fields, pads, constant
  strings and slot fields. A patch changes no field's width
  (SPECOPS-D98), so length, count and offset slots stay true.
- **Finding a field.** A field at a position fixed at expansion, as every
  field of a Lexter cell is, is written at that position with no reads. A
  field after a variable-length one would be found by reading the fields that
  locate it, as a selective read does (SPECOPS-D80); that case is deferred by
  SPECOPS-D101.
- **Destinations.** Vectors only, since a patch reads before it writes;
  a stream or a function is an error when the code runs.

Backpatching a slot (SPECOPS-D34, SPECOPS-D73) is the same operation, a
change to part of a structure already written, so the option shares its
name; the steward judged the shared word a help rather than a conflict, since
backpatching is named mainly inside the layer.
**Alternatives:** A separate macro, such as `patch` (not taken: the
steward chose an option of `marshal`, which writes part of a record as
`unmarshal` reads part of one when given keys); `:amend` (not taken by the
steward: an amendment is usually understood to add to what it amends, as to a
document, where a patch changes part of it in place); `:update` (not taken by
the steward, as too generic); `:deposit`, after Common Lisp's `dpb` and
`deposit-field` (not taken: the steward chose `:patch`); writes of single
fields through `setf` of a keyed `unmarshal` form only (not taken as the
mechanism: it can be added later as a form of this option, for one field).

### SPECOPS-D98 — A patch refuses what would change the record's layout or leave a checksum stale

**Status:** Accepted
**Context:** A patch writes some fields and trusts the rest. A field
whose width can change would move the fields after it, which are not
rewritten; a field that selects a later field's form would change how that
field is read; and a field covered by a checksum would leave the checksum
wrong.
**Decision:** These are expansion errors under `(:patch t)`, each naming the
field and the option:

- a field whose width is not fixed: a vector without a constant count, a
  terminated or stop-rule field, a LEB128 field, or a repeat;
- a field that selects a `:case` branch, or gives a count or a length, unless
  every branch it can select is of one fixed width and no count or length
  depends on it, so that no other field moves; a glyph word's type bit, whose
  branches are all one word, may be patched;
- a field inside a region a `:checksum-of` slot covers (the slot's
  recomputation is SPECOPS-O21).

A full write accepts all of these, so the messages say that the patch is
the reason.
**Alternatives:** Rewriting the fields after a field whose width changes
(rejected: they would have to be read and written again, which is a full
write); leaving the checks to the writer (rejected: a record silently
inconsistent is the fault the checks of SPECOPS-D69 exist to catch on read,
and here it can be caught at expansion).

### SPECOPS-D99 — A field inside a sub-manifest or a `:case` branch is named by a path

**Status:** Implemented
**Context:** A selective read names a top-level field, or a sub-manifest
whole. Lexter's glyph words are sub-manifests chosen by type bits, and a
writer changes a layer's ink slot, a mask's reach, or one glyph, alone.
**Decision:** A key, in a selective read or in a patch's pairs, may be a
list of field names from the top level down: `(:g3 :reach)` names the
`:reach` field of the sub-manifest or branch held by `:g3`. Through a `:case`
field, the path names a field of whichever branch is in the record; when the
code runs, the record's selector is read, and a path whose field the selected
branch lacks signals an error naming the path and the branch found. A path
that no branch can satisfy is an expansion error. A key that is a keyword
names a top-level field, as in 0.1.0.
**Alternatives:** Names joined into one keyword, such as `:g3-reach` (not
taken: it would clash with field names and could not be told apart from
them); a path that names the branch as well, such as `(:g3 :mask :reach)`
(not taken: the branch is in the record, and naming it would only add a way
to be wrong, which the run-time check catches anyway).

### SPECOPS-D100 — A patch returns the position after the record

**Status:** Accepted
**Context:** A full write returns the position after the record, so that
records can be written one after another (SPECOPS-D96). A
patch writes some fields, so the position after its last unit is not, in
general, the record's end (SPECOPS-O20).
**Decision:** A patching write returns the position after the record, as a
full write does, so that patches chain like full writes. For a record whose
size is fixed at expansion, as every Lexter cell's is, the position is the
record's start plus that size, with no reads. For one whose size is not, the
patch passes over the fields after those it writes, as a selective read
passes over fields (SPECOPS-D81), to find the record's end.
**Alternatives:** The position after the last unit written (not taken by the
steward: a result that is not the record's end cannot chain); nothing (not
taken: a writer of records in sequence would have to compute each record's
end itself).

### SPECOPS-D101 — Patching a field whose position depends on an earlier field of varying width is reserved for later

**Status:** Implemented
**Context:** SPECOPS-D97 finds a field after a
variable-length one by reading the fields that locate it. Lexter's cells, the
case in view, place every field at a fixed position, and locating a field by
reading is the hardest part of a patch.
**Decision:** A patch of a field whose position is not fixed at expansion is
an expansion error, naming the field and the earlier field of varying width
it follows, until a later step builds the reading that locates it. Fields at
fixed positions are patched as SPECOPS-D97 describes.
**Alternatives:** Locating such fields by reading in the same step (not taken:
the steward chose to reserve it, since no format in view needs it, and
building it would delay the patches that Lexter does need).

### SPECOPS-D102 — Writes use code made for each field, no span map without slots, and a fast path for records of fixed size written to simple vectors

**Status:** Accepted
**Context:** The prototype measured a write of one of Lexter's cells at some
2,500 ns, with some 1,300 bytes allocated, where the same bytes packed by hand
take 8 ns; the steward set an initial target of 80 ns or less, with no
allocation as the ideal. The [evaluation of the cost of a write](Eval.ManifestBenchmarks.md)
found the cost in the generic serializer every field is written through, from
a fresh `(width . value)` pair; in a map of each field's span, which only slots
read; and in the closures through which each unit is entered and checked
against the vector. Estimated by transforming `marshal`'s own expansion, its
remedies T1 to T4 bring the cell to 2,564, 2,344, 360 and 16 ns.
**Decision:** `marshal` takes the evaluation's three remedies that gain:

- **Each field written by code of its own (T3).** For each field, the
  expansion emits code of the field's width and byte order, which are known
  there, with the field's range check, in place of a call of the generic
  serializer with a fresh pair. This serves every record and every destination.
- **No span map without slots (T2).** A manifest with no slots keeps no map of
  its fields' spans; one with slots may keep only the spans its slots cover.
- **A fast path for records of fixed size (T4).** For a record whose size is
  fixed at expansion, the expansion adds a branch, chosen when the code runs
  when the destination is a simple vector of the manifest's unit type, that
  checks the record's units against the vector once and stores each unit
  directly, with the vector and the position declared. Any other destination
  takes the general code.

Making the serializers once, at load time (T1), is kept for whatever still
calls them, but is not a remedy on its own. The work is the roadmap's step
after paths and patches, which the steward has put first.
**Alternatives:** The serializers made once in the expansion of `defmanifest`
alone, the steward's first suspect (not taken as the remedy: the evaluation
measured no gain in time and 79 bytes fewer); the fast path without
declarations (rejected: 132 ns, above the target); the fast path for every
record (not possible as measured: a record of varying size has no extent to
check once, SPECOPS-O26); the fast path before paths
and patches (not taken by the steward, who has put it after them).

### SPECOPS-D103 — In this iteration, only a record of fixed size may be patched

**Status:** Implemented
**Context:** SPECOPS-D101 refuses a field whose position
depends on an earlier field of varying width, and
SPECOPS-D100 has a patch of a record of varying size pass over
the fields after those it writes to find the record's end. A record of fixed
size needs neither: every field's position and the record's end are known at
expansion. Lexter's cells are all of fixed size.
**Decision:** In this design iteration, `(:patch t)` applies only to a
manifest whose size is fixed at expansion; a patch of any other is an
expansion error naming the manifest and the first field of varying width.
Every field is then found at its position with no reads, and the patch
returns the record's start plus its size. This subsumes
SPECOPS-D101, and leaves the second case of
SPECOPS-D100 for a later iteration.
**Alternatives:** Patching records of varying size now, reading to locate
fields and to find the record's end (not taken by the steward for this
iteration).

### SPECOPS-D104 — `(:from record)` with `(:patch t)` is deferred

**Status:** Implemented
**Context:** SPECOPS-D97 has a patch with `(:from record)`
write the fields the record gives a value for. Which those are is known only
when the code runs, so every field of the manifest would need a test then.
**Decision:** In this iteration, `(:patch t)` with `(:from record)` is an
expansion error; a patch writes the fields its pairs give.
**Alternatives:** Each field written when the record gives it a value (not
taken: the steward chose to defer it).

### SPECOPS-D105 — Nested values in a patch write only the fields they give, and a selector given in the patch chooses its branch

**Status:** Implemented
**Context:** A full write takes a sub-manifest's value as a literal list of
its field pairs, and writes the child whole. A patch writes only what it is
given, and a field in a `:case` branch depends on a selector that the patch
may give or leave as it is.
**Decision:** In a patch, a sub-manifest's literal pairs, `:g0 (:glyph 103)`,
write only those fields of the child, as the path `(:g0 :glyph)` does, and
leave its other fields as they are. A field reached through a `:case` branch
is written in the branch its selector chooses: the selector's value given in
the same patch when there is one, as `:type-1 1` with `(:g1 :reach) 2`, and
otherwise the selector as it stands in the vector, read when the code runs.
A path whose field the chosen branch lacks is an error, as for a read
(SPECOPS-D99).
**Alternatives:** Nested pairs that rewrite the whole child with defaults
(rejected: a patch would then write fields it was not given); the selector
always read from the vector (rejected: a patch that changes a type bit and its
glyph word together would write the word in the old branch's form).

### SPECOPS-D106 — A patch checks the whole record against the vector before it writes

**Status:** Implemented
**Context:** A full write checks each unit against the vector as it writes it,
so an overrun leaves the units before it written. A patch writes into a record
that is already there, where a partial write would leave it inconsistent.
**Decision:** A patch checks that the record, from its start to its fixed
end, lies within the vector before it writes any unit, and signals
`manifest-overrun` otherwise, with the vector's size and the record's end as
the position, writing nothing.
**Alternatives:** Checking unit by unit as a full write does (not taken: the
steward chose the check before writing, and an overrun would otherwise leave
a record partly patched).

### SPECOPS-DRAFT-over-arrays-D13 — Binary values are coded by a code builder of their own, which the manifest effectors call

**Status:** Proposed
**Context:** SPECOPS-D102's remedy T3 has each field written by code of its
own width and byte order. The manifest layer's effectors build a field's code
from its plan and a context whose hooks, `:write` and `:read-at`, are
functions of a width and a value (`src/manifest/scan.lisp:write-effector@v0.1.4`),
and those hooks call the generic serializer; plain scalars, masques,
constants, pads, strings and slots call it directly
(`src/manifest/manifest.lisp:marshal@v0.1.4`). What coding a value needs is
small: its width in units, the unit's size, the order of its units, its
signedness and range, and where its units go. The assembler and the program
layer serialize values too, outside manifests, through
`src/serial.lisp:serialize@v0.1.4`.
**Decision:** A code builder for binary values, in its own file,
`src/units.lisp`, loaded before the manifest layer and independent of it,
builds the forms that store and load a value. An *encoding* gives a value's
width in units, the unit's bits, the order of its units and its signedness. A
*target* is where units are stored: a function entered with each unit, or a
vector at a position, declared simple where it is known to be. A *source* is
where they are loaded from: a vector, an entering function or a stream, or an
integer. Its generic functions return forms: a store, with the value's range
check, of a value's units in their order; a load, assembling a value,
signed where the encoding is; and a target's single check of a record's
extent. Units are split out at expansion, so a constant is stored as literal
units. Its classes and outputs are simpler than the manifest effectors', and
the manifest effectors place its forms in theirs: the `:write` and
`:read-at` hooks call it, as do the writers that call the serializers
directly now. It is internal until the manifest layer has proven it, then to
be documented and its symbols exported. `serialize` and `serializer-for`
remain, for the assembler and the program layer.
**Alternatives:** The store as an effector of the manifest's own effector
system, a second axis of dispatch on the destination (not taken: the steward
chose a builder usable independently of manifests, whose code the manifest
effectors place); `serialize` made faster in place (rejected: a generic
function called when the code runs cannot know a field's width and order at
expansion, which is where T3's gain comes from).

### SPECOPS-DRAFT-over-arrays-D14 — The fast path is two targets of the code builder, and patches and scalar reads use it too

**Status:** Proposed
**Context:** SPECOPS-D102 adopts T3, code of each field's own; T2, no span map
without slots; and T4, one bounds check and direct stores for a record of
fixed size written to a simple vector of its unit type. A patch's masque
groups are read and written back through `deserializer-for` and the generic
serializer, and a selector it is not given is read through a selective
`unmarshal` (`src/manifest/manifest.lisp:marshal@v0.1.4`). `unmarshal` decodes
the order of units through `swap-segments`, as `marshal` encodes it.
**Decision:** Every unit `marshal` writes is stored through
SPECOPS-DRAFT-over-arrays-D13's builder. The general path, T3, stores through
an entering target, so streams, functions and the check of each unit work as
before. For a record of fixed size, the expansion also holds a second rendering
of its fields, T4, storing through a vector target after one check of the
record's extent, and a test of the destination's type when the code runs
chooses between them. A manifest without slots keeps no span map, T2. Patches
take the same code: a masque's groups are loaded, deposited and stored, and
the selectors are loaded directly. `unmarshal`'s scalar reads load through the
builder from a general source, so that the order of units is defined in one
place; the reads' own fast path is the roadmap's next step. With every unit
written through the builder, the serializers `marshal` makes on each call are
no longer needed, and T1 with them. A test asserts that writing one of
Lexter's cells through the fast path allocates nothing; times stay in
`scripts/bench-manifests.lisp` (SPECOPS-D67).
**Alternatives:** Patches left on the generic serializer (not taken: the
steward chose to apply T3 to them as well); `swap-segments` fixed for reads,
and replaced in the reads step (not taken: the order of units would be
defined in two places meanwhile).

### SPECOPS-DRAFT-over-arrays-D15 — A byte order is the order of a field's units, and mixed orders are written explicitly

**Status:** Proposed
**Context:** `:little` reverses a field's bytes, not its units, so in a
manifest of 16-bit units `(:u 2)` holding `#x01020304` is written
`#x0403 #x0201` (`src/serial.lisp:swap-segments@v0.1.4`), which the Reference
lists as a limit. Formats of 16-bit units order them both ways: Modbus
devices commonly store a 32-bit value low register first, and x86 stores a
32-bit value low word first. `:middle`, also `:pdp`, writes `#x01020304` in an
8-bit manifest as `03 04 01 02`, the order often called word-swapped (CDAB),
while the PDP-11 stores it as `02 01 04 03` (BADC); "middle-endian" names
either in different sources. No manifest in the repository uses `:middle` or
`:pdp`, or `:little` with units wider than 8 bits.
**Decision:** `:big` stores a field's most significant unit first, and
`:little` its least significant first, whatever the unit's width. A mixed
order is written `(outer inner group)`: the field's units in groups of
GROUP, the groups in the OUTER order and the units within each group in the
INNER one. `(:big :little 2)` is BADC and `(:little :big 2)` is CDAB, in an
8-bit manifest. `:pdp` names the PDP-11's order, BADC, `(:big :little 2)`.
`:middle` is retired, an expansion error that names the explicit forms. Reads
follow the same orders.
**Alternatives:** `:pdp` corrected to BADC and `:middle` kept as CDAB (not
taken: the steward chose the explicit form, since "middle-endian" names
different orders); the byte orders left as they are, with their limit
documented (rejected: a manifest of 16-bit units could not describe a
little-endian 32-bit value).

### SPECOPS-DRAFT-over-arrays-D16 — The fast path signals the errors the general path does, and writes nothing before an overrun

**Status:** Proposed
**Context:** SPECOPS-O27 asks whether the fast path must report errors as the
general path does. A range check is the same test on either path, and its
message is built only when it fails. The general path checks each unit
against the vector as it stores it, so an overrun leaves the units before it
written, and signals `manifest-overrun` at the first unit that does not fit.
The fast path checks the record's extent once, before it stores anything.
**Decision:** The fast path signals the same conditions, with the same
messages and readers, as the general path. An overrun's position is the unit
the general path would report, the greater of the record's start and the
vector's size, computed only when the check fails. The fast path writes
nothing before an overrun, so a record that also holds a value out of range
signals the overrun, where the general path signals whichever comes first.
This answers SPECOPS-O27.
**Alternatives:** Writing unit by unit up to an overrun, as the general path
does (rejected: it needs the check of each unit that T4 removes); reporting the
record's end, as a patch does (SPECOPS-D106) (not taken: the steward chose
the general path's reports).

### SPECOPS-DRAFT-over-arrays-D17 — A patch reads its selectors and checks its branches before it writes

**Status:** Proposed
**Context:** SPECOPS-O36 asks whether a patch should find a key that the
chosen branch lacks before it writes, as SPECOPS-D106 finds an overrun.
**Decision:** A patch reads every selector it needs and does not give, and
checks each key it gives through a `:case` field against the branch chosen,
before it stores its first unit. A key the branch lacks signals the error
SPECOPS-DRAFT-manifest-D76 describes, with nothing written. The selectors are
read once each, and the values read choose the branches written. This
answers SPECOPS-O36.
**Alternatives:** The writes through `:case` fields ordered last (rejected:
it narrows a partial write but does not remove it); the partial write
accepted and documented (not taken: the steward chose the check first).

## Lexter's cell model as a test case

Lexter's record is eight little-endian 32-bit words in an array of
`(unsigned-byte 8)`, and every field lies on a byte boundary or within one
byte. Manifests in 8-bit units with `:endian :little` describe it with the
features of 0.1.0, as one manifest for each format; a writer dispatches on
byte 0, the format, as Lexter's design intends (LEXTER-D81, LEXTER-D82). The
manifest of each format is flat, because a glyph word's form is chosen by a
type bit in byte 1, and a sub-manifest cannot read its parent's fields.

```lisp
(defmanifest glyph-layer (:unit 8 :endian :little :record t)
  (:glyph (:u 3) :default 0)
  (masque "b:00000iis" (s :selector :u :default 0) (i :ink :u :default 0)))

(defmanifest glyph-mask (:unit 8 :endian :little :record t)
  (:glyph (:u 3) :default 0)
  (masque "b:00rrroos" (s :selector :u :default 0)
          (o :operation :u :default 0 :enumerate-by mask-operation)
          (r :reach :u :default 0)))

(defmanifest plain-cell (:unit 8 :endian :little :record t)
  0                                                        ; byte 0: the plain format
  (masque "b:edcbannn" (n :glyph-count :u :default 0)      ; byte 1
          (a :type-1 :u :default 0) (b :type-2 :u :default 0) (c :type-3 :u :default 0)
          (d :type-4 :u :default 0) (e :type-5 :u :default 0))
  (masque "b:00000kqw" (w :wide :u :default 0)             ; byte 2: drawing flags
          (q :right-half :u :default 0) (k :blink :u :default 0))
  0                                                        ; byte 3: no parameters
  (:swatch :u16 :default 0)                                ; bytes 4–5
  (:palette :u8 :default 0)                                ; byte 6
  (masque "b:000000gg" (g :background :u :default 0))      ; byte 7
  (manifest :g0 glyph-layer)                               ; glyph 0 is a layer
  (:g1 (:case :type-1 (0 (manifest glyph-layer)) (1 (manifest glyph-mask))))
  ;; :g2 to :g5 alike, chosen by :type-2 to :type-5
  )
```

Description: The sketch, not yet run, declares Lexter's plain cell: a
constant format byte; the glyph count and five type bits; the drawing flags;
a byte of parameters that the plain format leaves zero; the header word's
swatch, palette slot and background slot; and six glyph words, the first a
layer and each of the others a layer or a mask by its type bit. Reserved bits
are constant zeros, which a read checks.

The other formats follow the same pattern. Tiles add a flag at bit 23, fields
in byte 3 and in the header word's last byte, two 16-bit tile fields and four
swatch indices; inline colours hold twelve bytes of colour after three glyph
words; the void's payload is reserved until Lexter settles it. A manifest
`cell-common`, of the first eight bytes only, is a view through which a
patch changes the fields every format shares, the drawing flags and the
header word's colours, without asking which format a cell has:

```lisp
(marshal cell-common grid (:at (* 32 i) :patch t) :blink 1)        ; one byte rewritten
(marshal plain-cell grid (:at (* 32 i) :patch t) (:g1 :ink) 0)     ; one glyph word's byte
(unmarshal plain-cell grid (:at (* 32 i)) (:g1 :reach))            ; a path through a branch
```

What the prototype must establish before the features are built:

- A masque of 16 bits in a little-endian manifest, as the tile fields need,
  which no test covers.
- Masque groups selecting `:case` branches by integer keys, glyph word by
  glyph word.
- A record of zeros reads as a blank plain cell, and a plain cell written
  with defaults is all zeros, as Lexter's design requires (LEXTER-D83).
- What a full write of one cell costs, against packing it by hand, which
  decides whether the fast path of SPECOPS-O15 is needed.

The prototype, the roadmap's first step, has since run these manifests and
established each point (see the update of 2026-11-11 on the prototype). Their
current form is in the tests, from
`test/test-manifest-arrays.lisp:plain-cell@v0.1.4`, and differs from the
sketch in details, such as `:operation`'s default, `:union`.

Lexter decides for itself whether its descriptor of the record (LEXTER-D104)
is built on these manifests.

## Scope and Non-Goals

- **Streams and functions.** An index and a patch are for vectors. A
  stream positioned by `file-position` already serves sequential records, and
  patching a stream in place is not attempted.
- **Fields that read their parent's fields.** A sub-manifest whose fields
  depend on an enclosing manifest's would let Lexter's cell share one
  manifest across formats; per-format manifests serve, so it is left out.
- **Recomputing slots in a patch**, beyond refusing the case, is
  SPECOPS-O21.
- **Generating shader code.** The layer will not write GLSL. What a
  generator would need from it is SPECOPS-O22.
- **Lexter's invariants across fields,** such as type bits beyond the glyph
  count being zero, stay Lexter's to assert; reserved bits written as
  constants are checked on read.

## Later Additions

These are recorded for after the roadmap's main steps, each to be taken up if
the prototype or a user shows the need. The fast path for records of fixed
size (SPECOPS-O15), first listed here, is now decided by
SPECOPS-D102 and is a step of the roadmap.

- **The layout as data** (SPECOPS-O18, SPECOPS-O22). A
  function giving each fixed field's unit offset, bit offset and width, which
  a patch computes anyway, would let a generator, such as Lexter's
  descriptor, write a shader's decoder from the same manifest.
- **Patches of records of varying size** (SPECOPS-D103, SPECOPS-D101,
  SPECOPS-D100). Fields located by reading the fields before them, as a
  selective read does, and the record's end found by passing over the fields
  after those written.
- **`(:from record)` in a patch** (SPECOPS-D104). The fields a record gives a
  value for written, each tested when the code runs.
- **Element indices in paths** (SPECOPS-O24). Paths into the instances of a
  repeat, such as `(:points 2 :x)`, to whose design the steward has prior art
  of his own to contribute.

## Open Questions

### SPECOPS-O20 — What does a patching write return?

A full write returns the position after the record. A patch writes some
fields, so the record's end is known only when its size is fixed at
expansion, as every Lexter cell's is. The thinking when the question was
raised was the position after the record when its size is fixed, and
otherwise the position after the last unit written, which does not chain.
Computing a record's size without writing it, SPECOPS-O17, would give the
record's end in every case.

Answered by SPECOPS-D100: the position after the record, in every case.

### SPECOPS-O21 — Should a patch recompute the checksum slots over the fields it writes?

A patch refuses a field under a `:checksum-of` slot
(SPECOPS-D98). In a vector, the region is there to read, so
the slot could be recomputed after the fields are written, at the cost of
locating the region and reading all of it. Whether a format that needs this
appears is open.

### SPECOPS-O22 — Should a manifest's layout be available as data, for generators such as shader code?

SPECOPS-O18 asks for a printed table of a manifest's layout. A generator needs
the same table as data: each fixed field's name, unit offset, bit offset,
width, signedness and enumeration, and which fields vary. Lexter's descriptor
is the first such generator in view. Is the table data first and printed from
it, and which of its fields does a shader generator need?

### SPECOPS-O23 — Should a read at an index report where the record ends?

`marshal`'s result lets a writer place the next record after the last. A
reader of a vector of records of varying size needs the same, but
`unmarshal` returns the record, or the keyed values as multiple values, and
has no place for a position. A parameter naming a place to store the end, or
an extra value under a parameter, are the candidates. Lexter's records are of
one size, so it does not need this.

### SPECOPS-O24 — How do paths name elements of repeats?

A path names fields, so it cannot reach into a repeated sub-manifest
(SPECOPS-D99). Paths that mix field names with element
indices, as `var.foo.bar[2].baz` does in JavaScript, would let a read or a
patch name one instance's field, such as `(:points 2 :x)`. The steward has
planned this for a later stage, and has prior art of his own to contribute to
its design.

### SPECOPS-O36 — Should a patch check its branches before it writes?

A patch checks the record's bounds before it writes any unit, because a
partial write would leave the record inconsistent (SPECOPS-D106). A key given
within a `:case` field that the chosen branch lacks is found only when the
code reaches that field, after the fields before it are written, so the
patch stops with the record partly changed
(SPECOPS-DRAFT-manifest-D76, `src/manifest/manifest.lisp:marshal@345f72d`).
The candidates:

- each selector the patch does not give read, and each branch's keys
  checked, before the first write; a Lexter cell's five type bits lie in one
  byte, so one read would serve them all;
- the writes of fields through `:case` fields ordered last, which narrows the
  partial write but does not remove it;
- the partial write accepted, and documented in the Reference.

Which of these SPECOPS-D106's reasoning requires, and at what cost to a patch
that the fast path (SPECOPS-D102) is to make cheap, is open.

Answered by SPECOPS-DRAFT-over-arrays-D17: the first, before the patch
writes.

## Prior Art

Table: Prior systems and what this plan draws from each.

| System | What to draw from |
|---|---|
| Python `struct` (`pack_into`, `unpack_from`) | A format written into, and read from, a buffer at an offset, the shape of `(:at index)` |
| Python `ctypes` (`from_buffer`) | A structure laid over a buffer at an offset, whose field assignments write the buffer in place: a patch of one field |
| NumPy structured arrays | Arrays of records with named fields, read and assigned one field at a time, across one record or all |
| Common Lisp `ldb`, `dpb`, `deposit-field` | Writing a field into an integer while keeping its other bits, which a patch does to a masque's units |
| Common Lisp `replace`, `read-sequence`, `write-sequence` | An index into a sequence given as a keyword, here a record's start rather than a range's |
| binary-structures ([SPECOPS-0004](Eval.LispBinaryTools.md)) | Offsets tracked at expansion and bounds checked once for a run of fixed fields, for the fast path; a printed layout, for the layout as data |
| The Cell Model Architecture (Lexter) | The test case: 32-byte records in an upload array, changed a field at a time, with generated accessors that mask the rest of a word |

## Roadmap

1. **The prototype.** Lexter's plain, tiles and inline-colour cells, and the
   view `cell-common`, as a test format in a suite `manifest-arrays`, with the
   checks listed under Lexter's cell model, and a measurement of a cell's
   full write against packing by hand.
2. **Records at an index** (SPECOPS-D95,
   SPECOPS-D96). `(:at index)` for `marshal` and `unmarshal`
   over vectors, with the positions of SPECOPS-D96, tested on Lexter's cells, on PNG
   and WOFF records placed at an index, and on records chained by `marshal`'s
   result. The Reference updated, released as 0.1.3.
3. **Paths** (SPECOPS-D99). Keys that name fields inside
   sub-manifests and `:case` branches, for selective reads, with the run-time
   check of the branch.
4. **Patches** (SPECOPS-D97,
   SPECOPS-D98, SPECOPS-D100,
   SPECOPS-D103 to SPECOPS-D106).
   `(:patch t)` for records of fixed size, with paths and nested pairs that
   write only what they give, masque groups combined into one write of their
   units, selectors given or read, the record checked against the vector
   first, the refusals of SPECOPS-D98, and the record's end
   returned. `marshal`'s internal variable `patching`,
   which marks a manifest with slots written through a buffer
   (`src/manifest/manifest.lisp:marshal@v0.1.0`), is renamed `backpatching`,
   so that the code does not use one word for two things. The Reference
   updated, its sentence on what cannot be patched in place saying
   "backpatched", and released as 0.1.4, with the paths of step 3.
5. **The fast path** (SPECOPS-D102, SPECOPS-DRAFT-over-arrays-D13 to
   SPECOPS-DRAFT-over-arrays-D17). The code builder for binary values,
   `src/units.lisp`, with a suite of its own; every unit `marshal` writes
   stored through it, the general path through an entering target and, for
   records of fixed size, a second rendering through a vector target after
   one check of the extent; no span map without slots; patches through the
   same code, with their selectors read and branches checked first; scalar
   reads loaded through it; byte orders as orders of units. A test that a
   cell's fast write allocates nothing. Measured again with the direct cases
   of `scripts/bench-manifests.lisp` against the initial target, a full write
   of a Lexter cell in 80 ns or less, and the gain of the general path for
   streams and functions, which the evaluation did not measure, measured as
   well. The Reference updated, its byte orders and the path error of
   `7eb7ddf` among the changes, and released as 0.2.0.
6. **Reads** (SPECOPS-O25). Reads measured as writes were, and remedied
   through the code builder's sources: a fixed record's one bounds check and
   direct loads. Before recursive manifests (SPECOPS-0010).
7. **Later additions**, as users show the need: the layout as data of
   SPECOPS-O22, and the others listed under
   [Later Additions](#later-additions).

Each step is recorded in an update to the
[Manifest Development Log, SPECOPS-0006](Log.Manifest.md) and a progress
update to this plan.

## Update 2026-11-11 — Progress: the prototype

This installment charts the plan's progress at commit `f85a370`, which
completes its first step: Lexter's cell model as a test format, in the suite
`manifest-arrays`, with a measurement of what a cell costs. The
[Manifest Development Log's](Log.Manifest.md) update of the same date records
the work. The steward has since settled two of the plan's questions, recorded
above as SPECOPS-D100 and SPECOPS-D101, and
renumbered the releases: records at an index are 0.1.3, and paths with
patches 0.1.4, the steps being too small for 0.2.0, which is to follow further
work.

### Progress

Table: State of each Roadmap step at commit `f85a370`.

| Step | State | Done | Remaining |
|---|---|---|---|
| 1. The prototype | Done | Lexter's plain, inline-colour and tile cells, and the view `cell-common`, in 14 tests against the worked records of Lexter's architecture; the format's three open points settled; the cost measured | — |
| 2. Records at an index | Not started | — | `(:at index)`, released as 0.1.3 |
| 3. Paths | Not started | — | Keys through sub-manifests and `:case` branches |
| 4. Patches | Not started | — | `(:patch t)`, released as 0.1.4 with paths |
| 5. Later additions | Not started | — | The fast path; the layout as data |

The prototype found that Lexter's format needs no feature beyond release
0.1.2: a 16-bit masque in a little-endian manifest, glyph words chosen by
type bits, and a record of zeros as a blank cell all work as the plan
expected. It also found that a full write of one cell costs some 2,500 ns
and allocates some 1,340 bytes, about three hundred times the cost of packing
the cell by hand, and that reading one flag costs some 210 ns and allocates
144 bytes (`scripts/bench-cell.lisp`). A screen written in full each frame
would spend tens of milliseconds and megabytes of allocation on it.

### Priorities

1. **The cost of a call,** if the steward brings it forward: finding where
   the time and the allocation go, then the fast path for records of fixed
   size (SPECOPS-O15), before `:at` and patches are built on the code as it
   stands. A patch of a fixed record would otherwise carry the same costs. The
   order is the steward's to decide.
2. **Records at an index,** released as 0.1.3.
3. **Paths and patches,** released as 0.1.4.

### Dependencies

Records at an index depend on nothing open. Patches depend on paths, for the
fields of Lexter's glyph words. The fast path depends on nothing, and the
work after it would be simpler for having it; the layout as data waits for
patches, which compute the positions it would describe.

## Update 2026-11-11 — Progress: records at an index

This installment charts the plan's progress at commit `cba66c2`, tagged
`v0.1.3`, which completes its second step: `marshal` and `unmarshal` take
`(:at index)`, with positions counted as SPECOPS-D96
decides. The [Manifest Development Log's](Log.Manifest.md) update of the same
date records the work, with SPECOPS-DRAFT-manifest-D73 for the record's start,
which every nested manifest shares, and the
[Manifest Reference](Ref.Manifest.md) describes release 0.1.3. All 460 tests
pass, with 747 assertions.

### Progress

Table: State of each Roadmap step at commit `cba66c2`. Entries new since the
previous update are marked "new".

| Step | State | Done | Remaining |
|---|---|---|---|
| 1. The prototype | Done | As before | — |
| 2. Records at an index | Done | New: `(:at index)` for vectors, released as 0.1.3; Lexter's cells, a PNG and placed fields at an index, records written in sequence, and the errors tested | A record with slots in a vector that is not simple is buffered from position 0 rather than from its index |
| 3. Paths | Not started | — | Keys through sub-manifests and `:case` branches |
| 4. Patches | Not started | — | `(:patch t)`, released as 0.1.4 with paths |
| 5. Later additions | Not started | — | The fast path; the layout as data |

### Priorities

1. **The cost of a call,** which the steward has chosen to take up next:
   measuring where the time and the allocation of a call go, then deciding
   what of the fast path of SPECOPS-O15 to build, before paths and patches.
2. **Paths and patches,** released as 0.1.4.

### Dependencies

The assessment depends on nothing open. Patches gain from it, since a patch
of a fixed record would otherwise carry each call's present costs.

## Update 2026-11-11 — Progress: the cost of a write assessed

This installment records the assessment the previous update put first, made
at commit `cba66c2` and written up as the
[evaluation of the cost of a write](Eval.ManifestBenchmarks.md). Its
measurements, against packing by hand and against binary-structures, show
three remedies reaching the steward's target of 80 ns with no allocation,
which SPECOPS-D102 adopts. The steward has put their
implementation after paths and patches, as the roadmap's fifth step.

### Progress

Table: State of each Roadmap step at commit `cba66c2`. Entries new since the
previous update are marked "new".

| Step | State | Done | Remaining |
|---|---|---|---|
| 1. The prototype | Done | As before | — |
| 2. Records at an index | Done | As before | As before |
| 3. Paths | Not started | — | Keys through sub-manifests and `:case` branches |
| 4. Patches | Not started | — | `(:patch t)`, released as 0.1.4 with paths |
| 5. The fast path | New: decided | New: the cost of a write measured, and its remedies chosen (SPECOPS-D102) | Code made for each field, no span map without slots, and the fast path for records of fixed size |
| 6. Later additions | Not started | — | The layout as data; reads measured |

### Priorities

1. **Paths and patches,** released as 0.1.4.
2. **The fast path,** as a release of its own after them.

### Dependencies

Patches depend on paths. The fast path depends on nothing, and will carry
patches with it: a patch of a fixed record stores its fields through the same
code as a full write.

## Update 2026-11-12 — Progress: paths and patches

This installment charts the plan's progress at commit `345f72d`, tagged
`v0.1.4`, which completes its third and fourth steps: `unmarshal` takes paths,
and `marshal` takes `(:patch t)` for records of fixed size, as
SPECOPS-D97 to SPECOPS-D100 and
SPECOPS-D103 to SPECOPS-D106 decide. The
[Manifest Development Log's](Log.Manifest.md) update of the same date records
the work, with SPECOPS-DRAFT-manifest-D74 for how a path is read,
SPECOPS-DRAFT-manifest-D75 for how a patch is made from the manifest's layout,
and SPECOPS-DRAFT-manifest-D76 for how keys through a `:case` field are
checked; the [Manifest Reference](Ref.Manifest.md) describes release 0.1.4.
All 476 tests pass, with 798 assertions.

### Progress

Table: State of each Roadmap step at commit `345f72d`. Entries new since the
previous update are marked "new".

| Step | State | Done | Remaining |
|---|---|---|---|
| 1. The prototype | Done | As before | — |
| 2. Records at an index | Done | As before | As before |
| 3. Paths | New: done | New: paths through sub-manifests and `:case` branches in selective reads, checked at expansion and the branch when the code runs; released as 0.1.4 | Element indices in paths (SPECOPS-O24) |
| 4. Patches | New: done | New: `(:patch t)` for records of fixed size, with paths and nested pairs, masque groups written into their units, selectors given or read, the record checked first, the refusals, and the record's end returned; Lexter's cells patched in ten tests; `backpatching` renamed; released as 0.1.4 | Records of varying size (SPECOPS-D103); `(:from record)` (SPECOPS-D104); a branch mismatch found before the patch writes (SPECOPS-O36) |
| 5. The fast path | Decided | As before | As before |
| 6. Later additions | Not started | — | The layout as data; reads measured |

A patch checks the record's bounds before it writes, as
SPECOPS-D106 decides, but a key that the branch chosen when
the code runs lacks is found only when that branch is reached, after the
fields before it are written; SPECOPS-O36 asks whether a
patch should check its branches first.

### Priorities

1. **The fast path,** as a release of its own: code of its own for each field,
   no span map without slots, and one bounds check with direct stores for
   records of fixed size, measured again with `scripts/bench-manifests.lisp`.
2. **Later additions,** as users show the need.

### Dependencies

The fast path depends on nothing open. Of a patch's code, only the fields it
writes whole go through a full write's code, and gain from the fast path with
it. A masque's groups are read and written back through `deserializer-for`
and the generic serializer, from a fresh `(width . value)` pair
(`src/manifest/manifest.lisp:marshal@345f72d`), which is the cost T3 removes,
so for patches to gain, step 5 must replace that code too. A selector that a
patch does not give is read through a selective `unmarshal`, and a path reads
its first field whole. Both are reads, which SPECOPS-D102 does not cover; they
fall to the cost of reads (SPECOPS-O25).

## Update 2026-11-12 — Progress: the fast path

This installment charts the plan's progress at commit `023a7c6`, tagged
`v0.2.0`, which completes its fifth step. The steward settled the step's
design before it was built, as SPECOPS-DRAFT-over-arrays-D13 to
SPECOPS-DRAFT-over-arrays-D17: a code builder for binary values of its own,
the fast path as two of its targets, byte orders as orders of units, the fast
path's errors those of the general path, and a patch's branches checked
first. The [Manifest Development Log's](Log.Manifest.md) update of the same
date records the work, with SPECOPS-DRAFT-manifest-D77 for the two paths,
SPECOPS-DRAFT-manifest-D78 for stored orders and SPECOPS-DRAFT-manifest-D79
for a patch's selectors; the [Manifest Reference](Ref.Manifest.md) describes
release 0.2.0. All 497 tests pass, with 902 assertions.

### Progress

Table: State of each Roadmap step at commit `023a7c6`. Entries new since the
previous update are marked "new".

| Step | State | Done | Remaining |
|---|---|---|---|
| 1. The prototype | Done | As before | — |
| 2. Records at an index | Done | As before | As before |
| 3. Paths | Done | As before | As before |
| 4. Patches | Done | As before; new: branches checked before the first write (SPECOPS-DRAFT-over-arrays-D17) | Records of varying size (SPECOPS-D103); `(:from record)` (SPECOPS-D104) |
| 5. The fast path | New: done | New: the code builder, `src/units.lisp`, with the suite `units`; every unit `marshal` writes stored through it; a record of fixed size with no slots or placed fields written to a simple vector in 12 ns with nothing allocated, against 2,612 ns and 1,343 bytes before; the general path ten to fourteen times faster; patches through the same code, in 8 ns; scalar reads through it; byte orders as orders of units; a test that the fast path allocates nothing; released as 0.2.0 | The fast path for records with slots or placed fields (SPECOPS-O26) |
| 6. Reads | Not started | — | Reads measured and remedied through the builder's sources (SPECOPS-O25) |
| 7. Later additions | Not started | — | The layout as data, and the others listed |

The target the steward set, a full write of a Lexter cell in 80 ns or less,
is met at 12 ns, within 4 ns of packing by hand. A read of one field already
costs half what it did, 108 ns against 224, and allocates nothing, since the
array of deserializers made on each call is gone, but still calls a
deserializer for each field.

The fast path covers records of fixed size with no slots and no placed
fields, which all of Lexter's cells are; the step's roadmap entry and
SPECOPS-DRAFT-over-arrays-D14 name records of fixed size without that
condition. Records of fixed size with slots take the general path, some ten
times faster than before. SPECOPS-DRAFT-manifest-D77 records the condition,
and SPECOPS-O26 asks how far the fast path can reach.

### Priorities

1. **Reads,** the roadmap's sixth step: measured as writes were, then a fixed
   record's one bounds check and direct loads through the builder's vector
   source, before recursive manifests (SPECOPS-0010).
2. **Later additions,** as users show the need.

### Dependencies

Reads depend on nothing open: the builder's sources exist, and the general
source already defines the order of units for every read. Recursive manifests
wait for reads, so that a cycle's functions are built on the read code that
will stay.
