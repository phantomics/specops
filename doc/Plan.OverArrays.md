---
id:            SPECOPS-DRAFT-over-arrays
title:         Manifests over Arrays of Records Plan
genre:         Plan
scope:         component
project:       SpecOps
component:     Manifest
language:      en
status:        Draft
provenance:
  assistant:   claude-opus (OpenCode)
  session:     Manifests over arrays
relates-to:
  - SPECOPS-0007
  - SPECOPS-0006
  - SPECOPS-0003
  - SPECOPS-0004
decisions:
  - SPECOPS-DRAFT-over-arrays-D1
  - SPECOPS-DRAFT-over-arrays-D2
  - SPECOPS-DRAFT-over-arrays-D3
  - SPECOPS-DRAFT-over-arrays-D4
  - SPECOPS-DRAFT-over-arrays-D5
open-questions:
  - SPECOPS-DRAFT-over-arrays-O1
  - SPECOPS-DRAFT-over-arrays-O2
  - SPECOPS-DRAFT-over-arrays-O3
  - SPECOPS-DRAFT-over-arrays-O4
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

### SPECOPS-DRAFT-over-arrays-D1 — A record may begin at an index of a vector, given by `(:at index)`

**Status:** Proposed
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
(SPECOPS-DRAFT-over-arrays-D3). It applies to vectors only: with a stream, a
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

### SPECOPS-DRAFT-over-arrays-D2 — Positions inside a record count from its start; positions reported outside it count from the vector's

**Status:** Proposed
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

### SPECOPS-DRAFT-over-arrays-D3 — `marshal` given `(:patch t)` writes only the fields it is given, into the record already there

**Status:** Proposed
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
  (SPECOPS-DRAFT-over-arrays-D4), so length, count and offset slots stay true.
- **Finding a field.** A field at a position fixed at expansion, as every
  field of a Lexter cell is, is written at that position with no reads. A
  field after a variable-length one is found by reading the fields that
  locate it, as a selective read does (SPECOPS-D80).
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

### SPECOPS-DRAFT-over-arrays-D4 — A patch refuses what would change the record's layout or leave a checksum stale

**Status:** Proposed
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
  recomputation is SPECOPS-DRAFT-over-arrays-O2).

A full write accepts all of these, so the messages say that the patch is
the reason.
**Alternatives:** Rewriting the fields after a field whose width changes
(rejected: they would have to be read and written again, which is a full
write); leaving the checks to the writer (rejected: a record silently
inconsistent is the fault the checks of SPECOPS-D69 exist to catch on read,
and here it can be caught at expansion).

### SPECOPS-DRAFT-over-arrays-D5 — A field inside a sub-manifest or a `:case` branch is named by a path

**Status:** Proposed
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
  SPECOPS-DRAFT-over-arrays-O2.
- **Generating shader code.** The layer will not write GLSL. What a
  generator would need from it is SPECOPS-DRAFT-over-arrays-O3.
- **Lexter's invariants across fields,** such as type bits beyond the glyph
  count being zero, stay Lexter's to assert; reserved bits written as
  constants are checked on read.

## Later Additions

These are recorded for after the roadmap's main steps, each to be taken up if
the prototype or a user shows the need:

- **A fast path for records of fixed size** (SPECOPS-O15). Today a write in
  place calls a closure for each unit and checks the vector's bounds at each.
  A record whose extent is fixed at expansion could be written with one bounds
  check and direct stores. The prototype's measurement decides it.
- **The layout as data** (SPECOPS-O18, SPECOPS-DRAFT-over-arrays-O3). A
  function giving each fixed field's unit offset, bit offset and width, which
  a patch computes anyway, would let a generator, such as Lexter's
  descriptor, write a shader's decoder from the same manifest.

## Open Questions

### SPECOPS-DRAFT-over-arrays-O1 — What does a patching write return?

A full write returns the position after the record. A patch writes some
fields, so the record's end is known only when its size is fixed at
expansion, as every Lexter cell's is. The current thinking is the position
after the record when its size is fixed, and otherwise the position after the
last unit written, which does not chain. Computing a record's size without
writing it, SPECOPS-O17, would give the record's end in every case.

### SPECOPS-DRAFT-over-arrays-O2 — Should a patch recompute the checksum slots over the fields it writes?

A patch refuses a field under a `:checksum-of` slot
(SPECOPS-DRAFT-over-arrays-D4). In a vector, the region is there to read, so
the slot could be recomputed after the fields are written, at the cost of
locating the region and reading all of it. Whether a format that needs this
appears is open.

### SPECOPS-DRAFT-over-arrays-O3 — Should a manifest's layout be available as data, for generators such as shader code?

SPECOPS-O18 asks for a printed table of a manifest's layout. A generator needs
the same table as data: each fixed field's name, unit offset, bit offset,
width, signedness and enumeration, and which fields vary. Lexter's descriptor
is the first such generator in view. Is the table data first and printed from
it, and which of its fields does a shader generator need?

### SPECOPS-DRAFT-over-arrays-O4 — Should a read at an index report where the record ends?

`marshal`'s result lets a writer place the next record after the last. A
reader of a vector of records of varying size needs the same, but
`unmarshal` returns the record, or the keyed values as multiple values, and
has no place for a position. A parameter naming a place to store the end, or
an extra value under a parameter, are the candidates. Lexter's records are of
one size, so it does not need this.

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
2. **Records at an index** (SPECOPS-DRAFT-over-arrays-D1,
   SPECOPS-DRAFT-over-arrays-D2). `(:at index)` for `marshal` and `unmarshal`
   over vectors, with the positions of SPECOPS-DRAFT-over-arrays-D2, tested on Lexter's cells, on PNG
   and WOFF records placed at an index, and on records chained by `marshal`'s
   result. The Reference updated, released as 0.2.0.
3. **Paths** (SPECOPS-DRAFT-over-arrays-D5). Keys that name fields inside
   sub-manifests and `:case` branches, for selective reads, with the run-time
   check of the branch.
4. **Patches** (SPECOPS-DRAFT-over-arrays-D3,
   SPECOPS-DRAFT-over-arrays-D4). `(:patch t)`, with paths, masque groups
   combined into one write of their units, fixed positions found at
   expansion and others by reading, the refusals of
   SPECOPS-DRAFT-over-arrays-D4, and an answer to
   SPECOPS-DRAFT-over-arrays-O1. `marshal`'s internal variable `patching`,
   which marks a manifest with slots written through a buffer
   (`src/manifest/manifest.lisp:marshal@v0.1.0`), is renamed `backpatching`,
   so that the code does not use one word for two things. The Reference
   updated, its sentence on what cannot be patched in place saying
   "backpatched", and released as 0.3.0.
5. **Later additions**, as the prototype and users show the need: the fast
   path of SPECOPS-O15 and the layout as data of
   SPECOPS-DRAFT-over-arrays-O3.

Each step is recorded in an update to the
[Manifest Development Log, SPECOPS-0006](Log.Manifest.md) and a progress
update to this plan.
