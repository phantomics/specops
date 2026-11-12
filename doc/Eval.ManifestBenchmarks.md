---
id:            SPECOPS-DRAFT-manifest-benchmarks
title:         The Cost of Writing a Record with marshal Evaluation
genre:         Eval
subtype:       tradeoff
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
  - SPECOPS-0008
  - SPECOPS-0004
  - SPECOPS-0006
  - SPECOPS-0007
open-questions:
  - SPECOPS-DRAFT-manifest-benchmarks-O1
  - SPECOPS-DRAFT-manifest-benchmarks-O2
  - SPECOPS-DRAFT-manifest-benchmarks-O3
cites:
  - title:    binary-structures (Yukari Hafner)
    locator:  Shirakumo Quicklisp dist, commit d91c7ad — define-io-structure; the octet-vector backend
    external: true
  - title:    SBCL 2.3.4 User Manual
    locator:  "§ Statistical Profiler: sb-sprof, :cpu and :alloc modes"
    external: true
  - title:    The Cell Model Architecture (Lexter)
    locator:  Lexter repository, doc/Arch.CellModel.md at commit 76741bc — the record by word
    external: true
---

# The Cost of Writing a Record with marshal: Evaluation

Writing one of Lexter's 32-byte cells with `marshal` costs some 2,500 ns and
allocates some 1,300 bytes, about three hundred times what packing the same
bytes by hand costs (SPECOPS-0006, the update for Lexter's cell model). This
Eval finds where that cost goes, and measures what each of four remedies to
the generated code would gain, against packing by hand and against
binary-structures, the Lisp library whose generated code the prior-art
evaluation rated best (SPECOPS-0004). It informs the decision the
[plan for manifests over arrays](Plan.OverArrays.md) has deferred: what the
fast path of SPECOPS-O15 should be, before paths and patches are built. The
steward has set the first target at 80 ns per cell, with no allocation as the
ideal, and has asked for writes to be measured first.

The plan has since adopted this evaluation's recommendation as SPECOPS-D102,
as the fast path of its step 5, which the steward has put after paths and
patches; those were released in 0.1.4. The analysis below is as it was made,
before that decision.

## Method

- **The target.** `src/manifest/manifest.lisp:marshal@cba66c2`, release 0.1.3,
  with the serializers of `src/serial.lisp:serializer-for@cba66c2`.
- **The machine.** An AMD Ryzen 9 3900X under Linux, SBCL 2.3.4, at the
  default optimization settings.
- **The measurements.** `scripts/bench-manifests.lisp` writes each case a
  million times, in each of five runs, and reports the fastest run: the
  wall-clock time per record, and the bytes allocated per record, from
  `sb-ext:get-bytes-consed`. Every case is checked to write the same 32 bytes.
  Garbage collection took some 1.5% of the time of the slowest case, so the
  times are the work itself. A rerun at commit `7eb7ddf`, after release 0.1.4,
  gave times within some 10% of those below for every case above 100 ns, the
  case of constant values the furthest apart, at 2,828 ns against 2,588, and
  within 4 ns for the faster cases, T4 at 12 ns against 16; no conclusion
  depends on a difference of that size.
- **The profile.** SBCL's statistical profiler, in `:alloc` and `:cpu` modes,
  over the same loop, for where the allocation and the time go. Its CPU mode
  sampled fewer ticks than the run took, so only its proportions are used.
- **The expansion.** `marshal`'s expansion of the cell was read, and its
  closures, arrays, conses and calls counted.
- **The remedies.** Each remedy was estimated by transforming the expansion
  itself, T1 to T4, each including the ones before, and compiling the result:
  the code is what the layer generates, less what the remedy removes. The
  transformations hold only for this cell, a record of fixed size with no
  slots, written to a simple vector of octets; they are measurements, not code
  the layer generates. They match the shape of `marshal`'s expansion as it
  stands, so once the layer's code changes, as the fast path changes it, only
  the script's direct cases, `marshal`, packing by hand and binary-structures,
  can be measured again with it.
- **What was not examined:** reads; records with slots, whose backpatching the
  cell lacks; variable-length fields; destinations other than a simple vector
  of octets; Lisp implementations other than SBCL; and binary-structures beyond
  this one structure.

## The Shared Scenario

The record is Lexter's plain cell holding g underlined in the text's colour,
from the suite `manifest-arrays`: a constant format byte, three masques of the
glyph count and type bits, the drawing flags and the background slot, a
constant parameters byte, the swatch and the palette slot, and six glyph
words, each a `:case` on its type bit between a layer and a mask, of which two
are in use. It is written to a 32-byte simple vector of octets, with the
glyph, the ink slot and the palette slot known only when the code runs, as
when a terminal writes a character.

binary-structures requires all data to be octet-aligned, so it is measured on
the same cell as whole bytes and 32-bit words, its bit fields packed by the
caller, and so is an equivalent manifest, `cell-bytes`, for a like
comparison. binary-structures writes from a structure, which the loop reuses,
setting the two fields that change.

## The Rubric

- **R1, time:** the time to write one cell, against the target of 80 ns, at
  which a screen of 200 by 60 cells is written in about a millisecond.
- **R2, allocation:** the bytes allocated per cell; none is the ideal.
- **R3, reach:** which records and destinations the remedy serves: every
  record, or only those of fixed size, or only simple vectors of the unit type.
- **R4, safety:** whether the checks of a full write are kept: a value too wide
  for its field, a masque group's overflow, and an overrun of the vector.

## Where the cost goes

The expansion of one write of the cell holds, besides the fields' own code:

- **For each call,** a fresh array of serializers, filled by calling
  `serializer-for`, which returns a fresh closure; and two closures, one that
  enters a unit at the position and one that writes at a given position,
  which both capture and change the position, so SBCL keeps it in a cell of its
  own on the heap.
- **For each field,** a fresh pair `(width . value)` for the serializer, even
  for a constant; and, for each named field, a list of its name and span pushed
  onto a map, closed by a search of the map, although the cell has no slot to
  read the map.
- **For each unit,** a call of the entering closure, which checks the position
  against the vector's size.

The serializer is generic: for each field it dispatches on the value's type,
finds the value's width, swaps its bytes for the byte order through
`swap-segments`, takes the pair apart, and decomposes the value unit by unit
with generic arithmetic. In the profile, the serializer's closure takes some
56% of the time, `ash` and `swap-segments` alone 36%, and the entering closure
13%. Of the allocation, 89% is conses, the pairs and the span map, and the rest
the closures and the array.

## The renditions

Table: The cost of writing the cell, the fastest of five runs of a million
records each. Each of T1 to T4 includes the remedies before it.

| Case | Time per cell | Bytes allocated per cell |
|---|---|---|
| `marshal`, as released in 0.1.3 | 2,560 ns | 1,343 |
| `marshal`, every value a constant | 2,588 ns | 1,343 |
| T1: the serializers made once, at load time | 2,564 ns | 1,264 |
| T2: and no span map | 2,344 ns | 368 |
| T3: and each field written by code of its own width and byte order | 360 ns | 64 |
| T4 without the declarations of T4 | 132 ns | 0 |
| T4: and one bounds check for the record, and direct stores into a declared vector | 16 ns | 0 |
| The same bytes packed by hand | 8 ns | 0 |
| `marshal`, the cell as bytes and words | 2,168 ns | 1,184 |
| binary-structures, the cell as bytes and words | 36 ns | 0 |

### `marshal` as released

- **R1:** 2,560 ns, thirty times the target.
- **R2:** 1,343 bytes, which a screen written in full turns into some 16 MB.
- **R3, R4:** every record and destination, with every check.

Values that are constants at expansion gain nothing, since the serializer is
generic when the code runs whatever the values.

### T1: the serializers made once

The array of serializers and its closures made once, at load time, as the
expansion of `defmanifest` could intern them, rather than on each call.

- **R1:** no measurable gain. **R2:** 79 bytes less.
- **R3, R4:** every record; no check lost.

The array is a cost, but a small one: its closures are reused for every field,
and the time goes into what they do, not into making them.

### T2: no span map

The map of each field's span, which only slots read, left out of a manifest
with no slots.

- **R1:** some 220 ns less. **R2:** 896 bytes less, two thirds of the total.
- **R3:** every record without slots; a record with slots keeps it, though it
  could keep only the spans its slots cover. **R4:** no check lost.

### T3: each field written by code of its own

Each field written by code made for its width and byte order at expansion,
which are known there, with its value checked to fit, instead of by the
generic serializer from a fresh pair.

- **R1:** some 1,980 ns less, to 360 ns: the largest single gain, but still
  above the target. **R2:** 304 bytes less; the 64 that remain are the
  closures and the position's cell.
- **R3:** every record and every destination: the units still go through the
  entering closure, so a stream or a function is written as before. Its gain
  for those destinations was not measured, and is to be measured in the plan's
  step 5.
- **R4:** the range check of each field kept, in the transformation as a type
  and bounds test on the value.

### T4: one bounds check, and direct stores

The record's extent, fixed at expansion, checked against the vector once,
and each unit stored into the vector directly, with the vector declared a
simple array of octets and the position a fixnum.

- **R1:** 16 ns, twice the time of packing by hand, and five times under the
  target. Without the declarations, 132 ns: the declarations, which
  let SBCL store an octet without a generic call, are most of what T4 gains.
- **R2:** nothing allocated, the ideal.
- **R3:** only a record whose size is fixed at expansion, written to a simple
  vector of its unit type. A manifest would choose this code when the code
  runs, by testing the destination's type, and fall back to T3's code for
  anything else.
- **R4:** an overrun is still signalled, by the one check, with the vector's
  size as the position; the range checks of T3 are kept.

### Packing by hand

- **R1:** 8 ns. **R2:** nothing. **R3:** one record, one destination type.
- **R4:** no range checks: a value too wide is silently masked or corrupts its
  neighbour, which is what a manifest exists to prevent.

### binary-structures

- **R1:** 36 ns on the byte-level cell, which an equivalent manifest writes in
  2,168 ns. **R2:** nothing allocated, writing from a structure that is reused.
- **R3:** octet-aligned data only, so Lexter's masques and bit fields are the
  caller's to pack; the cell is written from a structure, which a writer must
  fill first.
- **R4:** an overrun is checked once for a run of fixed fields, as T4 does;
  values are typed by the structure's slot types.

It confirms the direction SPECOPS-O15 drew from it: typed stores, with offsets
tracked at expansion and the bounds checked once for a fixed run. T4 does that
and also gains from computing the cell's masques at expansion, where
binary-structures leaves them to the caller.

## Synthesis

The cost of a call is the generic serializer and the closures it writes
through, not the work a cell needs. Three remedies together reach the target
with no allocation, and they divide by reach:

1. **Each field written by its own code (T3)** serves every record and every
   destination, and removes three quarters of the cost. It needs `marshal` to
   emit, for each field, code of the field's width and byte order, with its
   range check, where it now emits a call of the generic serializer with a
   pair.
2. **No span map without slots (T2)** serves every record without slots, and
   removes most of the allocation that T3 leaves. A record with slots could map
   only the spans its slots cover.
3. **One bounds check and direct stores, for a record of fixed size written to
   a simple vector of its unit type (T4),** reaches the target, at 16 ns with
   nothing allocated, and is the case Lexter's cells, and the patches the plan
   builds next, are made of. It needs a branch of the expansion chosen by the
   destination's type when the code runs, with the declarations, beside the
   general code for other destinations.

Making the serializers once (T1), the steward's first suspect, is sound and
simple, and belongs in the expansion of `defmanifest` if the serializers remain;
but it gains little on its own, and with T3 the fields would no longer call
them.

The recommendation, for the plan's decision, is a fast path of T4 for records
of fixed size written to simple vectors of their unit type, built on T3 and T2
for every record, so that the general path is fast as well and the fast path
differs from it only in how a unit is stored.

## Open Questions

### SPECOPS-DRAFT-manifest-benchmarks-O1 — What do reads cost, and do the same remedies serve them?

Reading one flag of a cell costs some 210 ns and allocates 144 bytes
(SPECOPS-0006). `unmarshal` makes a vector of deserializers on each call and
reads through them, as `marshal` writes through its serializers. The same
measurements, made for reads, would show whether fields read by their own
code and a fixed record's one bounds check give reads what they give writes;
patches, which read a masque's units before writing them, depend on it. A
patch now reads in both ways: a masque's units through `deserializer-for`,
and a selector it is not given through a selective `unmarshal`
(`src/manifest/manifest.lisp:marshal@v0.1.4`).

### SPECOPS-DRAFT-manifest-benchmarks-O2 — How much of T4 serves records with slots and variable-length fields?

The cell has neither. A record with slots is backpatched, through a buffer
when the destination is not a simple vector of its unit type, and a field of
variable length has no extent fixed at expansion. T3 serves both; how far one
bounds check can reach, such as over the fixed fields before the first
variable one, is to be measured on records such as PNG's.

### SPECOPS-DRAFT-manifest-benchmarks-O3 — Do the remedies keep every error a full write signals?

T3's range check stands in for the serializer's, with a message of its own,
and T4's bounds check reports the vector's size, where a write unit by unit
reports the unit that would not fit. Whether the fast path must report as the
general path does, word for word, is the steward's to decide; the suites'
tests of errors would show any difference. SPECOPS-D106 has since settled the
like question for patches: a patch checks its whole record before it writes,
and reports the record's end as the position of an overrun, a precedent for
T4's one check.
