---
id:            SPECOPS-0007
title:         Manifest Reference — defmanifest, marshal and unmarshal
genre:         Ref
scope:         component
project:       SpecOps
component:     Manifest
language:      en
status:        Current
authors:
  - Andrew Sengul
approved-by:   Andrew Sengul
reviewed:      2026-11-01
api-version:   0.1.3
provenance:
  assistant:   claude-opus (OpenCode)
  session:     Manifest Reference
relates-to:
  - SPECOPS-0006
  - SPECOPS-0003
  - SPECOPS-0001
glossary:      SPECOPS-0002
---

# Manifest Reference

This reference describes SpecOps's manifest layer as released in version
0.1.3, tagged `v0.1.3`: `defmanifest`, which declares the layout of a binary
record; `marshal`, which writes a record's fields to a vector, a stream or a
function; `unmarshal`, which reads them back from a vector, an integer or a
stream; the record classes and conditions these define and signal; the stop
rule protocol through which a user extends variable-length fields; and the
serialization primitives the layer is built on, which a user may call
directly. All of these are exported from the package `specops`, in the system
`specops`. The design and its history are in the
[Manifest Development Log, SPECOPS-0006](Log.Manifest.md); this document gives
the interface only.

Every example below is a test of the suite `manifest-ref`, in
`test/test-manifest-ref.lisp`, named in the line that introduces it, so the
examples are run with the rest of the suite and cannot drift from the code
(SPECOPS-D93).

## Conventions

- **Units.** A manifest is written and read in *units* of 8, 16, 32 or 64
  bits, set by its `:unit` parameter. Widths, positions, lengths, counts and
  offsets all count units. A *position* is the index of a unit from the start
  of the destination or source.
- **Field names** are keywords. A plist of field names and values is the
  basic form of a record: `marshal` takes one and `unmarshal` returns one.
- **Expansion time.** `marshal` and `unmarshal` are macros that read the
  manifest's definition when they expand and inline its code, so a manifest
  must be defined before code that writes or reads it is compiled. Errors in
  how a manifest is used are signalled at expansion where the information is
  there, and called *expansion errors* below.
- **Examples** are evaluated in a package that uses `specops`, with this
  helper, which builds the source vectors of 8-bit manifests
  (`test/test-manifest-ref.lisp:octets@v0.1.3`):

  ```lisp
  (defun octets (&rest units)
    (make-array (length units) :element-type '(unsigned-byte 8) :initial-contents units))
  ```

  A result is shown after `;; =>`, as the reader would accept it, in
  hexadecimal where that is clearer, and `*` stands for the previous result.

## Declaring manifests

### `defmanifest`

```lisp
(defmanifest name (&key unit endian length pad record) entry...)
```

Defines the manifest NAME: registers its entries, at compile time as well as
load time, where `marshal` and `unmarshal` find them, and, given `:record`,
defines its record class (see [Records](#records)). Each ENTRY is a field,
whose first element is a keyword; a directive, whose first element is a
symbol: `str`, `vec`, `pad`, `masque`, `span`, `manifest` or `place`
(SPECOPS-D31); or a constant number, bare or as `(width . value)` (see
[Constants, masques and pads](#constants-masques-and-pads)). Directives are recognized by their symbol's name, in any
package (SPECOPS-D48). Defined in
`src/manifest/manifest.lisp:defmanifest@v0.1.3`.

Table: The parameters of `defmanifest`.

| Parameter | Value | Meaning |
|---|---|---|
| `:unit` | `8`, `16`, `32` or `64`; or `0` to `3`; or `:byte`, `:word`, `:doubleword` or `:quadword` | The width of a unit, in bits or as a power of two above 8. Required: keyword types and masques need it. |
| `:endian` | `:big`, `:little`, or `:middle` (also `:pdp`) | The byte order of every field that gives none of its own. Default `:big` (SPECOPS-D56). |
| `:length` | An integer | Pads the record with the `:pad` value to this many units from its start; a record already longer is an error when the code runs. |
| `:pad` | An integer | The fill unit of `:length`. Default 0. |
| `:record` | `t` or a symbol | Defines a record class, named after the manifest for `t` (SPECOPS-D59). |

`:index`, `:vextend-by` and `:count-from` are accepted and ignored.

Example (`test/test-manifest-ref.lisp:ref-shape-written-and-read@v0.1.3`):

```lisp
(defenum shape-kind nil :circle 1 :square 2 :triangle 3)

(defmanifest shape (:unit 8 :endian :big :record t)
  (:kind (:u 1) :default :circle :enumerate-by shape-kind)
  (:x    (:s 2) :default 0)
  (:y    (:s 2) :default 0 :endian :little)
  (:size :u16   :default 1))

(let ((buf (make-array 7 :element-type '(unsigned-byte 8))))
  (list (marshal shape buf :kind :square :x -2 :y 300 :size 10) buf))
;; => (7 #(2 255 254 44 1 0 10))

(unmarshal shape (octets 2 255 254 44 1 0 10))
;; => (:KIND :SQUARE :X -2 :Y 300 :SIZE 10)
```

### Fields

```lisp
(name type &key default endian enumerate-by count end-by stop-by codec slot place)
```

NAME is a keyword, or `_` for an unnamed field, which is written with its
default and passed over on read. TYPE gives the field's class, width and
modifiers.

Table: Field types. N is a width in units.

| Type | Meaning |
|---|---|
| `(:u n)` | An unsigned integer of N units |
| `(:s n)` | A signed integer of N units, in two's complement |
| `(:f n)` | An IEEE binary float of N units: 16, 32, 64 or 128 bits (SPECOPS-D91) |
| `(:u n :vec)`, `(:s n :vec)`, `(:f n :vec)` | A vector of such elements |
| `(:u n :leb)`, `(:s n :leb)` | An unsigned or signed LEB128 integer, read in elements of N units |
| `(:u n :vec :leb)` | A vector of LEB128 integers (SPECOPS-D53) |
| `(:case selector branch...)` | A field whose type an earlier field chooses (see [`:case`](#case)) |
| `:u8`, `:s8`, `:u16`, `:s16`, `:u32`, `:s32`, `:u64`, `:s64` | Integers named by their bits, in 8-bit manifests only |
| `:f16`, `:f32`, `:f64`, `:f128` | Floats named by their bits, in 8-bit manifests only |

A keyword that names no type, or `:u` or `:s` with no width, is an expansion
error in a field; `:u` and `:s` alone are the types of
[masque groups](#constants-masques-and-pads). A width counts units, so
in a 16-bit manifest `(:u 1)` is one 16-bit unit and
`(:u 2)` two (SPECOPS-D32; `test/test-manifest-ref.lisp:ref-word-sample-widths-count-units@v0.1.3`):

```lisp
(defmanifest word-sample (:unit 16 :endian :big)
  (:a (:u 1) :default 0)
  (:b (:u 2) :default 0))

(let ((buf (make-array 3 :element-type '(unsigned-byte 16))))
  (marshal word-sample buf :a #x1234 :b #x56789ABC)
  buf)
;; => #(#x1234 #x5678 #x9ABC)
```

Table: Field options.

| Option | Value | Meaning |
|---|---|---|
| `:default` | A constant | The value written when none is given. A keyword goes through the field's enumeration. A field with no default and no value is an error when the code runs. |
| `:endian` | As for `defmanifest` | The field's own byte order |
| `:enumerate-by` | The name of a `defenum` | Values are written as their codes and read as their keywords (SPECOPS-D50) |
| `:count` | An integer, or an earlier field's name | The number of elements of a vector, or the size of a fixed buffer with `:end-by` |
| `:end-by` | An integer, a character, a string or a vector of codes | The terminator of a vector: one element or several (see [Vectors and terminators](#vectors-and-terminators)) |
| `:stop-by` | A stop rule's class name, or `(class initarg...)` | A user's rule for where a vector ends (see [Stop rules](#stop-rules)) |
| `:codec` | A function, such as `#'specops.map.ebcdic:ebcdic-code-cp037` | Maps a vector's characters to codes and back; such a field reads as a string (SPECOPS-D43) |
| `:slot` | A slot form | Makes the field a length, count, offset or checksum of another (see [Slots](#slots)) |
| `:place` | A tag | Writes the field where `(place tag)` stands (see [Placed fields](#placed-fields)) |

A value too wide for an integer field is an error when the code runs, and a
float too large for its format is an error (SPECOPS-D91).

### Byte orders

`:big` writes the most significant byte first, `:little` the least. `:middle`,
or `:pdp`, writes a 32-bit field's 16-bit halves least significant first, each
half big-endian: `#x01020304` is written `03 04 01 02`. A constant string is
written in stream order, whatever the byte order (SPECOPS-D56).

In manifests of units wider than 8 bits, only `:big` is dependable in 0.1.3:
`:little` and `:middle` reorder the field's bytes, not its units (see
[Limits in 0.1.3](#limits-in-013)).

### `defenum`

```lisp
(defenum name params key code key code...)
```

Defines the enumeration NAME, pairing keywords with integer codes, for
`:enumerate-by` and for `:case` keys. PARAMS is reserved; give `nil`. A field
enumerated by NAME writes a keyword as its code, and an integer as itself; on
read, a code the enumeration lacks reads as the integer, so it can be written
back unchanged (SPECOPS-D57;
`test/test-manifest-ref.lisp:ref-shape-unknown-code@v0.1.3`):

```lisp
(unmarshal shape (octets 9 0 0 0 0 0 1) nil :kind)
;; => 9
```

A keyword default goes through the enumeration
(`test/test-manifest-ref.lisp:ref-shape-defaults@v0.1.3`):

```lisp
(let ((buf (make-array 7 :element-type '(unsigned-byte 8))))
  (marshal shape buf :x 5)
  buf)
;; => #(1 0 5 0 0 0 1)
```

### Vectors and terminators

A `:vec` field is a vector of elements, ended in one of these ways
(SPECOPS-D45, SPECOPS-D49):

- **A count.** `:count 4`, or `:count :len` for an earlier field `:len`.
- **A terminator.** `:end-by 0` ends the vector at the element 0, which is
  written after the data and dropped on read. A terminator of several
  elements, such as `:end-by #(13 10)` or `:end-by "--"`, ends it at that
  sequence. A character terminator goes through the field's codec, an integer
  one is a raw code (SPECOPS-D44). Data that contains the terminator is an
  error on write.
- **A fixed buffer.** `:count` with `:end-by` is a buffer of that many
  elements: the data, then terminators to the end. Data that fills the buffer
  has none. A buffer that cannot hold a whole terminator after the data is an
  error (SPECOPS-D54). On read, what follows the terminator is skipped.
- **A length slot.** A vector with neither, covered by a `:length-of` slot,
  has the length the slot gives, on read.
- **A stop rule.** `:stop-by` (see [Stop rules](#stop-rules)).

A vector's value may be a vector or a list of elements, or a string, whose
characters are written as their codes, through the codec when one is given.
Elements are read back as a vector of codes, or as a string under a codec.
A LEB128 field writes the fewest bytes its value needs; a vector of LEB128
integers takes `:count`, `:end-by` or a length slot as any vector does.

Example (`test/test-manifest-ref.lisp:ref-entry-name-written@v0.1.3` and
`test/test-manifest-ref.lisp:ref-entry-name-read@v0.1.3`):

```lisp
(defmanifest entry-name (:unit 8 :endian :big)
  (:len   :u8 :slot (:count-of :name))
  (:name  (:u 1 :vec) :count :len)
  (:path  (:u 1 :vec) :end-by 0)
  (:tag   (:u 1 :vec) :count 4 :end-by 0)
  (:size  (:u 1 :leb) :default 0)
  (:title (:u 1 :vec) :count 3 :default "   " :codec #'specops.map.ebcdic:ebcdic-code-cp037)
  (:ratio :f32 :default 0))

(let ((buf (make-array 19 :element-type '(unsigned-byte 8))))
  (marshal entry-name buf :name "ab" :path "/x" :tag "id" :size 300 :title "HI " :ratio 0.5)
  buf)
;; => #(2 97 98  47 120 0  105 100 0 0  #xAC #x02  #xC8 #xC9 #x40  #x3F 0 0 0)

(unmarshal entry-name *)
;; => (:LEN 2 :NAME #(97 98) :PATH #(47 120) :TAG #(105 100) :SIZE 300
;;     :TITLE "HI " :RATIO 0.5)
```

### `:case`

```lisp
(:case selector (keys . type) ... (otherwise . type))
```

A field whose type the value of an earlier field, SELECTOR, chooses
(SPECOPS-D46, SPECOPS-D47). KEYS is an integer, a list of integers, or
keywords of the selector's enumeration. The branch type is written as a field
type without its parentheses, `(0 :u8)` or `(1 :s 4)`, or as a sub-manifest,
`(key (manifest name))` (SPECOPS-D64). A selector known at expansion chooses
the branch then; otherwise the branch is chosen when the code runs, and an
unmatched selector with no `otherwise` is an error. A masque's keyword group
may be a selector (SPECOPS-D90). A key the selector's enumeration lacks, or a
selector after the field it selects for, is an expansion error.

Example (`test/test-manifest-ref.lisp:ref-value-cell-branches@v0.1.3`):

```lisp
(defmanifest value-cell (:unit 8 :endian :big)
  (:kind  :u8 :default 0)
  (:value (:case :kind (0 :u8) (1 :s 4) (2 :f 8) (otherwise :u 1 :leb)) :default 0))

(let ((buf (make-array 5 :element-type '(unsigned-byte 8))))
  (marshal value-cell buf :kind 1 :value -5)
  buf)
;; => #(1 #xFF #xFF #xFF #xFB)

(let ((buf (make-array 3 :element-type '(unsigned-byte 8)))
      (kind 9))
  (marshal value-cell buf :kind kind :value 300)
  buf)
;; => #(9 #xAC #x02)

(unmarshal value-cell (octets 2 #x3F #xF8 0 0 0 0 0 0))
;; => (:KIND 2 :VALUE 1.5d0)
```

### Constants, masques and pads

- **A bare number,** a non-negative integer, is a constant written in as few
  units as it holds, in the manifest's byte order: `0` is one zero unit, and
  `#x010203` three units in an 8-bit manifest or two in a 16-bit one.
  **`(width . value)`** is a constant of exactly WIDTH units, as for a value
  whose leading units are zero, which the reader does not keep: `(4 . 1)` is
  `00 00 00 01` in a big-endian manifest and `01 00 00 00` in a little-endian
  one. This is the pair `serialize` takes.
- **`(vec unit...)`** writes its units, integers that each fit one unit, in
  the order given, whatever the byte order, as a magic number in a format's
  tables is written.
- Constants are checked on read: a unit that differs is a failed check of kind
  `:constant`, at the unit where the constant begins, whose report names the
  constant and the manifest. They hold no field. A negative number, a value
  wider than its width, and a `vec` element that does not fit one unit are
  expansion errors.
- **`(str text &key codec end-by)`** writes the constant TEXT, through
  CODEC when given, then END-BY when given. On read, the units are compared
  with the constant, and a difference is a failed check of kind `:constant`.
- **`(masque mask binding...)`** packs several groups into one integer laid
  out by MASK, as [`masque`](#masque-and-unmasque) does. A binding
  `(letter value)` is a static group, written as VALUE and checked on read; a
  binding `(letter name type option...)` is a field, named NAME, whose
  options are a field's. A group's width and position are those of its
  letters in MASK, so its type gives only whether it is signed: write `:u`
  or `:s`. A field type such as `:u8` is accepted, in 8-bit manifests only,
  but its width is ignored. The masque takes the units its largest value
  needs.
  A value too wide for its group is an error when the code runs
  (SPECOPS-D89).
- **`(pad fill n)`** writes N units of FILL. **`(pad fill :to n)`** pads to N
  units from the start of the record, and **`(pad fill :align n)`** to a
  multiple of N units from the start of the destination (SPECOPS-D42). On
  read, pads are passed over; written back, they hold FILL (SPECOPS-D62).

Example (`test/test-manifest-ref.lisp:ref-constants@v0.1.3`), the start of
a WebAssembly module, its magic number in stream order and its version, 1, in
four units:

```lisp
(defmanifest wasm-preamble (:unit 8 :endian :little)
  (vec 0 #x61 #x73 #x6D)
  (4 . 1))

(let ((buf (make-array 8 :element-type '(unsigned-byte 8))))
  (marshal wasm-preamble buf)
  buf)
;; => #(0 #x61 #x73 #x6D 1 0 0 0)

(handler-case (unmarshal wasm-preamble (octets 0 #x61 #x73 #x6D 2 0 0 0))
  (manifest-check-failed (c) (check-kind c)))
;; => :CONSTANT
```

Example (`test/test-manifest-ref.lisp:ref-frame-header-layout@v0.1.3` and
`test/test-manifest-ref.lisp:ref-frame-header-constant-checked@v0.1.3`):

```lisp
(defmanifest frame-header (:unit 8 :endian :big :length 12)
  (str "FR")
  (masque "h:vvff" (v #x01) (f :flags :u :default 0))
  (:count :u8 :default 0)
  (pad 0 :align 4)
  (:id :u8 :default 0))

(let ((buf (make-array 12 :element-type '(unsigned-byte 8) :initial-element #xEE)))
  (marshal frame-header buf :flags 3 :count 2 :id 9)
  buf)
;; => #(#x46 #x52 #x01 3 2 0 0 0 9 0 0 0)

(handler-case (unmarshal frame-header (octets #x46 #x51 1 3 2 0 0 0 9 0 0 0))
  (manifest-check-failed (c) (list (check-kind c) (check-position c))))
;; => (:CONSTANT 1)
```

### Spans

```lisp
(span name entry...)
```

Groups ENTRIES into a region named NAME, a keyword, which a slot may cover.
A span adds no level to the record: its fields are read and written as the
manifest's own (SPECOPS-D35, SPECOPS-D38).

### Slots

A field with `:slot` holds a fact about another field or span, its *region*,
which `marshal` computes and writes once the region is written, and
`unmarshal` checks (SPECOPS-D35, SPECOPS-D36, SPECOPS-D69). A slot field is
never taken from the values given; it is recomputed on every write
(SPECOPS-D60).

Table: Slot forms.

| Slot | Holds | On read |
|---|---|---|
| `(:length-of region)` | The region's length in units | Gives the length of a vector, or of a repeat `:until :length`, inside the region; checked against the units read |
| `(:count-of field)` | The number of elements of a vector or instances of a repeat (SPECOPS-D68) | Gives the count of a repeat `:count` this field; checked otherwise |
| `(:offset-of field)` | The position at which FIELD begins, counted from the record's start | Gives where a placed field is read (SPECOPS-D84); checked otherwise |
| `(:checksum-of region :by function)` | `(funcall function buffer start end)` over the region's units | Checked |

A checksum function takes a vector of units, a start and an end, as `crc`,
`adler32` and `sfnt-checksum` do (SPECOPS-D72). A slot's region must be in the
same manifest, or in an instance it repeats (SPECOPS-D66); a region the
manifest lacks is an expansion error.

Example (`test/test-manifest-ref.lisp:ref-chunk-slots-written@v0.1.3` and
`test/test-manifest-ref.lisp:ref-chunk-read@v0.1.3`):

```lisp
(defmanifest chunk (:unit 8 :endian :big :record t)
  (:length (:u 4) :slot (:length-of :data))
  (span :body
    (str "DATA")
    (:data (:u 1 :vec)))
  (:crc (:u 4) :slot (:checksum-of :body :by #'crc)))

(let ((buf (make-array 15 :element-type '(unsigned-byte 8))))
  (marshal chunk buf :data #(1 2 3))
  buf)
;; => #(0 0 0 3  #x44 #x41 #x54 #x41  1 2 3  #xAD #xEA #x58 #x81)

(unmarshal chunk *)
;; => (:LENGTH 3 :DATA #(1 2 3) :CRC 2917816449)
```

### Sub-manifests and repeats

```lisp
(manifest name child &key count until place)
```

A field NAME holding an instance of the manifest CHILD, whose code is
inlined into the parent's (SPECOPS-D37, SPECOPS-D63). With `:count` or
`:until` it is a *repeat*, holding a list of instances (SPECOPS-D65):

- `:count n` or `:count :field`: N instances, or as many as an earlier field
  gives.
- `:until :length`: instances until the units a `:length-of` slot over the
  repeat gives are used.
- `:until (field code ...)`: instances until one whose fields have these
  codes, which is kept. A keyword code goes through the field's enumeration.
- `:until :end`: instances to the end of the source.

`:place tag` places the instance (see [Placed fields](#placed-fields)).

Example (`test/test-manifest-ref.lisp:ref-polygon-counted@v0.1.3` and
`test/test-manifest-ref.lisp:ref-repeats-until@v0.1.3`):

```lisp
(defmanifest point (:unit 8 :endian :big :record t)
  (:x :s8 :default 0)
  (:y :s8 :default 0))

(defmanifest polygon (:unit 8 :endian :big :record t)
  (:n :u8 :slot (:count-of :points))
  (manifest :points point :count :n))

(defmanifest path (:unit 8 :endian :big)
  (:size   :u8 :slot (:length-of :points))
  (manifest :points point :until :length)
  (:closed :u8 :default 0))

(defmanifest track (:unit 8 :endian :big)
  (manifest :points point :until (:x 0)))

(let ((buf (make-array 5 :element-type '(unsigned-byte 8))))
  (marshal polygon buf :points (list (list :x 1 :y 2) (make-point :x -1 :y 3)))
  buf)
;; => #(2  1 2  #xFF 3)

(unmarshal polygon *)
;; => (:N 2 :POINTS ((:X 1 :Y 2) (:X -1 :Y 3)))

(unmarshal path (octets 4  1 2  3 4  1))
;; => (:SIZE 4 :POINTS ((:X 1 :Y 2) (:X 3 :Y 4)) :CLOSED 1)

(unmarshal track (octets 1 2  3 4  0 0))
;; => (:POINTS ((:X 1 :Y 2) (:X 3 :Y 4) (:X 0 :Y 0)))
```

### Placed fields

```lisp
(place tag &key align)
```

A field or sub-manifest given `:place tag` is not written where it stands,
but where a `place` directive for TAG stands, in the same manifest or one
enclosing it (SPECOPS-D83). The directive writes the fields queued under its
tag in the order they were queued, padding before each, and after the last,
to a multiple of ALIGN units (SPECOPS-D86). A placed field needs an
`:offset-of` slot, which comes before it; `:length-of` and `:checksum-of`
slots may cover it too. On read, a placed field is read at the offset its slot
gives (SPECOPS-D84). A tag no directive declares, a slot after its placed
field, and a placed field inside a span are expansion errors.

Example (`test/test-manifest-ref.lisp:ref-archive-placed@v0.1.3`):

```lisp
(defmanifest archive-entry (:unit 8 :endian :big :record t)
  (:offset :u8 :slot (:offset-of :data))
  (:size   :u8 :slot (:length-of :data))
  (:data   (:u 1 :vec) :place :blobs))

(defmanifest archive (:unit 8 :endian :big :record t)
  (:n :u8 :slot (:count-of :entries))
  (manifest :entries archive-entry :count :n)
  (place :blobs :align 4))

(let ((buf (make-array 16 :element-type '(unsigned-byte 8))))
  (marshal archive buf :entries (list (list :data #(1 2 3)) (list :data #(4 5))))
  buf)
;; => #(2  8 3  12 2  0 0 0  1 2 3 0  4 5 0 0)

(unmarshal archive *)
;; => (:N 2 :ENTRIES ((:OFFSET 8 :SIZE 3 :DATA #(1 2 3))
;;                    (:OFFSET 12 :SIZE 2 :DATA #(4 5))))
```

## Writing: `marshal`

```lisp
(marshal name destination [(option...)] field value ...)
```

Writes the manifest NAME to DESTINATION and returns the position after the
last unit written, counted from the start of the destination (SPECOPS-D28,
SPECOPS-D60). Defined in
`src/manifest/manifest.lisp:marshal@v0.1.3`.

- **Destination.** A vector, written from position 0, or from the index
  `(:at index)` gives; a binary output
  stream; or a function of one argument, called with each unit in turn. A
  vector too small signals `manifest-overrun`; its fill pointer is neither
  read nor set (SPECOPS-D75). A manifest with slots is written through a
  buffer to a stream or a function, which cannot be patched in place
  (SPECOPS-D73).
- **Values.** Each FIELD, a keyword, is followed by a form giving its value,
  evaluated when the code runs. A sub-manifest's value is a literal list of
  its own field pairs, `:origin (:x 1 :y 2)`, expanded inline; a repeat's is
  a form giving a list of plists or records. A field given no value is taken
  from the `:from` record when there is one, else takes its default.
- **Options.**
  - `(:from record)`: RECORD, a plist or a manifest's record, evaluated when
    the code runs, supplies every field not given, a sub-manifest's included,
    at any depth.
  - `(:buffer b)`: B, a `manifest-buffer`, keeps the buffer through which a
    manifest with slots is written, for later calls to reuse (SPECOPS-D74).
  - `(:at index)`: the record begins at INDEX of a vector destination, a form
    evaluated when the code runs; see [Records at an index](#records-at-an-index).

  An unknown option is an expansion error. Options are one list:
  `(:at i :from record)`.

Examples (`test/test-manifest-ref.lisp:ref-marshal-to-a-function@v0.1.3`,
`test/test-manifest-ref.lisp:ref-marshal-from-a-record@v0.1.3`,
`test/test-manifest-ref.lisp:ref-marshal-to-and-from-a-stream@v0.1.3` and
`test/test-manifest-ref.lisp:ref-marshal-buffer-reused@v0.1.3`):

```lisp
(let ((units nil))
  (marshal shape (lambda (unit) (push unit units)) :kind :square :x 1)
  (reverse units))
;; => (2 0 1 0 0 0 1)

(let ((buf (make-array 7 :element-type '(unsigned-byte 8))))
  (marshal shape buf (:from (list :kind :square :y 4)) :x 9)
  buf)
;; => #(2 0 9 4 0 0 1)

(with-open-file (out file :direction :output :if-exists :supersede
                          :element-type '(unsigned-byte 8))
  (marshal chunk out :data #(1 2 3))
  (marshal chunk out :data #(4)))

(let ((b (make-manifest-buffer))
      (units nil))
  (dolist (data '(#(1) #(2 3)))
    (marshal chunk (lambda (u) (push u units)) (:buffer b) :data data))
  (length units))
;; => 27
```

### `make-manifest-buffer`

```lisp
(make-manifest-buffer) => manifest-buffer
```

Makes an empty `manifest-buffer`, a holder for the array through which
`marshal` writes a manifest with slots to a stream or a function, and in which
`unmarshal` keeps the units it reads from a stream (SPECOPS-D79). The array
grows as needed and is kept between calls. Defined with the structure
`src/manifest/manifest.lisp:manifest-buffer@v0.1.3`.

## Reading: `unmarshal`

```lisp
(unmarshal name source [params] key...)
```

Reads the manifest NAME from SOURCE (SPECOPS-D40, SPECOPS-D61). Defined in
`src/manifest/manifest.lisp:unmarshal@v0.1.3`.

- **Source.** A vector whose element type is the manifest's unit, such as
  `(unsigned-byte 8)`, read from position 0, or from the index `(:at index)`
  gives; an integer, whose first unit is
  its most significant; a pair `(units . integer)`, which gives the
  integer's length in units, for leading zero units; or a binary input
  stream, which is read to the end of the record and no further, so records
  can be read one after another (SPECOPS-D77, SPECOPS-D78).
- **Result.** With no keys, a plist of every named field, in manifest order,
  with a sub-manifest's fields as a nested plist and a repeat's as a list of
  plists; or, under `(:as :record)`, the manifest's record. With keys, their
  values, as multiple values in the order given.
- **Params**, a list of options, or `nil` before keys:
  - `(:as :record)` returns a record; the manifest must have a record class,
    and no keys are allowed. `(:as :plist)` is the default.
  - `(:check nil)` leaves the checks out.
  - `(:buffer b)` keeps a stream's units in B, a `manifest-buffer`.
  - `(:at index)` reads the record at INDEX of a vector source; see
    [Records at an index](#records-at-an-index).

  An unknown option is an expansion error.

Example (`test/test-manifest-ref.lisp:ref-unmarshal-sources@v0.1.3`):

```lisp
(unmarshal shape #x02FFFE2C01000A)
;; => (:KIND :SQUARE :X -2 :Y 300 :SIZE 10)

(unmarshal shape '(7 . #x02FFFE2C01000A))
;; => (:KIND :SQUARE :X -2 :Y 300 :SIZE 10)

(with-open-file (in file :element-type '(unsigned-byte 8))
  (list (unmarshal chunk in nil :data) (unmarshal chunk in nil :data)))
;; => (#(1 2 3) #(4))
```

### Records at an index

`(:at index)`, among `marshal`'s options or `unmarshal`'s params, places the
record at INDEX of a vector, so that records lying side by side in one array,
such as the cells of a screen, are written and read where they lie
(SPECOPS-DRAFT-over-arrays-D1). It applies to vectors only: with a stream, a
function or an integer source it is an error when the code runs, and an
expansion error where the form shows it, as a `lambda` destination or a
literal integer source does. INDEX must be a non-negative integer.

Positions inside the record count from its start, and positions reported
outside it from the vector's (SPECOPS-DRAFT-over-arrays-D2):

Table: What each position counts from, with `(:at index)`.

| Position | Counts from |
|---|---|
| A `:to` pad's target | The record's start |
| An `:align` pad's boundary | The vector's start |
| The value of an `:offset-of` slot, and where a placed field is read | The record's start |
| The range a `:checksum-of` slot's function is given | Indices into the vector, where the region lies |
| `check-position` and `overrun-position` | The vector's start |
| `marshal`'s result | The vector's start: the position after the record |

A record therefore holds the same units at any index, unless an `:align` pad
falls differently, and `marshal`'s result is where the next record begins.

Example (`test/test-manifest-ref.lisp:ref-records-at-an-index@v0.1.3`):

```lisp
(let ((shapes (make-array 21 :element-type '(unsigned-byte 8)))
      (i 0))
  (dolist (x '(1 2 3))
    (setf i (marshal shape shapes (:at i) :kind :square :x x)))
  (list i shapes (unmarshal shape shapes (:at 7) :x)))
;; => (21 #(2 0 1 0 0 0 1  2 0 2 0 0 0 1  2 0 3 0 0 0 1) 2)
```

### Selective reads

Given keys, `unmarshal` decodes those fields and the fields that locate them,
such as counts, lengths and selectors, and passes over the rest through each
field's own skip code (SPECOPS-D80, SPECOPS-D81). From a vector or an
integer, reading stops after the last key; a stream is read to the end of the
record, so that it is left where a full read would leave it. Only the slots
and constants in the part read are checked (SPECOPS-D82). A key that names a
sub-manifest gives its whole plist. A key the manifest lacks is an expansion
error.

Example (`test/test-manifest-ref.lisp:ref-unmarshal-selective@v0.1.3`):

```lisp
(unmarshal shape (octets 2 #xFF #xFE #x2C #x01 0 10) nil :size :kind)
;; => 10, :SQUARE
```

### Checks

Slots, constant strings and their terminators, and masques' static groups are
checked as they are read, and a failure signals `manifest-check-failed`, with
restarts to accept the value or substitute another (SPECOPS-D69,
SPECOPS-D70; see [Conditions](#conditions)).

Example (`test/test-manifest-ref.lisp:ref-chunk-check-failed@v0.1.3`):

```lisp
(let ((bad (octets 0 0 0 3  #x44 #x41 #x54 #x41  1 7 3  #xAD #xEA #x58 #x81)))
  (list (handler-case (unmarshal chunk bad)
          (manifest-check-failed (c)
            (list (check-kind c) (check-field c) (check-position c))))
        (handler-bind ((manifest-check-failed #'continue))
          (unmarshal chunk bad nil :crc))
        (handler-bind ((manifest-check-failed
                         (lambda (c) (use-value (check-expected c) c))))
          (getf (unmarshal chunk bad) :crc))
        (getf (unmarshal chunk bad (:check nil)) :crc)))
;; => ((:CHECKSUM :CRC 11) 2917816449 3499994308 2917816449)
```

## Records

A manifest defined with `(:record t)`, or `(:record class-name)`, has a record
class: a standard class with a slot for each named field, in the same set of
fields as `unmarshal`'s plist (SPECOPS-D59). Defined in
`src/manifest/manifest.lisp:define-manifest-record@v0.1.3`.

- Each slot has an initarg, the field's keyword, and an accessor named after
  the class and the field, such as `shape-x`; a missing value is `nil`.
- `make-` and the class name, such as `make-shape`, makes a record from
  initargs.
- A record prints with its fields: `#<SHAPE :KIND :TRIANGLE :X 3 :Y 4 :SIZE NIL>`.
- A sub-manifest field holds a record of the child's class, or a plist when
  the child has none; a repeat holds a list of them.
- The derived names are interned in the package of the class's name.
  Redefining the manifest redefines the class with its new fields.

A record is written with `(:from record)`, its missing fields taking their
defaults, and read with `(:as :record)`. Round trips are canonical: what is
read and written back reproduces the source, except where the source holds pad
units other than the pad's fill, or slots that disagree with their regions,
which are written in canonical form (SPECOPS-D62).

Example (`test/test-manifest-ref.lisp:ref-records@v0.1.3`):

```lisp
(let ((s (make-shape :kind :triangle :x 3)))
  (setf (shape-y s) 4)
  (list (shape-x s) (field-value s :y) (field-value s :size)))
;; => (3 4 NIL)

(unmarshal shape (octets 2 #xFF #xFE #x2C #x01 0 10) (:as :record))
;; => #<SHAPE :KIND :SQUARE :X -2 :Y 300 :SIZE 10>
```

### `field-value`

```lisp
(field-value record name) => value
```

A generic function: the value of the field NAME, a keyword, in RECORD, a plist
or a record; `nil` when it holds none. Each record class adds a method.
`marshal` calls it for `(:from record)`. Defined in
`src/manifest/manifest.lisp:field-value@v0.1.3`.

## Conditions

Table: The conditions of the manifest layer, their readers and restarts.

| Condition | Signalled by | Readers | Restarts |
|---|---|---|---|
| `manifest-check-failed`, a `simple-error` | `unmarshal`, when a value read fails a check | `check-manifest`, `check-field`, `check-kind`, `check-expected`, `check-actual`, `check-position` | `continue`; `use-value`, for slots |
| `manifest-overrun`, an `error` | `marshal`, when a vector destination is too small | `overrun-manifest`, `overrun-size`, `overrun-position` | — |

For `manifest-check-failed`, defined in
`src/manifest/manifest.lisp:manifest-check-failed@v0.1.3`:

- `check-kind` is `:checksum`, `:length`, `:offset` or `:count` for a slot
  field, or `:constant` for a constant string, a terminator or a masque's
  static bits.
- `check-field` is the slot field's name, or `nil` for a constant.
- `check-actual` is the value read, and `check-expected` what the region gives.
- `check-position` is the unit at which the slot field begins, or the first
  unit that differs from a constant: a masque's first unit for its bits.
- The report names the field, the manifest, the unit and both values.
- `continue` accepts the value read and reads on; `use-value` substitutes a
  value for a slot field, such as `check-expected`.

For `manifest-overrun`, defined in
`src/manifest/manifest.lisp:manifest-overrun@v0.1.3`, `overrun-size` is the
vector's allocated size and `overrun-position` the unit that would not fit
(`test/test-manifest-ref.lisp:ref-marshal-overrun@v0.1.3`).

## Stop rules

A `:vec` field may end where a user's rule says: `:stop-by class`, or
`:stop-by (class initarg value ...)` with constant initargs, names a subclass
of `stop-rule` (SPECOPS-D55). A fresh instance is made for each field each
time it is read or written. The class must exist when a manifest that names
it is expanded, so define it inside `eval-when` with all three situations.
Defined in `src/manifest/scan.lisp:stop-rule@v0.1.3`.

Table: The stop rule protocol, generic functions specialized on the rule.

| Generic function | When it is called | Returns | Default |
|---|---|---|---|
| `(done-p rule)` | On read, before each element | True when the field is complete | `nil` |
| `(feed rule element)` | On read, after each element | `nil` to go on, `:keep` to stop with ELEMENT as data, `:drop` to stop with it as a delimiter | `nil` |
| `(trailer rule)` | On read, after the scan | How many of the elements collected as data to drop | 0 |
| `(frame-element rule element last-p)` | On write, for each data element | The code to write; LAST-P is true for the last | ELEMENT |
| `(frame-trailer rule)` | On write, after the data | A list of codes to write | `nil` |

A rule may instead define methods on `stop-effector` and `frame-effector`,
which return code fragments that are inlined at expansion in place of calls to
the five functions, with no change to the manifests that use it
(`src/manifest/scan.lisp:stop-effector@v0.1.3` and
`src/manifest/scan.lisp:frame-effector@v0.1.3` document the fragments).

Example (`test/test-manifest-ref.lisp:ref-stop-rule@v0.1.3`):

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass until-byte (stop-rule)
    ((byte :initarg :byte :reader until-byte-of))))

(defmethod feed ((r until-byte) element)
  (and (eql element (until-byte-of r)) :drop))

(defmethod frame-trailer ((r until-byte))
  (list (until-byte-of r)))

(defmanifest marked-list (:unit 8 :endian :big)
  (:items (:u 1 :vec) :stop-by (until-byte :byte #xFF))
  (:after :u8 :default 0))

(let ((buf (make-array 4 :element-type '(unsigned-byte 8))))
  (marshal marked-list buf :items #(1 2) :after 9)
  (list buf (unmarshal marked-list buf)))
;; => (#(1 2 #xFF 9) (:ITEMS #(1 2) :AFTER 9))
```

## Serialization primitives

### `masque` and `unmasque`

```lisp
(masque mask [(param...)] (letter value)...) => integer
(unmasque mask integer (letter...) form...) => values of FORMS, or NIL
```

`masque` composes an integer laid out by the string MASK; `unmasque` takes
one apart, binding each LETTER to its group's value around FORMS, and
returns `nil` when a constant digit differs. MASK begins with a radix,
`b:`, `o:`, `d:` or `h:`, binary when none is given, followed by digits,
which are constant, and letters, each run of one letter a group; `.` is
spacing. In a hexadecimal mask, `A` to `F` are digits and lowercase letters
groups. In a decimal mask each position is a decimal digit, counted from the
right as a binary mask's bits are (SPECOPS-D87).
`masque`'s PARAM `(:overflow . mode)` decides a value too wide for its group:
`:mask`, the default, reduces it to the group's width, two's complement for a
negative value; `:error` signals an error, at expansion for a constant value;
`nil` leaves it as it is (SPECOPS-D88). Defined in
`src/serial.lisp:masque@v0.1.3` and `src/serial.lisp:unmasque@v0.1.3`.

Example (`test/test-manifest-ref.lisp:ref-masque-and-unmasque@v0.1.3` and
`test/test-manifest-ref.lisp:ref-masque-overflow@v0.1.3`):

```lisp
(masque "h:aabb" (a #x12) (b #x34))       ;; => #x1234
(masque "b:aaaa1bbb" (a 5) (b 2))         ;; => #b01011010
(masque "d:aa00bb" (a 3) (b 9))           ;; => 30009
(unmasque "h:aabb" #x1234 (a b) (list a b))
;; => (#x12 #x34)
(masque "h:aabb" (a #x123) (b 4))         ;; => #x2304
```

### `serialize`

```lisp
(serialize item unit collector)
```

Calls COLLECTOR with each UNIT-bit unit of ITEM, most significant first: an
integer in as few units as it needs, a pair `(width . value)` in WIDTH units,
or a vector's elements as they are. Defined in
`src/serial.lisp:serialize@v0.1.3`.

Example (`test/test-manifest-ref.lisp:ref-serialize@v0.1.3`):

```lisp
(let ((units nil))
  (flet ((collect (u) (push u units)))
    (serialize #x1234 8 #'collect)
    (serialize '(3 . 5) 8 #'collect)
    (serialize #(7 8) 8 #'collect))
  (reverse units))
;; => (#x12 #x34 0 0 5 7 8)
```

### Checksums

Table: Checksum functions, which `:checksum-of` slots name with `:by`.

| Function | Computes |
|---|---|
| `(crc buffer start end)` | CRC-32, as PNG and zlib use (`src/checksum.lisp:crc@v0.1.3`) |
| `(adler32 buffer start end)` | Adler-32 (`src/checksum.lisp:adler32@v0.1.3`) |
| `(adler32-checksum sequence &key start end)` | Adler-32 with keyword bounds (`src/checksum.lisp:adler32-checksum@v0.1.3`) |
| `(sfnt-checksum buffer start end)` | The sum, modulo 2^32, of big-endian 32-bit words, the last padded with zeros, as sfnt and WOFF fonts use (`src/checksum.lisp:sfnt-checksum@v0.1.3`) |

Example (`test/test-manifest-ref.lisp:ref-checksums@v0.1.3`):

```lisp
(let ((v (map '(vector (unsigned-byte 8)) #'char-code "123456789")))
  (list (crc v 0 9) (adler32 v 0 9) (sfnt-checksum v 0 9)))
;; => (#xCBF43926 #x091E01DE #x9F686A6C)
```

### IEEE float bits

```lisp
(encode-float-bits value bits &optional name) => integer
(decode-float-bits pattern bits) => number or keyword
```

Convert between a number and its bit pattern in the IEEE binary format of 16,
32, 64 or 128 BITS (SPECOPS-D91, SPECOPS-D92). A value is rounded to the
nearest the format holds, ties to even; a finite value too large is an error,
naming NAME when given. Half floats read as single floats, which hold them
exactly; single and double floats as such; quad floats as exact rationals.
Infinities and NaN read as `:infinity`, `:-infinity` and `:nan`, and those
keywords are written as them; on SBCL, single and double precision use its
own floats, so an infinity or a NaN, its payload included, reads as an SBCL
float. Defined in `src/serial.lisp:encode-float-bits@v0.1.3` and
`src/serial.lisp:decode-float-bits@v0.1.3`.

Example (`test/test-manifest-ref.lisp:ref-float-bits@v0.1.3`):

```lisp
(encode-float-bits 1.5 32)                                  ;; => #x3FC00000
(encode-float-bits 1/3 16)                                  ;; => #x3555
(decode-float-bits #x3C00 16)                               ;; => 1.0
(decode-float-bits #x7FFF0000000000000000000000000000 128)  ;; => :INFINITY
```

### Codecs

A codec is a function of one argument that maps a character to its code and
a code to its character, returning `nil` for one it lacks. `defcodetable`
defines one from a table of code pages; the map modules define
`specops.map.ebcdic:ebcdic-code-cp037`, `specops.map.ebcdic:ebcdic-code-cp310`
and `specops.map.cp437:cp437-code`, in the systems `specops.map.ebcdic` and
`specops.map.cp437`. A character or a code that a field's codec lacks is an
error (SPECOPS-D43). Defined in `src/serial.lisp:defcodetable@v0.1.3`.

## Limits in 0.1.3

- **Byte orders in wider units.** In a manifest of units wider than 8 bits,
  `:little` and `:middle` reorder the bytes of a field, not its units:
  `(:u 1)` holding 1 in a little-endian 16-bit manifest is written `#x0100`.
- **Source vectors.** `unmarshal` reads a vector whose element type is the
  manifest's unit; a general vector such as `#(2 255)` is an error, reported
  as "Incompatible type for collection array."
- **Counts on write.** `marshal` does not check a vector's value against its
  `:count`: a shorter value writes fewer units, and the fields after it move.
- **Sub-manifest values.** A sub-manifest's value in `marshal`'s pairs must
  be a literal list of pairs; a value known only when the code runs is given
  through `(:from record)`.
- **Record names.** A record class named by a symbol of the `common-lisp`
  package, such as `directory`, cannot be defined, since its derived names
  would be interned there.
- **Quad negative zero** reads as 0, which a rational cannot sign.
- `str`'s `:encode-by` and `:length` options are reserved.

## Project Structure

```text
specops.asd                  systems specops and specops/test
src/
  package.lisp               the specops package and its exports
  checksum.lisp              crc, adler32, adler32-checksum, sfnt-checksum
  serial.lisp                serialize, defcodetable, masque, unmasque, stream
                             sources, IEEE float bits
  manifest/
    scan.lisp                stop rules, field plans and value assemblers
    manifest.lisp            defenum, defmanifest, records, marshal, unmarshal,
                             conditions
mod/map/ebcdic/, mod/map/cp437/
                             codecs: specops.map.ebcdic, specops.map.cp437
test/
  test-manifest-ref.lisp     this document's examples, the suite manifest-ref
  test-manifest-*.lisp       the manifest suites
  test-serial.lisp           the serialization suite
```

Description: The tree lists the files that implement the manifest layer, the
codec modules and the tests, with what each holds. The core files load in the
order shown.

The design decisions are recorded in the
[Manifest Development Log, SPECOPS-0006](Log.Manifest.md), and the work that
completed the layer in the
[Manifest Completion Plan, SPECOPS-0003](Plan.ManifestComplete.md). The
layer's place in SpecOps is described in the
[SpecOps Architecture, SPECOPS-0001](Arch.SpecOps.md), and its terms in the
[SpecOps Terms glossary, SPECOPS-0002](Glossary.SpecOps.md).
