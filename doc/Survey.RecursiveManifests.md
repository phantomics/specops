---
id:            SPECOPS-DRAFT-recursive-manifests
title:         Recursive Manifests Survey
genre:         Survey
scope:         component
project:       SpecOps
component:     Manifest
language:      en
status:        Accepted
provenance:
  assistant:   claude-opus (OpenCode)
  session:     Recursive manifests
relates-to:
  - SPECOPS-0007
  - SPECOPS-0006
  - SPECOPS-0008
  - SPECOPS-0004
open-questions:
  - SPECOPS-DRAFT-recursive-manifests-O1
  - SPECOPS-DRAFT-recursive-manifests-O2
  - SPECOPS-DRAFT-recursive-manifests-O3
  - SPECOPS-DRAFT-recursive-manifests-O4
  - SPECOPS-DRAFT-recursive-manifests-O5
  - SPECOPS-DRAFT-recursive-manifests-O6
  - SPECOPS-DRAFT-recursive-manifests-O7
cites:
  - title:    Vox Survey (Vox)
    locator:  Vox repository, doc/Survey.Vox.md — The encoding; SpecOps's role; VOX-DRAFT-vox-O5
    external: true
  - title:    RFC 8200, Internet Protocol, Version 6 (IPv6) Specification
    locator:  §4, extension headers; the Next Header field and Hdr Ext Len
    external: true
  - title:    RFC 791, Internet Protocol
    locator:  "§3.1, the header: IHL, total length and header checksum"
    external: true
  - title:    ITU-T X.690, ASN.1 encoding rules (BER, CER and DER)
    locator:  §8.1, identifier and length octets; §10.1, DER's definite lengths of minimal form
    external: true
  - title:    WebAssembly Core Specification
    locator:  "§5.4.1, control instructions: block, loop, if, else and end"
    external: true
  - title:    Kaitai Struct User Guide
    locator:  doc.kaitai.io — user-defined types used within themselves; repeat-until
    external: true
  - title:    Construct (Python declarative binary parsing and building library)
    locator:  construct.readthedocs.io — LazyBound, for recursive structures
    external: true
  - title:    Scapy
    locator:  scapy.readthedocs.io — bind_layers, layers chosen by a field of the layer below
    external: true
---

# Recursive Manifests: Survey

A SpecOps manifest cannot contain itself: `marshal` and `unmarshal` inline a
sub-manifest's code into its parent's at expansion (SPECOPS-D37,
SPECOPS-D63), so a manifest that refers to itself, directly or through
others, would expand without end. This Survey asks whether the manifest layer
should allow such references, how it could, and which formats it would reach:
chains of headers such as IPv6's, nested type-length-value encodings such as
ASN.1's BER, and nested code such as WebAssembly's control blocks. It also
records the further gaps those formats expose, which recursion alone does not
close, and the limits that reading recursive data from untrusted sources
requires. It answers, from SpecOps's side, the Vox Survey's question of
whether SpecOps should grow recursive manifests (VOX-DRAFT-vox-O5). It assumes
the interface of the [Manifest Reference, SPECOPS-0007](Ref.Manifest.md) and
the decisions of the [Manifest Development Log, SPECOPS-0006](Log.Manifest.md).
A Survey is exploratory and non-normative: it records the territory and its
open questions, and leaves the decisions to a Plan.

## Motivation

- **Vox asked.** Vox, the Psyche program's exchange of work and data between
  Lisp images, declares its fixed structures as manifests, and its Survey
  found that its recursive Datum trees fit less directly, because a manifest
  cannot contain itself (VOX-DRAFT-vox-O5).
- **Formats nest.** Many binary formats contain structures of their own kind:
  an IPv6 extension header is followed by a header chosen by its Next Header
  field, which may be another extension header; a BER element of constructed
  type holds BER elements; a WebAssembly `block` holds instructions, which may
  be blocks. Each can be written by hand around the manifest layer, but then
  its checks, selective reads and round trips are the writer's to build.
- **SpecOps writes WebAssembly.** The format module `specops.format.wasm` and
  the architecture module `specops.arch.wasm` produce code whose control
  instructions nest, so the layer's own users meet the case.
- **The question shapes later work.** The arrays plan proposes a manifest's
  layout as data, for generators such as shader code
  (SPECOPS-O22). What a layout is, once manifests may recur,
  should be settled before either is built.

## Selection by fields

The manifest layer chooses a field's form from fields: a `:case` on an
earlier field's value, a count or a length from a slot, a repeat's end from an
instance's fields (SPECOPS-D46, SPECOPS-D65). This is what lets it build the
region table at expansion (SPECOPS-D70), check a record as it reads, read a
record selectively (SPECOPS-D80), and describe a layout as data. A form chosen
by a Lisp value's type, such as writing an integer, a string or a list each
under its own tag, would put arbitrary Lisp code into the layout, and the
manifest could no longer be analysed at expansion. Manifests describe
structure only, and logic stays at the call site (SPECOPS-D29).

This Survey keeps that rule. Recursion, as explored here, adds no new way to
choose a form: a recursive reference is a sub-manifest reference whose target
happens to be on the path being expanded. Encodings that choose by a value's
type, such as Vox's Datum on write, stay in the caller.

## The mechanism

**Inline until a cycle, then call.** While `marshal` or `unmarshal` expands a
manifest, it inlines each sub-manifest as now, keeping the manifests it is
inside on a stack. When it meets a manifest already on the stack, it emits a
call to a function that writes or reads that manifest, instead of its code.
Finding the cycle is cheap: `src/manifest/manifest.lisp:walk-manifest@v0.1.0`
already visits each manifest nested in another once, so that the layer's
walks over nested manifests end. A manifest that is never re-entered is
inlined as before, at no new cost.

**The functions.** Each manifest at which a cycle closes needs a writer, a
reader and, for selective reads, a function that passes over an instance.
Their bodies are `marshal` and `unmarshal` code, which can be expanded only
once every manifest of the cycle is defined; how they are defined is
SPECOPS-DRAFT-recursive-manifests-O1.

**What crosses a call.**

Table: What a recursive call takes and gives.

| Direction | Takes | Gives |
|---|---|---|
| Write | The destination, the position, and the instance's values, as a plist or a record | The position after the instance |
| Read | The source, the position, and the read's options, such as `(:check nil)` and `(:as :record)` | The instance, as a plist or a record, and the position after it |
| Pass over | The source and the position | The position after the instance |

**What a call boundary stops.** Code outside the call cannot see the fields
inside it, so:

- **Slots** already resolve within an instance (SPECOPS-D66), and lose
  nothing.
- **Placed fields** cannot be queued across a call to a directive outside it;
  a placed field under a recursive reference would be an expansion error.
- **Constant selectors** cannot choose a branch at expansion inside the call,
  whose code serves every caller, so the choice is made when the code runs.
- **Selective reads** pass over a recursive field through its pass-over
  function, which must still read the instance to find its end, unless a
  length slot gives it.

**Limits.** A recursive reader of untrusted input must be bounded, and so
must a repeat whose count comes from the source, which can ask for more
instances than memory holds even without recursion. Following the steward's
direction, a field states its own limits:

- **A repeat or a vector** whose count or end comes from the source may state
  the most elements it takes, as `(manifest :elements ber-element :until :length :max 1024)`.
- **A recursive reference** may state the deepest nesting it allows through
  it, as `(manifest :body wasm-instr :until (:op :end) :max-depth 64)`.

A read that would exceed a limit signals a condition naming the field, the
manifest and the limit; a write that would exceed it is an error, so that
what is written can be read back. Where limits are stated and how they
combine is SPECOPS-DRAFT-recursive-manifests-O2.

## Three sketches

The sketches below are written in the proposed form and have not been run.
Each shows a different kind of recursion.

### A chain: IPv6 extension headers

An IPv6 header's Next Header field gives the type of what follows it: an
extension header, an upper-layer header such as TCP's, or nothing. Each
extension header has a Next Header field of its own, so the headers form a
chain, ended by an upper-layer protocol (*RFC 8200*, §4).

```lisp
(defenum ip-next nil :hop-by-hop 0 :tcp 6 :udp 17 :routing 43 :fragment 44
                     :icmpv6 58 :none 59 :destination-options 60)

(defmanifest ipv6-packet (:unit 8 :endian :big)
  (masque "h:6ttfffff" (t :traffic-class :u :default 0)
          (f :flow-label :u :default 0))
  (:payload-length :u16 :slot (:length-of :rest))
  (:next :u8 :default :none :enumerate-by ip-next)
  (:hop-limit :u8 :default 64)
  (:source (:u 1 :vec) :count 16)
  (:destination (:u 1 :vec) :count 16)
  (:rest (:case :next
           ((:hop-by-hop :routing :destination-options) (manifest ipv6-options-header))
           (:fragment (manifest ipv6-fragment-header))
           (:tcp (manifest tcp-segment))
           (:udp (manifest udp-datagram))
           (otherwise :u 1 :vec))))

(defmanifest ipv6-options-header (:unit 8 :endian :big)
  (:next :u8 :default :none :enumerate-by ip-next)
  (:length :u8)                                  ; 8-octet units, not counting the first
  (:options (:u 1 :vec) :count ...)              ; 6 + 8 × length octets: a derived count
  (:rest (:case :next
           ((:hop-by-hop :routing :destination-options)
            (manifest ipv6-options-header))      ; the cycle closes here
           (:fragment (manifest ipv6-fragment-header))
           (:tcp (manifest tcp-segment))
           (:udp (manifest udp-datagram))
           (otherwise :u 1 :vec))))
```

Description: The packet's fixed header ends in a `:case` on its Next Header
field. An options header, for hop-by-hop, routing or destination options,
ends in the same `:case` on its own Next Header field, whose branches include
the options header itself. That reference closes the cycle, so it becomes a
call. The fragment header would have the same tail.

The chain is the case recursion serves most simply. It also shows three
gaps that recursion does not close:

- **A derived count.** The options' length is `6 + 8 × length` octets, and
  the manifest layer has no count that is a function of a field
  (SPECOPS-DRAFT-recursive-manifests-O4).
- **The branch list is written twice,** in the packet and in each header,
  because a sub-manifest cannot read its parent's Next Header field: the
  `:case` that dispatches on it must sit in the manifest that holds it
  (SPECOPS-DRAFT-recursive-manifests-O6).
- **TCP's checksum,** inside `tcp-segment`, covers a pseudo-header made of
  the enclosing header's addresses, which the segment cannot see
  (SPECOPS-DRAFT-recursive-manifests-O6).

### A tree: ASN.1 BER

A BER element is an identifier octet, a length, and contents. An element of
constructed type holds elements, to the end of its length; one of primitive
type holds octets (*ITU-T X.690*, §8.1). X.509 certificates, SNMP and LDAP are
written this way.

```lisp
(defmanifest ber-element (:unit 8 :endian :big)
  (masque "b:ccpttttt" (c :class :u :default 0)
          (p :constructed :u :default 0) (t :tag :u :default 0))
  (:body (:case :constructed
           (0 (manifest ber-primitive))
           (1 (manifest ber-constructed)))))

(defmanifest ber-primitive (:unit 8 :endian :big)
  (:length :u8 :slot (:length-of :octets))      ; the short form: lengths below 128
  (:octets (:u 1 :vec)))

(defmanifest ber-constructed (:unit 8 :endian :big)
  (:length :u8 :slot (:length-of :elements))
  (manifest :elements ber-element :until :length :max 1024))  ; the cycle closes here
```

Description: An element's constructed bit chooses its body. A primitive body
is a length and its octets; a constructed body is a length and a repeat of
elements read until that length is used, with a limit on how many. The repeat
refers to `ber-element`, which closes the cycle.

What the sketch leaves out marks what BER needs beyond recursion:

- **High tag numbers,** of 31 and above, continue in octets whose top bit
  marks all but the last, which a user stop rule already reads, as the
  suites' `test/helpers.lisp:high-bit-run@v0.1.0` does.
- **Long-form lengths.** A length of 128 or more is an octet giving the
  number of length octets that follow, then the length in them. The width of
  the length field then depends on its value, and DER requires the shortest
  form (*X.690*, §10.1). A slot is backpatched in place, so its width must be
  known before its value is (SPECOPS-DRAFT-recursive-manifests-O7).
- **Indefinite lengths,** in BER but not DER, end a constructed element at an
  end-of-contents element of tag 0 and length 0, which a repeat `:until` an
  instance whose fields have those values could express.

### Nesting by terminator: WebAssembly control blocks

A WebAssembly `block`, `loop` or `if` is followed by a block type and a
sequence of instructions ended by `end` (opcode #x0B); any of those
instructions may itself be a block (*WebAssembly Core Specification*, §5.4.1).

```lisp
(defenum wasm-op nil :unreachable #x00 :nop #x01 :block #x02 :loop #x03 :if #x04
                     :else #x05 :end #x0B :br #x0C :br-if #x0D :call #x10
                     :local-get #x20 :i32-const #x41 :i32-add #x6A)

(defmanifest wasm-no-immediates (:unit 8 :endian :little))

(defmanifest wasm-instr (:unit 8 :endian :little)
  (:op :u8 :enumerate-by wasm-op)
  (:args (:case :op
           ((:block :loop :if) (manifest wasm-block))
           ((:br :br-if :call :local-get) :u 1 :leb)
           (:i32-const :s 1 :leb)
           (otherwise (manifest wasm-no-immediates)))))

(defmanifest wasm-block (:unit 8 :endian :little)
  (:type :u8 :default #x40)                      ; the empty block type, or a value type
  (manifest :body wasm-instr :until (:op :end) :max 65536 :max-depth 64))  ; the cycle
```

Description: An instruction is an opcode and arguments chosen by it: a block
for `block`, `loop` and `if`, a LEB128 integer for branches, calls and local
reads, a signed one for `i32.const`, and nothing for the rest. A block is a
block type and a repeat of instructions until one whose opcode is `end`,
which the repeat keeps, as `:until` keeps the instance that ends it. The
repeat refers to `wasm-instr`, which closes the cycle, with a limit on the
instructions in one block and on the depth of nesting.

What the sketch shows and leaves out:

- **`else` is an instruction** in the body of an `if`, with no immediates, so
  the two arms read as one list with an `else` between them, and the writer
  gives the `end` as the last instruction. A form that splits the arms would
  need a repeat ended by either of two opcodes, which `:until` already allows
  by its keys, and a second repeat after it.
- **Block types** may also be type indices, written as a signed LEB128 of 33
  bits, which would make `:type` a `:case` on its own first octet.
- **An empty branch,** here a manifest with no fields, has not been tested.

## Vox's Datum values

With recursion, a manifest could describe Datum's wire form: a type code, then
a `:case` on it, with a list's branch a repeat of `datum` itself. Two things
would still stand between that and Vox's use:

- **Writing chooses by type.** To write `5`, `"abc"` or `(1 2)`, the writer
  must choose the branch from the value's Lisp type, which is the selection by
  type this Survey keeps out of manifests. Vox's encoder makes that choice,
  and could then marshal each node through its branch's manifest.
- **Reading gives records.** Read through the manifest, `5` would come back
  as a tagged record such as `(:type :fixnum :value 5)`, and a walker would
  rebuild the Lisp value from it, passing over the tree a second time. A
  function on a branch that turns its record into a value on read, as a codec
  turns codes into characters, would remove the second pass; whether that
  belongs in manifests is SPECOPS-DRAFT-recursive-manifests-O3.

Datum's canonical form also requires a reader to reject a LEB128 integer not
written in its fewest bytes, which the layer does not check.

Until those are settled, the arrangement the Vox Survey describes serves:
manifests for Vox's fixed structures, the frame, word, completion and array
headers, and a recursive walker for Datum values, built on the primitives
SpecOps exports, `serialize`, LEB128 fields, and `encode-float-bits` and
`decode-float-bits` (SPECOPS-0007). Recursive manifests serve formats whose
nested structure is made of records, as in the three sketches.

## Gaps beyond recursion

The formats above, and the TCP/IP family with them, need more than recursion.
Most of TCP/IP is reachable now: layers chosen by EtherType and protocol
fields as `:case` branches, VLAN and MPLS stacks as repeats that end on a
field's value, TCP and IPv4 options as repeats of type-length-value entries,
and the bit fields of their headers as masques. What it lacks:

Table: Gaps the surveyed formats expose, beyond recursion.

| Gap | Formats | Question |
|---|---|---|
| Lengths and counts scaled or derived from fields | IPv4's IHL and TCP's data offset count 32-bit words; IPv4's total length includes the header; an IPv6 options header is 8 × (length + 1) octets | SPECOPS-DRAFT-recursive-manifests-O4 |
| A checksum over a region that holds it | IPv4's header checksum, computed with its own field taken as zero | SPECOPS-DRAFT-recursive-manifests-O5 |
| A sub-manifest reading its parent's fields | TCP's and UDP's checksums over the pseudo-header; IPv6's Next Header dispatch | SPECOPS-DRAFT-recursive-manifests-O6 |
| A length whose width depends on its value | BER's long-form lengths, minimal in DER | SPECOPS-DRAFT-recursive-manifests-O7 |
| Limits on counts and depth | Every format read from an untrusted source | SPECOPS-DRAFT-recursive-manifests-O2 |

The arrays plan meets the third of these too, in Lexter's cells, whose glyph
words are chosen by type bits a sub-manifest cannot see, and leaves it out of
its scope (SPECOPS-0008).

## Honest Limits

- **A call costs what inlining saves.** Each recursive instance is a function
  call, with its arguments and a returned position, where an inlined
  sub-manifest shares its parent's variables. For deep or wide trees, such as
  large WebAssembly bodies, this may matter, and nothing here measures it.
- **Selective reads lose some of their reach.** Passing over a recursive
  field reads it whole, unless a length gives its end; BER's definite lengths
  do, WebAssembly's blocks do not.
- **Recursion does not make the layer a serializer of Lisp values.** Writing
  chooses by fields, so Vox's Datum, and encodings like it, keep their
  type-driven encoder outside manifests.
- **The sketches are unrun.** They use forms that do not exist yet,
  `:max` and `:max-depth` among them, and some that exist but are untested in
  these positions, such as a length slot over a `:case` field and a manifest
  with no fields.
- **Limits are only as good as the ones stated.** A field from a source
  without a stated limit stays unbounded; whether the layer should require
  limits for untrusted reads, or supply defaults, is part of
  SPECOPS-DRAFT-recursive-manifests-O2.

## Relationship to Other Work

- **[Manifest Reference, SPECOPS-0007](Ref.Manifest.md)** — the interface this
  Survey would extend, and the limits of 0.1.0 it lists.
- **[Manifest Development Log, SPECOPS-0006](Log.Manifest.md)** — the
  decisions recursion touches: inlining through a shared lexicon
  (SPECOPS-D37, SPECOPS-D63), repeats as scans of instances (SPECOPS-D65),
  slots within an instance (SPECOPS-D66), the region table (SPECOPS-D70),
  selective reads (SPECOPS-D80, SPECOPS-D81), and structure apart from logic
  (SPECOPS-D29).
- **[Manifests over Arrays of Records, SPECOPS-0008](Plan.OverArrays.md)**
  — independent of this Survey; its layout as data
  (SPECOPS-O22) would need to say how a recursive reference
  appears in a layout, and it shares the gap of parent fields.
- **[Lisp Binary-Format Libraries, SPECOPS-0004](Eval.LispBinaryTools.md)** —
  its question of computing a record's size before writing it (SPECOPS-O17)
  bears on lengths whose width depends on their value.
- **The Vox Survey** — Vox's question VOX-DRAFT-vox-O5, which this Survey
  answers from SpecOps's side, and the walker it describes for Datum values.
- **Kaitai Struct** — user-defined types may be used within their own
  definitions, so a recursive format is declared directly, and read by
  generated code that calls each type's reader.
- **Construct** — `LazyBound` names a structure that is bound only when it is
  used, so a structure can refer to itself; it is the call at a cycle, made
  explicit by the writer.
- **Scapy** — layers bound to the field of the layer below, with
  `bind_layers`, which is the IPv6 chain's dispatch by Next Header, and which
  Scapy resolves at run time where SpecOps would resolve it at expansion.

## Open Questions

### SPECOPS-DRAFT-recursive-manifests-O1 — How are a cycle's functions defined?

Their bodies can be expanded only once every manifest of the cycle exists.
They could be defined by a form placed after the cycle, naming the manifests
at which it closes; by a form that defines the cycle's manifests together;
or by `defmanifest` itself, expanding its functions at load time rather than
compile time. Which manifests of a cycle get functions, how the functions are
named, and whether users may call them are open too.

### SPECOPS-DRAFT-recursive-manifests-O2 — Where are limits stated, and how do they combine?

The steward's direction is that a field states its own limits: the most
elements a repeat or a vector takes, and the deepest nesting a recursive
reference allows. Open are: how depth is counted when several references
close the same cycle, as an IPv6 chain's options and fragment headers do;
whether a call to `unmarshal` may tighten a manifest's limits for one source;
whether a field read from a source without a stated limit is allowed, or has a
default; and whether a limit exceeded on read is a `manifest-check-failed` of
a new kind, with a `continue` restart, or a condition of its own without one.

### SPECOPS-DRAFT-recursive-manifests-O3 — Should a branch convert its record to a Lisp value on read?

A function on a `:case` branch, applied to the record it reads, would let a
manifest read Vox's Datum, or any tagged encoding, straight into Lisp values,
as a codec reads codes as characters. It would serve reading only, since
writing chooses by type, and the two directions would then differ; it would
also make a read's result depend on code outside the manifest. Its costs and
benefits are to be explored before it is decided.

### SPECOPS-DRAFT-recursive-manifests-O4 — Should lengths and counts be scaled or derived from other fields?

IPv4's IHL counts 32-bit words, an IPv6 options header's length counts 8-octet
units after the first, and IPv4's total length covers the header and the
payload, so that the payload's length is a difference. A scale and an offset
on a slot would cover the first two; the third needs a length computed from
two fields. Which forms keep a manifest analysable at expansion?

### SPECOPS-DRAFT-recursive-manifests-O5 — Should a checksum slot cover a region that holds it?

IPv4's header checksum is computed over the header with the checksum field
taken as zero, and on read the stored value is checked by the same sum. A
`:checksum-of` slot inside its own region would need its units zeroed for the
computation, on write and on read.

### SPECOPS-DRAFT-recursive-manifests-O6 — Should a sub-manifest read its parent's fields?

TCP's checksum covers its enclosing IP header's addresses, IPv6's dispatch by
Next Header is repeated in every header that holds one, and Lexter's glyph
words are chosen by type bits outside them. A sub-manifest that could name a
field of an enclosing manifest, inlined or across a call, would serve all
three. How would such a reference be written, and what does it mean across a
recursive call, whose code serves every caller?

### SPECOPS-DRAFT-recursive-manifests-O7 — Should a length's width depend on its value?

BER's lengths below 128 take one octet and longer ones several, and DER
requires the shortest. A slot is written in place after its region, so its
width must be fixed before its value is known. Writing the region first to a
buffer, or computing its size before writing it (SPECOPS-O17), would let a
slot choose its width; which serves, and how a reader checks that a DER
length is minimal, are open.
