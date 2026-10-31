---
id:            SPECOPS-DRAFT-restructure
title:         Repository Restructure Development Log
genre:         Log
scope:         project
project:       SpecOps
language:      en
status:        Draft
provenance:
  assistant:   claude-opus-5-5 (OpenCode)
  session:     Restructure
relates-to:
  - SPECOPS-DRAFT-specops
  - SPECOPS-DRAFT-manifest-complete
glossary:      SPECOPS-DRAFT-glossary
decisions:
  - SPECOPS-DRAFT-restructure-D1
  - SPECOPS-DRAFT-restructure-D2
  - SPECOPS-DRAFT-restructure-D3
  - SPECOPS-DRAFT-restructure-D4
  - SPECOPS-DRAFT-restructure-D5
  - SPECOPS-DRAFT-restructure-D6
  - SPECOPS-DRAFT-restructure-D7
cites:
  - title:    IBM's next-gen mainframe chip is the first to run Arm and Z workloads on the same cores (VentureBeat)
    locator:  venturebeat.com/infrastructure/ibms-next-gen-mainframe-chip-is-the-first-to-run-arm-and-z-workloads-on-the-same-cores
    external: true
---

# Repository Restructure: Development Log

This document chronicles the reorganization of the SpecOps repository from a
flat layout into a core under `src/` and modules under `mod/`, grouped by
kind: instruction sets in `mod/arch/`, object and container formats in
`mod/format/`, and two-way mapping tables in `mod/map/`. It covers the
directory moves, the renaming of systems and packages to match, the move of
System Z to `s390x`, and the split of the 2,547-line `specops.lisp` into one
file per subsystem. No behavior was changed. The work starts from commit
`738105a` and is complete at commit `13b5887`, in three commits: `0ac7223`
(moves), `ada2d19` (renames and new map systems) and `13b5887` (the split).
Commit `0ac7223` does not load by itself, because its `specops.asd` still
names the core files at the repository root; bisect over `ada2d19` instead.
The update of 2026-10-31 at the end of this log carries the record forward to
commit `50e231d`, which adds the format systems and updates the README and
the other documents.

## Problem

At `738105a` the repository had no structure that matched the design of the
[SpecOps architecture, SPECOPS-DRAFT-specops](Arch.SpecOps.md). That design
keeps the program model independent of both the instruction set and the
object format (SPECOPS-DRAFT-specops-D11), but the files mixed them:

- The core (`package.lisp`, `checksum.lisp`, `scan.lisp`, `specops.lisp`,
  `program.lisp`) sat at the repository root beside seven architecture
  directories, the ELF and GOFF writers, the PNG manifest demo, and editor
  files.
- The WebAssembly module writer, `emit-program :wasm`, lived in the wasm
  architecture directory (`wasm/program.lisp:emit-program@738105a`).
- `formats/` held character code pages, not formats, and its systems were
  named `specops/format.ebcdic` and `specops/format.cp437`.
- One file, `specops.lisp`, held two subsystems: serialization with the
  manifest layer, and the assembler core.

The architecture's Roadmap asks for the format layer to be packaged and for
per-module test suites, and the [Manifest Completion Plan,
SPECOPS-DRAFT-manifest-complete](Plan.ManifestComplete.md) ends with putting
`goff.lisp` into an ASDF system. Both need modules with a place of their own.
The sibling projects Classic and Origin already use `src/` for a core and
`mod/` for modules, which gave a convention to follow.

## Design Decisions

### SPECOPS-DRAFT-restructure-D1 — The core lives in `src/`, and modules in `mod/`, grouped by kind

**Status:** Proposed
**Context:** SpecOps has seven architecture modules, three object-format
writers (ELF, GOFF and WebAssembly) and two code tables, and ARM is on the
Roadmap. Classic and Origin place each module
directly under `mod/`, in a directory named for its system
(`mod/classic.engine.ref/`).
**Decision:** The core system's files are in `src/`, with `specops.asd`
naming `:pathname "src/"`. Modules are grouped by kind: `mod/arch/<isa>/`,
`mod/format/<format>/` and `mod/map/<table>/`. Tests are in `test/`.
**Alternatives:** A flat `mod/` with one directory per system, as in Classic
and Origin (rejected: over fifteen directories, with the kind of each visible
only in its name). Kind directories at the repository root, `arch/` and
`format/` (rejected: diverges from the sibling projects and keeps the root
crowded). Leaving the layout as it was (rejected: the problems above).

### SPECOPS-DRAFT-restructure-D2 — System and package names name the module's kind

**Status:** Proposed
**Context:** ASDF finds a system named with a slash, such as
`specops/format.ebcdic`, only in the file of its primary system,
`specops.asd`. A module with its own directory and `.asd` file therefore needs
a dotted name. Architecture systems were named `specops.<arch>`, and the
README loads them by those names.
**Decision:** Systems and their packages are named `specops.arch.<isa>`,
`specops.format.<format>` and `specops.map.<table>`, and each system's `.asd`
file is named for the system. The package of each architecture module was
renamed with its system. Internal symbols, such as the prototype variable
`*assembler-prototype-z*`, were not renamed.
**Alternatives:** Keeping the architecture names `specops.<arch>` and giving
new names only to new modules (rejected by the steward: the names would no
longer show the kind, and at version 0.0.1 there are few users to break).
Keeping slash names (not possible outside `specops.asd`).

### SPECOPS-DRAFT-restructure-D3 — Architecture modules are named for instruction sets; System Z becomes `s390x`

**Status:** Proposed
**Context:** A module targets an instruction set, not a machine or a product
family. "System Z" names IBM's mainframe line, whose main instruction set is
z/Architecture, called `s390x` by Linux, Debian and QEMU (GCC says `s390`,
LLVM `SystemZ`). The steward cites a report (see `cites:`) that IBM's next
mainframe processor runs Arm workloads as well as Z workloads on the same
cores, so the machine name no longer identifies one instruction set.
**Decision:** `system-z/` became `mod/arch/s390x/`, and `specops.system-z`
became `specops.arch.s390x`. The other modules already follow the rule:
`6502`, `m68k`, `superh`, `wasm`, `x86`, `z80`. Variants and older modes of an
instruction set belong to its module, through execution modes and
`:for-types`, as x86's modes do.
**Alternatives:** Keeping `system-z` (rejected: names a machine family that
may host more than one instruction set). `systemz`, after LLVM, or
`z-architecture` (rejected: `s390x` is the more widely used target name).

### SPECOPS-DRAFT-restructure-D4 — Object formats are modules separate from architectures

**Status:** Proposed
**Context:** An instruction set can be written in more than one format: s390x
code goes into ELF on Linux and into GOFF on z/OS. GOFF belongs to an
operating system, not an instruction set. WebAssembly is both an instruction
set and a binary module format.
**Decision:** Formats are modules in `mod/format/`. `goff.lisp` went to
`mod/format/goff/`, not into the s390x module. WebAssembly is split: the
instruction set stays in `mod/arch/wasm/` (`base.lisp`, `ops.lisp`) and the
module writer, `program.lisp`, went to `mod/format/wasm/`. Dependencies run
one way: map modules depend only on the core; formats depend on the core and
map modules, and on an architecture only when the format exists for that one
instruction set, as WebAssembly's does; architectures depend on the core and
map modules, never on formats. Architectures pass facts to formats through
generic functions of the core program protocol, such as
`src/program.lisp:asm-elf-machine@13b5887`.
**Alternatives:** Putting GOFF in the s390x module (rejected: s390x also
emits ELF, and GOFF is tied to z/OS). Keeping WebAssembly whole in
`mod/arch/` (rejected: the module format is a format like the others).
Defining `asm-elf-machine` in the ELF module, with one small glue system per
architecture (deferred: more systems, for no present need).

### SPECOPS-DRAFT-restructure-D5 — Two-way mapping tables are map modules

**Status:** Proposed
**Context:** `formats/` held the CP437 and EBCDIC code tables, defined with
`defcodetable`, which are used as string codecs by manifests. They are not
formats. The steward expects to add other mappings that can be expressed as
tables. Three names were considered: `code`, which already means machine
code, the `code` section of `program` and encoded values; `table`, which
already means operation tables (`:tabular`) and instruction tables; and
`map`, which already means the manifest layer's region map and the opcode maps
of the Z80 and 6502 manuals.
**Decision:** The directory is `mod/map/`, with systems
`specops.map.ebcdic` and `specops.map.cp437`, each in its own subdirectory and
`.asd` file. A map module holds a two-way mapping between value spaces that
belongs to no single architecture or format. "Map" names what the module does
rather than how it is written, so mappings that are not tables, such as
variable-width text encodings, can also live there. An opcode map belongs to
its architecture module.
**Alternatives:** `mod/code/` (rejected: the most overloaded word in the
codebase). `mod/table/` (rejected: names the representation, and would
exclude mappings computed rather than tabulated). `mod/charset/` (rejected:
too narrow for non-character mappings).

### SPECOPS-DRAFT-restructure-D6 — `specops.lisp` is split by subsystem, with every definition moved unchanged

**Status:** Proposed
**Context:** `specops.lisp:marshal@738105a` and the rest of the manifest
layer, which is where the current work happens, shared a file of 2,547 lines
with the assembler core. The two have no dependency on each other beyond the
serialization and masque functions they share.
**Decision:** The file is replaced by `src/serial.lisp` (serialization,
`defcodetable`, `masque` and `unmasque`), `src/manifest/manifest.lisp`
(`defenum`, `defmanifest`, `marshal`, `unmarshal` and their helpers) with
`scan.lisp` moved beside it, and `src/assembler.lisp` (from `mqbase` to the
end). Two definitions changed places: `deserializer-for`, the inverse of
`serializer-for`, went to `src/serial.lisp:deserializer-for@13b5887`, and the
unused checksum helper `cs-adapt` to `src/checksum.lisp:cs-adapt@13b5887`.
Commented-out code stays with the part of the file it sat in. The split was
made in a commit separate from the moves, with no edits to the moved
definitions.
**Alternatives:** Keeping one file (rejected: the manifest layer would stay
buried). Splitting the assembler core further, into operands, assembler
classes and specification macros (deferred: the split by subsystem was the
needed step, and a finer split is easier once the manifest work settles).
Splitting in the same commit as the moves (rejected: keeping moves and content
changes apart lets history follow the moved files).

## Implementation

### Directory moves

Commit `0ac7223` moved every tracked file with `git mv`, so that Git records
renames and `git log --follow` traces each file:

- `package.lisp`, `checksum.lisp`, `scan.lisp`, `specops.lisp` and
  `program.lisp` to `src/`;
- `manifest.lisp` and `test-scripts/*.lisp` to `test/`;
- `6502/`, `m68k/`, `superh/`, `wasm/`, `x86/` and `z80/` to `mod/arch/`, and
  `system-z/` to `mod/arch/s390x/`
  (SPECOPS-DRAFT-restructure-D3);
- `wasm/program.lisp` to `mod/format/wasm/program.lisp`, and `elf.lisp` and
  `goff.lisp` to `mod/format/elf/` and `mod/format/goff/`
  (SPECOPS-DRAFT-restructure-D4);
- `formats/ebcdic.lisp` and `formats/cp437.lisp` to `mod/map/ebcdic/` and
  `mod/map/cp437/` (SPECOPS-DRAFT-restructure-D5).

Each architecture's `.asd` file was renamed for its new system. Unused files
moved with their modules unchanged: `x86/base.old.lisp`, `x86/ops.main2.lisp`,
`superh/ops-nyi.lisp`, and the scraper `z80/comber.lisp`, which needs a
library and is kept outside every system (SPECOPS-DRAFT-specops-D1).
Untracked compiled files and editor backups were moved with their sources.

### System and package renames

Commit `ada2d19` renamed systems and packages per
SPECOPS-DRAFT-restructure-D2, in `defsystem`, `defpackage`, `in-package` and
`find-package` forms, qualified symbols and comments. For example:

- `mod/arch/s390x/package.lisp:specops.arch.s390x@ada2d19` defines the
  package formerly `specops.system-z`;
- `mod/format/wasm/program.lisp:emit-program@ada2d19` remains in the package
  `specops.arch.wasm`, since giving the format module its own package would
  have meant more than a rename;
- `mod/map/ebcdic/ebcdic.lisp:ebcdic-code-cp037@ada2d19` is now in the
  package `specops.map.ebcdic`, and the manifest suites and a commented
  example in `specops.lisp` refer to it there.

The test scripts' `(load …)` paths and `in-package` forms were updated to the
new locations. The temporary file name `/tmp/test-specops.wasm` in
`test-emit.lisp` was deliberately left alone.

### `specops.asd` and the map systems

The core system gained `:pathname "src/"`, and `specops/demo.png` gained
`:pathname "test/"` and now depends on `specops.map.ebcdic`. The secondary
systems `specops/format.ebcdic` and `specops/format.cp437` were removed from
`specops.asd` and became `mod/map/ebcdic/specops.map.ebcdic.asd` and
`mod/map/cp437/specops.map.cp437.asd`, with the same descriptions and
dependencies. No systems were created for the format modules: ELF, GOFF and
the WebAssembly writer were in no system before, and remain so.

Quicklisp's local-projects index caches the location of each `.asd` file, so
`(ql:register-local-projects)` must be run once after the move for the new
systems to be found.

### Split of `specops.lisp`

Commit `13b5887` replaced `specops.lisp` per SPECOPS-DRAFT-restructure-D6.
The new files were cut from line ranges of the old one, each under a header
and an `in-package` form:

- `src/serial.lisp:serialize@13b5887` and the serialization functions, the
  `join*` functions, `flipbits`, `defcodetable`, and
  `src/serial.lisp:masque@13b5887` with `quantify-mask-string` and
  `unmasque`, followed by `deserializer-for`;
- `src/manifest/manifest.lisp:defmanifest@13b5887`, with `codec-for`, the
  `*manifests*` and `*enums*` registries, `defenum`, `marshal`, `enumerated`,
  `unmarshal`, the helpers between them, and the commented PNG sketch, LEB128
  encoders and `make-masquer`;
- `src/assembler.lisp:mqbase@13b5887` and everything after it: operand and
  storage classes, assembler classes and generic functions, the specification
  macros, `assemble`, `interpret` and the commented-out code at the end;
- `cs-adapt`, appended to `src/checksum.lisp`.

`scan.lisp` moved to `src/manifest/scan.lisp`. The core system now loads
`package`, `checksum`, `serial`, the `manifest` module (`scan`, then
`manifest`), `assembler` and `program`. The test scripts, which load source
files directly, load `serial.lisp`, `manifest/manifest.lisp` and
`assembler.lisp` in place of `specops.lisp`. As before, they load neither
`checksum.lisp` nor `scan.lisp`.

## Design Properties

- **No change in behavior.** Every definition moved unchanged; only package
  names, paths and header comments were edited.
- **History follows the moves.** Git records 53 renames in `0ac7223`. Lines from the
  split are traced back to `specops.lisp` by `git blame -C -C`.
- **Existing code references stay valid.** References in the other documents
  are pinned to revisions before the move, such as `1e53ae7`, and resolve
  there.

## Verification

Each check was run on `738105a` before the work, and again after each step.
The final runs were at `3dac8bc`, which adds only an unloaded backup copy of
the manifest files to the code of `13b5887`.

**Manifest suites.** Run via:

```
sbcl --non-interactive --eval '(asdf:load-system :specops/demo.png :force t)' \
  --eval '(specops::run-png-demo)' --eval '(specops::run-unmarshal-demo)' \
  --eval '(specops::run-chunk-unmarshal-demo)' --eval '(specops::run-masque-demo)' \
  --eval '(specops::run-string-demo)' --eval '(specops::run-pad-demo)' \
  --eval '(specops::run-terminated-demo)' --eval '(specops::run-leb-demo)' \
  --eval '(specops::run-case-demo)' --eval '(specops::run-vec-scan-demo)'
```

Result: the PNG structural verification and 135 checks in nine suites, all
passing, with output identical to the run at `738105a`. The first run after
each change printed three compiler notes, from the first compilation of the
core at its new paths; later runs printed none.

**Compiler warnings.** The core system was compiled fresh, with warnings
collected, at `ada2d19` and in the split tree. Both produced the same 49
distinct warnings, so the split changed no load-order dependency.

**Content.** The 2,330 non-blank lines of `specops.lisp` after its header
appear exactly once across the new files; the lines dropped at the cut
points are blank.

**Test scripts.** Each of `test/test-build.lisp`, `test/test-emit.lisp`,
`test/test-goff.lisp`, `test/test-program.lisp` and
`test/test-wasm-macro.lisp`, run from the repository root with
`sbcl --noinform --non-interactive --load test/<name>.lisp`, ends with its
"All … tests passed" line, as at `738105a`.

**Architecture systems.** Each was loaded with `asdf:load-system` in a clean
image, before and after.

Table: Load results of the architecture systems at commits `738105a` and
`13b5887`.

| System at `738105a` | System at `13b5887` | Result, unchanged |
|---|---|---|
| `specops.6502` | `specops.arch.6502` | Loads |
| `specops.m68k` | `specops.arch.m68k` | Fails: `*m68k-layout*` is unbound |
| `specops.superh` | `specops.arch.superh` | Fails: invalid initarg `:type` |
| `specops.system-z` | `specops.arch.s390x` | Loads |
| `specops.wasm` | `specops.arch.wasm` | Fails: no dependency on `specops` |
| `specops.x86` | `specops.arch.x86` | Loads |
| `specops.z80` | `specops.arch.z80` | Loads |

The three failures are the load-health problems of
[SPECOPS-DRAFT-specops-O5](Arch.SpecOps.md#specops-draft-specops-o5--how-is-a-modules-conformance-verified),
and were left as they were. Both map systems load.

**Documents.** `compass check` reports no errors.

## Files

Table: Files moved, added or changed from commit `738105a` to commit
`13b5887`.

| File | Action | Description |
|------|--------|-------------|
| `src/package.lisp`, `src/checksum.lisp`, `src/program.lisp` | Moved | from the repository root; `checksum.lisp` gains `cs-adapt` |
| `src/serial.lisp` | **New** | serialization, `defcodetable`, `masque`, from `specops.lisp` |
| `src/manifest/manifest.lisp` | **New** | `defenum`, `defmanifest`, `marshal`, `unmarshal`, from `specops.lisp` |
| `src/manifest/scan.lisp` | Moved | from `scan.lisp`, unchanged |
| `src/assembler.lisp` | **New** | the assembler core, from `specops.lisp` |
| `specops.lisp` | Deleted | split into the three files above |
| `specops.asd` | Modified | `:pathname` for the core and the demo; `manifest` module; map systems removed |
| `mod/arch/<isa>/*` | Moved | the seven architecture modules; `system-z` becomes `s390x`; systems and packages renamed |
| `mod/format/elf/elf.lisp`, `mod/format/goff/goff.lisp` | Moved | from the repository root |
| `mod/format/wasm/program.lisp` | Moved | from `wasm/program.lisp` |
| `mod/map/ebcdic/ebcdic.lisp`, `mod/map/cp437/cp437.lisp` | Moved | from `formats/`; packages renamed |
| `mod/map/ebcdic/specops.map.ebcdic.asd`, `mod/map/cp437/specops.map.cp437.asd` | **New** | the map systems, formerly secondary systems in `specops.asd` |
| `test/manifest.lisp` | Moved | from the repository root; refers to `specops.map.ebcdic` |
| `test/test-*.lisp` | Moved | from `test-scripts/`; load paths and packages updated |
| `doc/Log.Restructure.md` | **New** | this log |

## Metrics

- Files moved: 53 in `0ac7223`, one of which, `specops.lisp`, was split in
  `13b5887`.
- Files added: 5 (3 from the split, 2 `.asd` files).
- Files deleted: 1 (`specops.lisp`).
- Test checks: 135 plus 1 PNG verification, and 5 test scripts; regressions: 0.

## Outstanding Work

- **The README** still loads `specops.m68k`, `specops.x86` and
  `specops.system-z`, and enters their packages by the old names.
- **The architecture document.** The Project structure appendix and the
  module table of [SPECOPS-DRAFT-specops](Arch.SpecOps.md) describe commit
  `1e53ae7`. A current layout, the dependency rule of
  SPECOPS-DRAFT-restructure-D4 and the naming rule of
  SPECOPS-DRAFT-restructure-D3 belong in an update there. The Glossary needs
  entries for architecture, format and map modules.
- **Format systems.** `mod/format/elf/`, `mod/format/goff/` and
  `mod/format/wasm/` have no `.asd` files. `specops.format.wasm` would depend
  on `specops.arch.wasm`, whose system first needs a dependency on `specops`,
  and its file still uses the architecture's package.
- **GOFF constants.** The GOFF attribute constants are still in
  `mod/arch/s390x/program.lisp`, with z/OS program-layout helpers. Moving the
  constants to `mod/format/goff/` splits that file. The helpers are platform
  conventions, a third axis beside instruction set and format; they can stay
  in s390x until a platform layer is needed.
- **Tests behind `test-op`.** The test scripts load source files directly, and
  `specops/demo.png` combines PNG manifests with the manifest suites. Suites
  for the core and each module, run by ASDF's `test-op`, would replace both
  (SPECOPS-DRAFT-specops-O5). The PNG manifests stay a test case: image
  formats are outside SpecOps's scope.
- **Internal names.** `*assembler-prototype-z*` and `assembler-z` keep the
  System Z naming; several `.asd` files still carry placeholder descriptions
  and authors.
- **Unused files.** `x86/base.old.lisp`, `x86/ops.main2.lisp` and
  `superh/ops-nyi.lisp` should be removed or archived; `z80/comber.lisp` could
  move to a `tools/` directory.
- **A finer split of `src/assembler.lisp`**, deferred by
  SPECOPS-DRAFT-restructure-D6.

## Update 2026-10-31 — Format systems, README and documents

This installment covers commit `50e231d`, which completes four items of the
Outstanding Work above. It gives the format modules ASDF systems, lets the
WebAssembly architecture system load, updates the README to the new system
names, and brings the architecture document and the glossary up to date with
the new layout. It also records the steward's decision on the scope of format
modules.

### SPECOPS-DRAFT-restructure-D7 — Image and media formats are outside SpecOps; PNG is a test case

**Status:** Proposed
**Context:** The manifest layer was built and tested on PNG, and the PNG
manifests in `test/manifest.lisp` describe a real file format. With format
modules in place, PNG could have become one. But SpecOps is a framework for
assemblers and the files that carry programs, and image formats bring
concerns of their own, such as compression and pixel layouts.
**Decision:** Format modules are for the object and container formats that
carry programs. The PNG manifests stay in `test/` as a test case for the
manifest layer and are not a module. Native readers for image formats such as
PNG, JPEG and GIF belong in a project of their own that depends on SpecOps.
**Alternatives:** A PNG format module, `specops.format.png` (rejected by the
steward: an image format is outside SpecOps's scope, and adding it would be
major scope creep). Removing the PNG manifests (rejected: they are the
manifest layer's main test case).

### Implementation

- **Format systems.** Each format module has a system that depends on
  `specops`: `mod/format/elf/specops.format.elf.asd:specops.format.elf@50e231d`
  and `mod/format/goff/specops.format.goff.asd:specops.format.goff@50e231d`.
  The GOFF writer carries its own EBCDIC table, so it does not depend on
  `specops.map.ebcdic`.
  `mod/format/wasm/specops.format.wasm.asd:specops.format.wasm@50e231d` also
  depends on `specops.arch.wasm`, the one case
  SPECOPS-DRAFT-restructure-D4 permits. Its file is still in the package
  `specops.arch.wasm`.
- **WebAssembly architecture system.**
  `mod/arch/wasm/specops.arch.wasm.asd:specops.arch.wasm@50e231d` now depends
  on `specops`, the cause of its load failure in
  [SPECOPS-DRAFT-specops-O5](Arch.SpecOps.md#specops-draft-specops-o5--how-is-a-modules-conformance-verified).
  Its placeholder description and author are unchanged.
- **README.** The `load-system` and `in-package` examples name
  `specops.arch.m68k`, `specops.arch.x86` and `specops.arch.s390x`, and the
  System Z section explains that the module is named `s390x` for its
  instruction set. Mentions of System Z as a platform are unchanged.
- **Architecture document.** [SPECOPS-DRAFT-specops](Arch.SpecOps.md) now
  describes, in Module design, the three module kinds, the naming rule and
  the dependency rule, citing SPECOPS-DRAFT-restructure-D1 through
  SPECOPS-DRAFT-restructure-D5 rather than restating them. Its
  object-format and binary-format sections name the format and map modules
  and state SPECOPS-DRAFT-restructure-D7. Its Project structure appendix shows
  the new layout, and notes on SPECOPS-DRAFT-specops-O5, Roadmap step 4 and
  the module table record what has changed since `1e53ae7`.
- **Glossary.** [SPECOPS-DRAFT-glossary](Glossary.SpecOps.md) has a new
  table defining core, module, architecture module, format module, map
  module and opcode map, and its entry for code tables names map modules.

### Verification

At `50e231d`, in clean images:

- The manifest suites, run with the command under Verification above, give
  the PNG verification and 135 passing checks, with output identical to the
  run at `738105a`.
- `specops.arch.wasm`, `specops.format.elf`, `specops.format.goff` and
  `specops.format.wasm` each load with `asdf:load-system` and no full
  warnings. `specops.arch.wasm` failed to load before this commit.
- With `specops.arch.s390x` and the three format systems loaded through ASDF,
  `emit-program` has its `:elf`, `:goff` and `:wasm` methods.
- The five test scripts pass.
- `compass check` reports no errors.

### Files

Table: Files added or changed in commit `50e231d`.

| File | Action | Description |
|------|--------|-------------|
| `mod/format/elf/specops.format.elf.asd` | **New** | the ELF format system |
| `mod/format/goff/specops.format.goff.asd` | **New** | the GOFF format system |
| `mod/format/wasm/specops.format.wasm.asd` | **New** | the WebAssembly format system |
| `mod/arch/wasm/specops.arch.wasm.asd` | Modified | depends on `specops` |
| `README.md` | Modified | system and package names; why the System Z module is `s390x` |
| `doc/Arch.SpecOps.md` | Modified | module kinds and rules, format and map modules, scope of formats, layout |
| `doc/Glossary.SpecOps.md` | Modified | terms for the core and the module kinds |
| `doc/Log.Restructure.md` | **New** | this log |

### Outstanding Work

The README, the architecture document, the glossary and the format systems
items above are done. The rest remains:

- **GOFF constants**, still in `mod/arch/s390x/program.lisp`.
- **The WebAssembly format module's package**, still `specops.arch.wasm`;
  and the architecture's prototype `*assembler-prototype-wasm*`, still
  unbound, with `ops.lisp` outside the system.
- **Tests behind `test-op`**, deferred by the steward.
- **Internal names and placeholder `.asd` metadata.**
- **Unused files**, and `z80/comber.lisp`.
- **A finer split of `src/assembler.lisp`.**
