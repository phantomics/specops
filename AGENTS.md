## Documentation — Compass

Project documentation follows **Compass** (Common Ontological Model for Prose
Artifacts by Structural Standard). Use the `compass-*` skills rather than
improvising documents:

- `compass-author` — scaffold a new document (Survey/Eval/Plan/Log/Ref/Guide/
  Spec/Arch/Glossary/Memo) with correct front-matter and per-genre section
  shape.
- `compass-review` — the toolchain's checks plus a judgment-level review before
  accepting a doc.
- `compass-lookup` — resolve identifiers, sections, records, and references.

The skills carry the standard and its reference bundle. Load @Compass.md on
demand only if you need the source text — do not preload it.

Run `compass check` before finishing any change to the documentation, and fix
the errors it reports in the files you changed. `compass help` lists the other
commands (`show`, `outline`, `refs`, `index`, `next`, `rules`, and others).
Numbers are assigned with `compass assign` only when a person asks for it; never
edit the ledger, `doc/REGISTRY.sexp`, by hand.

Project-specific facts the skills cannot infer:

- Compass namespace: SPECOPS (steward Andrew Sengul, solo approval; see
  `compass.sexp`)
- Docs directory: doc/
- The `compass` command: not on the PATH; run
  `/home/sloane/src/lisp/compass/bin/compass`
- Federated repositories: none
- Migration: none; every document in doc/ carries front-matter. `README.md`
  is not a Compass document.

Essentials (full rules in Compass.md): every document opens with a §7 YAML
front-matter block; genre and status use the controlled vocabularies; decision
records are ADR-shaped (`D`-records); new documents and their records keep
provisional `SPECOPS-DRAFT-<slug>` identifiers until a steward assigns
numbers; code references in Log/Plan are commit-pinned (`path:symbol@revision`),
never bare line numbers; documents authored with LLM help carry `provenance:`;
an assistant never sets `approved-by` or an authoritative status.

## Testing — Square (Common Lisp)

Automated tests in this project follow **Square** (Software QUality Assurance
Regimen & Ethos). Use the `square-*` skills rather than improvising a suite:

- `square-author` — scaffold the test system and write conforming `def-test`s.
- `square-review` — judgment-level conformance review before accepting tests.
- `square-run` — run the suite via the ASDF `test-op` seam and triage failures.

The skills carry the standard and its reference bundle. Load @Square.md on
demand only if you need the source text — do not preload it.

Project-specific facts the skills cannot infer:

- Compass namespace: SPECOPS
- Test directory: test/
- Test-system form: secondary `specops/test`, defined in `specops.asd`
- Run command: `sbcl --non-interactive --eval '(asdf:test-system :specops)'`
- Suites: root `specops`; `serial`; `manifest`, with `manifest-basics`,
  `-fields`, `-records`, `-variable`, `-reads`, `-destinations` and
  `-formats`, one `test-*.lisp` file each. No `*-slow` suite exists yet.
- The test package `specops-tests` imports the SpecOps internals it uses as an
  explicit list in `test/package.lisp`; add a symbol there rather than writing
  `specops::`.
- Manifests used by more than one test file go in `test/helpers.lisp`; a
  manifest must be defined before the tests that marshal or unmarshal it are
  compiled.
- Isolation: the tests hold no global state, so there is no `with-clean-*`
  fixture; each test builds its own data, and files go through
  `uiop:with-temporary-file`.
- The scripts in `scripts/` are not under Square (SPECOPS-DRAFT-manifest-D67).
- Library dependencies (FiveAM, cl-hamcrest) are allowed in the test system
  only (SPECOPS-DRAFT-specops-D1).

Essentials (full rules in Square.md): FiveAM + cl-hamcrest only — no homegrown
assertion macros or alternative frameworks; every test isolated via a
`with-clean-*` fixture; no bare `sleep` in async tests (poll with a timeout);
every `def-test` opens with a docstring stating the invariant it proves.
