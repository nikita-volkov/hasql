# Test organisation

`src/library-tests` is organised in two independent dimensions: first by what a
test needs from the environment, then by what it tests.

## By environment

- `Pure/` - no database at all.
- `Integration/Sharing/` - safe to run against a shared database. The shared
  hook (`Integration/Sharing/SpecHook.hs`) starts one container per Postgres
  distro and hands each spec a `Scripts.ScopeParams` triple of adapter, host
  and port.
- `Integration/Isolated/` - needs its own database or its own connection
  lifecycle.

`Integration/SpecHook.hs` sits above both integration categories and iterates
the pqi adapters once, so every integration spec runs against `pqi-ffi` and
`pqi-native` alike. `Pure/` is outside that hook, since it needs no adapter.

## By unit under test

Within each environment category, a spec's path traces back to what it tests
in the library's public API. There is no `ByFeature/` or `ByBug/` - every test
is filed under the module and, where useful, the definition it exercises. The
path takes one of three shapes:

- **`[Module]Spec.hs`** - the module doesn't split meaningfully by individual
  definition, or the tests are inherently about the module as a whole (e.g.
  `ErrorsSpec.hs`, since the error constructors are exercised together, not
  one per file). Also used when a module has only one export worth testing
  (e.g. `PipelineSpec.hs` - `Hasql.Pipeline` exports only `statement`).

- **`[Module]/[Definition]Spec.hs`** - the common case: one file gathering the
  general-purpose scenarios for a single exported function or type, as
  `describe`/`it` blocks. E.g. `Connection/UseSpec.hs` for `Connection.use`,
  `Session/ScriptSpec.hs` for `Session.script`.

- **`[Module]/[Definition]/[Case]Spec.hs`** - reserved for a single scenario
  substantial enough to clutter the shared `[Definition]Spec.hs` if inlined. A
  case earns its own file when it has *any* of:
  - dedicated top-level helper functions or values used only by that
    scenario (not shared `Helpers/*`);
  - a multi-paragraph explanatory comment (a subtle race, a bug/commit
    reference, a non-obvious mechanism);
  - a parameter sweep or statistical/repeated-attempt loop rather than a
    single deterministic run.

  Otherwise the scenario stays as a `describe`/`it` block merged into
  `[Definition]Spec.hs`. A definition can have both a shared
  `[Definition]Spec.hs` and one or more isolated `[Definition]/[Case]Spec.hs`
  files side by side, e.g. `Decoders/CompositeSpec.hs` alongside
  `Decoders/Composite/OidMismatchSpec.hs`.

## Other components

`connection-state-tests` covers that layer directly; `benchmarks` and
`profiling` are separate executables.
