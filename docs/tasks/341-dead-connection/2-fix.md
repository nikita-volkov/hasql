# Task 2 of 2: fix #341

Issue: <https://github.com/nikita-volkov/hasql/issues/341>. Branch: `v2.0`. Prerequisite: [task 1](1-reproduce.md) is done. Its spec is red and its Findings section is filled in. Read Findings first, because the raw libpq sequence there settles the timing question in the rule below.

## Constraint: no breaking change

`v2.0` has had no breaking change since `v2.0.1.0`, and this fix keeps it that way.

- Public modules keep their exports, types and constructors. The new constructor goes in `Hasql.Comms.Recv`, which belongs to the private `comms` sublibrary.
- The public error reported for a dead connection is the existing `ConnectionSessionError`.
- The visible change is a reclassification. A dead connection used to surface as `StatementSessionError` or `ScriptSessionError` (sometimes with a SQLSTATE such as `57P01`), or as `DriverSessionError`. It now surfaces as `ConnectionSessionError`, and any SQLSTATE appears only in the message text. The maintainer chose this deliberately, so do not narrow it.

## The rule

> A command's outcome is **connection lost** when `Pq.status` is `ConnectionBad` after the command's results have been consumed. It does not matter what the results contained.

The rule checks connection status rather than "the result has no SQLSTATE", so it covers both the silent close and a server FATAL (`57P01`, `57P02`). It checks after consumption rather than on the first result because libpq can return a server FATAL before it has read the EOF that follows it. The Findings section shows whether that happens on each adapter. If a cell shows status turning bad only on a later `getResult`, that is the reason for this design.

## Changes

1. **`Hasql.Comms.Recv`**
   - Add `ConnectionLostError context (Maybe ByteString)`, with haddock, and extend the `Comonad` instance.
   - For details, use the failing result's `Pq.resultErrorMessage` when there is one. Otherwise use `Pq.errorMessage connection`.
   - **`singleResult`**: once both `getResult` calls have run, whatever the first one returned, check `Pq.status`. If it is `ConnectionBad`, return `ConnectionLostError`. This takes precedence over `NoResultsError`, `TooManyResultsError` and the decoder's verdict.
   - **`allResults`**: when the loop ends, check `Pq.status` the same way. A lost connection takes precedence over any `ResultError` already collected.
   - **`<*>`**: if either side is `ConnectionLostError`, return it. Otherwise keep first-error-wins. This matters for pipelines, where an earlier statement's ordinary server error would otherwise hide the connection loss reported by a later step or by the sync.
2. **`Hasql.Comms.ResultDecoder.serverError`**: when `DiagMessagePrimary` is absent, fall back to `Pq.resultErrorMessage` so that an error libpq makes up on a live connection no longer renders as `""`. Keep `code` as it is.
3. **`Hasql.Engine.Errors`**
   - In `fromRecvError` and `fromRecvErrorInScript`, map `ConnectionLostError` to `ConnectionSessionError (maybe "" decodeUtf8Lenient details)`.
   - `fromRoundtripError` goes through `fromRecvError`, so it is covered too.
   - Add a comment at the mapping that states the reason: pools drop connections on this constructor, and `isTransient` reports it as retryable.
4. **`Hasql.Engine.Contexts.Pipeline`**: check that `extract` on a `ConnectionLostError` still produces the context whose statement cache is kept. No change is expected.
5. **CHANGELOG.md**: add a `# Upcoming` section with a `## Fixes` entry. It should say that a connection lost before or during a receive is reported as `ConnectionSessionError`, list what it used to be reported as, and link #341.

Leave out of scope:
- Finishing the connection inside `use`. That is the redesign `master` carries, and it is breaking.
- libpq's default notice processor printing to stderr.

## Done when

- Every cell of task 1's spec is green on both adapters and both distros.
  - If a `pqi-native` cell stays red because the adapter never reports `ConnectionBad`, stop and report it as a `pqi-native` bug. Do not work around it in hasql.
- `library-tests`, `comms-tests` and `connection-state-tests` all pass.
- `git diff v2.0.1.0 -- src/library` shows no change to any exposed module's export list or exported types.
- Ormolu has been run on every changed `.hs` file, and the CHANGELOG entry is in place.
