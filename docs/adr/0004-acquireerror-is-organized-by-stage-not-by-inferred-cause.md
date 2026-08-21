# `AcquireError` is organized by stage, not by inferred cause

> **Status: Superseded by [ADR 0005](0005-acquireerror-is-a-flat-set-of-observed-failures.md)**
>
> This document proposed deleting `AcquireError`'s substring-matching classifier and replacing the
> constructor set with one constructor per stage of `Hasql.Connection.acquire`, with the surviving
> facts - `PQconnectionNeedsPassword`, and the two knowledge levels of a session-initialization
> failure - demoted to a `Bool` and an `Either` field.
>
> ADR 0005 accepts in full the argument about *which facts the type may publish* and rejects only
> the grouped shape derived from it: the two shapes are isomorphic, so the observability argument
> cannot choose between them, and on reader cost the flat set wins. That argument, and the case
> against the classifier that both documents share, now live in 0005 in full, so nothing is
> preserved here that is not said better there.

See [ADR 0005](0005-acquireerror-is-a-flat-set-of-observed-failures.md).
