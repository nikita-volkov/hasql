#### 1.3.0.3

  - Fix the docs (see [f1d2015]( https://github.com/nikita-volkov/hasql/commit/f1d2015 ));
  - README Change: Trivial to hopefully easier (see [#93]( https://github.com/nikita-volkov/hasql/pull/93 ));
  - Merge branch 'semigroup-fixes' (see [3514b62]( https://github.com/nikita-volkov/hasql/commit/3514b62 ));

#### 1.3.0.2

  - Downgrade GHC in Travis (see [0c19027]( https://github.com/nikita-volkov/hasql/commit/0c19027 ));
  - Update the Travis setup (see [9b7d9b3]( https://github.com/nikita-volkov/hasql/commit/9b7d9b3 ));

#### 1.3.0.1

  - Restore the correct docs (see [e50dd83]( https://github.com/nikita-volkov/hasql/commit/e50dd83 ));
  - Rename Query to Statement (see [509dafd]( https://github.com/nikita-volkov/hasql/commit/509dafd ));

#### 1.3

  - Fix a typo in the docs (see [1516b51]( https://github.com/nikita-volkov/hasql/commit/1516b51 ));
  - Rename some result decoders (see [726097e]( https://github.com/nikita-volkov/hasql/commit/726097e ));

#### 1.2

  - Make the codec names more consistent (see [8eb3830]( https://github.com/nikita-volkov/hasql/commit/8eb3830 ));
  - Reimplement to add support for query param printing in failed queries (see [ea4874c]( https://github.com/nikita-volkov/hasql/commit/ea4874c ));
  - Update the Tasty dep (see [3b4d713]( https://github.com/nikita-volkov/hasql/commit/3b4d713 ));
  - Fix readme link for real (#91) (see [75c0d37]( https://github.com/nikita-volkov/hasql/commit/75c0d37 ));
  - Fix Readme link (see [#90]( https://github.com/nikita-volkov/hasql/pull/90 ));
  - Allow tasty-quickcheck 0.10 (see [#88]( https://github.com/nikita-volkov/hasql/pull/88 ));
  - Allow tasty 1.0 (see [#87]( https://github.com/nikita-volkov/hasql/pull/87 ));
  - Allow tasty-hunit 0.10 (see [#84]( https://github.com/nikita-volkov/hasql/pull/84 ));
  - Migrate from EitherT to ExceptT (see [b806c6e]( https://github.com/nikita-volkov/hasql/commit/b806c6e ));

#### 1.1.1

  - Update the test deps (see [32018a6]( https://github.com/nikita-volkov/hasql/commit/32018a6 ));
  - Update the version (see [4622f29]( https://github.com/nikita-volkov/hasql/commit/4622f29 ));

#### 1.1

  - Merge branch 'pr/81' (see [732194b]( https://github.com/nikita-volkov/hasql/commit/732194b ));
  - Fix the Travis config (see [aa7b373]( https://github.com/nikita-volkov/hasql/commit/aa7b373 ));

#### 1

  - Update the benchmark (see [3e6ce84]( https://github.com/nikita-volkov/hasql/commit/3e6ce84 ));
  - Update Travis (see [39a54c7]( https://github.com/nikita-volkov/hasql/commit/39a54c7 ));

#### 0.19.18.2

  - Update Travis (see [5dac210]( https://github.com/nikita-volkov/hasql/commit/5dac210 ));
  - Reversed evaulation order when executing dropRemaining. (see [#78]( https://github.com/nikita-volkov/hasql/pull/78 ));
  - Update the package version (see [e650bc5]( https://github.com/nikita-volkov/hasql/commit/e650bc5 ));

#### 0.19.18.1

  - Add GHC 8.2.1 to the Travis configuration (see [771cc1c]( https://github.com/nikita-volkov/hasql/commit/771cc1c ));
  - Merge branch 'pr/77' (see [403ca85]( https://github.com/nikita-volkov/hasql/commit/403ca85 ));
  - Connect through socket in benchmarks (see [2d5a248]( https://github.com/nikita-volkov/hasql/commit/2d5a248 ));
  - Merge branch 'master' of https://github.com/nikita-volkov/hasql (see [810adac]( https://github.com/nikita-volkov/hasql/commit/810adac ));
  - Add rtsopts to benchmark (see [1127e75]( https://github.com/nikita-volkov/hasql/commit/1127e75 ));
  - Add profiling (see [52adc79]( https://github.com/nikita-volkov/hasql/commit/52adc79 ));
  - Revert "Enlarge the benchmark samples" (see [22b5f8b]( https://github.com/nikita-volkov/hasql/commit/22b5f8b ));
  - Enlarge the benchmark samples (see [e41f83f]( https://github.com/nikita-volkov/hasql/commit/e41f83f ));
  - New benchmarks (see [e9cf07f]( https://github.com/nikita-volkov/hasql/commit/e9cf07f ));
  - Update a builder dep (see [1a2e7ee]( https://github.com/nikita-volkov/hasql/commit/1a2e7ee ));

#### 0.19.18

  - Migrate to "postgresql-binary-0.12.1" (see [7cba5bd]( https://github.com/nikita-volkov/hasql/commit/7cba5bd ));
  - Fix the compatibility with the newer "base-prelude" (see [2801829]( https://github.com/nikita-volkov/hasql/commit/2801829 ));

#### 0.19.17.1

  - Update the version (see [3e4c7c0]( https://github.com/nikita-volkov/hasql/commit/3e4c7c0 ));

#### 0.19.17

  - Support for `inet` data type. (see [#71]( https://github.com/nikita-volkov/hasql/pull/71 ));
  - Relax the "vector" dependency (see [b258dfe]( https://github.com/nikita-volkov/hasql/commit/b258dfe ));

#### 0.19.16

  - Merge branch 'pr/64' (see [b41ca06]( https://github.com/nikita-volkov/hasql/commit/b41ca06 ));
  - Update the deps (see [98de66f]( https://github.com/nikita-volkov/hasql/commit/98de66f ));
  - Comment about "IN" queries. (see [#63]( https://github.com/nikita-volkov/hasql/pull/63 ));
  - Add tests for "IN" and "NOT IN" simulations (see [a1a5e15]( https://github.com/nikita-volkov/hasql/commit/a1a5e15 ));
  - Fix the Composite decoder (see [08564ab]( https://github.com/nikita-volkov/hasql/commit/08564ab ));

#### 0.19.15.2

  - Fix the docs (see [a802f90]( https://github.com/nikita-volkov/hasql/commit/a802f90 ));

#### 0.19.15.1

  - Update README.md (see [5c56677]( https://github.com/nikita-volkov/hasql/commit/5c56677 ));
  - Let's be modest (see [16e88cc]( https://github.com/nikita-volkov/hasql/commit/16e88cc ));
  - Update README.md (see [e5d4a2b]( https://github.com/nikita-volkov/hasql/commit/e5d4a2b ));

#### 0.19.15

  - Update the "aeson" bounds (see [9276a11]( https://github.com/nikita-volkov/hasql/commit/9276a11 ));
  - Move all the private modules into the according namespace (see [af4583f]( https://github.com/nikita-volkov/hasql/commit/af4583f ));
  - Correct README (see [d0bc025]( https://github.com/nikita-volkov/hasql/commit/d0bc025 ));
  - Add references to the Cursor libraries (see [464a1eb]( https://github.com/nikita-volkov/hasql/commit/464a1eb ));
  - Update README.md (see [39b6bae]( https://github.com/nikita-volkov/hasql/commit/39b6bae ));
  - Update the version (see [a80697d]( https://github.com/nikita-volkov/hasql/commit/a80697d ));

#### 0.19.14

  - Style corrections (see [fe23060]( https://github.com/nikita-volkov/hasql/commit/fe23060 ));
  - Expose access to raw libpq connection (see [#55]( https://github.com/nikita-volkov/hasql/pull/55 ));
  - Update the Travis setup (see [5450369]( https://github.com/nikita-volkov/hasql/commit/5450369 ));
  - Update the version (see [32e7406]( https://github.com/nikita-volkov/hasql/commit/32e7406 ));

#### 0.19.13

  - Bump dlist upper bound to 0.9 (see [#53]( https://github.com/nikita-volkov/hasql/pull/53 ));
  - Add the GHC 8 target to the Travis configuration (see [398fb71]( https://github.com/nikita-volkov/hasql/commit/398fb71 ));
  - Relax the "data-default-class" dependency (see [d72ad54]( https://github.com/nikita-volkov/hasql/commit/d72ad54 ));

#### 0.19.12

  - Provide an instance of Decidable to the Params encoder (see [0023195]( https://github.com/nikita-volkov/hasql/commit/0023195 ));

#### 0.19.11

  - Fix #46 by updating the "postgresql-binary" dependency (see [09dafef]( https://github.com/nikita-volkov/hasql/commit/09dafef ));

#### 0.19.10

  - Reproduce the empty array issue (#46) (see [c883289]( https://github.com/nikita-volkov/hasql/commit/c883289 ));
  - Add threads test (see [4f23fe5]( https://github.com/nikita-volkov/hasql/commit/4f23fe5 ));
  - Update README.md (see [c0d96eb]( https://github.com/nikita-volkov/hasql/commit/c0d96eb ));
  - Update the version (see [508e095]( https://github.com/nikita-volkov/hasql/commit/508e095 ));

#### 0.19.9

  - Update PreparedStatementRegistry and fix the bug (see [b03ab41]( https://github.com/nikita-volkov/hasql/commit/b03ab41 ));
  - Failing prepared statements test (see [12a5e53]( https://github.com/nikita-volkov/hasql/commit/12a5e53 ));
  - Test prepared statements and errors (see [6cb8641]( https://github.com/nikita-volkov/hasql/commit/6cb8641 ));
  - Semigroup (see [7954c8d]( https://github.com/nikita-volkov/hasql/commit/7954c8d ));
  - Update README.md (see [1389b08]( https://github.com/nikita-volkov/hasql/commit/1389b08 ));
  - Update the deps (see [dba2c00]( https://github.com/nikita-volkov/hasql/commit/dba2c00 ));

#### 0.19.8

  - Raw JSON support (see [ff3dd81]( https://github.com/nikita-volkov/hasql/commit/ff3dd81 ));

#### 0.19.7

  - Update README.md (see [5ec981f]( https://github.com/nikita-volkov/hasql/commit/5ec981f ));
  - Update README.md (see [f321528]( https://github.com/nikita-volkov/hasql/commit/f321528 ));
  - Add at least a minor introduction (see [7618ae1]( https://github.com/nikita-volkov/hasql/commit/7618ae1 ));
  - Update the extensions (see [62bc811]( https://github.com/nikita-volkov/hasql/commit/62bc811 ));
  - Relax deps (see [80026b8]( https://github.com/nikita-volkov/hasql/commit/80026b8 ));

#### 0.19.6

  - JSONB (see [6135f5f]( https://github.com/nikita-volkov/hasql/commit/6135f5f ));

#### 0.19.5

  - Merge commit 'e89208e7826e6ec46ca6564cc9586d64ec5b7cbf' (see [b0431a9]( https://github.com/nikita-volkov/hasql/commit/b0431a9 ));
  - "custom" decoder (see [8527580]( https://github.com/nikita-volkov/hasql/commit/8527580 ));

#### 0.19.4

  - Update the version (see [5130f25]( https://github.com/nikita-volkov/hasql/commit/5130f25 ));

#### 0.19.3.3

  - Fix some potential bugs (see [9f8fc65]( https://github.com/nikita-volkov/hasql/commit/9f8fc65 ));
  - Fix the bug (see [24e3116]( https://github.com/nikita-volkov/hasql/commit/24e3116 ));
  - Reproduce a bug (see [b38a881]( https://github.com/nikita-volkov/hasql/commit/b38a881 ));

#### 0.19.3.2

  - Fix the bug (see [67576ac]( https://github.com/nikita-volkov/hasql/commit/67576ac ));
  - Reproduce the bug (see [129775f]( https://github.com/nikita-volkov/hasql/commit/129775f ));
  - Update the test DSL (see [9d3bc1c]( https://github.com/nikita-volkov/hasql/commit/9d3bc1c ));
  - Test Unknown (see [4e596bd]( https://github.com/nikita-volkov/hasql/commit/4e596bd ));

#### 0.19.3.1

  - A hotfix of the inverted integer-datetimes detection bug in encoders (see [25a66b2]( https://github.com/nikita-volkov/hasql/commit/25a66b2 ));
  - Add the "unknown" encoder (see [1871bde]( https://github.com/nikita-volkov/hasql/commit/1871bde ));

#### 0.19.3

  - Update the version (see [b723519]( https://github.com/nikita-volkov/hasql/commit/b723519 ));

#### 0.19.2

  - Merge origin/master (see [90b7868]( https://github.com/nikita-volkov/hasql/commit/90b7868 ));
  - MonadIO for Session (see [d4757f3]( https://github.com/nikita-volkov/hasql/commit/d4757f3 ));

#### 0.19.1

  - Move Settings to Connection (see [15be208]( https://github.com/nikita-volkov/hasql/commit/15be208 ));

#### 0.19

  - Return ConnectionError (see [032f3be]( https://github.com/nikita-volkov/hasql/commit/032f3be ));

#### 0.18

  - Revision (see [b777ae2]( https://github.com/nikita-volkov/hasql/commit/b777ae2 ));

#### 0.17

  - Session and Query Arrow (see [838bc8c]( https://github.com/nikita-volkov/hasql/commit/838bc8c ));

#### 0.16

  - Update the Travis setup (see [0e12cb0]( https://github.com/nikita-volkov/hasql/commit/0e12cb0 ));

#### 0.15.0.2

  - Update the version (see [e8f5c6a]( https://github.com/nikita-volkov/hasql/commit/e8f5c6a ));
  - Drop the unsafe freeing of result (see [11ea136]( https://github.com/nikita-volkov/hasql/commit/11ea136 ));
  - Update the tests and reproduce the bug (see [d5f5c90]( https://github.com/nikita-volkov/hasql/commit/d5f5c90 ));
  - Docs (see [12494a3]( https://github.com/nikita-volkov/hasql/commit/12494a3 ));

#### 0.15.0.1

  - Encoding/Decoding -> Encoders/Decoders (see [ef1b6ff]( https://github.com/nikita-volkov/hasql/commit/ef1b6ff ));

#### 0.15

  - Fixes (see [20677fa]( https://github.com/nikita-volkov/hasql/commit/20677fa ));

#### 0.14.0.2

  - Fixes (see [77cf80c]( https://github.com/nikita-volkov/hasql/commit/77cf80c ));
  - Merge branch 'master' of https://github.com/nikita-volkov/hasql (see [194d8dd]( https://github.com/nikita-volkov/hasql/commit/194d8dd ));
  - Docs correction (see [7ec4644]( https://github.com/nikita-volkov/hasql/commit/7ec4644 ));

#### 0.14.0.1

  - Acquire/release (see [8f95a93]( https://github.com/nikita-volkov/hasql/commit/8f95a93 ));

#### 0.14

  - Release (see [f4535f3]( https://github.com/nikita-volkov/hasql/commit/f4535f3 ));

#### 0.13.0.1

  - Correct docs (see [6fafd9c]( https://github.com/nikita-volkov/hasql/commit/6fafd9c ));
  - typo (see [0edd4d1]( https://github.com/nikita-volkov/hasql/commit/0edd4d1 ));
  - Deep isolation (see [93805d4]( https://github.com/nikita-volkov/hasql/commit/93805d4 ));

#### 0.13

  - Docs (see [1ab1bd1]( https://github.com/nikita-volkov/hasql/commit/1ab1bd1 ));

#### 0.12.1

  - Params unit (see [c5aad73]( https://github.com/nikita-volkov/hasql/commit/c5aad73 ));
  - Tuple instances (see [bd9174a]( https://github.com/nikita-volkov/hasql/commit/bd9174a ));

#### 0.12

  - "noResult" -> "unit" (see [66eee19]( https://github.com/nikita-volkov/hasql/commit/66eee19 ));
  - Update the extensions (see [ccc13a9]( https://github.com/nikita-volkov/hasql/commit/ccc13a9 ));

#### 0.11

  - Docs (see [108a7dc]( https://github.com/nikita-volkov/hasql/commit/108a7dc ));
  - Docs (see [d1425a7]( https://github.com/nikita-volkov/hasql/commit/d1425a7 ));
  - Docs (see [9e80ecf]( https://github.com/nikita-volkov/hasql/commit/9e80ecf ));
  - Inlining (see [7760d4d]( https://github.com/nikita-volkov/hasql/commit/7760d4d ));
  - Docs (see [05dc480]( https://github.com/nikita-volkov/hasql/commit/05dc480 ));
  - Simplify Settings (see [ed23377]( https://github.com/nikita-volkov/hasql/commit/ed23377 ));
  - Make Settings strict to conform with the rest of the API (see [8dde5b3]( https://github.com/nikita-volkov/hasql/commit/8dde5b3 ));
  - Make Query a dedicated data-type (see [f46bd40]( https://github.com/nikita-volkov/hasql/commit/f46bd40 ));

#### 0.10.1.1

  - Docs (see [940955f]( https://github.com/nikita-volkov/hasql/commit/940955f ));
  - Docs (see [5a2987f]( https://github.com/nikita-volkov/hasql/commit/5a2987f ));
  - Map the "char" encoder to the "text" OID (see [7bade85]( https://github.com/nikita-volkov/hasql/commit/7bade85 ));
  - Docs (see [521a8c0]( https://github.com/nikita-volkov/hasql/commit/521a8c0 ));
  - Docs (see [0e98ce7]( https://github.com/nikita-volkov/hasql/commit/0e98ce7 ));
  - Docs (see [53baaa7]( https://github.com/nikita-volkov/hasql/commit/53baaa7 ));
  - Update "contravariant-extras" (see [5935393]( https://github.com/nikita-volkov/hasql/commit/5935393 ));

#### 0.10.1

  - Release (see [71df29a]( https://github.com/nikita-volkov/hasql/commit/71df29a ));

#### 0.10

  - Update "contravariant-extras" (see [31e4c32]( https://github.com/nikita-volkov/hasql/commit/31e4c32 ));
  - Switch to the Encoding/Decoding idioms (see [7dc7b79]( https://github.com/nikita-volkov/hasql/commit/7dc7b79 ));
  - Docs (see [9d5687e]( https://github.com/nikita-volkov/hasql/commit/9d5687e ));
  - Docs (see [6fe2069]( https://github.com/nikita-volkov/hasql/commit/6fe2069 ));
  - Optimize Vector (see [36eedbe]( https://github.com/nikita-volkov/hasql/commit/36eedbe ));

#### 0.9

  - Optimize "foldl" (see [89470ff]( https://github.com/nikita-volkov/hasql/commit/89470ff ));

#### 0.8.2

  - The "getvalue'" optimization! (see [23c3946]( https://github.com/nikita-volkov/hasql/commit/23c3946 ));

#### 0.8.1

  - Fix the benchmarks declaration evaluation issues (see [9d7bfad]( https://github.com/nikita-volkov/hasql/commit/9d7bfad ));
  - Clean up (see [f0b6708]( https://github.com/nikita-volkov/hasql/commit/f0b6708 ));
  - Inlining (see [c1ee280]( https://github.com/nikita-volkov/hasql/commit/c1ee280 ));
  - SCC (see [8a6a265]( https://github.com/nikita-volkov/hasql/commit/8a6a265 ));
  - "strictCons" (see [d8b7bc8]( https://github.com/nikita-volkov/hasql/commit/d8b7bc8 ));
  - Add "deepseq" to the benchmark's Prelude (see [22b60cd]( https://github.com/nikita-volkov/hasql/commit/22b60cd ));
  - Drop demo (see [f1fb618]( https://github.com/nikita-volkov/hasql/commit/f1fb618 ));
  - Drop doctest (see [75df827]( https://github.com/nikita-volkov/hasql/commit/75df827 ));
  - Revert a useless optimization (see [d46f657]( https://github.com/nikita-volkov/hasql/commit/d46f657 ));
  - Attempt to optimize Result (see [5698065]( https://github.com/nikita-volkov/hasql/commit/5698065 ));
  - SCC (see [c6cb62d]( https://github.com/nikita-volkov/hasql/commit/c6cb62d ));
  - unsafeFreeResult (see [f1e5ac7]( https://github.com/nikita-volkov/hasql/commit/f1e5ac7 ));
  - SCC (see [36c18a2]( https://github.com/nikita-volkov/hasql/commit/36c18a2 ));
  - Update benchmarks (see [e511cb7]( https://github.com/nikita-volkov/hasql/commit/e511cb7 ));
  - Another attempt to optimize Row (see [5c094a9]( https://github.com/nikita-volkov/hasql/commit/5c094a9 ));
  - Attempt to optimize Row (see [a3b34e6]( https://github.com/nikita-volkov/hasql/commit/a3b34e6 ));
  - Pragmas (see [3dd489b]( https://github.com/nikita-volkov/hasql/commit/3dd489b ));
  - Profiling configuration options (see [4d9f3b9]( https://github.com/nikita-volkov/hasql/commit/4d9f3b9 ));

#### 0.8

  - List decoding test (see [7ed6735]( https://github.com/nikita-volkov/hasql/commit/7ed6735 ));
  - Refactoring (see [de69caa]( https://github.com/nikita-volkov/hasql/commit/de69caa ));
  - Benchmarks (see [4ec8931]( https://github.com/nikita-volkov/hasql/commit/4ec8931 ));
  - Remove the "Results" layer from the public deserialization API (see [7694de6]( https://github.com/nikita-volkov/hasql/commit/7694de6 ));
  - Rename a few things (see [eb9c4be]( https://github.com/nikita-volkov/hasql/commit/eb9c4be ));
  - Tests (see [79d3be1]( https://github.com/nikita-volkov/hasql/commit/79d3be1 ));
  - Unit Default Params (see [3438be4]( https://github.com/nikita-volkov/hasql/commit/3438be4 ));
  - Drop the NonparametricQuery, since its backing "libpq" API does not support the binary protocol (see [d4534fe]( https://github.com/nikita-volkov/hasql/commit/d4534fe ));
  - Update errors (see [676f52c]( https://github.com/nikita-volkov/hasql/commit/676f52c ));
  - Non-parametric query execution and refactorings (see [1f34c3f]( https://github.com/nikita-volkov/hasql/commit/1f34c3f ));
  - Unite Query and Connection into Hasql (see [663cc1a]( https://github.com/nikita-volkov/hasql/commit/663cc1a ));
  - Init tasty (see [85782bd]( https://github.com/nikita-volkov/hasql/commit/85782bd ));
  - Enum (see [9e68417]( https://github.com/nikita-volkov/hasql/commit/9e68417 ));
  - Docs (see [535b910]( https://github.com/nikita-volkov/hasql/commit/535b910 ));
  - Consistent styling (see [5c9cc57]( https://github.com/nikita-volkov/hasql/commit/5c9cc57 ));
  - Complete reimplementation (see [c9ef7ed]( https://github.com/nikita-volkov/hasql/commit/c9ef7ed ));
  - Fix Travis (see [310cbd5]( https://github.com/nikita-volkov/hasql/commit/310cbd5 ));

#### 0.7.4

  - Relax dependencies (see [42f6bea]( https://github.com/nikita-volkov/hasql/commit/42f6bea ));
  - Adds a basic README file (see [#34]( https://github.com/nikita-volkov/hasql/pull/34 ));
  - Relax the "either" dependency (see [d34c06a]( https://github.com/nikita-volkov/hasql/commit/d34c06a ));

#### 0.7.3.2

  - Update the Attoparsec dep (see [2ece62b]( https://github.com/nikita-volkov/hasql/commit/2ece62b ));

#### 0.7.3.1

  - GHC 7.10 support (see [7482cf2]( https://github.com/nikita-volkov/hasql/commit/7482cf2 ));

#### 0.7.3

  - Support for free variables by the quasi-quoter (see [52cd7db]( https://github.com/nikita-volkov/hasql/commit/52cd7db ));

#### 0.7.2

  - Multivalue clauses test (see [ae151ac]( https://github.com/nikita-volkov/hasql/commit/ae151ac ));
  - Relax the dependency on "monad-control" (see [3f996b8]( https://github.com/nikita-volkov/hasql/commit/3f996b8 ));

#### 0.7.1

  - Release (see [6c2f78a]( https://github.com/nikita-volkov/hasql/commit/6c2f78a ));

#### 0.7.0

  - The TxStream type alias (see [8336379]( https://github.com/nikita-volkov/hasql/commit/8336379 ));
  - Rename "SEx" to "Ex", since we're not teenagers here (see [23cf4b9]( https://github.com/nikita-volkov/hasql/commit/23cf4b9 ));
  - Statement executor (see [9607cf8]( https://github.com/nikita-volkov/hasql/commit/9607cf8 ));
  - Accommodate to the new backend API (see [f449ddc]( https://github.com/nikita-volkov/hasql/commit/f449ddc ));
  - RowParser -> CxRow (see [284b5b7]( https://github.com/nikita-volkov/hasql/commit/284b5b7 ));
  - Instances for PoolSettings (see [260425a]( https://github.com/nikita-volkov/hasql/commit/260425a ));
  - Description correction (see [f3af810]( https://github.com/nikita-volkov/hasql/commit/f3af810 ));
  - Add 'singleTx', update docs, rename UnparsableResult to ResultError (see [14c8601]( https://github.com/nikita-volkov/hasql/commit/14c8601 ));

#### 0.6.0

  - Implement "listTx" (see [0015dd5]( https://github.com/nikita-volkov/hasql/commit/0015dd5 ));
  - Reimplement "maybeTx" in terms of "vectorTx" (see [40326f3]( https://github.com/nikita-volkov/hasql/commit/40326f3 ));
  - Docs correction (see [8d688d8]( https://github.com/nikita-volkov/hasql/commit/8d688d8 ));
  - TxError -> SessionError (see [84d11c5]( https://github.com/nikita-volkov/hasql/commit/84d11c5 ));
  - Cancel the MonadError instance for Tx, since as it turns out, Postgres won't support it (see [ed11b5e]( https://github.com/nikita-volkov/hasql/commit/ed11b5e ));
  - Session MFunctor instance (see [ab2d2df]( https://github.com/nikita-volkov/hasql/commit/ab2d2df ));
  - Explicit instance of MonadError for Tx (see [073acd5]( https://github.com/nikita-volkov/hasql/commit/073acd5 ));
  - Export MaybeT in Prelude (see [c02ba95]( https://github.com/nikita-volkov/hasql/commit/c02ba95 ));
  - Tx tests (see [2fda4e7]( https://github.com/nikita-volkov/hasql/commit/2fda4e7 ));
  - Clean up whitespace during the quasi-quote parsing (see [9572af0]( https://github.com/nikita-volkov/hasql/commit/9572af0 ));

#### 0.6.0-RC1

  - Clean up (see [89875c7]( https://github.com/nikita-volkov/hasql/commit/89875c7 ));
  - A shortcut for empty vectors generation (see [ab44c1b]( https://github.com/nikita-volkov/hasql/commit/ab44c1b ));
  - Test UTF-8 templates (see [784c8ee]( https://github.com/nikita-volkov/hasql/commit/784c8ee ));
  - Make Session a newtype (see [95c6996]( https://github.com/nikita-volkov/hasql/commit/95c6996 ));
  - Rename "q" to more meaningful "stmt" (see [e8089b6]( https://github.com/nikita-volkov/hasql/commit/e8089b6 ));
  - Reintroduce Session and fix Demo (see [aabe550]( https://github.com/nikita-volkov/hasql/commit/aabe550 ));
  - A complete overhaul (see [3997013]( https://github.com/nikita-volkov/hasql/commit/3997013 ));

#### 0.5.0

  - Update the "hasql-postgres" deps (see [78b1cf9]( https://github.com/nikita-volkov/hasql/commit/78b1cf9 ));
  - Merge branch '0.4' (see [ac620b0]( https://github.com/nikita-volkov/hasql/commit/ac620b0 ));
  - Update the deps on "list-t" and "monad-control" (see [caaa535]( https://github.com/nikita-volkov/hasql/commit/caaa535 ));
  - Update Travis (see [23bae31]( https://github.com/nikita-volkov/hasql/commit/23bae31 ));
  - Drop redundant dependencies (see [7248e75]( https://github.com/nikita-volkov/hasql/commit/7248e75 ));

#### 0.4.0

  - Avoid "mtl-prelude" and update the "transformers" bounds (see [c4f064d]( https://github.com/nikita-volkov/hasql/commit/c4f064d ));
  - Remove duplicate deps (see [60f8dcd]( https://github.com/nikita-volkov/hasql/commit/60f8dcd ));
  - Detect the semicolons with the quasi-quoter (see [e13d21a]( https://github.com/nikita-volkov/hasql/commit/e13d21a ));
  - Update Travis (see [6ce1f5d]( https://github.com/nikita-volkov/hasql/commit/6ce1f5d ));
  - Fix the "list-t" dep (see [746b934]( https://github.com/nikita-volkov/hasql/commit/746b934 ));
  - Merge branch 'master' into 0.3 (see [93a1185]( https://github.com/nikita-volkov/hasql/commit/93a1185 ));

#### 0.3.0

  - Docs (see [8c40ec1]( https://github.com/nikita-volkov/hasql/commit/8c40ec1 ));
  - Remove bounds from Hasql deps (see [efc213c]( https://github.com/nikita-volkov/hasql/commit/efc213c ));
  - Docs (see [03373ff]( https://github.com/nikita-volkov/hasql/commit/03373ff ));
  - Increment the major version (see [cc02d23]( https://github.com/nikita-volkov/hasql/commit/cc02d23 ));
  - Merge branch 'issue-10' (see [78d8cb7]( https://github.com/nikita-volkov/hasql/commit/78d8cb7 ));
  - Docs (see [a76c6ec]( https://github.com/nikita-volkov/hasql/commit/a76c6ec ));
  - Instances for "Session" (see [0c03eb3]( https://github.com/nikita-volkov/hasql/commit/0c03eb3 ));
  - Implement "sessionUnlifter" (Fixes the issue #11) (see [0492ab8]( https://github.com/nikita-volkov/hasql/commit/0492ab8 ));
  - Fix Travis (see [2268273]( https://github.com/nikita-volkov/hasql/commit/2268273 ));
  - Fix the quasi-quoter (see [93f0ffe]( https://github.com/nikita-volkov/hasql/commit/93f0ffe ));

#### 0.2.0

  - Update the link to the demo (see [378b8fc]( https://github.com/nikita-volkov/hasql/commit/378b8fc ));
  - Relax the bounds on "hasql-backend" (see [b8a52e7]( https://github.com/nikita-volkov/hasql/commit/b8a52e7 ));
  - Use a newer "hasql-postgres" in the demo (see [727151b]( https://github.com/nikita-volkov/hasql/commit/727151b ));
  - Update Travis (see [9fadea7]( https://github.com/nikita-volkov/hasql/commit/9fadea7 ));
  - Update Travis and dependencies (see [dd7d037]( https://github.com/nikita-volkov/hasql/commit/dd7d037 ));

#### 0.1.6

  - Update Travis to test older transformers (see [a36a648]( https://github.com/nikita-volkov/hasql/commit/a36a648 ));
  - Docs correction (see [e07ac5b]( https://github.com/nikita-volkov/hasql/commit/e07ac5b ));
  - Increment version (see [8106362]( https://github.com/nikita-volkov/hasql/commit/8106362 ));
  - Update description (see [38be1c0]( https://github.com/nikita-volkov/hasql/commit/38be1c0 ));
  - Relax dependencies (see [8f24970]( https://github.com/nikita-volkov/hasql/commit/8f24970 ));

#### 0.1.5

  - Update demo to a newer postgres backend (see [9dc1289]( https://github.com/nikita-volkov/hasql/commit/9dc1289 ));
  - Standardize constraints (see [109c14e]( https://github.com/nikita-volkov/hasql/commit/109c14e ));
  - Mention SQL in description (see [46eced3]( https://github.com/nikita-volkov/hasql/commit/46eced3 ));
  - Refactoring (see [8409c3a]( https://github.com/nikita-volkov/hasql/commit/8409c3a ));
  - Update Travis (see [f065f70]( https://github.com/nikita-volkov/hasql/commit/f065f70 ));
  - Clean up description (see [b8f750d]( https://github.com/nikita-volkov/hasql/commit/b8f750d ));
  - Refer to benchmarks (see [7f62fcc]( https://github.com/nikita-volkov/hasql/commit/7f62fcc ));

#### 0.1.4

  - Show and Eq for Error (see [1332b3e]( https://github.com/nikita-volkov/hasql/commit/1332b3e ));
  - Typo (see [caa8416]( https://github.com/nikita-volkov/hasql/commit/caa8416 ));
  - Typo (see [ec66704]( https://github.com/nikita-volkov/hasql/commit/ec66704 ));
  - Update demo (see [5fb7f48]( https://github.com/nikita-volkov/hasql/commit/5fb7f48 ));
  - Polishing (see [5da25e2]( https://github.com/nikita-volkov/hasql/commit/5da25e2 ));
  - Update docs (see [5e22ec6]( https://github.com/nikita-volkov/hasql/commit/5e22ec6 ));
  - Update demo (see [37672d0]( https://github.com/nikita-volkov/hasql/commit/37672d0 ));
  - Docs (see [32a0d38]( https://github.com/nikita-volkov/hasql/commit/32a0d38 ));
  - Fix the link to the demo again …. (see [5ef0074]( https://github.com/nikita-volkov/hasql/commit/5ef0074 ));

#### 0.1.3

  - Fix the demo link (see [ed41a3a]( https://github.com/nikita-volkov/hasql/commit/ed41a3a ));

#### 0.1.2

  - Adapt to the older Haddock version, which Hackage is running (see [66a950f]( https://github.com/nikita-volkov/hasql/commit/66a950f ));
  - Update the docs and the Cabal file (see [d1e9235]( https://github.com/nikita-volkov/hasql/commit/d1e9235 ));

#### 0.1.1

  - Add a demo (see [5633a87]( https://github.com/nikita-volkov/hasql/commit/5633a87 ));
  - Update the description (see [4e1c057]( https://github.com/nikita-volkov/hasql/commit/4e1c057 ));
  - Remove SCCs (see [d4a6c0a]( https://github.com/nikita-volkov/hasql/commit/d4a6c0a ));

#### 0.1.0

  - Accomodate the RC7 API of the backend (see [dc21a81]( https://github.com/nikita-volkov/hasql/commit/dc21a81 ));
  - Accomodate to the RC6 backend API (see [89b92bd]( https://github.com/nikita-volkov/hasql/commit/89b92bd ));

#### 0.1.0.RC7

  - Accomodate to the RC5 backend API (see [45becba]( https://github.com/nikita-volkov/hasql/commit/45becba ));

#### 0.1.0.RC6

  - Optimize the row parser a bit (see [47d86b9]( https://github.com/nikita-volkov/hasql/commit/47d86b9 ));

#### 0.1.0.RC5

  - Accomodate the backend API RC4 (see [aed09f3]( https://github.com/nikita-volkov/hasql/commit/aed09f3 ));
  - Accomodate to the backend API RC3 (see [2013640]( https://github.com/nikita-volkov/hasql/commit/2013640 ));

#### 0.1.0.RC4

  - Description (see [688bad4]( https://github.com/nikita-volkov/hasql/commit/688bad4 ));

#### 0.1.0.RC3

  - Remove the dependency on "hashable" (see [f72d626]( https://github.com/nikita-volkov/hasql/commit/f72d626 ));
  - Simplify the QQ parser (see [4bcf7d4]( https://github.com/nikita-volkov/hasql/commit/4bcf7d4 ));
  - Cleanup (see [67f5138]( https://github.com/nikita-volkov/hasql/commit/67f5138 ));
  - Accomodate to updates in "hasql-backend" (see [3c1657d]( https://github.com/nikita-volkov/hasql/commit/3c1657d ));
  - Update errors (see [dab6530]( https://github.com/nikita-volkov/hasql/commit/dab6530 ));
  - Centralize exception handling (see [ccf7431]( https://github.com/nikita-volkov/hasql/commit/ccf7431 ));
  - Get rid of the pool (see [ad3ed50]( https://github.com/nikita-volkov/hasql/commit/ad3ed50 ));
  - Cleanup (see [c98d630]( https://github.com/nikita-volkov/hasql/commit/c98d630 ));
  - Simplify function names (see [6ab4148]( https://github.com/nikita-volkov/hasql/commit/6ab4148 ));
  - Session settings (see [d1f1a93]( https://github.com/nikita-volkov/hasql/commit/d1f1a93 ));
  - Unite the streaming executors and introduce a single (see [9baf77d]( https://github.com/nikita-volkov/hasql/commit/9baf77d ));
  - Move the streaming functions out of the base monad (see [d946f44]( https://github.com/nikita-volkov/hasql/commit/d946f44 ));
  - Make the API Session-centric (see [81c9d99]( https://github.com/nikita-volkov/hasql/commit/81c9d99 ));
  - Rename ResultsStream (see [5379bf6]( https://github.com/nikita-volkov/hasql/commit/5379bf6 ));
  - Correct the docs on "q" (see [a47ac65]( https://github.com/nikita-volkov/hasql/commit/a47ac65 ));
  - Remove Statement from exports (see [1a83e91]( https://github.com/nikita-volkov/hasql/commit/1a83e91 ));
  - Docs on TxListT (see [b1d2aa8]( https://github.com/nikita-volkov/hasql/commit/b1d2aa8 ));
  - Remove duality from execution model (see [b07d0c5]( https://github.com/nikita-volkov/hasql/commit/b07d0c5 ));
  - Export IsolationLevel (see [8f5a8e3]( https://github.com/nikita-volkov/hasql/commit/8f5a8e3 ));
  - Docs (see [eabf0be]( https://github.com/nikita-volkov/hasql/commit/eabf0be ));
  - Transaction -> Tx (see [98def4b]( https://github.com/nikita-volkov/hasql/commit/98def4b ));
  - Reimplement Pool (see [136237f]( https://github.com/nikita-volkov/hasql/commit/136237f ));
  - Cleanup (see [589fa7a]( https://github.com/nikita-volkov/hasql/commit/589fa7a ));
  - Cleanup (see [f43aaff]( https://github.com/nikita-volkov/hasql/commit/f43aaff ));
  - parse -> parseRow (see [d47e98e]( https://github.com/nikita-volkov/hasql/commit/d47e98e ));
  - Unify the API in a single file (see [e787181]( https://github.com/nikita-volkov/hasql/commit/e787181 ));
  - A dual execution model (see [25cea66]( https://github.com/nikita-volkov/hasql/commit/25cea66 ));
  - Docs (see [499b0bb]( https://github.com/nikita-volkov/hasql/commit/499b0bb ));
  - Get rid of the Integer mapping dependency (see [829caa6]( https://github.com/nikita-volkov/hasql/commit/829caa6 ));
  - Extract backend to external library (see [5969dc4]( https://github.com/nikita-volkov/hasql/commit/5969dc4 ));

#### 0.1.0.RC2

  - Simplify as hell (see [3bc2383]( https://github.com/nikita-volkov/hasql/commit/3bc2383 ));
  - Rename alias (see [2c6dfcb]( https://github.com/nikita-volkov/hasql/commit/2c6dfcb ));
  - Comments (see [3a3faef]( https://github.com/nikita-volkov/hasql/commit/3a3faef ));
  - Update mtl-prelude (see [d69d828]( https://github.com/nikita-volkov/hasql/commit/d69d828 ));
  - Rename stuff a bit (see [59c4ec1]( https://github.com/nikita-volkov/hasql/commit/59c4ec1 ));
  - Clean up (see [2668da3]( https://github.com/nikita-volkov/hasql/commit/2668da3 ));
  - Function aliases (see [8722e0e]( https://github.com/nikita-volkov/hasql/commit/8722e0e ));
  - Update API (see [ba9bbe3]( https://github.com/nikita-volkov/hasql/commit/ba9bbe3 ));
  - Update transaction conflicts handling (see [06e5973]( https://github.com/nikita-volkov/hasql/commit/06e5973 ));
  - Rename to "hasql" (see [d16efd9]( https://github.com/nikita-volkov/hasql/commit/d16efd9 ));
  - Fix stream hoisting (see [6a47cac]( https://github.com/nikita-volkov/hasql/commit/6a47cac ));
  - Identity and drop type rep (see [979e406]( https://github.com/nikita-volkov/hasql/commit/979e406 ));
  - Update errors model (see [034368c]( https://github.com/nikita-volkov/hasql/commit/034368c ));
  - WIP (see [c626121]( https://github.com/nikita-volkov/hasql/commit/c626121 ));
  - Extract Pool from API (see [7ef9f79]( https://github.com/nikita-volkov/hasql/commit/7ef9f79 ));
  - Remove class (see [1e68602]( https://github.com/nikita-volkov/hasql/commit/1e68602 ));
  - New Transaction API (see [2508b72]( https://github.com/nikita-volkov/hasql/commit/2508b72 ));
  - New API with example (see [fec1f2b]( https://github.com/nikita-volkov/hasql/commit/fec1f2b ));
  - Partially reimplement API (see [14981bf]( https://github.com/nikita-volkov/hasql/commit/14981bf ));
  - Cleanup deps (see [a5d4418]( https://github.com/nikita-volkov/hasql/commit/a5d4418 ));
  - Row (see [928f89c]( https://github.com/nikita-volkov/hasql/commit/928f89c ));
  - Reimplement Backend (see [e940079]( https://github.com/nikita-volkov/hasql/commit/e940079 ));
  - Update description (see [e877f2b]( https://github.com/nikita-volkov/hasql/commit/e877f2b ));
  - Clean up (see [d7fb2ce]( https://github.com/nikita-volkov/hasql/commit/d7fb2ce ));
  - Update the deps (see [5fb0fe3]( https://github.com/nikita-volkov/hasql/commit/5fb0fe3 ));
  - Add Decimal (see [d5e4327]( https://github.com/nikita-volkov/hasql/commit/d5e4327 ));
  - Lose the "Value_" prefix (see [e4fee82]( https://github.com/nikita-volkov/hasql/commit/e4fee82 ));
  - Drop redundant dependency (see [30b3593]( https://github.com/nikita-volkov/hasql/commit/30b3593 ));
  - Docs, exports and API finishing (see [8e245cd]( https://github.com/nikita-volkov/hasql/commit/8e245cd ));
  - Implement API (see [873ccbf]( https://github.com/nikita-volkov/hasql/commit/873ccbf ));
  - Update Value instances generation (see [129731f]( https://github.com/nikita-volkov/hasql/commit/129731f ));
  - Rearrange Row and ValueConversion into Conversion (see [7262c14]( https://github.com/nikita-volkov/hasql/commit/7262c14 ));
  - Maybe Value (see [4d0f57a]( https://github.com/nikita-volkov/hasql/commit/4d0f57a ));
  - Row instances (see [cca38b5]( https://github.com/nikita-volkov/hasql/commit/cca38b5 ));
  - Select with parsing (see [50c7b53]( https://github.com/nikita-volkov/hasql/commit/50c7b53 ));
  - Implement transaction and update errors (see [2479055]( https://github.com/nikita-volkov/hasql/commit/2479055 ));
  - Prelude correction (see [4988ae5]( https://github.com/nikita-volkov/hasql/commit/4988ae5 ));
  - Switch from transformer-based resource management to IO due to potential concurrency issues (see [399d517]( https://github.com/nikita-volkov/hasql/commit/399d517 ));
  - Get rid of HDBC (see [268acda]( https://github.com/nikita-volkov/hasql/commit/268acda ));
  - Updates and more rearrangement (see [28b30c3]( https://github.com/nikita-volkov/hasql/commit/28b30c3 ));
  - Major rearrangement (see [6f3ad25]( https://github.com/nikita-volkov/hasql/commit/6f3ad25 ));
  - Merge branch 'qq' (see [56b59bb]( https://github.com/nikita-volkov/hasql/commit/56b59bb ));
  - Cabal updates (see [48cea97]( https://github.com/nikita-volkov/hasql/commit/48cea97 ));
  - Results Stream sketch (see [dad0f81]( https://github.com/nikita-volkov/hasql/commit/dad0f81 ));
  - Levels and Privileges sketch (see [0c28e6c]( https://github.com/nikita-volkov/hasql/commit/0c28e6c ));
  - CompositionT (see [dfd6558]( https://github.com/nikita-volkov/hasql/commit/dfd6558 ));
  - Docs (see [f51dcd4]( https://github.com/nikita-volkov/hasql/commit/f51dcd4 ));
