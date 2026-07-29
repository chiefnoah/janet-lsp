# Changelog
All notable changes to this project will be documented in this file.
Format for entires is <version-string> - release date.

## Unreleased

- Protocol transport, JSON-RPC lifecycle, cancellation, URI, and position
  encoding compliance have been hardened.
- Document state, workspace folders, completion resolution, formatting,
  signature help, and diagnostics now have integration coverage.
- Workspace execution is restricted by default. Startup files, macros,
  imports, and `:flycheck` functions run only for explicitly trusted roots.
- Added one-session workspace trust prompts and persistent client-managed
  `trustedWorkspaces` initialization options.
- Added indexed definitions, symbols, references, workspace rename, semantic
  tokens, deterministic quick fixes, and conservative inlay hints.
- Added binding-aware document highlights, nested syntax and comment folding,
  progressive selection ranges, and execution-free module/source links.
- Added indexed call hierarchy with incoming and outgoing calls for local,
  imported, recursive, and macro callables.
- Added type-definition and implementation navigation through explicit,
  import-aware `:janet-lsp/type-definition` and `:janet-lsp/implements` metadata.
- Added complete-form range formatting and minimal canonical on-type edits for
  closing delimiters.
- Expanded versioned quick fixes for calls, unused bindings/imports, and unique
  missing imports, plus deterministic sort and organize-import source actions.
- Extended workspace rename through aliases, selective imports, re-exports, and
  safely supported module file and import-path operations.
- Added semantic token range and delta requests with stable result IDs and
  expanded identity-aware syntax and binding classification.
- Added lazy reference, test, flycheck, and runnable-definition code lenses
  with configurable categories and trust-gated command execution.
- Expanded conservative inlay hints for static named signatures, literal
  constant values, and explicit return metadata.
- Added cooperative cancellation checkpoints, per-request source reuse, bounded
  trusted command execution, and a synthetic large-workspace benchmark.
- Reported workspace index spawn, scan, parse, and cache failures to clients
  with workspace context and retry-safe state.
- Centralized platform-specific temporary paths, path comparison, and
  executable lookup, and added VS Code, Neovim, Helix, and Zed protocol smoke
  profiles.
- Added stable unused-parameter warnings; trusted compiler diagnostics report
  unknown symbols, positional arity errors, and unsupported named arguments.
- Split the server into focused lifecycle, document, workspace, navigation,
  editor-feature, diagnostics, and shared state modules, and extracted the
  timeout-bounded integration client.
- Workspace indexing now runs for clients without progress support; supported
  clients receive registered, cancellable work-done progress. Indexer failures
  are reported and reaped without leaving scans stuck.
- Static linting respects the analysis size bound, handles destructured
  parameters, and suppresses unused warnings when macros may synthesize reads.
- Restricted analysis now validates provable same-file positional and named
  calls. Completion and signature help expose `&named` parameters and suppress
  names already supplied by the caller.
- Workspace indexing now uses positioned parser nodes for multiline definitions,
  nested symbols, destructured parameters, imports, aliases, selective imports,
  re-exports, generated trusted definitions, and cross-file binding identities.
- Documents now negotiate validated incremental synchronization and retain a
  bounded cache of versioned parse, diagnostic, environment, index, reference,
  signature, and semantic snapshots shared across language features. Workspace
  ownership, index, and trust changes invalidate affected snapshots.
- Workspace indexes now persist in a versioned user cache, reuse metadata-valid
  records across restarts, incrementally rebuild changed files, update from
  watched-file events, and discard malformed or incompatible cache data. Open
  buffers and overlapping workspace roots remain isolated from disk records.
- Rejected incremental edit batches now require full-content resynchronization,
  and completion resolution uses the item's originating versioned snapshot.
- Restricted analysis now reports conservative undefined symbols, duplicate
  definitions, lexical shadowing, unreachable code, and literal conditions
  without expanding macros or executing source. Categories support runtime
  severity configuration and source-comment suppression directives.
- Document and workspace pull diagnostics now provide deterministic result IDs,
  unchanged reports, closed-file analysis, open-buffer overlays, cancellation,
  and client refresh requests after configuration changes.
- Completion now distinguishes module paths, aliases and exports, binding,
  keyword, table-key, and metadata contexts. It adds capability-gated Janet
  snippets, scope/module/usage ranking, and unique missing-import edits while
  excluding private exports and stale or overlapping edits.

## 0.0.11 - 2026-02-14

- Logging 
  - Clarify logging levels using more standard labeling (debug, info, warn, error, fatal, unknown)
- Diagnostics 
  - Better location for runtime errors 
    - Previously defaulted to line 0, column 0 regardless of actual error location
  - Clarify type of diagnostic error in diagnostic messages (i.e. compile vs parsing vs runtime)
  - Report linter warnings in addition to errors, with lower severity
  - Respect new `:flycheck` metadata convention added by Janet v1.40.0
    - User-defined functions with `:flycheck` metadata tags will be fully executed by LSP (!)
- Misc
  - Apply `spork/fmt` throughout

## 0.0.10 - 2024-12-22

- Logging 
  - Rotate log files and overwrite eventually to avoid indefinite log file size
  - Adjusted some log levels
- New methods
  - `enableDebug` and `disableDebug` - Allow clients to set `(dyn :debug)` while running
  - `setLogLevel` and `setLogToFileLevel` - Allow clients to change debug level to console and file

## 0.0.9 - 2024-12-07

- Bugfixes
  - Decode percent encoding in URIs before saving to or lookup from `state`
  - Typo: ":documnts" rather than ":documents", causing redundant keys in `state` when diagnostics are pull (vs push)
  - Don't exit loop when handle-message returns an `:error` result, instead report it and reenter loop gracefully
- Misc
  - Formatting tweaks
  - New "janet/tellJoke" method (testing for future custom LSP RPC calls)

## 0.0.8 - 2024-11-24

- Bug Fixes
  - Additional jpm defs (by @strangepete)
  - New `eval-env`s should set `*out*` to `stderr`

## 0.0.7 - 2024-08-11

- Core loop
  - More explicit `(os/exit 0)` instead of implicit process exit
- Logging
  - Overhaul logging module and include significantly more logging statements throughout
- Completion
  - Simplify `binding-type` lookup
- Bug Fixes
  - Attempting to index on null ds in `on-document-definition`
  - Catch error when attempting to write to `janetlsp.log.txt` fails
  - Diagnostics/Completion: Maintain separate eval-envs for each document uri instead of overwriting shared eval results
- Testing
  - Fix tests throughout
  - Progress on Integration testing
- Misc
  - Formatting tweaks
  - Capture current commit in version outputs
  - Catch and handle errors from `handle-message` instead of crashing

## 0.0.6 - 2024-07-31

- Core Functionality
  - Factored out `line-ending` and `read-offset` functions/values by @CFiggers in https://github.com/CFiggers/janet-lsp/pull/25
  - Fix bug with line endings (communication over `stdio` was broken on Widows) by @CFiggers in https://github.com/CFiggers/janet-lsp/pull/25
- Logging
  - Now fail gracefully when unable to write to `janetlsp.log.txt` by @CFiggers in https://github.com/CFiggers/janet-lsp/pull/25
- Jump to Definition
  - Preliminary work (not completed yet) by @CFiggers in https://github.com/CFiggers/janet-lsp/pull/25

## 0.0.5 - 2024-06-14

- Diagnostics
  - Only syntax highlight function signatures in pop-up hover definitions by @CFiggers in [#23](https://github.com/CFiggers/janet-lsp/pull/23)
  - Fix bug with publishing diagnostics (was causing last diagnostic warning to not clear when using for e.g. nvim-lsp) by @CFiggers in [#23](https://github.com/CFiggers/janet-lsp/pull/23)
- Tests
  - Additional tests and reorganization by @CFiggers in [#23](https://github.com/CFiggers/janet-lsp/pull/23)

## 0.0.4 - 2024-01-26

- Project
  - We now install `janet-lsp` as a binscript instead of building an executable at all by @CFiggers in https://github.com/CFiggers/janet-lsp/pull/19
- Formatting
  - Tweak to vendored copy of `spork/fmt` by @CFiggers in https://github.com/CFiggers/janet-lsp/pull/19
- Diagnostics
  - `eval-buffer` now starts with a clean environment on every evaluation (resolving many consistency issues) by @CFiggers in https://github.com/CFiggers/janet-lsp/pull/19
  - Can now push diagnostics to clients that prefer not to request by issuing `testDocument/publishDiagnostics` RPC notifications by @CFiggers in https://github.com/CFiggers/janet-lsp/pull/18
- Completion and Hover Definitions
  - Bugs with jpm definitions resolved by @CFiggers in https://github.com/CFiggers/janet-lsp/pull/19
- Signature helps
  - Fix bugs in `sexp-at` (off-by-one and crash on unparenthesized top-level forms) by @CFiggers in https://github.com/CFiggers/janet-lsp/pull/19
- RPC
  - Can now send properly formatted LSP notifications (in addition to responses, needed for publishing diagnostics)by @CFiggers in https://github.com/CFiggers/janet-lsp/pull/19
- Testing
  - Migrated Judge tests from main.janet into a separate fileby @CFiggers in https://github.com/CFiggers/janet-lsp/pull/19
- CLI
  - `--debug` flag now works correctlyby @CFiggers in https://github.com/CFiggers/janet-lsp/pull/19
- Misc
  - Source code formatting and comment cleanup by @CFiggers in https://github.com/CFiggers/janet-lsp/pull/17

## 0.0.3 - 2024-01-09

- Completion
  - Add basic CompletionItemKind by @fnurk in https://github.com/CFiggers/janet-lsp/pull/9
- Formatting
  - New feature: Document formatting by @CFiggers in https://github.com/CFiggers/janet-lsp/pull/13
- Signature Helps
  - New Feature: Signature Helps by @CFiggers in https://github.com/CFiggers/janet-lsp/pull/14
- Debug Console
  - New Feature: Debug Console by @CFiggers in https://github.com/CFiggers/janet-lsp/pull/15
- Diagnostics
  - Improvement: Multiple Diagnostic Errors by @CFiggers in https://github.com/CFiggers/janet-lsp/pull/16
- Cross-cutting
  - Separate environment for flychecking user code (better separation between running server env and diagnostic eval env)
  - Replace `spork/argparse` with `ianthehenry/cmd` for command line argument parsing
  - Bug fixes

## 0.0.2 - 2023-10-24

- Hover
  - Improved hover documentation (more detailed info, syntax highlighting for function signatures)
- Completion
  - Added `project.janet` symbols for jpm (`declare-project`, `declare-native`, `declare-executable`, etc.)
- General
  - Improved module loading logic for eval/completion environment
  - Replaced `spork/json` dependency with `CFiggers/jayson` for .jimage compatibility
  - Handle `shutdown` and `exit` lifecycle requests properly

## 0.0.1 - 2023-09-26

- Hard forked this project from [JohnDoneth/janet-language-server](https://github.com/JohnDoneth/janet-language-server).
  
