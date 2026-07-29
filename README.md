# Janet LSP

A Language Server (LSP) for the [Janet](https://janet-lang.org) programming language.

## Overview

The goal of this project is to provide an augmented editor/tooling experience for [Janet](https://janet-lang.org), via a self-contained, [Language Server Protocol](https://microsoft.github.io/language-server-protocol/)-compliant language server (which is itself implemented in Janet!).

### Stable Features

- Validated incremental open/change/close synchronization with UTF-16 and UTF-8 positions
- Context-aware core, lexical, module, keyword, snippet, and auto-import completion
- Named-argument completion, hover documentation, and active-parameter signature help
- Push and pull parse, compile, warning, and bounded runtime diagnostics
- Whole-document formatting
- Local, imported, indexed, and runtime source-map definition lookup
- Explicit source-indexed type-definition and implementation navigation
- Parser-backed multiline symbols, module aliases, selective imports, and re-exports
- Document/workspace symbols, binding-aware references, and workspace rename
- Binding-aware document highlights, structural folding and selection ranges,
  and links for indexed modules and explicit source paths
- Binding-aware incoming and outgoing call hierarchy for functions, macros,
  imports, recursion, and local lambdas
- Full-document semantic tokens and deterministic diagnostic quick fixes
- Conservative parameter-name inlay hints
- Single-root, multi-root, and standalone-file analysis
- Bounded versioned analysis snapshots shared across language features

### Experimental Features

- Trust-gated execution-based workspace analysis, including startup files,
  imports, macros, and `:flycheck` functions
- The network debug console and runtime logging controls

## Caveats

- MacOS support is _mostly_ untested (but as far as I know there shouldn't be major differences). 
- The only editor integration currently tested against is [Visual Studio Code](https://code.visualstudio.com/).
- I've never written a language server before, so I don't really know what I'm doing. Help me, if you'd like!

## Clients (i.e. Editors)

Currently, Janet LSP is being regularly tested and is expected to work out of the box with two major editors:

- [Visual Studio Code](https://code.visualstudio.com/), which you can try/take advantage of by installing the [Janet++](https://github.com/CFiggers/vscode-janet-plus-plus) extension [from the VS Code marketplace](https://marketplace.visualstudio.com/items?itemName=CalebFiggers.vscode-janet-plus-plus), and
- [Neovim](https://neovim.io/), which ships with support for LSP servers.

Other editors that implement LSP client protocols, either built-in or through editor extensions, include:

- Emacs
- Vim
- Sublime Text
- Helix
- Kakoune
- Zed

If you get Janet LSP working with any of these options, please let me know!

### Workspace Trust

Janet LSP treats every workspace as untrusted by default. Untrusted analysis
parses source for syntax and local symbols, but does not compile or execute
workspace macros, imports, functions marked `:flycheck`, or
`.janet-lsp/startup.janet`. This prevents opening a repository from executing
code on the editor user's machine.

Clients may explicitly opt a workspace into execution-based analysis by
including its exact root URI in `initializationOptions.trustedWorkspaces`:

```json
{
  "rootUri": "file:///home/user/project",
  "initializationOptions": {
    "trustedWorkspaces": ["file:///home/user/project"]
  }
}
```

Trust applies only to the listed workspace URI and can be granted only through
client initialization options; repository files cannot enable it. Trusted mode
loads `.janet-lsp/startup.janet`, discovers workspace module paths, expands
macros, follows imports, and executes `:flycheck` functions. These operations
run arbitrary Janet code with the user's permissions, so clients should expose
this setting only as an explicit user decision.

For an untrusted file workspace, Janet LSP also sends one
`window/showMessageRequest` after the client's `initialized` notification. The
user may choose **Trust for This Session** to enable execution-based analysis
until that language-server process exits, or **Keep Restricted** to continue in
safe mode. Dismissing the prompt keeps the workspace restricted. Clients can
use `trustedWorkspaces` for persistent user-approved trust and should not derive
that list from workspace configuration files.

### Workspace Folders

Janet LSP uses the client's `workspaceFolders` from initialization, falling
back to `rootUri` and then legacy `rootPath`. The server process does not need
to be launched from a workspace directory. Folder add/remove notifications are
applied immediately, and nested workspaces own files under the most specific
matching root. Analysis environments, module discovery, startup files, and
trust are isolated per root.

### Workspace Index Cache

Workspace symbol indexes are cached across server restarts under
`$JANET_LSP_CACHE_DIR`, `$XDG_CACHE_HOME/janet-lsp`, or
`$HOME/.cache/janet-lsp`, in that order. Cache entries are versioned and
validated against the current root, exclusions, file set, size, modification
time, inode, and device. Valid records are reused; only new or changed Janet
files are reparsed by the background indexer. A fully valid cache avoids the
startup index subprocess entirely.

Watched-file notifications update the in-memory and persisted disk index
atomically. Corrupt or incompatible cache files are discarded, and open-buffer
records remain separate so unsaved source is never persisted as a disk record.

An open document outside all configured file workspaces is treated as a
standalone file. It retains parsing, core documentation, hover, signature, and
local completion features, but remains restricted and does not inherit module
paths, startup configuration, or trust from another workspace.

### Inlay Hints

Parameter-name hints are enabled by default. Janet LSP emits them only for
resolved, non-variadic calls when the argument name does not already match the
parameter, and only inside the requested range. Hints are informational and do
not include text edits. Disable the category with:

```json
{
  "initializationOptions": {
    "inlayHints": {"parameterNames": false}
  }
}
```

### Contextual Completion

Completion uses parser snapshots and the owning workspace index without loading
or executing modules. It completes indexed paths in `import`, `use`, `require`,
and `dofile`, imported aliases and public module members, selective `:only`
exports, binding markers, observed keywords, table keys, and binding metadata.
Private definitions declared with a `-` form or `:private` metadata are excluded.
Clients advertising snippet support receive Janet form snippets for common
forms such as `defn`, `fn`, `let`, and `if`. General expression completion is
ranked by lexical scope, imported-module proximity, workspace usage, and stable
label order. A unique missing workspace export may include a version-aware
`additionalTextEdits` import; overlapping first-line edits are not offered, and
stale import edits are removed during completion resolution.

### Type Navigation

Janet has no native type, interface, or protocol declarations, so Janet LSP does
not infer types from runtime values, table keys, prototype naming, or structural
method conventions. Projects can opt into reliable, execution-free navigation
with namespaced binding metadata:

```janet
(def Shape {})
(def Circle {:janet-lsp/type-definition "Shape"} {})
(defn draw-circle {:janet-lsp/implements "Shape"} [circle]
  circle)
(defn draw-many {:janet-lsp/implements ["Shape" "Drawable"]} [values]
  values)
```

`textDocument/typeDefinition` on `Circle` resolves `Shape`.
`textDocument/implementation` on `Shape` returns `draw-circle` and `draw-many`.
Target strings name Janet symbols resolved in the declaration's indexed module
and import context, including aliases and re-exports. Missing, malformed, imported
private, or ambiguous targets produce no result. This metadata is source-only;
dynamic prototype changes, factory return values, C abstract types, generated
bindings, and convention-based dispatch are intentionally unsupported.

### Static Diagnostics

Janet LSP reports parse errors, deterministic unused-parameter warnings,
undefined symbols, duplicate top-level definitions, lexical shadowing,
unreachable code after `break`, literal `if`/`while` conditions, and provable
same-file positional and named-argument mistakes in all workspaces. Call checks
include missing or extra positional arguments, odd named pairs, unknown named
arguments, and duplicate named arguments. Prefix an intentionally unused
parameter with `_` to suppress its warning.

Static checks do not expand macros or evaluate conditions. Quoted forms, import
syntax, and arguments to macros or unresolved calls remain opaque, so Janet LSP
reports only facts it can prove from source. Undefined-symbol checks involving
imports also wait for a complete workspace index.

Trusted workspace analysis also invokes Janet's compiler. This extends checks
to unknown symbol reads, imported or generated functions, and macro arity.
Named parameters introduced by `&named` are optional in Janet; passing an
unsupported name is a warning, while omitting a named parameter is not an
error. Compiler-backed checks remain trust-gated because compiling Janet can
execute macros and imports.

Pull-diagnostic clients receive stable result IDs and unchanged reports from
both `textDocument/diagnostic` and `workspace/diagnostic`. Workspace reports
include closed indexed files, while open unsaved content takes precedence over
disk. Clients advertising diagnostic refresh support receive
`workspace/diagnostic/refresh` after severity configuration changes.

Diagnostic severities can be changed under `initializationOptions` at startup,
or under `settings` through `workspace/didChangeConfiguration`. Values may be
`error`, `warning`, `information`, `hint`, `off`, or the corresponding LSP
severity number from 1 through 4:

```json
{
  "diagnostics": {
    "undefinedSymbol": "error",
    "duplicateDefinition": "warning",
    "shadowing": "hint",
    "unreachableCode": "warning",
    "constantCondition": "information",
    "unusedParameter": "off",
    "calls": "warning"
  }
}
```

The same object may be nested under `janetLsp.diagnostics`. Additional
categories are `parse`, `compile`, `runtime`, and `analysis`. Source comments
can suppress an exact diagnostic code, a category, or all diagnostics:

```janet
# janet-lsp: ignore-next-line undefinedSymbol
generated-name

# janet-lsp: disable shadowing, constantCondition
# janet-lsp: enable shadowing
# janet-lsp: enable all
```

## Getting Started (for Development)

### Clone this project and Build the stand-alone binary and .jimage file

Requires [Janet](https://github.com/janet-lang/janet) and [jpm](https://github.com/janet-lang/jpm).

```shell
$ git clone https://github.com/CFiggers/janet-lsp
$ cd janet-lsp
$ jpm deps
$ jpm build
```

### Testing

The test suite requires Janet 1.41 or newer. Install the locked dependencies into
the repository-local JPM tree, then run the project test task:

```shell
$ jpm --local load-lockfile
$ jpm --local run test
```

The test task runs the same Judge unit and integration suite used by CI. Keeping
dependencies local avoids relying on modules installed for other Janet projects.

A .jimage (Janet image) file will be generated in `/build`. Using a .jimage file makes Janet LSP fully cross-platform (wherever there is a compatible Janet binary on the user's path). But it also means that you must have a Janet binary to use Janet LSP (this author struggles to imagine a scenario where you would both need the LSP and NOT have Janet itself installed).

### Installing

After running the commands above, the following command will copy the `janet-lsp` binscript to a location that can be executed via the command line.

```shell
$ jpm install
```

Test successful install by running the following:

```shell
$ janet-lsp --version
```

### Debug Console

Starting in version 0.0.3, you can start a debug console by passing `--console` to any invocation of Janet LSP, including any of the following:

```console
$ ./build/janet-lsp --console
  OR
$ janet -i ./build/janet-lsp.jimage --console
  OR
$ janet ./src/main.janet --console
```

In this mode, the LSP will launch a simple RPC server that listens on port 8037 (by default, configurable with the `--debug-port` flag). Janet LSPs with version `>= 0.0.3` will check for a listening server on port 8037 (or the port specified by `--debug-port`) and, if found, transmit anything sent through the `(logging/log)` function to be printed out by the debug console.

In the future, the debug console may function as a networked REPL allowing commands to be sent to the running language server process (but right now it functions in listen-only mode).

## Contributions

Issues and Pull Requests welcome.

## Prior Art

This project is a hard fork from (with much appreciation to) [JohnDoneth/janet-language-server](https://github.com/JohnDoneth/janet-language-server), which is Copyright (c) 2022 JohnDoneth and contributors.
