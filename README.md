# Janet LSP

A Language Server (LSP) for the [Janet](https://janet-lang.org) programming language.

## Overview

The goal of this project is to provide an augmented editor/tooling experience for [Janet](https://janet-lang.org), via a self-contained, [Language Server Protocol](https://microsoft.github.io/language-server-protocol/)-compliant language server (which is itself implemented in Janet!).

Current features include:

- [x] Auto-completion based on symbols in the Janet Standard Library and defined in user code
- [x] On-hover definition of symbols as returned by `(doc ,symbol)`
- [x] Inline compiler errors
- [x] Pop-up signature help 

Planned features include:

- [ ] Jump to definition/implementation
- [ ] Find references from definition/implementation
- [ ] Refactoring helps
- [ ] Symbol renaming helps

Possible (but de-prioritized) features include:

- [ ] Syntax highlighting for Janet via semantic tokens

Desirable, but possibly more complicated/difficult features include:

- [ ] Stand-alone (i.e. non-Editor-dependent) usage via API/CLI

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
