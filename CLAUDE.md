# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

VS Code extension for Erlang. Two codebases in one repo:

- `lib/` — TypeScript VS Code client (extension host, debug adapter, LSP client, rebar/eunit runners).
- `apps/erlangbridge/` — an OTP application (`vscode_lsp`) that *is* the language server and the in-VM debugger bridge. Erlang, not TypeScript, does all code analysis.

`grammar/` is a git submodule (erlang-ls/grammar) providing `grammar/Erlang.plist`. Clone with `--recurse-submodules` or `git submodule update --init`, otherwise syntax highlighting contributions break.

## Commands

```bash
npm install                 # deps
npm run compile             # tsc -p ./  -> out/ (out/lib/extension.js is package.json "main")
npm run webpack-dev         # dev bundle, --watch
npm run webpack             # dev bundle, one shot
./rebar3 compile            # build erlangbridge -> _build/default/lib/vscode_lsp
./rebar3 ct                 # Erlang common_test suites (apps/erlangbridge/test)
npm test                    # pretest runs compile, then vscode-test (headless VS Code)
vsce package                # VSIX; runs vscode:prepublish = webpack --mode production
```

Single tests:

```bash
npm run compile && npx vscode-test --grep "Extension should be present"   # also: --run out/test/test-suite/x.test.js
./rebar3 ct --suite apps/erlangbridge/test/lsp_navigation_SUITE
./rebar3 ct --suite apps/erlangbridge/test/lsp_navigation_SUITE --case some_case
```

TS tests: `.vscode-test.mjs` runs `out/test/**/*.test.js` (mocha `tdd` ui) against workspace `test/test-fixtures/`. `test/test-suite/index.ts` is the older mocha runner, used only by the `Launch Tests` config in `.vscode/launch.json`. Interactive debugging: `Launch Extension` / `Launch Tests` in the Run sidebar.

CI (`.github/workflows/pr-verify.yml`) runs `xvfb-run -a npm test` then `./rebar3 ct` on Linux with system Erlang.

## Architecture

### Activation (`lib/extension.ts`)

`activate()` wires four independent things: `RebarRunner` (rebar commands + problem matcher), `EunitRunner`, the debug configuration provider + debug adapter factory, and `LspClient.activate()`. The debug adapter factory is chosen from the `erlang.debuggerRunMode` setting — `external` (separate process, default), `server` (socket), `inline` (in extension host, best for breakpoints in adapter code) — see `lib/ErlangAdapterDescriptorFactory.ts`.

### Language server path

1. `lib/lsp/lspclientextension.ts` compiles the bridge on activation with the *bundled* `./rebar3` (`RebarShell`), picks a free TCP port, then `ErlangShellLSP.Start` spawns `erl -noshell -pa src -pa ebin -s int -s vscode_lsp_entry start <port>` with cwd `_build/default/lib/vscode_lsp`.
2. Client and server speak LSP over a plain TCP socket (`StreamInfo` reader/writer), not stdio.
3. `vscode_lsp_entry:compile_needed_modules/0` **recompiles and hot-loads a hardcoded list of `src/*.erl` modules in memory** before starting the app. Editing an existing bridge module only needs an LSP restart; **adding a new module means adding it to that list** or it will run stale/missing.
4. `gen_lsp_server` maps the LSP method name to an atom (`textDocument/hover` -> `textDocument_hover`) and `apply(lsp_handlers, F, [Socket, ArgsMap])`. A method whose handler isn't exported from `lsp_handlers` is silently unsupported — new LSP features start with an export there.

Supervision: `vscode_lsp_app` -> `vscode_lsp_app_sup` -> `gen_lsp_sup` plus `gen_lsp_doc_server` (open documents, syntax trees, references/inlay-hint caches), `gen_lsp_config_server` (settings pushed from the client, standard modules/BIFs), `gen_lsp_help_server` (OTP doc lookup). Analysis lives in `lsp_navigation`, `lsp_syntax`, `lsp_parse`, `lsp_completion`, `lsp_signature`, `lsp_inlayhints`, `lsp_rename`.

Settings cross the boundary through the client middleware `Configuration.computeConfiguration` (`lspclientextension.ts`), which answers `workspace/configuration` for section `erlang` (via `resolveErlangSettings`) and a synthetic `<computed>` section (autosave, tmpdir, username). `erlang.cacheManagement` is passed on the `erl` command line as `-vscode_cache_mgmt ...` and read by `gen_lsp_doc_server`.

### Debugger path

`lib/erlangDebug.ts` is a second webpack entry point (`out/lib/erlangDebug.js`) running `ErlangDebugSession` (DAP). It starts two things: an `ErlangConnection` HTTP server in Node, and the debuggee `erl` via `ErlangShellDebugger`, loading `vscode_connection.beam` from `_build/default/lib/ebin` (note: `ebin` next to `vscode_lsp`, compiled by `erlangConnection.ts`, deliberately separate from the LSP beams). The debuggee uses OTP `int`; `vscode_connection.erl` receives commands as small HTTP POSTs and posts events back to the Node server. Breakpoints/`int:ni/1` are shipped via a generated "arguments file" that is compiled and `configure()`d inside the debuggee.

### Vendored Erlang code

`vscode_erlfmt*` (erlfmt, drives `textDocument/formatting` with `erlang.formattingLineLength`) and `vscode_jsone*` (JSON) are vendored under `apps/erlangbridge/src` with a `vscode_` prefix. `vscode_erlfmt_parse.erl` is generated from `vscode_erlfmt_parse.yrl`. Treat these as upstream copies — don't refactor them casually.

## Conventions

- New user-facing setting: add to `contributes.configuration` in `package.json`, surface it in `ErlangConfigurationProvider`/`erlangSettings.ts`, and read it in Erlang via `gen_lsp_config_server`.
- New command: `contributes.commands` in `package.json` + registration in the matching runner (`RebarRunner`, `EunitRunner`).
- `erlang.verbose` enables the `Erlang Language Server` output channel and technical traces; `erlang.verboseExcludeFilter` filters noisy methods.
- Grammar edits go through the `grammar` submodule (upstream repo), not this one; see `syntaxes/README.md` for the plist/yaml workflow.
- use always rg instead of grep

