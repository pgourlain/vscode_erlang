# Help & Troubleshooting

Symptoms, causes and fixes for the Erlang extension. Find your symptom below;
if none matches, see [Reporting a bug](#reporting-a-bug).

## First, check this

Three commands answer most questions (`Ctrl+Shift+P` / `Cmd+Shift+P`):

- **`Erlang: Check Erlang/OTP and rebar3 installation`** - shows which `erl` and
  which `rebar3` the extension actually uses.
- **`Erlang: Show Language Server Output`** - the server's log. Set
  `"erlang.verbose": true` for technical traces (noisy methods are filtered by
  `erlang.verboseExcludeFilter`).
- **`Erlang: Restart Language Server`** - after changing a path setting or
  installing another OTP version.

The status bar shows the language server state and the OTP version it runs on.

## Nothing works: no completion, no errors, no navigation

The language server did not start. Run `Erlang: Check Erlang/OTP and rebar3
installation`, then open the output channel.

`erl` not found: set `erlang.erlangPath` to the **directory** holding
`erl`/`escript`, not to the binary:

```json
{
    "erlang.erlangPath": "/home/me/.asdf/installs/erlang/27.2/bin"
}
```

`${workspaceFolder}` is substituted in `erlang.erlangPath` and
`erlang.rebarPath` (only in those two settings).

rebar3 is looked up in `erlang.rebarPath`, then in the project folder, then on
the `PATH`, and finally falls back to the rebar3 bundled with the extension.

## `spawn /bin/sh ENOENT`, or nothing starts in a sandbox or a VS Code fork

The extension spawns `erl`/`escript`/`erlc` through a shell only where one is
needed. Control it with `erlang.useShell`:

- `auto` (default) - a shell on Windows only (`cmd.exe` is needed there for
  `.bat`/`.cmd` and for quoting), direct spawn elsewhere.
- `never` - force direct spawning, for sandboxed environments that block or do
  not ship a shell.
- `always` - when your `erlangArgs`, `erlangPath` or `rebarPath` rely on shell
  expansion.

## Paths with spaces

Arguments are quoted when spawning through a shell. If a custom `erlangPath` or
`rebarPath` containing spaces still fails, set `"erlang.useShell": "always"`.

## "Go to definition" lands in `_build`, or finds nothing

Exclude the build output from the workspace. The language server merges VS
Code's `files.exclude` and `search.exclude`, so either works - in your
`settings.json`:

```json
{
    "search.exclude": {
        "**/_build": true
    }
}
```

## False "can't find include file" errors

Include directories are searched in this order:

1. The directory of the file itself, and the sibling `include/` directory when
   the file is under `src/` or `test/`.
2. `erlang.includePaths` (relative paths are resolved against the workspace
   root).
3. `{i, "..."}` entries in `erl_opts` of the nearest `rebar.config`.
4. `apps/`, `lib/`, `_build/default/lib`, `_build/default/plugins` under the
   workspace root.

Prefer adding `{i, "..."}` to `rebar.config`, so that the compiler and the
extension agree. Use `erlang.includePaths` for headers living outside the
project.

## Other false errors: parse transforms, behaviours, missing deps

Analysis compiles your modules, so dependencies and parse transforms must exist
as beams: run `Erlang: rebar compile` (or the `rebar3: compile` task) once, then
`Erlang: Restart Language Server`.

Last resort: `"erlang.linting": false` disables validation while typing.

## Wrong colors, or a semantic tokens error

Set `"erlang.semanticTokensEnabled": false` to fall back to the TextMate grammar
alone, and report the error with `erlang.verbose` enabled.

Pure highlighting bugs (a keyword or a construct colored wrong with semantic
tokens off) come from the grammar, which lives in the `grammar` submodule - see
[syntaxes/README.md](./syntaxes/README.md).

## The extension uses too much memory on a large project

`erlang.cacheManagement` decides where the large cache tables go:

- `memory` (default) - fastest.
- `compressed memory` - about 50% less memory.
- `file` - temporary files, lowest memory.

Restart the language server after changing it.

## Breakpoints are never hit

- Modules must be compiled with `debug_info` (the rebar3 default). Add
  `"preLaunchTask": "rebar3: compile"` to the launch configuration so that
  modified files are rebuilt before debugging.
- Function breakpoints use the format `module:function/arity`.
- Attaching to a running node requires a distributed node (`-sname`/`-name`) on
  the same machine, with the `debugger` and `inets` applications available - see
  [Attach to a running node](./README.md#attach-to-a-running-node).

## Changing the debugger mode has no effect

`erlang.debuggerRunMode` is read at extension startup: restart VS Code after
changing it.

## Formatting does too much, or nothing at all

Formatting is [erlfmt](https://github.com/WhatsApp/erlfmt); it reformats the
whole document by design.

- `erlang.formatterEnabled` - turn document, selection and on-type formatting
  off.
- `erlang.formattingLineLength` - maximum line length (default `100`).
- On-type formatting triggers on `.`, `;`, `,` and newline. Format-on-save is
  off by default for Erlang files; enable it under `"[erlang]"` in your
  settings.

## No CodeLens, no inlay hints

Both are **disabled by default**:

```json
{
    "erlang.codeLensEnabled": true,
    "erlang.inlayHintsEnabled": true
}
```

Inlay hints only cover local calls, and are shown when the argument name does
not match the parameter name.

## Remote SSH, dev containers, WSL

The extension runs on the remote side, so Erlang/OTP and rebar3 must be
installed **there**, and `erlang.erlangPath` must be a remote path. Set it in
the remote or workspace settings, not in your local user settings.

A ready-made container setup:
[vscode-remote-try-erlang](https://github.com/pgourlain/vscode-remote-try-erlang).

## Reporting a bug

Open an issue at
[pgourlain/vscode_erlang/issues](https://github.com/pgourlain/vscode_erlang/issues)
and include:

- the output of `Erlang: Check Erlang/OTP and rebar3 installation`;
- OTP version, extension version, VS Code (or fork) version and OS;
- the `Erlang Language Server` output with `"erlang.verbose": true`;
- the smallest rebar3 project that reproduces the problem.
