# vscode_erlang — feature roadmap

Multi-session task tracker. **Read this file first when resuming work.**

## How to use this file

- Each task has: **id**, goal, target files, verification command, dependencies, status.
- Status values: `todo` / `in-progress` / `done` / `blocked`.
- Update the status line as part of the same change set, in the working tree.
  Never commit it yourself — see "Git discipline" above.
- Work one task per session where possible. Tasks are sized to fit a single session.

## ⛔ Git discipline (hard rule)

Never run `git add`, `git commit`, `git push`, `git merge`, `git rebase`,
`git reset`, or `gh pr create`. Version control is a human decision, with no
exception — including when a task looks finished and its verification passes.

Read-only git is fine: `git status`, `git diff`, `git log`, `git show`.

At the end of a task: leave the working tree dirty, print `git diff --stat`,
and propose a commit message **as text** in the reply. Staging, committing and
pushing are the human's job.


## Architectural rules (do not violate)

1. **Erlang does the analysis. TypeScript is only a wrapper.** Any new language feature is an Erlang module under `apps/erlangbridge/src/` plus an export in `lsp_handlers`. TS changes are limited to `package.json` declarations, settings plumbing, and the two places VS Code has no LSP equivalent (Test Explorer, debug adapter).
2. `gen_lsp_server:call_handler/3` dispatches **reflectively**: any exported `Fun/2` in `lsp_handlers` is automatically an LSP endpoint (method name `/` → `_`, leading `$/` stripped). Adding a feature = new module + export + capability flag.
3. **Every new `src/*.erl` module must be added to `vscode_lsp_entry:compile_needed_modules/0`** or it runs stale/missing at runtime.
4. Vendored `vscode_erlfmt*` and `vscode_jsone*` are upstream copies — do not refactor them.
5. `grammar/` is a submodule. Grammar edits go upstream, not here.

## Reuse before writing new code

| Need | Already exists |
|---|---|
| definition / references / hover / codelens / symbols | `lsp_navigation.erl` (`function_clauses/2`, `local_function_references/*`, `symbol_info`, `codelens_info`) |
| syntax tree walking, macro extraction | `lsp_syntax.erl` |
| AST ranges for functions and types | `lsp_fun_utils.erl` (`get_function_range/1`, `get_type_range/1`) |
| URI ↔ path, LSP range/position building, glob exclusion | `lsp_utils.erl` (`client_range/3,4`, `client_position/1`) |
| cached syntax trees, references, inlay hints, module→file index, project scan | `gen_lsp_doc_server.erl` |
| settings and feature toggles | `gen_lsp_config_server.erl` |
| OTP docs / EEP-48 chunks | `gen_lsp_help_server.erl` |
| WorkspaceEdit construction | `lsp_rename.erl` |

---

# ⛔ PHASE 0 — CHARACTERIZATION TEST NET (HARD GATE)

**No file under `apps/erlangbridge/src/` or `lib/` may be modified until every Phase 0 task is `done`.**

Why: today's coverage is 5 CT cases and 4 TS cases. Hover, completion, signature help, formatting, codeLens, documentSymbol, inlayHints, inlineValues, rename and references have **zero** tests. Every later phase edits `lsp_handlers.erl`. Without this net, breakage is silent.

**Characterization discipline**: pin behaviour *exactly as it is today*, bugs included. Where a test documents a known bug, mark it:

```erlang
%% CHARACTERIZATION: current behaviour is wrong, see task 5.3
```

Do not fix anything in Phase 0.

**Suite pattern** — follow `apps/erlangbridge/test/lsp_navigation_SUITE.erl`:

```erlang
init_per_suite(Config) ->
    ?assertEqual(ok, application:start(vscode_lsp, permanent)),
    gen_lsp_config_server:update_config(erlang, #{verbose => false}),
    Config.
```

then call the analysis module directly against `.erl` fixtures in `<suite>_SUITE_data/`.

---

### 0.1 — `lsp_protocol_SUITE` (protocol-level net)

- **Status**: todo
- **Goal**: TCP client helper (`Content-Length` framing + `vscode_jsone`), `initialize` handshake, assert the **full capability map verbatim** against a golden term. Also: unknown method → error `-32001`; `shutdown`/`exit`; `$/cancelRequest`; `$/setTrace`.
- **Why it matters most**: module-level tests cannot reach `lsp_handlers` (handlers take a `Socket`). This suite is the tripwire that makes every later `lsp_handlers.erl` edit safe, and forces each capability flip to be a visible, deliberate diff.
- **Files**: `apps/erlangbridge/test/lsp_protocol_SUITE.erl` (new)
- **Reference**: capability map at `apps/erlangbridge/src/lsp_handlers.erl:22-54`; framing/dispatch at `gen_lsp_server.erl:114-205`
- **Deps**: none — **start here**
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_protocol_SUITE`

### 0.2 — `lsp_syntax_SUITE`

- **Status**: todo
- **Goal**: `erl_lint` diagnostics for fixtures with: syntax error, unused variable, unused function, missing include, bad record field. Pin severity mapping and the `data`/`correlation_data` field shape. Plus `lsp_parse` on `.src` and `rebar.config` fixtures.
- **Files**: `apps/erlangbridge/test/lsp_syntax_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_handlers.erl:472-500` (publishDiagnostics + severity map), `lsp_syntax.erl`, `lsp_parse.erl`
- **Deps**: 0.15 helpful, not required
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_syntax_SUITE`

### 0.3 — `lsp_completion_SUITE`

- **Status**: todo
- **Goal**: completion after `mod:`, after `#`, after `?`, after `-`, bare atom prefix, variable in scope, record field. Pin item kinds and `insertText`.
- **Files**: `apps/erlangbridge/test/lsp_completion_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_completion.erl`, dispatch at `lsp_handlers.erl:188`
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_completion_SUITE`

### 0.4 — `lsp_hover_SUITE`

- **Status**: todo
- **Goal**: hover on an OTP function (EEP-48 path), on a project function (head clauses), on a macro, on a record, on a type. Covers `hover_doc_layout` and `gen_lsp_help_server`.
- **Files**: `apps/erlangbridge/test/lsp_hover_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_handlers.erl:179`, `lsp_navigation.erl` hover_info, `hover_doc_layout.erl`
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_hover_SUITE`

### 0.5 — `lsp_signature_SUITE`

- **Status**: todo
- **Goal**: signature help at each argument position, with and without `-spec`, retrigger on `,`. Covers `lsp_signature_doc_layout`.
- **Files**: `apps/erlangbridge/test/lsp_signature_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_signature.erl`, `lsp_handlers.erl:326`
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_signature_SUITE`

### 0.6 — `lsp_format_SUITE`

- **Status**: todo
- **Goal**: `vscode_erlfmt` round-trip — idempotence (format twice = identical), `erlang.formattingLineLength` honored, already-formatted file unchanged. Pin the current hardcoded `0,0 → 999999,255` result range as characterization.
- **Files**: `apps/erlangbridge/test/lsp_format_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_handlers.erl:201-219`, `:427-455` (OTP ≥ 21 → erlfmt, else `erl_tidy`)
- **Note**: mark the hardcoded range with `%% CHARACTERIZATION: ... see task 5.3`
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_format_SUITE`

### 0.7 — `lsp_symbols_SUITE`

- **Status**: todo
- **Goal**: `documentSymbol` result shape, and `codeLens` counts (exported / references / unused) for a fixture with known call sites.
- **Files**: `apps/erlangbridge/test/lsp_symbols_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_handlers.erl:220-250` (codeLens), `:276` (documentSymbol)
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_symbols_SUITE`

### 0.8 — `lsp_inlayhints_SUITE`

- **Status**: todo
- **Goal**: hints for local calls. Pin the two known limits as characterization: no hints for remote calls, no `-spec`-derived arg names.
- **Files**: `apps/erlangbridge/test/lsp_inlayhints_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_inlayhints.erl:28` (`%TODO, get args from spec if exists`), `lsp_handlers.erl:252-275`
- **Note**: mark both limits with `%% CHARACTERIZATION: ... see task 5.8`
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_inlayhints_SUITE`

### 0.9 — `lsp_inlinevalues_SUITE`

- **Status**: todo
- **Goal**: variable values for a paused-frame scenario. Pin the non-standard plural method name `textDocument/inlineValues` (which is what the client actually calls).
- **Files**: `apps/erlangbridge/test/lsp_inlinevalues_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_handlers.erl:290-306`, client at `lib/lsp/lsp-inlinevalues.ts:36-38`
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_inlinevalues_SUITE`

### 0.10 — `lsp_rename_SUITE`

- **Status**: todo
- **Goal**: `prepareRename` accept/reject positions; `rename` of a local variable, local function, exported function (cross-file `WorkspaceEdit`), record, macro.
- **Files**: `apps/erlangbridge/test/lsp_rename_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_rename.erl`, `lsp_handlers.erl:354-368`
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_rename_SUITE`

### 0.11 — Extend `lsp_navigation_SUITE`

- **Status**: todo
- **Goal**: add `references` coverage (currently untested), plus definition on records, types, macros, includes, behaviour callbacks.
- **Files**: `apps/erlangbridge/test/lsp_navigation_SUITE.erl` (extend), `lsp_navigation_SUITE_data/` (add fixtures)
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_navigation_SUITE`

### 0.12 — `gen_lsp_doc_server_SUITE`

- **Status**: todo
- **Goal**: open/change/close lifecycle, cache invalidation, project scan, module→file index, all three `erlang.cacheManagement` modes (`memory`, `compressed memory`, `file`).
- **Why**: this module is rewritten by 1.4 (incremental sync) and 4.1 (symbol index). Needs the strongest net.
- **Files**: `apps/erlangbridge/test/gen_lsp_doc_server_SUITE.erl` + `_SUITE_data/` (new)
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/gen_lsp_doc_server_SUITE`

### 0.13 — `gen_lsp_config_server_SUITE`

- **Status**: todo
- **Goal**: every getter (`codeLensEnabled/0`, `inlayHintsEnabled/0`, `linting/0`, `autosave/0`, `formatting_line_length/0`, `verbose/0`) against a config map matching what the TS side actually sends.
- **Files**: `apps/erlangbridge/test/gen_lsp_config_server_SUITE.erl` (new)
- **Reference**: TS payload built in `lib/ErlangConfigurationProvider.ts` (`resolveErlangSettings`) and `lib/lsp/lspclientextension.ts:67` (`computeConfiguration`)
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/gen_lsp_config_server_SUITE`

### 0.14 — Module-list guard

- **Status**: todo
- **Goal**: CT case asserting every `src/lsp_*.erl` and `src/gen_lsp_*.erl` module appears in `vscode_lsp_entry:compile_needed_modules/0`.
- **Expected**: **this test fails on first write** — `lsp_fun_utils` and `lsp_signature_doc_layout` are missing from the list today. That failure is the point; it is fixed in task 1.0. Until then leave the case in the suite's `all()` and record the known failure here.
- **Files**: `apps/erlangbridge/test/vscode_lsp_entry_SUITE.erl` (new)
- **Reference**: `apps/erlangbridge/src/vscode_lsp_entry.erl:28-35`
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/vscode_lsp_entry_SUITE` (expect 1 failure until 1.0)

### 0.15 — Shared fixture project

- **Status**: todo
- **Goal**: one realistic multi-module rebar3 project reused by several suites instead of duplicating `_SUITE_data`: behaviour + implementor, records, macros, includes, types, cross-module calls, a `_SUITE.erl`, an eunit module.
- **Files**: `apps/erlangbridge/test/fixtures_common/` (new)
- **Deps**: do early — 0.2 through 0.12 all point at it
- **Verify**: `./rebar3 ct` (all suites still green)

### 0.16 — CI hardening

- **Status**: todo
- **Goal**: extend the workflow to an OTP-version matrix (code branches on OTP ≥ 21 for erlfmt vs `erl_tidy`), and fail the build on any CT failure.
- **Files**: `.github/workflows/pr-verify.yml`
- **Verify (human)**: after a manual branch push, confirm matrix jobs run and a
  deliberately broken test fails the build

### ✅ Phase 0 exit gate

All of the above `done`, plus:

```bash
./rebar3 ct          # green
npm run compile && npm test   # green
```

Only then may `apps/erlangbridge/src/` or `lib/` be edited.

---

# PHASE 1 — foundation (first production edits)

### 1.0 — Fix the module list

- **Status**: todo
- **Goal**: add `lsp_fun_utils` and `lsp_signature_doc_layout` to `compile_needed_modules/0`. Makes 0.14 green.
- **Files**: `apps/erlangbridge/src/vscode_lsp_entry.erl`
- **Deps**: 0.14
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/vscode_lsp_entry_SUITE`

### 1.1 — Engine bump to `^1.75.0`

- **Status**: todo
- **Goal**: bump `engines.vscode` and `@types/vscode`, then fix every type error from 23 releases of API churn.
- **Breaking surface to audit**: drop the superseded `vscode-test@^1.3.0` devDep (`@vscode/test-electron` is already present); `DiagnosticTag` / `CodeActionKind` enum additions; deprecated `rootPath`; `tsconfig.json` `lib` is `es7` — may need raising.
- **Files**: `package.json`, `tsconfig.json`, any `lib/**/*.ts` the compiler flags
- **Deps**: Phase 0 gate
- **Verify**: `npm run compile && npm test`

### 1.2 — Manifest hygiene

- **Status**: todo
- **Goal**: add the missing `onCommand:extension.rebareunit` to `activationEvents`; declare `capabilities.untrustedWorkspaces` and `capabilities.virtualWorkspaces` (the extension spawns `erl`, so both are restricted — must be explicit or VS Code warns the user).
- **Files**: `package.json`
- **Verify**: `npm run compile && npm test`; check the Extensions view shows no trust warning

### 1.3 — `workspaceFolders` / `rootUri`

- **Status**: todo
- **Goal**: `initialize/2` reads only the deprecated `rootPath`. Read `workspaceFolders`, advertise `workspace.workspaceFolders.supported`, handle `workspace/didChangeWorkspaceFolders`. Prerequisite for multi-root umbrella projects, which are very common in Erlang.
- **Files**: `apps/erlangbridge/src/lsp_handlers.erl:16-19`, `gen_lsp_config_server.erl`, `gen_lsp_doc_server.erl`
- **Deps**: 0.1, 0.12, 0.13
- **Verify**: `./rebar3 ct`; update the golden capability map in `lsp_protocol_SUITE` deliberately

### 1.4 — Incremental document sync

- **Status**: todo
- **Goal**: `textDocumentSync => 1` (Full) resends the whole buffer on every keystroke. Move to `2` (Incremental) with range-apply in `gen_lsp_doc_server`. Do this **before** semantic tokens, which is latency-sensitive.
- **Files**: `apps/erlangbridge/src/lsp_handlers.erl` (`textDocument_didChange/2`, capability), `gen_lsp_doc_server.erl`
- **Deps**: 0.12 (strong net required)
- **Verify**: `./rebar3 ct`; manual typing test with `erlang.verbose: true`

### 1.5 — Dead code removal

- **Status**: todo
- **Goal**: three cleanups on the TS side —
  1. delete `lib/lsp/lsp-rename.ts` (`ErlangRenameProvider` throws "not implemented", registration already commented out; rename works via the standard LSP capability);
  2. remove the vestigial `**/.clientrc` watcher (`lspclientextension.ts:231-233`), left over from the LSP sample;
  3. fix the file watcher (`lspclientextension.ts:104-112`) which sends `uri: uri.fsPath` — a raw path where the protocol wants a URI string — and ignores change events.
- **Files**: `lib/lsp/lsp-rename.ts` (delete), `lib/lsp/lspclientextension.ts`
- **Verify**: `npm run compile && npm test`; confirm rename still works in `Launch Extension`

---

# PHASE 2 — code actions & quick fixes

Highest daily value for Erlang developers. `codeActionProvider` is `false` today.

Set `codeActionProvider => #{codeActionKinds => [...], resolveProvider => true}` and add `executeCommandProvider`. Extend the diagnostic `data`/`correlation_data` field so each fix matches its diagnostic without re-analysis.

| id | goal | status |
|---|---|---|
| 2.1 | Infrastructure: `textDocument_codeAction/2`, `codeAction/resolve`, `workspace/executeCommand`, `WorkspaceEdit` builders (reuse `lsp_rename.erl`) | todo |
| 2.2 | `erl_lint`-driven fixes: unused variable → prefix `_`; unused function → add to `-export`; unused/missing include; undefined function → create stub clause; unbound record field | todo |
| 2.3 | Export/spec actions: add/remove from `-export`; generate `-spec` from inferred clause heads (reuse `lsp_navigation:function_clauses/2` + arg-name logic in `lsp_inlayhints.erl`) | todo |
| 2.4 | Behaviour support: on `-behaviour(X)`, "Implement missing callbacks" — callbacks via `gen_lsp_help_server`/EEP-48, generate stubs | todo |
| 2.5 | Refactors: extract function from selection; inline variable; convert `if`↔`case`. Ship after 2.2-2.4 | todo |
| 2.6 | Source actions: organize/sort `-export`; add missing `-module` | todo |

- **New file**: `apps/erlangbridge/src/lsp_codeaction.erl` (+ add to `compile_needed_modules/0`)
- **New test**: `apps/erlangbridge/test/lsp_codeaction_SUITE.erl`
- **Deps**: Phase 0 gate, 0.2 (diagnostic shape)
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_codeaction_SUITE`

---

# PHASE 3 — semantic tokens

Highlighting is TextMate-only today. Semantic tokens give real macro/record/type/variable-binding coloring from the AST.

| id | goal | status |
|---|---|---|
| 3.1 | Legend + full-document tokenizer over the cached syntax tree. Types: `namespace` (module), `function`, `macro`, `variable`, `parameter`, `type`, `struct` (record), `property` (record field), `string`, `number`, `comment`, `keyword`, `operator`. Modifiers: `definition`, `declaration`, `readonly`, `deprecated` (from `-deprecated`), `defaultLibrary` (OTP modules — already known to `gen_lsp_config_server`) | todo |
| 3.2 | Range variant + delta (per-document token cache keyed by result id, alongside existing caches) | todo |
| 3.3 | `contributes.semanticTokenScopes` in `package.json` mapping Erlang tokens to TextMate scopes so themes without semantic support degrade correctly | todo |
| 3.4 | Setting `erlang.semanticTokensEnabled` (default `true`), plumbed like `codeLensEnabled` | todo |

- **New file**: `apps/erlangbridge/src/lsp_semantic_tokens.erl` (+ `compile_needed_modules/0`)
- **Exports**: `textDocument_semanticTokens_full/2`, `_range/2`, `_full_delta/2`
- **Correctness bar**: must not fight the TextMate grammar in `grammar/Erlang.plist` (submodule — not edited here)
- **Deps**: 1.1 (engine), 1.4 (incremental sync)
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_semantic_tokens_SUITE`; visual check in `Launch Extension` against a fixture with macros, records and types

---

# PHASE 4 — navigation & hierarchies

| id | goal | status |
|---|---|---|
| 4.1 | **`workspace/symbol`** — export `workspace_symbol/2`, capability `workspaceSymbolProvider => #{resolveProvider => true}`. Build a symbol index (functions, records, types, macros, behaviours) on the existing `gen_lsp_doc_server` module→file index and project scan. **Single most-missed navigation feature.** | todo |
| 4.2 | `textDocument/declaration` — the line is already written and commented out at `lsp_handlers.erl:27`. Cheap win. | todo |
| 4.3 | `textDocument/typeDefinition` — from a `-spec`/`-type` usage to the `-type`/`-opaque` definition. `lsp_fun_utils:get_type_range/1` already exists. | todo |
| 4.4 | `textDocument/implementation` — from a `-callback` to all modules with `-behaviour(That)`, and from a `-behaviour` attribute to the behaviour module | todo |
| 4.5 | Call hierarchy — `textDocument/prepareCallHierarchy`, `callHierarchy/incomingCalls`, `outgoingCalls`. Built on `lsp_navigation:local_function_references/*` + the project references cache | todo |
| 4.6 | Type hierarchy — behaviour ↔ implementors, over the 4.4 index | todo |
| 4.7 | `textDocument/documentHighlight` — all occurrences of the variable/function/record under the cursor; Read/Write kinds where the AST allows | todo |

- **New files**: `lsp_workspace_symbol.erl`, `lsp_hierarchy.erl` (+ `compile_needed_modules/0`)
- **Deps**: Phase 0 gate, 0.11, 0.12
- **Verify**: per-module CT suite each

---

# PHASE 5 — editing polish

| id | goal | status |
|---|---|---|
| 5.1 | Folding range — functions, clauses, `case`/`receive`/`try`, `-export` lists, comment blocks (`region` kind), `%% region` / `%% endregion` markers | todo |
| 5.2 | Selection range — expand-selection following AST nesting | todo |
| 5.3 | Range formatting — `documentRangeFormattingProvider => true`. `vscode_erlfmt` works on whole forms, so map the requested range to enclosing forms. **Also replace the hardcoded `0,0 → 999999,255` result range** (`lsp_handlers.erl:201-219`) with the real document end; update the 0.6 characterization assertion deliberately | todo |
| 5.4 | On-type formatting on `.`, `;`, `,`, newline — AST-driven indent that properly fixes the guard-outdent bug at `lib/extension.ts:167`, replacing the ~17 regex `onEnterRules` | todo |
| 5.5 | `erlang.formatterEnabled` setting; CT case for `editor.formatOnSave` behaviour | todo |
| 5.6 | `completionItem/resolve` — advertise `resolveProvider => true`, move doc/detail fetching (currently eager) into resolve, a big latency win. Add snippet support (`insertTextFormat => 2`) for calls with argument placeholders; widen trigger characters beyond `:#.` (`?` for macros, `-` for attributes at line start) | todo |
| 5.7 | `textDocument/documentLink` — make `-include`/`-include_lib` paths and comment URLs clickable | todo |
| 5.8 | `inlayHint/resolve` + polish — add `tooltip`, `paddingLeft/Right`, `textEdits`; fix the two limits pinned in 0.8 (local-calls-only, no `-spec`-derived names); add type-hint mode for `-spec` returns | todo |
| 5.9 | Pull diagnostics — `diagnosticProvider` (LSP 3.17) alongside the existing push model, plus `workspace/diagnostic` for project-wide problems without opening files | todo |
| 5.10 | `codeLens/resolve` — server-side resolve so lens computation is lazy; today resolve is a client no-op (`lib/lsp/lspcodelens.ts:59`) | todo |

- **New file**: `lsp_folding.erl` (+ `compile_needed_modules/0`)
- **Deps**: Phase 0 gate; 5.3 depends on 0.6; 5.8 depends on 0.8

---

# PHASE 6 — Test Explorer (Erlang-driven)

EUnit/CT results currently surface only as diagnostics. TS is a **thin shell** over custom LSP requests — no test logic in TypeScript.

| id | goal | status |
|---|---|---|
| 6.1 | Custom request `erlang/discoverTests` → Erlang scans for EUnit (`*_test/0`, `*_test_/0`, `-ifdef(TEST)` blocks) and Common Test (`*_SUITE.erl`, `all/0`, `groups/0`), returns a tree with source ranges. Reuse the `gen_lsp_doc_server` project scan and `lsp_syntax` parsing | todo |
| 6.2 | TS `TestController` building `TestItem`s from that tree, refreshed on `didSave` / watcher events | todo |
| 6.3 | Custom request `erlang/runTests` with progress notifications → Erlang runs eunit/ct and streams per-test results. Reuse `eunit_jsonreport.erl`; add a CT hook equivalent | todo |
| 6.4 | Map results to `TestRun.passed/failed/skipped` with `TestMessage` (expected/actual + location), replacing diagnostics-only reporting at `lib/eunitRunner.ts:20` | todo |
| 6.5 | `TestRunProfile` of kind `Debug` launching the existing DAP session with the test as entry point | todo |
| 6.6 | Coverage — `cover`-based `TestRunProfile` kind `Coverage`. **Needs engine ≥ 1.88**, so gated behind a second engine bump. Stretch | blocked |

- **New files**: `apps/erlangbridge/src/lsp_testing.erl` (+ `compile_needed_modules/0`), `lib/lsp/lsp-testcontroller.ts`
- **Deps**: 1.1 (engine ≥ 1.75 for the `vscode.tests` API)
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_testing_SUITE`; then `Launch Extension` and run the fixture project's tests from the Testing sidebar

---

# PHASE 7 — ecosystem & polish

| id | goal | status |
|---|---|---|
| 7.1 | Task provider — `contributes.taskDefinitions` + `vscode.tasks.registerTaskProvider` for rebar3 targets (compile, ct, eunit, dialyzer, release, shell), auto-detected from `rebar.config`; complements the ad-hoc commands in `RebarRunner` | todo |
| 7.2 | Dialyzer — keep the command, add incremental PLT status in the status bar | todo |
| 7.3 | Status bar — persistent item showing LSP state (starting / ready / failed) + OTP version. Today a startup failure is silent unless `erlang.verbose` | todo |
| 7.4 | Walkthrough — `contributes.walkthroughs` for first-run setup (find `erl`, pick rebar3, run a build). Good marketplace signal | todo |
| 7.5 | `configurationDefaults` — sensible `[erlang]` editor defaults (tab size, format-on-save opt-in, word pattern) | todo |
| 7.6 | Debug adapter — `attach` request (attach to a running node); only `launch` is declared today. Plus `configurationSnippets` and `variables` for `erlpath` | todo |
| 7.7 | Docs — README settings list is stale (missing `debuggerRunMode`, `formattingLineLength`, `verboseExcludeFilter`); add a feature matrix; update `CHANGELOG.md` per phase | todo |

- **New file**: `lib/erlangTaskProvider.ts`

---

## Capability scoreboard

Source of truth: `apps/erlangbridge/src/lsp_handlers.erl:22-54`. Update as flags flip.

| Capability | Today | Target task |
|---|---|---|
| `textDocumentSync` | `1` (Full) | 1.4 → `2` |
| `completionProvider` | ✅ (no resolve) | 5.6 |
| `hoverProvider` | ✅ | — |
| `signatureHelpProvider` | ✅ | — |
| `definitionProvider` | ✅ | — |
| `referencesProvider` | ✅ | — |
| `documentSymbolProvider` | ✅ | — |
| `codeLensProvider` | ✅ (no resolve) | 5.10 |
| `documentFormattingProvider` | ✅ | — |
| `renameProvider` | ✅ | — |
| `inlineValueProvider` | ✅ | — |
| `inlayHintProvider` | ✅ (no resolve) | 5.8 |
| `declarationProvider` | ❌ commented out | 4.2 |
| `typeDefinitionProvider` | ❌ | 4.3 |
| `implementationProvider` | ❌ | 4.4 |
| `documentHighlightProvider` | ❌ | 4.7 |
| `codeActionProvider` | ❌ | 2.1 |
| `documentLinkProvider` | ❌ | 5.7 |
| `colorProvider` | ❌ | n/a for Erlang |
| `documentRangeFormattingProvider` | ❌ | 5.3 |
| `documentOnTypeFormattingProvider` | ❌ | 5.4 |
| `foldingRangeProvider` | ❌ | 5.1 |
| `executeCommandProvider` | ❌ | 2.1 |
| `selectionRangeProvider` | ❌ | 5.2 |
| `linkedEditingRangeProvider` | ❌ | not planned |
| `callHierarchyProvider` | ❌ | 4.5 |
| `semanticTokensProvider` | ❌ | 3.1 |
| `monikerProvider` | ❌ | not planned |
| `typeHierarchyProvider` | ❌ | 4.6 |
| `diagnosticProvider` | ❌ (push only) | 5.9 |
| `workspaceSymbolProvider` | ❌ | 4.1 |
| `workspace.workspaceFolders` | ❌ absent | 1.3 |

## Commands

```bash
# Erlang
./rebar3 compile
./rebar3 ct
./rebar3 ct --suite apps/erlangbridge/test/<suite>
./rebar3 ct --suite apps/erlangbridge/test/<suite> --case <case>

# TypeScript
npm run compile
npm test
npx vscode-test --grep "<pattern>"

# Manual
# Run sidebar -> "Launch Extension", with erlang.verbose: true
# watch the "Erlang Language Server" output channel
```
