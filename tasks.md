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

- **Status**: done
- **Goal**: TCP client helper (`Content-Length` framing + `vscode_jsone`), `initialize` handshake, assert the **full capability map verbatim** against a golden term. Also: unknown method → error `-32001`; `shutdown`/`exit`; `$/cancelRequest`; `$/setTrace`.
- **Why it matters most**: module-level tests cannot reach `lsp_handlers` (handlers take a `Socket`). This suite is the tripwire that makes every later `lsp_handlers.erl` edit safe, and forces each capability flip to be a visible, deliberate diff.
- **Files**: `apps/erlangbridge/test/lsp_protocol_SUITE.erl` (new)
- **Reference**: capability map at `apps/erlangbridge/src/lsp_handlers.erl:22-54`; framing/dispatch at `gen_lsp_server.erl:114-205`
- **Deps**: none — **start here**
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_protocol_SUITE`

### 0.2 — `lsp_syntax_SUITE`

- **Status**: done
- **Goal**: `erl_lint` diagnostics for fixtures with: syntax error, unused variable, unused function, missing include, bad record field. Pin severity mapping and the `data`/`correlation_data` field shape. Plus `lsp_parse` on `.src` and `rebar.config` fixtures.
- **Files**: `apps/erlangbridge/test/lsp_syntax_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_handlers.erl:472-500` (publishDiagnostics + severity map), `lsp_syntax.erl`, `lsp_parse.erl`
- **Deps**: 0.15 helpful, not required
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_syntax_SUITE`

### 0.3 — `lsp_completion_SUITE`

- **Status**: done
- **Goal**: completion after `mod:`, after `#`, after `?`, after `-`, bare atom prefix, variable in scope, record field. Pin item kinds and `insertText`.
- **Files**: `apps/erlangbridge/test/lsp_completion_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_completion.erl`, dispatch at `lsp_handlers.erl:188`
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_completion_SUITE`

### 0.4 — `lsp_hover_SUITE`

- **Status**: done
- **Goal**: hover on an OTP function (EEP-48 path), on a project function (head clauses), on a macro, on a record, on a type. Covers `hover_doc_layout` and `gen_lsp_help_server`.
- **Files**: `apps/erlangbridge/test/lsp_hover_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_handlers.erl:179`, `lsp_navigation.erl` hover_info, `hover_doc_layout.erl`
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_hover_SUITE`

### 0.5 — `lsp_signature_SUITE`

- **Status**: done
- **Goal**: signature help at each argument position, with and without `-spec`, retrigger on `,`. Covers `lsp_signature_doc_layout`.
- **Files**: `apps/erlangbridge/test/lsp_signature_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_signature.erl`, `lsp_handlers.erl:326`
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_signature_SUITE`

### 0.6 — `lsp_format_SUITE`

- **Status**: done
- **Goal**: `vscode_erlfmt` round-trip — idempotence (format twice = identical), `erlang.formattingLineLength` honored, already-formatted file unchanged. Pin the current hardcoded `0,0 → 999999,255` result range as characterization.
- **Files**: `apps/erlangbridge/test/lsp_format_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_handlers.erl:201-219`, `:427-455` (OTP ≥ 21 → erlfmt, else `erl_tidy`)
- **Note**: mark the hardcoded range with `%% CHARACTERIZATION: ... see task 5.3`
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_format_SUITE`

### 0.7 — `lsp_symbols_SUITE`

- **Status**: done
- **Goal**: `documentSymbol` result shape, and `codeLens` counts (exported / references / unused) for a fixture with known call sites.
- **Files**: `apps/erlangbridge/test/lsp_symbols_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_handlers.erl:220-250` (codeLens), `:276` (documentSymbol)
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_symbols_SUITE`

### 0.8 — `lsp_inlayhints_SUITE`

- **Status**: done
- **Goal**: hints for local calls. Pin the two known limits as characterization: no hints for remote calls, no `-spec`-derived arg names.
- **Files**: `apps/erlangbridge/test/lsp_inlayhints_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_inlayhints.erl:28` (`%TODO, get args from spec if exists`), `lsp_handlers.erl:252-275`
- **Note**: mark both limits with `%% CHARACTERIZATION: ... see task 5.8`
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_inlayhints_SUITE`

### 0.9 — `lsp_inlinevalues_SUITE`

- **Status**: done
- **Goal**: variable values for a paused-frame scenario. Pin the non-standard plural method name `textDocument/inlineValues` (which is what the client actually calls).
- **Files**: `apps/erlangbridge/test/lsp_inlinevalues_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_handlers.erl:290-306`, client at `lib/lsp/lsp-inlinevalues.ts:36-38`
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_inlinevalues_SUITE`

### 0.10 — `lsp_rename_SUITE`

- **Status**: done
- **Goal**: `prepareRename` accept/reject positions; `rename` of a local variable, local function, exported function (cross-file `WorkspaceEdit`), record, macro.
- **Files**: `apps/erlangbridge/test/lsp_rename_SUITE.erl` + `_SUITE_data/` (new)
- **Reference**: `lsp_rename.erl`, `lsp_handlers.erl:354-368`
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_rename_SUITE`

### 0.11 — Extend `lsp_navigation_SUITE`

- **Status**: done
- **Goal**: add `references` coverage (currently untested), plus definition on records, types, macros, includes, behaviour callbacks.
- **Files**: `apps/erlangbridge/test/lsp_navigation_SUITE.erl` (extend), `lsp_navigation_SUITE_data/` (add fixtures)
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_navigation_SUITE`

### 0.12 — `gen_lsp_doc_server_SUITE`

- **Status**: done
- **Goal**: open/change/close lifecycle, cache invalidation, project scan, module→file index, all three `erlang.cacheManagement` modes (`memory`, `compressed memory`, `file`).
- **Why**: this module is rewritten by 1.4 (incremental sync) and 4.1 (symbol index). Needs the strongest net.
- **Files**: `apps/erlangbridge/test/gen_lsp_doc_server_SUITE.erl` + `_SUITE_data/` (new)
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/gen_lsp_doc_server_SUITE`

### 0.13 — `gen_lsp_config_server_SUITE`

- **Status**: done
- **Goal**: every getter (`codeLensEnabled/0`, `inlayHintsEnabled/0`, `linting/0`, `autosave/0`, `formatting_line_length/0`, `verbose/0`) against a config map matching what the TS side actually sends.
- **Files**: `apps/erlangbridge/test/gen_lsp_config_server_SUITE.erl` (new)
- **Reference**: TS payload built in `lib/ErlangConfigurationProvider.ts` (`resolveErlangSettings`) and `lib/lsp/lspclientextension.ts:67` (`computeConfiguration`)
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/gen_lsp_config_server_SUITE`

### 0.14 — Module-list guard

- **Status**: done (suite added; 1 expected failure remains until task 1.0)
- **Goal**: CT case asserting every `src/lsp_*.erl` and `src/gen_lsp_*.erl` module appears in `vscode_lsp_entry:compile_needed_modules/0`.
- **Expected**: **this test fails on first write** — `lsp_fun_utils` and `lsp_signature_doc_layout` are missing from the list today. That failure is the point; it is fixed in task 1.0. Until then leave the case in the suite's `all()` and record the known failure here.
- **Files**: `apps/erlangbridge/test/vscode_lsp_entry_SUITE.erl` (new)
- **Reference**: `apps/erlangbridge/src/vscode_lsp_entry.erl:28-35`
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/vscode_lsp_entry_SUITE` (expect 1 failure until 1.0)

### 0.15 — Shared fixture project

- **Status**: done
- **Goal**: one realistic multi-module rebar3 project reused by several suites instead of duplicating `_SUITE_data`: behaviour + implementor, records, macros, includes, types, cross-module calls, a `_SUITE.erl`, an eunit module.
- **Files**: `apps/erlangbridge/test/fixtures_common/` (new)
- **Deps**: do early — 0.2 through 0.12 all point at it
- **Verify**: `./rebar3 ct` (all suites still green)

### 0.16 — CI hardening

- **Status**: done (workflow edited; human verify via branch push still pending)
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

- **Status**: done
- **Goal**: add `lsp_fun_utils` and `lsp_signature_doc_layout` to `compile_needed_modules/0`. Makes 0.14 green.
- **Files**: `apps/erlangbridge/src/vscode_lsp_entry.erl`
- **Deps**: 0.14
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/vscode_lsp_entry_SUITE`

### 1.1 — Engine + LSP client bump to latest

- **Status**: done
- **Goal**: bump `engines.vscode` and `@types/vscode` to the latest stable release (check `npm view @types/vscode version` at execution time — was `1.136.0` as of 2026-09-06 — and pin that, not a hardcoded old number), **and** bump `vscode-languageclient` + `vscode-languageserver` from `^9.0.1` to latest (`10.1.1` as of 2026-09-06, check `npm view vscode-languageclient version` at execution time) — then fix every type error from the resulting API churn on both surfaces.
- **Breaking surface to audit**:
  - VS Code API: drop the superseded `vscode-test@^1.3.0` devDep (`@vscode/test-electron` is already present); `DiagnosticTag` / `CodeActionKind` enum additions; deprecated `rootPath`; `tsconfig.json` `lib` is `es7` — may need raising.
  - LSP client (9→10, a major bump): `lib/lsp/lsp-inlinevalues.ts` and `lib/lsp/lsp-rename.ts` import from `vscode-languageserver-protocol` directly, but that package is **not** declared in `package.json` — it only resolves today as a transitive dep of `vscode-languageserver`. Declare it explicitly while bumping (don't rely on the transitive resolution matching); check `vscode-languageclient`'s own migration notes for request/notification type changes between 9.x and 10.x.
- **Files**: `package.json`, `tsconfig.json`, any `lib/**/*.ts` the compiler flags
- **Deps**: Phase 0 gate
- **Verify**: `npm run compile && npm test`

### 1.2 — Manifest hygiene

- **Status**: done (visual "no trust warning" check in the Extensions view still needs a human look)
- **Goal**: add the missing `onCommand:extension.rebareunit` to `activationEvents`; declare `capabilities.untrustedWorkspaces` and `capabilities.virtualWorkspaces` (the extension spawns `erl`, so both are restricted — must be explicit or VS Code warns the user).
- **Files**: `package.json`
- **Verify**: `npm run compile && npm test`; check the Extensions view shows no trust warning

### 1.3 — `workspaceFolders` / `rootUri`

- **Status**: done (minimal plumbing scope: first folder adopted as root, real multi-root scan is a separate follow-up)
- **Goal**: `initialize/2` reads only the deprecated `rootPath`. Read `workspaceFolders`, advertise `workspace.workspaceFolders.supported`, handle `workspace/didChangeWorkspaceFolders`. Prerequisite for multi-root umbrella projects, which are very common in Erlang.
- **Files**: `apps/erlangbridge/src/lsp_handlers.erl:16-19`, `gen_lsp_config_server.erl`, `gen_lsp_doc_server.erl`
- **Deps**: 0.1, 0.12, 0.13
- **Verify**: `./rebar3 ct`; update the golden capability map in `lsp_protocol_SUITE` deliberately

### 1.4 — Incremental document sync

- **Status**: done
- **Goal**: `textDocumentSync => 1` (Full) resends the whole buffer on every keystroke. Move to `2` (Incremental) with range-apply in `gen_lsp_doc_server`. Do this **before** semantic tokens, which is latency-sensitive.
- **Files**: `apps/erlangbridge/src/lsp_handlers.erl` (`textDocument_didChange/2`, capability), `gen_lsp_doc_server.erl`
- **Deps**: 0.12 (strong net required)
- **Verify**: `./rebar3 ct`; manual typing test with `erlang.verbose: true`

### 1.5 — Dead code removal

- **Status**: done (manual "rename still works in Launch Extension" check still needs a human look)
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
| 2.1 | Infrastructure: `textDocument_codeAction/2`, `codeAction/resolve`, `workspace/executeCommand`, `WorkspaceEdit` builders (reuse `lsp_rename.erl`) | done |
| 2.2 | `erl_lint`-driven fixes: unused variable → prefix `_`; unused function → add to `-export`; unused/missing include; undefined function → create stub clause; unbound record field | done |
| 2.3 | Export/spec actions: add/remove from `-export`; generate `-spec` from inferred clause heads (reuse `lsp_navigation:function_clauses/2` + arg-name logic in `lsp_inlayhints.erl`) | done |
| 2.4 | Behaviour support: on `-behaviour(X)`, "Implement missing callbacks" — callbacks via `gen_lsp_help_server`/EEP-48, generate stubs | done (see note) |
| 2.5 | Refactors: extract function from selection; inline variable; convert `if`↔`case`. Ship after 2.2-2.4 | done (minimal scope, see note) |
| 2.6 | Source actions: organize/sort `-export`; add missing `-module` | done |

- **New file**: `apps/erlangbridge/src/lsp_codeaction.erl` (+ add to `compile_needed_modules/0`)
- **New test**: `apps/erlangbridge/test/lsp_codeaction_SUITE.erl`
- **Deps**: Phase 0 gate, 0.2 (diagnostic shape)
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_codeaction_SUITE`

---

# PHASE 3 — semantic tokens

Highlighting is TextMate-only today. Semantic tokens give real macro/record/type/variable-binding coloring from the AST.

| id | goal | status |
|---|---|---|
| 3.1 | Legend + full-document tokenizer over the cached syntax tree. Types: `namespace` (module), `function`, `macro`, `variable`, `parameter`, `type`, `struct` (record), `property` (record field), `string`, `number`, `comment`, `keyword`, `operator`. Modifiers: `definition`, `declaration`, `readonly`, `deprecated` (from `-deprecated`), `defaultLibrary` (OTP modules — already known to `gen_lsp_config_server`) | done (semantic-only scope, see note) |
| 3.2 | Range variant + delta (per-document token cache keyed by result id, alongside existing caches) | done |
| 3.3 | `contributes.semanticTokenScopes` in `package.json` mapping Erlang tokens to TextMate scopes so themes without semantic support degrade correctly | done |
| 3.4 | Setting `erlang.semanticTokensEnabled` (default `true`), plumbed like `codeLensEnabled` | done |

- **New file**: `apps/erlangbridge/src/lsp_semantic_tokens.erl` (+ `compile_needed_modules/0`)
- **Exports**: `textDocument_semanticTokens_full/2`, `_range/2`, `_full_delta/2`
- **Correctness bar**: must not fight the TextMate grammar in `grammar/Erlang.plist` (submodule — not edited here)
- **Deps**: 1.1 (engine), 1.4 (incremental sync)
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_semantic_tokens_SUITE`; visual check in `Launch Extension` against a fixture with macros, records and types

---

# PHASE 4 — navigation & hierarchies

| id | goal | status |
|---|---|---|
| 4.1 | **`workspace/symbol`** — export `workspace_symbol/2`, capability `workspaceSymbolProvider => #{resolveProvider => true}`. Build a symbol index (functions, records, types, macros, behaviours) on the existing `gen_lsp_doc_server` module→file index and project scan. **Single most-missed navigation feature.** | done |
| 4.2 | `textDocument/declaration` — the line is already written and commented out at `lsp_handlers.erl:27`. Cheap win. | done |
| 4.3 | `textDocument/typeDefinition` — from a `-spec`/`-type` usage to the `-type`/`-opaque` definition. `lsp_fun_utils:get_type_range/1` already exists. | done |
| 4.4 | `textDocument/implementation` — from a `-callback` to all modules with `-behaviour(That)`, and from a `-behaviour` attribute to the behaviour module | done |
| 4.5 | Call hierarchy — `textDocument/prepareCallHierarchy`, `callHierarchy/incomingCalls`, `outgoingCalls`. Built on `lsp_navigation:local_function_references/*` + the project references cache | done |
| 4.6 | Type hierarchy — behaviour ↔ implementors, over the 4.4 index | done |
| 4.7 | `textDocument/documentHighlight` — all occurrences of the variable/function/record under the cursor; Read/Write kinds where the AST allows | done |

- **New files**: `lsp_workspace_symbol.erl`, `lsp_hierarchy.erl` (+ `compile_needed_modules/0`)
- **Deps**: Phase 0 gate, 0.11, 0.12
- **Verify**: per-module CT suite each

---

# PHASE 5 — editing polish

| id | goal | status |
|---|---|---|
| 5.1 | Folding range — functions, clauses, `case`/`receive`/`try`, `-export` lists, comment blocks (`region` kind), `%% region` / `%% endregion` markers | done |
| 5.2 | Selection range — expand-selection following AST nesting | done (line-granular, see note) |
| 5.3 | Range formatting — `documentRangeFormattingProvider => true`. `vscode_erlfmt` works on whole forms, so map the requested range to enclosing forms. **Also replace the hardcoded `0,0 → 999999,255` result range** (`lsp_handlers.erl:201-219`) with the real document end; update the 0.6 characterization assertion deliberately | done |
| 5.4 | On-type formatting on `.`, `;`, `,`, newline — AST-driven indent that properly fixes the guard-outdent bug at `lib/extension.ts:167`, replacing the ~17 regex `onEnterRules` | done (server-side fix; `onEnterRules` left in place, see note) |
| 5.5 | `erlang.formatterEnabled` setting; CT case for `editor.formatOnSave` behaviour | done |
| 5.6 | `completionItem/resolve` — advertise `resolveProvider => true`, move doc/detail fetching (currently eager) into resolve, a big latency win. Add snippet support (`insertTextFormat => 2`) for calls with argument placeholders; widen trigger characters beyond `:#.` (`?` for macros, `-` for attributes at line start) | done |
| 5.7 | `textDocument/documentLink` — make `-include`/`-include_lib` paths and comment URLs clickable | done |
| 5.8 | `inlayHint/resolve` + polish — add `tooltip`, `paddingLeft/Right`, `textEdits`; fix the two limits pinned in 0.8 (local-calls-only, no `-spec`-derived names); add type-hint mode for `-spec` returns | done (polish only, see note) |
| 5.9 | Pull diagnostics — `diagnosticProvider` (LSP 3.17) alongside the existing push model, plus `workspace/diagnostic` for project-wide problems without opening files | done |
| 5.10 | `codeLens/resolve` — server-side resolve so lens computation is lazy; today resolve is a client no-op (`lib/lsp/lspcodelens.ts:59`) | done |

- **New file**: `lsp_folding.erl` (+ `compile_needed_modules/0`)
- **Deps**: Phase 0 gate; 5.3 depends on 0.6; 5.8 depends on 0.8

**Notes on scope trims (all deliberate, documented in-code):**
- **5.2**: expand-selection chain is line-granular (statement → clause → function → document), not column-precise sub-expression nesting - there is no generic "end position" on an erl_parse node, and getting one for every nesting level would need a token scan per level.
- **5.4**: the actual reindent is a real, AST-driven fix (`textDocument/onTypeFormatting` reuses `vscode_erlfmt`'s own range-aware formatting - see 5.3), so the guard-outdent bug's visible symptom is corrected automatically. `lib/extension.ts`'s `onEnterRules` were **not** removed - they still fire first, client-side, for the instant-feedback indent VS Code shows before the server round-trip lands; removing them risks a visible flicker between "wrong client guess" and "corrected server edit" that isn't easily verified without interactive testing.
- **5.8**: added `tooltip`/`paddingLeft`/`paddingRight`/`textEdits` and `inlayHint/resolve` (currently identity - nothing here is expensive enough yet to defer). The two deeper fixes (local-calls-only, no `-spec`-derived parameter names) and the new type-hint-from-`-spec`-returns mode were **not** implemented - each is a real change to `lsp_inlayhints.erl`'s own matching logic, out of scope for a polish pass alongside 9 other tasks.
- **5.10**: list no longer knows the reference count when deciding lens count, so an exported+referenced function now gets one combined lens ("exported, N references") instead of the old two separate ones - a necessary consequence of actually deferring the count to resolve, not an oversight.

---

# PHASE 6 — Test Explorer (Erlang-driven)

EUnit/CT results currently surface only as diagnostics. TS is a **thin shell** over custom LSP requests — no test logic in TypeScript.

| id | goal | status |
|---|---|---|
| 6.1 | Custom request `erlang/discoverTests` → Erlang scans for EUnit (`*_test/0`, `*_test_/0`, `-ifdef(TEST)` blocks) and Common Test (`*_SUITE.erl`, `all/0`, `groups/0`), returns a tree with source ranges. Reuse the `gen_lsp_doc_server` project scan and `lsp_syntax` parsing | done |
| 6.2 | TS `TestController` building `TestItem`s from that tree, refreshed on `didSave` / watcher events | done |
| 6.3 | Custom request `erlang/runTests` with progress notifications → Erlang runs eunit/ct and streams per-test results. Reuse `eunit_jsonreport.erl`; add a CT hook equivalent | done (new listener/hook modules, not `eunit_jsonreport.erl` itself, see note) |
| 6.4 | Map results to `TestRun.passed/failed/skipped` with `TestMessage` (expected/actual + location), replacing diagnostics-only reporting at `lib/eunitRunner.ts:20` | done (added alongside, not replacing, see note) |
| 6.5 | `TestRunProfile` of kind `Debug` launching the existing DAP session with the test as entry point | done |
| 6.6 | Coverage — `cover`-based `TestRunProfile` kind `Coverage`. **Needs engine ≥ 1.88**, so gated behind a second engine bump. Stretch | done (gate already cleared by 1.1's bump to 1.136.0) |

- **New files**: `apps/erlangbridge/src/lsp_testing.erl`, `lsp_testing_eunit_report.erl`, `lsp_testing_ct_hook.erl` (all three added to `compile_needed_modules/0`), `lib/lsp/lsp-testcontroller.ts`
- **Deps**: 1.1 (engine ≥ 1.75 for the `vscode.tests` API; 6.6 additionally needed ≥ 1.88 for `TestRunProfileKind.Coverage`, already satisfied by the same bump)
- **Verify**: `./rebar3 ct --suite apps/erlangbridge/test/lsp_testing_SUITE`; then `Launch Extension` and run the fixture project's tests from the Testing sidebar (automated coverage stops at `npm test`'s activation/diagnostics smoke test - actually clicking through the Testing sidebar UI, coverage gutters included, still needs a human look)

**Notes on scope trims (all deliberate, documented in-code):**
- **6.1**: EUnit tests are found by walking the *dodged* syntax tree (`gen_lsp_doc_server:get_dodged_syntax_tree/1`) with `erl_syntax`'s type-agnostic accessors, not `lsp_fun_utils:get_function_range/1` - every form in a dodged tree, even one with no macro use, comes back as a generic `erl_syntax` tree rather than the raw `{function, Anno, Name, Arity, Clauses}` tuple that helper (and the rest of the codebase, on the *normal* tree) matches on. Consequently a discovered EUnit test only gets a point range (its start position), not a real end-of-function range - reverting each clause to compute one is lossy through the macro calls a typical `?assertEqual(...)` test body is full of. CT test cases are 1-arity (`Config`) functions, found the ordinary way (normal tree, real ranges) as every exported 1-arity function that isn't a standard CT callback (`all/0`/`groups/0` are 0-arity and never collected in the first place).
- **6.3**: `lsp_testing_eunit_report.erl` is a sibling of `eunit_jsonreport.erl` (same `eunit_listener` behaviour, same `Data` proplist shape), not that module reused verbatim - `eunit_jsonreport.erl` only ever writes one final file through a hand-rolled, non-real JSON string builder, which doesn't fit streaming per-test progress. The CT hook (`lsp_testing_ct_hook.erl`) is modeled on OTP's own `cth_surefire.erl` (its real callback arities, taken from the installed common_test source, not guessed - `post_end_per_testcase/5`'s own `Result` argument doesn't tell you pass/fail; that comes an instant later via the separate `on_tc_fail/4`/`on_tc_skip/4` callbacks, same as upstream). Two non-obvious infrastructure fixes were needed along the way: `ct_hooks`' own installation step formats-then-reparses its `Opts`, so neither an ETS `tid()` nor a raw `port()` can be passed through it directly (both survive as an atom-named ETS table instead, with the socket stashed inside that table under a reserved key); and `ct:run_test/1` changes the process's cwd while running, so any not-yet-loaded module reachable only through the node's original (often relative) `-pa` entries must be `code:ensure_loaded/1`'d before calling it, not left to lazy-load.
- **6.4**: `TestMessage` carries the failure text and, for a line number, a `Location` - there's no separate expected/actual pair since Erlang's `?assertEqual` etc. already fold that into one formatted term. The **existing** `extension.erleunit` command and its diagnostics collection (`lib/eunitRunner.ts`) were left exactly as they were: Test Explorer is an additive path, not a replacement, since retiring a command someone may already have in muscle memory wasn't asked for.
- **6.5**: `TestRunProfile.Debug` builds a `DebugConfiguration` with `arguments` set to an extra `-eval` fragment (`eunit:test({M,F})` / `ct:run_test([{suite,M},{testcase,F}])`) appended after the existing breakpoints args-file - since that args file already `int:ni/1`'s every project file (test files included) before handing control to `arguments`, the test module is already interpreted and breakpoints in it already work with no further plumbing.
- **6.6**: `erlang/runTests` takes an extra `coverage => true` param; when set, `lsp_testing.erl` recompiles every target module through `cover:compile_module/2` instead of the plain compile+load path (unconditionally, not gated by `code:is_loaded/1` - a module already loaded by an earlier non-coverage run needs to be swapped for its instrumented twin) and, once the run finishes, reads back `cover:analyse(Module, calls, line)` per module before `cover:stop/0` reverts everything back to plain object code. Line **call counts**, not just covered/not-covered booleans, are returned - VS Code's `StatementCoverage.executed` accepts a number, and "how many times" is strictly more informative than a boolean for free. `erlang/runTests`'s response always carries a `coverage` field now (an empty list when coverage wasn't requested), rather than making it optional, so the TS side has one shape to deserialize regardless of which profile ran.

### 6.7 — Coverage must include the code under test (bug)

- **Status**: done. Deviation from the plan below: step 2 only instruments project modules that are *not already loaded* in the bridge node (and don't shadow an OTP module) - see `is_safe_to_instrument/1`. Cover-compiling a loaded module hot-swaps live code, which with vscode_erlang itself as the workspace means the bridge's own `lsp_*` modules.
- **Symptom**: "Run with Coverage" reports only the test modules themselves (`demo_tests.erl`, `demo_SUITE.erl` at 100%), never the modules they exercise (`demo.erl`) - so no gutter coverage on real source. Found while taking the README screenshots (`scripts/screenshots/`).
- **Root cause** (`lsp_testing.erl`): `run_tests/2` only ever cover-compiles the *test targets* (`ensure_module_loaded(M, true)` from `run_eunit/4` / `run_ct/4`) and `collect_coverage/1` only analyses that same list `[M || #{module := M} <- Targets]`. Project modules are loaded as plain beams from `_build/**/ebin` via `add_project_ebin_paths/0`, so `cover` never sees them.
- **Plan**:
  1. **Red test first** - new fixture pair in `lsp_testing_SUITE_data/`: `sample_lib.erl` (two functions, only one called) + `sample_lib_tests.erl` calling it. New case `run_tests_with_coverage_covers_code_under_test`: coverage result contains `sample_lib.erl`'s URI, the called function's body line `>= 1`, the other one `0`. Must fail on today's code.
  2. **Pick the modules to instrument** - new `coverage_modules(Targets)`: test target modules ∪ every project module from `gen_lsp_doc_server:project_modules/0` / `get_module_file/1`, **excluding** anything whose source lives under `_build/`, `_checkouts/` or `deps/` (dependencies are noise in a coverage report and slow to instrument).
  3. **Instrument once, up front** - in `run_tests/2`, right after `cover:start()` and `add_project_ebin_paths/0`, call the existing `ensure_module_loaded(M, true)` for every module from step 2 (source-based compile, same `{d,'TEST'}` + include paths as today, so the instrumented code matches what the tests are compiled against). Make `run_eunit/4` / `run_ct/4` skip modules already instrumented (`cover:is_compiled/1`) instead of compiling them twice.
  4. **Collect over the same list** - `collect_coverage(coverage_modules(Targets))`; `module_coverage/1` needs no change.
  5. **Robustness** - a module that fails to cover-compile (parse transform not on the path, `-on_load` NIF, syntax error) is skipped and logged via `logger` with the module name, never fails the run. Keep the existing `after` ordering (`cover:stop/0` before `remove_project_ebin_paths/1`).
  6. **No TS change expected** - `applyCoverage` in `lib/lsp/lsp-testcontroller.ts` already maps any file URI it receives.
- **Open question**: scope in big umbrella projects - instrumenting every project module can cost seconds per run. Start with "all project modules minus deps"; if too slow, narrow to the OTP apps that contain the requested tests.
- **Verify**:
  - `./rebar3 ct --suite apps/erlangbridge/test/lsp_testing_SUITE` (new case green, `run_tests_with_coverage_reports_per_line_call_counts` still green)
  - `./rebar3 ct` full
  - `npm run screenshots -- test-coverage`, re-shot with `demo.erl` open: gutter shows hit/missed lines
  - then restore the "line coverage shown in the editor gutter" wording in `README.md` and keep the CHANGELOG "Run with Coverage" line as is
- **Files**: `apps/erlangbridge/src/lsp_testing.erl`, `apps/erlangbridge/test/lsp_testing_SUITE.erl` (+ `_data` fixtures), `scripts/screenshots/take-screenshots.mjs` (coverage shot opens `demo.erl` again), `README.md`

# PHASE 7 — ecosystem & polish

| id | goal | status |
|---|---|---|
| 7.1 | Task provider — `contributes.taskDefinitions` + `vscode.tasks.registerTaskProvider` for rebar3 targets (compile, ct, eunit, dialyzer, release, shell), auto-detected from `rebar.config`; complements the ad-hoc commands in `RebarRunner` | done |
| 7.2 | Dialyzer — keep the command, add incremental PLT status in the status bar | done (see note) |
| 7.3 | Status bar — persistent item showing LSP state (starting / ready / failed) + OTP version. Today a startup failure is silent unless `erlang.verbose` | done |
| 7.4 | Walkthrough — `contributes.walkthroughs` for first-run setup (find `erl`, pick rebar3, run a build). Good marketplace signal | done |
| 7.5 | `configurationDefaults` — sensible `[erlang]` editor defaults (tab size, format-on-save opt-in, word pattern) | done (word pattern in `erlang.configuration.json`, see note) |
| 7.6 | Debug adapter — `attach` request (attach to a running node); only `launch` is declared today. Plus `configurationSnippets` and `variables` for `erlpath` | done (same-host only, see note) |
| 7.7 | Docs — README settings list is stale (missing `debuggerRunMode`, `formattingLineLength`, `verboseExcludeFilter`); add a feature matrix; update `CHANGELOG.md` per phase | done |

- **New files**: `lib/erlangTaskProvider.ts`, `lib/dialyzerStatus.ts`, `lib/erlangInstallation.ts` (check-installation command + `erlang.getErlPath` behind `${command:erlpath}`), `lib/lsp/lsp-status.ts`, `walkthrough/install-erlang.md`, `apps/erlangbridge/test/vscode_connection_SUITE.erl` (+ `_data`), `test/test-suite/erlangTaskProvider.test.ts`. No new bridge module (the attach code lives in `vscode_connection.erl`, which is not an LSP module).
- **Verify**: `./rebar3 ct` (210 green, incl. `vscode_connection_SUITE` and `lsp_protocol_SUITE`'s golden `serverInfo`); `npm test` (task detection); manual: `Launch Extension`, then status bar items, Run Task > rebar3, the walkthrough, an attach configuration against `erl -sname myapp`

**Notes on scope trims (all deliberate, documented in-code):**
- **7.1**: tasks run through `escript <rebar3>` like `RebarShell` (rebar3 found via `RebarShell.getRebarFullPath`, now public). New `$rebar3` matcher (handles the `file:line:col:` format the old `$erlang` one misparses) and multi-line `$rebar3-dialyzer`; `$erlang` is kept unchanged for existing `tasks.json` files. `release` only offered when `rebar.config` mentions `{relx,`. Activation on `workspaceContains:rebar.config` added so tasks and status items exist before an `.erl` file is opened.
- **7.2**: rebar3 already updates the PLT incrementally; "PLT status" is read from disk: newest `_build/*/*_plt`, and *stale* when `rebar.lock`/`rebar.config` is newer (the PLT covers OTP + deps, not project modules). Global PLT under `~/.cache/rebar3` and custom `plt_location` are not looked at. Running state comes from the dialyzer command and from `rebar3: dialyzer` tasks.
- **7.3**: OTP version comes from the server: `initialize` now returns `serverInfo => #{name => <<"vscode_erlang">>, version => <full OTP version>}` (golden result in `lsp_protocol_SUITE` updated deliberately). Startup failures (bridge compile failing with no previous build, `erl` exiting before the socket is reachable, socket never reachable) now reject the server-options promise instead of resolving with an undefined socket.
- **7.5**: `wordPattern` is a language-configuration key, not a setting, so it went into `erlang.configuration.json`; `?MACRO`/`#record` prefixes are deliberately *not* part of a word (it would break completion filtering on `?`/`#` triggers), and a leading `-` is not part of a number (`X-1`).
- **7.6**: the adapter starts a hidden helper node (`vscode_connection:attach/0`) that pushes `vscode_jsone`/`gen_connection`/`vscode_connection` into the target and calls `start_attached/2` there; disconnect sends `debugger_detach` (no breakpoints, releases processes at a break, `int:nn` everything, stops the connection processes) instead of `debugger_exit`, unless the client asks `terminateDebuggee`. Same-host only (target reads the sources by local path and posts to 127.0.0.1); target needs the `debugger` and `inets` applications (checked, clear error otherwise); node name and cookie validated before reaching the shell command line. Not exercised through a real VS Code debug session in automation - CT covers attach/detach/breakpoint release against a peer node, the helper's stdin-EOF shutdown was checked by hand.

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
| `declarationProvider` | ✅ | 4.2 |
| `typeDefinitionProvider` | ✅ | 4.3 |
| `implementationProvider` | ✅ | 4.4 |
| `documentHighlightProvider` | ✅ | 4.7 |
| `codeActionProvider` | ✅ (2.1-2.6 fixes/refactors/source actions shipped) | 2.1 |
| `documentLinkProvider` | ✅ | 5.7 |
| `colorProvider` | ❌ | n/a for Erlang |
| `documentRangeFormattingProvider` | ✅ | 5.3 |
| `documentOnTypeFormattingProvider` | ✅ | 5.4 |
| `foldingRangeProvider` | ✅ | 5.1 |
| `executeCommandProvider` | ✅ (empty commands list, infra only) | 2.1 |
| `selectionRangeProvider` | ✅ | 5.2 |
| `linkedEditingRangeProvider` | ❌ | not planned |
| `callHierarchyProvider` | ✅ | 4.5 |
| `semanticTokensProvider` | ✅ (full + delta + range, semantic types - see 3.1/3.2) | 3.1 |
| `monikerProvider` | ❌ | not planned |
| `typeHierarchyProvider` | ✅ | 4.6 |
| `diagnosticProvider` | ✅ (push + pull) | 5.9 |
| `workspaceSymbolProvider` | ✅ | 4.1 |
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
