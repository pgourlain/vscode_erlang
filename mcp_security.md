# MCP topology inspector: security note

Scope: the opt-in, read-only MCP server embedded in the Erlang node of a **debug session**
(`erlang.mcp.enabled`, see [HELP.MD](./HELP.MD#mcp-topology-inspector-ai-agents-debug-sessions-only)).

## Who is being protected from what

The developer who turns this on owns the machine, the code and the node: the controls are **not**
there to stop the developer from looking at their own application (the debugger already shows
variables and can evaluate expressions). They exist because the *client* of this server is an AI
agent, and because a local HTTP port is reachable by more than the developer:

1. **Data leaves the machine.** Whatever a tool returns goes into the agent's context, i.e. to a
   model provider. This is the main reason the default is metadata only.
2. **A prompt-injected or malicious agent.** Source files, logs, issues or web pages the agent reads
   can steer it. The server must stay harmless in the hands of such an agent: read-only, no
   evaluation, no mutation, bounded cost.
3. **Browser pages** attacking `localhost` (DNS rebinding, cross-origin requests).
4. **Other processes of the same user**, compromised developer tools, an accidentally forwarded port.

Binding to loopback is *not* an authorization boundary. All requests, parameters and identifiers are
untrusted.

## Two tiers

| Tier | What it returns | How it is enabled |
| --- | --- | --- |
| **Metadata** (default) | Topology and counters: applications, supervision trees, process names/status/queue lengths, ports (driver name, counters), ETS sizes, breakpoints. No application data | `erlang.mcp.enabled` |
| **Developer** (opt-in) | A *bounded sample of application data*: `process_state`, `mailbox_sample`, `ets_sample`; plus `allowed_ets_tables: all` and raised limits | the project's `rebar.config` names the tools in `allowed_tools` (never in the default set) |

The developer tier is meant for local debugging of your own code: pick it deliberately, knowing that
the sampled data goes to the agent. Redaction is best effort (below), not a guarantee.

## Controls (both tiers)

| Control | How |
| --- | --- |
| Secure default | `erlang.mcp.enabled=false`; started only by the debug adapter for a real debug session (never LSP, `noDebug`, rebar, eunit); untrusted workspace disables it |
| Loopback only | `127.0.0.0/8` / `::1` literals, or names resolving *only* to loopback; validated in the helper VM and again in the target; wildcard/LAN/public rejected |
| Per-session credential | 32 random bytes (`crypto:strong_rand_bytes/1`) per session; bearer token required on **every** request, checked (constant time) before the body is read; never accepted from the environment or workspace files; an explicit user-level `erlang.mcp.authToken` is the only opt-in override |
| Host / Origin | `Host` must be the bound endpoint; an `Origin` is refused unless it is that endpoint; no CORS headers |
| Transport limits | POST `/mcp` only, `application/json` only, body cap before reading, header/body/connection timeouts, bounded connections, no chunked bodies, no batches, no SSE |
| Read-only tools | Fixed tools; the project policy chooses among them; unknown tools are never turned into atoms or function names. Default set: 12 metadata tools. Developer tier: 3 more, only when named |
| No execution | No eval, `rpc:call`, shell, code loading, message sending, tracing, breakpoint changes or ETS writes. The only runtime calls that touch application processes are `sys:get_state/2` (developer tier, gen_server/gen_statem/gen_event only, never plain processes, never supervisors) and `process_info/2` |
| Data minimisation (metadata tier) | No process dictionary, stack, mailbox contents, state, child start arguments, application environment, source, code paths, cookies, socket addresses, command lines; node host redacted by default; ETS: only tables approved by `allowed_ets_tables` (private tables never) |
| Developer tier data | Bounded by depth, item count and binary size; values of keys named like a secret (`password`, `token`, `secret`, `api_key`, `cookie`, `auth`...) and credentials in URLs are replaced; supervisor state (child start arguments) is never returned; a mailbox above 20000 messages is not copied; ETS sampling only for approved, non-private tables |
| Safe identifiers | Registered names and table names are resolved with `binary_to_existing_atom/2` only (and a table name is checked against the approval list *before* any atom lookup); pids must be local (`<0.N.M>`); no `binary_to_term/1` on input; validated with `\z` anchors |
| Bounded work | Per-request timeout, monitored worker processes killed on timeout (late replies dropped), bounded concurrency (excess is rejected with an MCP error), bounded collections/cursors/identity bookkeeping, bounded result size (structured result *and* its text copy). Defaults are conservative; a project may raise a limit up to a fixed ceiling (see HELP.MD), never beyond |
| Session isolation | Tokens, ids, cursors (signed with a per-session secret and bound to tool + arguments) and workers belong to one session/node; a new session invalidates everything |
| Clean shutdown | Dedicated supervisor `mcp_sup` (fail-closed: any child exit stops everything); stopped by disconnect/detach; an 8 s renewable lease renewed by the adapter through the target's own command server (independent of application processes stopped at breakpoints) stops the inspector within ~9-10 s if the adapter disappears; an attached node is never stopped by the inspector |
| Secret hand-over | The token is returned by the target only to the adapter, written by the adapter to a `0600` file in a `0700` temp directory, read and deleted at once by the extension host, and then kept in memory only. It is never in a DAP event/output, log, crash report, setting, command line or telemetry. Explicit *Copy Authorization header* is the only way it leaves VS Code |
| Logs | The *Erlang MCP* output channel journals request metadata only (method, known tool name, status, durations, sizes, counts); only lifecycle events, tool name, status and duration go to the target's logger; the server and store processes redact the token and the cursor secret in `sys:get_status` and crash reports; verbose adapter logging redacts secret-like keys and the `-setcookie` value |

## What you can relax, and what you should not

Reasonable on your own machine, per project: the developer tier, `allowed_ets_tables: all`, higher
limits (for example a longer `collection_ttl_ms` so `changes_since` baselines survive), and a fixed
`erlang.mcp.authToken` for a static client such as Claude Code.

Keep: loopback binding, the bearer token, Host/Origin checks, read-only access, debug-session-only
activation. They cost nothing in daily use and are what protects you from items 2-4 above.

## Residual risks (not removed by the controls)

- Metadata can be sensitive: application, module, table and child-id names, process counts and
  memory sizes. Enable the tools/tables you are comfortable sharing with the agent (rebar policy).
- Developer tier: the sampled state, messages and rows can contain secrets that redaction does not
  recognise (a record without keys, a token in an unlabelled string, personal data). Treat them as
  disclosed to the model provider. A process stopped at a breakpoint cannot be sampled (and is
  reported as such); `sys:get_state` is answered by the process itself, so a very large state costs
  the target time and memory (bounded by the request timeout, not preemptible).
- Node-level authorization: selecting an application is a navigation filter, not a boundary between
  applications in the same VM.
- The token is bearer-only over plain HTTP on loopback: another process of the same user that can
  read the debugger's memory, the extension host, or the descriptor in the few milliseconds it
  exists, can use it. The descriptor's `0600` mode is not enforced on Windows (the per-user temp
  directory ACL applies).
- The existing debugger command channel of the target (`vscode_connection`) is an unauthenticated
  loopback HTTP endpoint (it can already evaluate expressions for the debugger). The MCP start
  command travels on it, so a local process could start or stop the inspector (a local process
  could already debug the node). The token is returned in the reply to the caller of that command.
- Connection slots are claimed before authentication (a bounded number, short timeouts): a local
  process can keep them busy and delay the real client (denial of service, no data exposure). With
  a fixed `erlang.mcp.port` on Windows the listener uses `SO_REUSEADDR`, which on that platform can
  let another local process share the port; prefer the default ephemeral port there.
- Optional `erlang.mcp.authToken` (user setting only): a fixed token replaces the per-session random
  one (validated 16-128 URL-safe characters). It lives in the user's settings file and, during a
  session, in the adapter and target memory; it survives across sessions, so it is exposed for as long
  as it is configured. Default is empty (random token).
- Distribution: attach uses the node's Erlang cookie as before.
- Not atomic and not free: collections are observation intervals; `application:which_applications/1`,
  `registered/0`, `erlang:processes/0`, `ets:all/0` and `supervisor:which_children/1` materialise lists
  in the target and cannot be interrupted (killing the worker only discards the late reply);
  `proc_lib:initial_call/1` is used internally, only as a safety gate (to avoid calling supervisor
  APIs on non-supervisors), and copies the target's process dictionary into the inspector's worker
  without returning it.
- Same-machine only: no port forwarding, remote-SSH/container arrangements are unsupported.
- The MCP modules are also built into the `vscode_lsp` application's `ebin` (rebar compiles
  `src/mcp`), but the LSP node never loads or starts them.
