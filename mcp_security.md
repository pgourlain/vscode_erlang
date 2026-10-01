# MCP topology inspector: security note

Scope: the opt-in, read-only MCP server embedded in the Erlang node of a **debug session**
(`erlang.mcp.enabled`, see [HELP.MD](./HELP.MD#mcp-topology-inspector-ai-agents-debug-sessions-only)).

## Threat model

Binding to loopback is *not* an authorization boundary. Assumed attackers: other processes of the
same user, compromised developer tools, browser pages attacking local services (DNS rebinding,
cross-origin requests), a malicious or prompt-injected MCP client, an accidentally forwarded
port. All requests, parameters and identifiers are untrusted.

## Controls

| Control | How |
| --- | --- |
| Secure default | `erlang.mcp.enabled=false`; started only by the debug adapter for a real debug session (never LSP, `noDebug`, rebar, eunit); untrusted workspace disables it |
| Loopback only | `127.0.0.0/8` / `::1` literals, or names resolving *only* to loopback; validated in the helper VM and again in the target; wildcard/LAN/public rejected |
| Per-session credential | 32 random bytes (`crypto:strong_rand_bytes/1`) per session; bearer token required on **every** request, checked (constant time) before the body is read; never accepted from the environment or workspace files; an explicit user-level `erlang.mcp.authToken` is the only opt-in override |
| Host / Origin | `Host` must be the bound endpoint; an `Origin` is refused unless it is that endpoint; no CORS headers |
| Transport limits | POST `/mcp` only, `application/json` only, body cap before reading, header/body/connection timeouts, bounded connections, no chunked bodies, no batches, no SSE |
| Least privilege | Ten fixed read-only tools; the project policy can only narrow them; unknown tools are never turned into atoms or function names |
| No execution | No eval, `rpc:call`, shell, code loading, message sending, `sys` calls, tracing, breakpoint changes, ETS writes or reads |
| Data minimisation | No process dictionary, stack, mailbox, state, child start arguments, application environment, source, code paths, cookies; ETS: metadata of approved, non-private named tables only; node host redacted by default; child identifiers are printed with a depth/size bound |
| Safe identifiers | Registered names are resolved with `binary_to_existing_atom/2` only; pids must be local (`<0.N.M>`); no `binary_to_term/1` on input |
| Bounded work | Per-request timeout, monitored worker processes killed on timeout (late replies dropped), bounded concurrency (excess is rejected with an MCP error), bounded collections/cursors/identity bookkeeping, bounded result size (structured result *and* its text copy) |
| Session isolation | Tokens, ids, cursors (signed with a per-session secret and bound to tool + arguments) and workers belong to one session/node; a new session invalidates everything |
| Clean shutdown | Dedicated supervisor `mcp_sup` (fail-closed: any child exit stops everything); stopped by disconnect/detach; an 8 s renewable lease renewed by the adapter through the target's own command server (independent of application processes stopped at breakpoints) stops the inspector within ~9-10 s if the adapter disappears; an attached node is never stopped by the inspector |
| Secret hand-over | The token is returned by the target only to the adapter, written by the adapter to a `0600` file in a `0700` temp directory, read and deleted at once by the extension host, and then kept in memory only. It is never in a DAP event/output, log, setting, command line or telemetry. Explicit *Copy Authorization header* is the only way it leaves VS Code |
| Logs | The *Erlang MCP* output channel journals request metadata only (method, known tool name, status, durations, sizes, counts); only lifecycle events, tool name, status and duration go to the target's logger; verbose adapter logging redacts secret-like keys and the `-setcookie` value |

## Residual risks (not removed by the controls)

- Metadata can be sensitive: application, module, table and child-id names, process counts and
  memory sizes. Enable the tools/tables you are comfortable sharing with the agent (rebar policy).
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
- Optional `erlang.mcp.authToken` (user setting only): a fixed token replaces the per-session random
  one (validated 16-128 URL-safe characters). It lives in the user's settings file and, during a
  session, in the adapter and target memory; it survives across sessions, so it is exposed for as long
  as it is configured. Default is empty (random token).
- Distribution: attach uses the node's Erlang cookie as before.
- Not atomic and not free: collections are observation intervals; `application:which_applications/1`,
  `registered/0` and `supervisor:which_children/1` materialise lists in the target and cannot be
  interrupted (killing the worker only discards the late reply); `proc_lib:initial_call/1` is used
  internally, only as a safety gate (to avoid calling supervisor APIs on non-supervisors), and
  copies the target's process dictionary into the inspector's worker without returning it.
- Same-machine only: no port forwarding, remote-SSH/container arrangements are unsupported.
- The MCP modules are also built into the `vscode_lsp` application's `ebin` (rebar compiles
  `src/mcp`), but the LSP node never loads or starts them.
