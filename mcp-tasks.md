# V1: Embedded MCP OTP Topology Inspector for Erlang Debug Sessions

## Objective

Add a feature to the extension's DAP debugger: when the Erlang extension/LSP setting `erlang.mcp.enabled` is explicitly enabled and an actual debug session starts, the target Erlang node starts an embedded Model Context Protocol (MCP) server. Configuration belongs to the existing Erlang settings layer; the DAP debugger owns the runtime lifecycle. Enabling the setting alone must not start a server in the LSP node.

The V1 goal is to let an agent discover, reconstruct, and explain the OTP topology of a selected application without knowing supervisor or process names in advance. Map applications, supervisors, processes, modules, and approved ETS metadata, with explicit relationships, evidence, and coverage limitations.

This is a structural map, not an observation of message exchanges or business workflows. Mailbox inspection is limited to queue length; ETS inspection is limited to metadata. No agent framework or model-specific runtime is required.

This is not a general administration API and must never be active in a normal LSP session or in a non-debug application launch.

## Expected Behavior

- The DAP debugger starts or attaches to an Erlang node through the existing debug flow.
- The existing Erlang configuration layer resolves MCP settings for the debug session's workspace folder and passes a validated internal configuration to the debugger. Do not add a user-facing `mcp` block to launch/attach configuration.
- The MCP runtime inspector starts only when both conditions are true:
	- The session is an actual debug session.
	- The folder-resolved `erlang.mcp.enabled` setting is explicitly true.
- The MCP server runs inside the application/node being debugged, under a dedicated OTP supervisor.
- When the debug session ends, disconnects, or fails, the MCP server is stopped and its resources are released.
- If MCP configuration, startup, or client registration fails, disable/clean up the inspector, report a sanitized actionable error, and keep ordinary debugging usable. There is no `required` option in V1.
- Normal LSP, DAP, compilation, indexing, and debug behavior must remain unchanged when the feature is disabled.

## Proposed Configuration

### Erlang Extension/LSP Settings

Use the existing VS Code `erlang` settings namespace, not a new configuration service. Resolve settings with the debug workspace folder URI, not a global cache or the first workspace folder in a multi-root workspace.

```json
{
	"erlang.mcp.enabled": true,
	"erlang.mcp.host": "127.0.0.1",
	"erlang.mcp.port": 0
}
```

`erlang.mcp.enabled` defaults to `false`. The example explicitly opts in. Changes apply to new sessions; changing the setting must not silently expand permissions or rebind an active inspector. Stopping the debug session revokes its credentials.

### Project Policy in rebar.config

Add an optional top-level extension-owned term to the target project's [rebar.config](rebar.config). This is metadata read by the extension's MCP integration, not a rebar plugin or an instruction for rebar to start a server. Builds/tests run outside the debugger must never activate MCP.

```erlang
{mcp, [
    {allowed_tools, [
        <<"runtime_summary">>,
        <<"application_overview">>,
        <<"supervision_tree">>,
        <<"process_info">>,
        <<"registered_processes">>,
        <<"ets_tables">>,
        <<"debug_session">>
    ]},
    {allowed_ets_tables, []},
    {limits, [
        {max_request_bytes, 16384},
        {max_result_bytes, 262144},
        {max_items, 500},
        {max_depth, 16},
        {max_binary_bytes, 4096},
        {max_traversal_depth, 8},
        {max_concurrency, 2},
        {request_timeout_ms, 3000},
        {collection_ttl_ms, 30000},
        {max_collection_bytes, 1048576},
        {max_collections, 2}
    ]}
]}.
```

The policy example shows the defaults. For an approved named table, use a binary name such as `<<"orders_index">>` in `allowed_ets_tables`. Metadata access remains opt-in and excludes private tables.

- Resolve the nearest target-project rebar configuration from the resolved debug `cwd`, within the selected trusted workspace folder. For an umbrella, use its enclosing project file when the application has none. Do not walk outside that folder, consult the extension's own build configuration instead, or fetch a remote target's files implicitly.
- If no project file or no `mcp` term exists, use the built-in policy. A malformed file or invalid/duplicate MCP keys disables MCP for that session with an actionable error; it does not abort ordinary debugging or fall back to a more permissive policy.
- V1 reads only the literal top-level term: no `rebar.config.script` execution, profile merging, includes, or evaluation. Detect an `mcp` policy placed only in a profile and report it as unsupported instead of silently ignoring it.
- Use a structured Erlang-term reader in a bounded helper outside the long-lived LSP/target VM, or a reviewed parser that does not intern arbitrary input as atoms. Existing `file:consult` usage is a starting point, not permission to parse client input or unbounded project files in the target. Apply Workspace Trust, input-size, parse-time, and output limits; return only normalized MCP policy, never other project terms.
- Normalize snake_case project keys into the internal camelCase contract once. Configuration ownership is disjoint: VS Code controls enablement/host/port; rebar controls tool/table allowlists and lower resource limits. Reject `enabled`, `required`, `host`, `port`, transport, and credentials inside the project term. Neither source can relax built-in security ceilings.
- Keep defaults and schemas in MCP-owned helpers. Projects without rebar remain usable with built-in defaults; do not create a project file automatically.

### Validation and Defaults

- `erlang.mcp.enabled` is the only user-facing activation switch. A user-authored launch/attach `mcp` block is unsupported and cannot override it.
- `required` is removed. All MCP failures are non-fatal to debugging, but must be visible and must leave no unauthenticated or partially initialized inspector running.
- V1 uses only `streamable_http`; do not expose an unused transport selector.
- `host` defaults to `127.0.0.1` and must only accept loopback addresses in this feature version.
- `port: 0` requests an ephemeral OS-assigned port and should be the default.
- Reject wildcard, LAN, container-wide, and public bindings such as `0.0.0.0` and `::`.
- Generate a new bearer token from at least 32 cryptographically random bytes per debug session. Do not accept an `authToken` configuration field or reuse an environment-provided token in V1.
- Internal `allowedTools` defaults to the seven tools in the rebar example; an explicit empty list enables none.
- Internal `allowedETSTables` defaults to an empty list. Accept exact local named-table names, resolved without creating atoms. Private tables remain excluded even if listed.
- The example's limits are V1 defaults and hard ceilings. Overrides must be positive integers no greater than the corresponding ceiling. Validate relationships between limits so error and result envelopes fit; reject unusable combinations rather than exceeding a limit. `maxItems` counts entities and relationships together per page, and ordinary list entries. `maxResultBytes` includes the serialized MCP response envelope.
- Validate all fields, ranges, tool names, and limits before starting the server.
- Unknown MCP configuration keys should produce a useful warning or validation error according to existing project conventions.
- Extend the existing Erlang settings schema and documentation; pass normalized configuration internally to the adapter without exposing duplicate user-facing debug options.

## Scope for Version 1

### Read-Only Tools

Implement exactly the seven tools below, with discoverable input/output schemas and descriptions. Do not expose prompts or resources in V1. *(Superseded after V1: five more tools, one prompt and one edge type were added deliberately; see "Post-V1 additions" under Implementation Progress. The text below keeps describing the original V1 contract.)*

#### `application_overview`

- Discover started OTP applications without a required application, supervisor, or PID argument.
- Return names, versions, declared dependencies, and discoverable root supervisors with reusable entity IDs. Declared dependencies are not evidence of runtime communication.
- Support application selection by discovered ID and pagination. Preserve applications with no discoverable root and explain unavailable metadata.
- Expose safe module names and declared OTP behaviours where available, without invoking application callbacks, loading code, returning compile paths, or reading application environment values.

#### `runtime_summary`

Return bounded runtime metadata:

- Erlang/OTP and ERTS versions.
- Node name, with an option to redact it if it contains host information.
- Uptime.
- Scheduler count.
- Process count.
- Memory summary.
- Current debug session identifier, using a non-secret opaque value.

#### `registered_processes`

Return a bounded list of locally registered process names and safe summary metadata.

Do not enumerate every process by default. Support application filtering and continuation; mark membership as confirmed, inferred, or unknown. Do not silently assign unrelated registered processes to the selected application.

#### `process_info`

Accept a process entity ID, registered name, or validated PID belonging to the authorized local debug node. Return only an explicit allowlist of fields:

- Registered name.
- Status.
- Current function.
- Initial call.
- Reductions.
- Memory.
- Message queue length.
- Links and monitors as bounded identifiers.
- Application membership with evidence and confidence.
- Callback module and OTP behaviour when available from safe metadata, otherwise an explicit unknown value. Do not read process dictionaries, call `sys:get_state`, or execute application callbacks to infer them.

Do not return process dictionaries, heap fragments, stack contents, full mailbox messages, or arbitrary process state in version 1.

#### `supervision_tree`

Traverse from an application or supervisor entity ID discovered by `application_overview`, or an explicitly supplied local supervisor. Use OTP supervisor APIs where possible. Bound depth, items, bytes, and execution time; detect loops and stale PIDs.

Return typed parent-child edges, safe child identifiers, worker/supervisor kind, module metadata, and available restart/shutdown metadata. Never return child start arguments. Preserve restarting or absent child entries without inventing live PIDs.

Support progressive subtree expansion and continuation. An unresponsive supervisor produces a partial result, not a blocked map. Use `timeout` unless debugger evidence specifically establishes `unavailable_while_paused`; never resume or alter application processes to inspect them.

#### `ets_tables`

Return metadata only for named tables explicitly approved in `allowedETSTables`: name, protection, type, owner entity ID, size, and memory with its unit. Check approved names directly instead of dumping a node-wide table inventory. Exclude private tables even if listed.

Return ownership edges joinable to process entities. Never read keys, objects, or values; `ets_lookup` is deferred beyond V1.

#### `debug_session`

Return safe debugger metadata such as launch versus attach, node connection status, interpreted modules, and active breakpoint locations if already available from the existing debug subsystem.

Do not expose source contents, environment values, command-line secrets, cookies, or credentials.

### Mapping Contract

Mapping tools share a versioned structured contract. Prose may supplement it but cannot replace it.

| ID | Requirement |
| --- | --- |
| MAP-01 | Discover applications and available roots without preconfigured process names. Distinguish the selected application, declared dependencies, and other node entities. |
| MAP-02 | Entity kinds are `application`, `process`, `module`, and `ets_table`. A supervisor is a process with role `supervisor`, not a duplicate entity. Opaque IDs are reusable across tools and collections in the live session, invalid across sessions/node incarnations, and different after process restart or table recreation. Bound identity bookkeeping and explicitly expire handles when identity cannot be retained safely. |
| MAP-03 | Typed edges are `depends_on`, `supervises`, `owns_table`, `linked_to`, `monitors`, `belongs_to`, and `uses_module`. Include evidence, observation time, and confidence (`confirmed`, `inferred`, `unknown`). Links/monitors do not imply supervision or message traffic. `uses_module` reflects callback/child metadata, not observed calls. |
| MAP-04 | Coverage is relative to the requested scope, not all runtime processes. Report limitations for unsupervised, unregistered, transient, and unattributed processes. A name or link alone cannot confirm application membership. |
| MAP-05 | Include `schemaVersion`, `sessionId`, `collectionId`, `startedAt`, `finishedAt`, `scope`, `entities`, `relationships`, `complete`, `truncated`, `omissions`, and `nextCursor`. Each call/subtree expansion identifies its collection. A collection is a bounded observation interval, never an atomic snapshot. |
| MAP-06 | Opaque cursors are bound to session, collection, tool, and arguments. Serve pages from bounded retained observations, not moving live offsets. Return `cursor_expired` after retention expires; never silently restart traversal. Explain non-resumable omissions. |
| MAP-07 | Expose safe module/behaviour names and available debugger locations for correlation through separate workspace/LSP tools. Explicitly report unavailable metadata. Do not load modules or return source contents through MCP. |

`omissions` distinguishes `policy_denied`, `disappeared`, `timeout`, `unavailable_while_paused`, `limit_reached`, and `unknown_membership` without disclosing forbidden identifiers. Edge endpoints outside the current page need safe references or an unresolved status, never invented entities.

Apply collection TTL, count, and retained-byte budgets per session. When acquisition exceeds a budget, return a partial map with a non-resumable reason and suggest a narrower scope. Pagination bounds output, not necessarily the cost of OTP APIs that materialize lists; review that cost explicitly.

Agents may compare collections using entity IDs, but absence from a partial collection is not proof of termination. V1 needs no historical database or server-side diff tool.

### Agent Exploration Workflow

1. Discover tool schemas; call `runtime_summary` and `application_overview`.
2. Select an application, follow its root IDs, and expand `supervision_tree` progressively.
3. Enrich selected processes with `process_info`, registered names, approved ETS ownership, and debugger metadata.
4. Join IDs and typed edges; separate confirmed relationships from inferences and unresolved references.
5. Correlate modules through separately authorized workspace/LSP access when available. Topology reconstruction must work without source access.
6. Explain the topology, observation interval, incomplete regions, and uncertainty. Do not infer business flows or actual message traffic from structural edges.

### Explicitly Out of Scope for Version 1

Do not implement any of the following:

- Arbitrary Erlang expression evaluation.
- `rpc:call` with client-controlled module, function, or arguments.
- Shell command execution.
- Dynamically loading or purging code.
- Sending arbitrary messages to processes.
- Changing `sys` state.
- Killing, suspending, or restarting processes.
- Setting or removing breakpoints through MCP.
- Writing to ETS.
- Reading ETS keys or values, including exact-key lookups.
- Dumping full ETS tables.
- Tracing messages, function calls, or business flows.
- Traversing distributed topology on other nodes.
- Returning full process mailboxes.
- Returning complete `sys:get_state/1` values.
- Accessing arbitrary files, source files, environment variables, application secrets, Erlang cookies, or OS credentials.
- Proxying arbitrary LSP or DAP requests.

If existing helper APIs make these actions easy, they must still not be exposed.

## Architecture

Implement the feature using OTP conventions and the repository's existing application structure.

The MCP implementation is entirely Erlang: HTTP transport, protocol handling, authentication, tool schemas and dispatch, project-policy parsing/validation, runtime inspection, encoding, limits, and supervision. Do not add a production TypeScript MCP SDK, server, policy engine, or duplicate implementation. TypeScript changes are limited to existing VS Code settings, debugger bootstrap/event forwarding, and client-registration/UI integration; they do not process MCP tool requests. An external MCP client used only for interoperability tests is not part of the server implementation.

### File Isolation and Naming

- Put all new Erlang MCP production files under `apps/erlangbridge/src/mcp/`, with filenames and module names starting with `mcp_`: `mcp_sup.erl`, `mcp_server.erl`, `mcp_policy.erl`, `mcp_runtime.erl`, `mcp_encoder.erl`, and `mcp_audit.erl` as needed. Subdirectories do not namespace Erlang modules; verify name collisions, including dependencies.
- Do not create a dedicated TypeScript MCP directory or helper subsystem. Keep thin VS Code integration hooks in the existing settings, extension, and debugger files; place MCP implementation helpers in Erlang under `apps/erlangbridge/src/mcp/`.
- New MCP tests, fixtures, schemas, and auxiliary files must also use the `mcp_` prefix and live in dedicated MCP subfolders under the existing appropriate test/source directories. A mandatory framework basename is an exception only when documented. Keep test code out of production `src`.
- Existing integration files retain their names and contain only necessary hooks. Do not rename shared files such as `package.json`, `rebar.config`, or existing settings/debugger modules. Keep documentation in existing docs; new MCP-only documentation uses the same prefix.
- Verify rebar's recursive source discovery for the supported toolchain, target-side compilation from `src/mcp`, beam/dependency packaging, and launch/attach loading explicitly. Do not assume relocating a file makes the debugger compile/load it. Never load MCP automatically through the LSP startup module list.
- Prefix rules apply to project-owned additions, not upstream dependency files; do not rename or vendor dependencies merely to satisfy naming.

Suggested components (combine helpers where appropriate):

- `mcp_sup`: dedicated supervisor, started only for an enabled debug session.
- `mcp_server`: transport lifecycle and MCP request dispatch.
- `mcp_policy`: tool allowlist, authorization, validation, redaction, and limits.
- `mcp_runtime`: safe wrappers around Erlang runtime and OTP introspection.
- `mcp_encoder`: bounded Erlang-term to MCP-content conversion.
- `mcp_audit`: structured security and lifecycle events without sensitive payloads.

Use names matching the existing codebase.

The implementation must:

- Keep the MCP supervision subtree isolated from the LSP and debugger core.
- Avoid blocking DAP or LSP handlers.
- Enforce request timeouts.
- Run inspection in monitored, bounded workers. Stop inspector-owned workers after timeout and discard late replies; ignoring results alone is insufficient. Never kill application processes. Document non-interruptible runtime operations and their residual resource risk.
- Enforce the shared mapping contract and bounded collection/identity retention.
- Bound concurrency and reject excess work with a clear MCP error.
- Handle dead PIDs, disappearing ETS tables, supervisor restarts, node disconnects, and malformed terms.
- Avoid atom creation from any client-controlled string.
- Avoid unsafe term decoding, especially `binary_to_term/1` on untrusted input.
- Avoid logging authorization tokens or returned runtime contents.
- Avoid converting arbitrary binaries to JSON strings without validation and limits.
- Expose MCP protocol errors without leaking Erlang stack traces or internal paths.

## Transport and Endpoint Discovery

Use a transport supported by the MCP library already present in the repository. If none exists, select a maintained Erlang-compatible implementation only after reviewing its license, dependency footprint, protocol support, and security posture. Avoid adding a custom partial MCP implementation unless repository constraints require it.

For a local HTTP-based transport:

- Bind only to an explicit loopback address.
- Default to an ephemeral port.
- Require the freshly generated per-session bearer token on every request.
- Compare tokens in a timing-resistant manner where practical.
- Validate `Origin` and `Host` for HTTP requests.
- Do not enable CORS.
- Do not provide a browser-oriented administrative UI.
- Cap request body size.
- Reject unsupported content types and methods.
- Apply connection, request, and idle timeouts.

Publish endpoint metadata through the DAP event mechanism. Deliver credentials only through an explicitly secret-handling debugger/client integration, never ordinary output events. Define this integration before implementation. Sanitize existing verbose launch/attach argument logging and protocol tracing as well as new logs. Never write tokens into normal logs, telemetry, crash reports, workspace files, or source control.

If the client requires a connection descriptor, create it with restrictive filesystem permissions, remove it at session end, and store it only in an OS-appropriate temporary location. Prefer passing secret material directly through the debugger integration rather than writing it to disk.

### Connecting an AI MCP Client

`port: 0` means the OS assigns the port when binding. The adapter must receive the actual bound address, port, MCP path, and session identity after readiness; never publish port zero as a usable endpoint. A URL in a debug log alone is not a complete client integration.

1. For clients supporting dynamic server registration, the extension registers the live endpoint and session credential through the verified client API, then unregisters it on teardown. The user selects the debug session's server rather than guessing a port. Verify the VS Code MCP provider API and minimum VS Code version in T02; do not assume every client supports it.
2. Provide an `Erlang MCP: Show Connection Details` command through a thin hook in the existing extension integration, displaying connection metadata supplied by Erlang for live sessions. In multi-session workspaces, require session selection. Show the real URL and supported client setup route without revealing tokens in ordinary output, telemetry, or clipboard automatically.
3. For static clients, users can set `erlang.mcp.port` to a fixed local port and configure that URL. They must still supply a fresh per-session token using a supported secure credential input/export flow. A fixed port does not make the URL alone sufficient, and an old token must not reconnect after restart.
4. If a fixed port is occupied, disable MCP and report the conflict while debugging continues. Do not silently choose a different port for a statically configured client. Concurrent sessions need separate ports or dynamic registration.

T02 must name at least one actually supported client and specify its complete endpoint-plus-credential setup, including any required user action and secure fallback. If no secure setup route exists, report a blocker rather than claiming automatic discovery or disabling authentication. Do not introduce an always-on proxy solely to hide ephemeral ports.

## Security Model and Threat Analysis

Localhost-only binding reduces network exposure, but it is not an authorization boundary. Other processes running as the same user, compromised development tools, browser-origin attacks against local services, malicious MCP clients, and accidentally forwarded ports may still reach the server.

Treat all MCP requests, parameters, and client-supplied identifiers as untrusted.

Required controls:

- **Secure default:** disabled unless explicitly enabled for a debug session.
- **Loopback enforcement:** reject every non-loopback bind address, including values obtained after hostname resolution.
- **Per-session authentication:** use a fresh, high-entropy token and invalidate it when the debug session ends.
- **Least privilege:** expose only the configured read-only tool allowlist.
- **No arbitrary execution:** no eval, RPC trampoline, shell, module loading, or client-controlled function calls.
- **Data minimization:** return summaries rather than raw state and redact likely secrets.
- **Resource limits:** cap request size, response size, item count, nesting depth, binary size, traversal depth, concurrency, and execution time.
- **Safe identifiers:** resolve registered names through existing atoms only. Never call `list_to_atom/1`, `binary_to_atom/1`, or equivalent on input.
- **Safe serialization:** handle PIDs, ports, references, functions, improper lists, maps with non-string keys, large binaries, and cyclic-looking graph traversal without crashing or producing unbounded output.
- **No secret logging:** log only lifecycle, tool name, result status, duration, and bounded metadata. Do not log parameters or results by default.
- **Session isolation:** bind tokens, IDs, cursors, and workers to one session and target node. V1 authorizes node-level metadata inspection; selecting an application is a navigation filter, not a security boundary between applications in the same VM. Never traverse remote PIDs. Concurrent sessions must not share inspector state or credentials.
- **Clean shutdown:** stop listeners, invalidate tokens, cancel workers, and release retained data on controlled termination. A target-side session-owner monitor and renewable lease must stop inspection within 10 seconds without renewal while the VM is schedulable. Renewal must not depend on application processes stopped at breakpoints. Never terminate an attached application merely to stop its inspector. Remove temporary descriptors normally and clean stale descriptors on the next extension startup; immediate file deletion after an OS crash is not guaranteed.
- **No telemetry payloads:** runtime inspection results and authentication material must not enter telemetry.
- **Dependency review:** pin and review any new MCP or HTTP dependency according to project policy.

Document residual risk: application names, child identifiers, module names, and table names can reveal sensitive information. Metadata-only access does not guarantee zero overhead, atomic observations, or isolation from other code in the same VM. The exclusions and allowlists reduce risk but do not remove it completely.

## Execution Instructions for Luna or Sonnet

This is an implementation work order, not a claim that the feature exists. Execute the tasks below in dependency order. Keep the specification in English to match the repository; report progress in the user's language.

- Read repository instructions and inspect the current working tree before editing. Preserve unrelated changes. Do not commit or create branches unless requested.
- Work on one task at a time. Inspect the owning code, state the expected behavior, add the smallest useful change, and immediately run a focused check. Keep new modules small, but do not create one module per checkbox mechanically.
- Each task has dependencies, starting surfaces, work, and a completion gate. Starting surfaces are navigation hints, not permission to edit all listed files. Verify actual APIs and ownership before choosing new module names.
- Reuse existing test helpers and suites. Add a fixture or suite only where no suitable home exists. Do not refactor vendored JSON/formatting code.
- Record baseline failures separately. Never mark a task complete on an unexecuted check, a mock-only substitute for a required integration test, or an unsupported assumption.
- Update the checkboxes and append a short evidence entry under Implementation Progress after each completed task: changed files, exact commands, results, and remaining risks. Leave blocked tasks unchecked and identify the decision needed.
- Stop for a decision if no maintained transport supports the required OTP range, secure client credential delivery is unavailable, or the current attach topology cannot satisfy local-only access. Do not silently raise the OTP floor, forward a port, implement partial MCP, or weaken authorization.
- Do not delegate, broaden V1, add cloud services, or introduce an agent framework unless separately authorized. All task checks are deterministic; no paid model invocation is required.

## Implementation Tasks

### T01: Verify Integration Points and Baseline

Dependencies: none.

Starting surfaces: [lib/erlangDebugSession.ts](lib/erlangDebugSession.ts), [lib/ErlangShellDebugger.ts](lib/ErlangShellDebugger.ts), [lib/erlangConnection.ts](lib/erlangConnection.ts), [lib/ErlangConfigurationProvider.ts](lib/ErlangConfigurationProvider.ts), [package.json](package.json), [rebar.config](rebar.config), existing Erlang bridge and test directories.

- [x] Trace launch, attach, `noDebug`, configuration validation, target readiness, disconnect, target loss, and adapter shutdown. Identify which process owns each resource.
- [x] Inspect target bridge compilation/loading, including its separation from the LSP beam directory and discovery of `src/mcp/mcp_*.erl`. Verify all new owned MCP files follow the isolation/naming rules. Inspect logging, tracing, telemetry if present, and all paths that serialize debug arguments.
- [x] Record supported/tested OTP, Node, and VS Code versions from configuration, docs, and CI. Distinguish claimed compatibility from tested compatibility.
- [x] Run the current TypeScript compile and relevant existing debugger/bridge tests; record pre-existing failures and environment prerequisites.

Completion gate: append a concise verified integration note to this document with symbols, paths, lifecycle diagram or sequence, baseline commands, and version constraints. Do not invent an MCP dependency or a secret-delivery API.

### T02: Resolve Transport and Client Integration

Dependencies: T01.

Starting surfaces: dependency manifests, existing Node/Erlang HTTP bridge, debugger adapter factories, VS Code extension activation.

- [ ] Review available maintained MCP/HTTP libraries for license, pinned version, transitive footprint, OTP support, protocol revision, Streamable HTTP behavior, cancellation, authentication hooks, and known security issues.
- [ ] Select one supported MCP protocol revision and real client fixture. Document initialization, notifications, version negotiation, content types, optional streaming behavior, and error handling; do not invent protocol extensions for overload/errors.
- [x] Specify endpoint and credential delivery to an actual supported MCP client using verified client/VS Code APIs: dynamic registration of the bound endpoint, the connection-details command, session selection/unregistration, and fixed-port secure-token setup. Separate non-secret discovery from secret transfer, including DAP tracing redaction and descriptor fallback permissions if needed.
- [ ] Determine where the HTTP client, adapter, and target run in launch and attach. V1 requires the client to reach target loopback without forwarding. Reject unsupported remote/container arrangements with a clear error; preserve ordinary debugging when MCP is optional.
- [x] Document target-side dependency loading and cleanup for launch/attach, protocol session IDs versus authorization tokens, and limits for HTTP connections and protocol sessions.

Completion gate: the integration note identifies a feasible dependency/client/OTP combination, secret path, and supported deployment topology. Resolve blockers with the user before adding production dependencies.

### T03: Define Schemas and Mapping Semantics

Dependencies: T01, T02.

Starting surfaces: existing configuration/schema conventions and new MCP policy/contract code chosen after T01.

- [x] Define strict input/output schemas for the seven tools, graph entities/edges, result envelope, pagination, omissions, and sanitized errors. Reject extra tool arguments and unknown tool names.
- [x] Define edge direction, scalar representations, timestamp units, memory units, membership evidence, and absent/restarting supervisor children. Specify the difference between `complete`, `truncated`, and an empty authorized result.
- [x] Define ID identity/expiry rules, collection/cursor retention, bounded identity bookkeeping, and response-size accounting including MCP framing. Use table identity, not its reusable name alone, to detect recreation.
- [x] Specify safe root and callback discovery for supported OTP versions, including explicit unsupported/unknown results. Do not use links as proof of supervision or group leader alone as proof of membership.
- [x] Create minimal contract examples and positive/negative schema tests using existing test infrastructure.

Completion gate: all MAP-01 through MAP-07 requirements map to schema fields and at least one planned assertion; example results validate and invalid inputs fail deterministically.

### T04: Integrate LSP Settings and rebar Policy

Dependencies: T03.

Starting surfaces: [package.json](package.json), [lib/ErlangConfigurationProvider.ts](lib/ErlangConfigurationProvider.ts), [lib/erlangSettings.ts](lib/erlangSettings.ts), [lib/ErlangShellDebugger.ts](lib/ErlangShellDebugger.ts), existing configuration/debugger tests. New MCP implementation helpers belong only in `apps/erlangbridge/src/mcp/mcp_*.erl`; TypeScript edits stay in existing integration files.

- [x] Add `erlang.mcp.enabled`, `erlang.mcp.host`, and `erlang.mcp.port` to Erlang settings and folder-aware resolution. Carry validated internal configuration through the provider to DAP without adding a public launch/attach `mcp` block or `required` option. Share defaults/normalization.
- [x] Implement the trusted, bounded, non-evaluating project-term reader and nearest-project lookup in Erlang. TypeScript supplies folder/cwd context and Workspace Trust status, not parsed project policy. Normalize `allowed_tools`, `allowed_ets_tables`, and `limits` in Erlang; enforce disjoint source ownership, missing-file defaults, and explicit errors for malformed/duplicate/unsupported policies.
- [x] Validate booleans, port integers from 0 to 65535, literal/resolved loopback addresses, exact allowlists, unique tool names, limits, and unknown fields. Reject caller-provided credentials, legacy `required`, project activation/bind overrides, and debug-block attempts to bypass the setting.
- [x] Enforce the debug gate independently of JSON schema: no MCP startup for normal LSP, ordinary application launches, or `noDebug: true`.
- [x] Treat all MCP configuration/startup failures as non-fatal to debugging but fatal to that inspector startup. Never silently fall back to an open listener or broaden policy after validation failure.
- [x] Test defaults, empty allowlists, invalid ranges/types, IPv4/IPv6 loopback, wildcard/LAN/public addresses, unsupported topology, and disabled paths. Cover distinct multi-root settings, plain/umbrella projects, no rebar file, malformed/duplicate terms, profile-only policies, script non-execution, trust/path boundaries, parser resource limits, and new-session setting changes.

Completion gate: focused Erlang policy/configuration tests, TypeScript integration tests, and compile checks pass; absent/disabled MCP preserves existing arguments and behavior. Server-side validation is authoritative in Erlang; VS Code schema checks do not replace it.

### T05: Build Deterministic OTP Mapping Fixtures

Dependencies: T03.

Starting surfaces: [apps/erlangbridge/test](apps/erlangbridge/test), [test/test-fixtures](test/test-fixtures), [test/test-suite](test/test-suite).

- [x] Reuse or add a small OTP fixture with nested supervisors, workers with callback modules, registered/unregistered processes, a linked but unsupervised process, monitors, and an application with no discoverable root.
- [x] Include approved/unapproved/private named ETS tables and an owner process. Store recognizable test-only sentinels in forbidden data to detect accidental leakage.
- [x] Add deterministic controls for a restarting child, disappearing process, deleted/recreated table, blocked supervisor, and enough children for pagination. These controls belong to tests, never MCP tools.
- [x] Define the expected graph independently of inspector output. Provide reliable teardown of nodes, ports, tables, and temporary files even after assertion failures.

Completion gate: the fixture starts/stops repeatedly without leaks and supplies an independent expected topology for later assertions.

### T06: Implement Safe Encoding and Identifiers

Dependencies: T03, T05.

Starting surfaces: existing vendored JSON encoder API and new bounded MCP encoder/policy helpers.

- [x] Encode atoms, tuples, PIDs, ports, references, maps with non-string keys, improper lists, binaries, and unsupported terms with explicit representations. Do not encode executable functions or opaque terms as recoverable execution handles.
- [x] Enforce traversal/encoding depth, item, binary, and full-response byte budgets before unbounded conversion; account for JSON escaping and base64 expansion.
- [x] Resolve names through existing atoms only and validate local PIDs/IDs. Never use untrusted term decoding or client-controlled MFA dispatch.
- [x] Implement bounded session/node-scoped identities and stale-handle handling. Same observed entity must join across tools; process restart/table recreation must not reuse the prior ID.
- [x] Test invalid Unicode, huge/nested terms, non-string map keys, repeated client identifiers, stale/remote PIDs, and cross-session rejection. Warm up runtime dependencies before atom-count assertions.

Completion gate: focused Erlang tests show bounded output, meaningful truncation, no input-driven atom growth, and correct identity reuse/expiry.

### T07: Implement Collections, Workers, and Continuations

Dependencies: T06.

Starting surfaces: new inspector supervision subtree, collection state, and policy facade.

- [x] Implement per-session collection TTL/count/byte budgets and identity accounting, with bounded eviction and explicit expiry errors.
- [x] Bind cursors to collection, tool, arguments, and session. Page retained observations deterministically; return narrower-scope guidance when acquisition was incomplete.
- [x] Run collection work in monitored workers with bounded concurrency and no unbounded queue. Enforce deadlines, terminate only inspector workers, and ignore late replies safely.
- [x] Audit runtime calls that can allocate whole lists or continue executing after a caller times out. Define protective limits and document remaining non-interruptible cost rather than claiming strict preemption.
- [x] Test overload, cancellation, timeout cleanup, pagination integrity, retention expiry, memory-budget rejection, and cross-session cursor isolation.

Completion gate: workers and retained state return to baseline after timeout/expiry/shutdown; partial collections cannot masquerade as complete snapshots.

### T08: Implement Runtime and Application Discovery

Dependencies: T05, T07.

Starting surfaces: new runtime facade and existing safe OTP/runtime helpers.

- [x] Implement `runtime_summary` with bounded metadata, units, and non-secret session identity; omit/redact node host information by default.
- [x] Implement `application_overview` using safe OTP metadata, without invoking application callbacks. Discover applications, declared dependencies, available roots, modules, and behaviours.
- [x] Distinguish unsupported root discovery from an application with no supervisor. Exclude inspector internals from the selected application's topology and identify them explicitly if shown in node scope.
- [x] Implement graph envelopes, application selection, and paginated inventory. No process name should be required to begin exploration.
- [x] Verify MAP-01 and MAP-07 against the fixture, including absent module metadata and dependencies not currently running.

Completion gate: a client can discover the fixture application and usable root IDs without preconfigured supervisor names or source access.

### T09: Implement Supervision Mapping

Dependencies: T08.

Starting surfaces: runtime facade, worker deadlines, mapping fixture.

- [x] Implement `supervision_tree` from discovered application/root IDs, using OTP supervisor APIs with worker deadlines.
- [x] Emit direct parent-child edges, child kind, safe identifier, available module/restart/shutdown metadata, and absent/restarting-child state; omit start arguments.
- [x] Bound traversal, detect loops/repeated nodes, support subtree expansion, and produce continuations or explicit non-resumable omissions.
- [x] Return partial results for blocked or stopped supervisors without resuming them. Use a paused-specific reason only when debugger evidence supports it.
- [x] Test exact expected edges, nested trees, dynamic restarts, stale roots, blocked branches, depth limits, and linked-but-unsupervised processes.

Completion gate: MAP-02 through MAP-06 pass for supervision mapping; no link or monitor becomes a supervision edge.

### T10: Implement Process, ETS, and Debugger Metadata

Dependencies: T09.

Starting surfaces: runtime facade, existing debugger metadata, configuration allowlists.

- [x] Implement `registered_processes` and `process_info` with safe fields, application filtering, membership evidence, local-ID validation, and bounded links/monitors.
- [x] Add callback/behaviour correlation only from approved metadata; report unknown membership for unattributed processes instead of guessing.
- [x] Implement `ets_tables` by checking approved named tables only, excluding private tables and returning owner IDs/edges with stable table identity. Never call an ETS object/key read API for this tool.
- [x] Implement `debug_session` from available debugger state through a narrow sanitized metadata channel, not a generic DAP proxy. If authoritative data is unavailable inside the target, return an explicit unavailable status.
- [x] Test safe field allowlists, unknown membership, disappearing processes/tables, owner correlation, metadata freshness, and absence of sentinel secrets.

Completion gate: all seven tools return contract-valid bounded data through the same policy facade; no `ets_lookup` or generic execution path exists.

### T11: Implement Authenticated MCP Transport

Dependencies: T02, T04, T10.

Starting surfaces: reviewed dependencies, MCP supervisor/server/policy, real client fixture.

- [x] Add pinned reviewed dependencies and load the dedicated `mcp_sup` supervisor only for a debug session enabled by Erlang settings. Keep it out of the normal LSP supervision tree; verify prefixed modules in `src/mcp` compile and load without dependency name collisions.
- [ ] Implement initialization/version negotiation, advertised capabilities, tool listing, schema validation, invocation, structured results, supported cancellation, and protocol-compliant errors through the selected library.
- [x] Bind explicit loopback only, allocate ephemeral ports, require authorization for every HTTP request, validate `Host` and `Origin`, and disable CORS. Document safe handling of absent `Origin` for non-browser clients.
- [x] Enforce supported methods/content types, request-body limits, connection/session limits, header/body/idle timeouts, and overload rejection before expensive parsing/inspection. Never reflect secrets or stack traces.
- [ ] Test real client negotiation/invocation plus missing/invalid tokens, disallowed tools, malformed JSON/arguments, hostile hosts/origins, oversized bodies, unsupported versions, and resource exhaustion.

Completion gate: a real MCP client invokes the seven tools only when authorized, and protocol tests exercise the pinned revision without custom partial implementations.

### T12: Integrate Launch, Attach, and Cleanup

Dependencies: T11.

Starting surfaces: [lib/erlangDebugSession.ts](lib/erlangDebugSession.ts), [lib/ErlangShellDebugger.ts](lib/ErlangShellDebugger.ts), [lib/erlangConnection.ts](lib/erlangConnection.ts), target Erlang debugger bridge.

- [x] Compile/package/load the inspector and approved dependencies into the actual target node using the verified debugger bridge path, not the LSP node. Loading trusted inspector code is debugger bootstrap, never an MCP capability.
- [x] Start only after target readiness; generate fresh credentials and bind the endpoint in Erlang. Register the actual endpoint using the thin T02 client integration. Implement the connection-details command in the existing extension code, using Erlang-provided metadata, and reject fixed-port conflicts without rebinding. Do not expose tokens in process arguments or ordinary DAP output.
- [x] Implement target-side owner monitoring/lease renewal, token invalidation, worker/listener shutdown, retained-data cleanup, and descriptor cleanup where applicable.
- [ ] Cover terminate, disconnect, failed launch/attach, adapter crash, target loss, inspector supervisor failure, and two concurrent sessions. Disabled sessions must not start listeners, timers, or workers.
- [x] Preserve an attached target on detach or any MCP failure. Roll back partial inspector/client-registration resources and report the failure while launch/attach debugging continues. No MCP error may kill an externally owned node.
- [ ] Test launch/attach and all supported adapter modes, application breakpoints, lease expiry, dynamic endpoint registration/removal, static-port conflicts, multi-session selection, and non-fatal MCP failure behavior.

Completion gate: lifecycle tests prove cleanup within the documented schedulable-VM lease bound and no change to disabled debugging or attached-node ownership.

### T13: Audit Security and Regression Boundaries

Dependencies: T12.

Starting surfaces: all touched entry points, existing logging/tracing, negative tests.

- [x] Audit that every transport route and runtime operation goes through authorization, allowlists, identifier validation, and limits. Unknown tool names must not become Erlang atoms or function names.
- [ ] Add log/telemetry capture assertions for launch, attach, successful calls, rejections, exceptions, and shutdown. Use sentinel credentials and forbidden runtime data; sanitize existing argument serialization before it can contain secrets.
- [x] Test absent/expired/cross-session credentials and cursors, PID/node escape attempts, hostile table names, repeated unique strings, and no eval/RPC/shell/file/environment/cookie/state/mailbox access.
- [x] Verify no trace, ETS value lookup, mutable debugger operation, or source-content endpoint is advertised or callable.
- [x] Run existing LSP/DAP regression checks with MCP disabled and validate normal LSP startup has no inspector subtree/listener.

Completion gate: no unexplained security-test failures; residual risks and non-guarantees are explicitly documented, not hidden by skipped assertions.

### T14: Prove Agent-Oriented End-to-End Mapping

Dependencies: T13.

Starting surfaces: real MCP client fixture, OTP graph fixture, debugger integration tests.

- [ ] Launch a debug target and initialize a real client using the actual endpoint/credential discovery integration, not a test-only secret shortcut.
- [ ] Discover the application without known process names, expand roots across multiple pages, join process and approved ETS-owner IDs, and compare reconstructed edges to the independent expected graph.
- [ ] Include a blocked branch, unknown membership, missing source metadata, and a linked-but-unsupervised process. Assert explicit incompleteness and no fabricated relationships.
- [ ] Collect before/after a deterministic restart and verify identity changes without treating absence from a partial collection as termination.
- [ ] Terminate/detach and prove endpoint closure, removal from client discovery, invalid credentials/cursors, cleared retained state, and survival of an externally owned attached target. Repeat the applicable workflow for attach, port zero, and a configured fixed port.

Completion gate: deterministic end-to-end tests prove the documented agent workflow without an LLM, preconfigured supervisor names, or source access.

### T15: Document, Package, and Hand Off

Dependencies: T14.

Starting surfaces: [README.md](README.md), [HELP.MD](HELP.MD), [DevelopersReadme.md](DevelopersReadme.md), [CHANGELOG.md](CHANGELOG.md), existing packaging/CI configuration.

- [x] Document supported OTP/client/topology combinations, folder-scoped Erlang enablement, the optional rebar policy term and precedence, no `required` option, dynamic and fixed-port endpoint/secret setup, seven tool schemas, mapping workflow, example partial graph, units, cursors, and breakpoint behavior in the most suitable existing docs.
- [x] Document node-level authorization versus application filters, exclusions, metadata leakage, local-process threats, no port forwarding, lease assumptions, overhead, and cleanup limitations. Avoid copying credentials into examples.
- [x] Add the changelog entry and ensure prefixed `src/mcp` modules and target dependencies ship in the extension artifact. Assert new project-owned MCP filenames/locations follow the isolation convention, including tests and helpers. Verify startup from a packaged artifact, not only the source tree; do not publish it.
- [x] Integrate deterministic new checks into existing CI conventions. Run supported compatibility checks available in the environment and explicitly report unavailable platforms/OTP versions.
- [ ] Run final compile, Erlang tests, extension tests, and packaging checks. Resolve regressions introduced by this feature; keep baseline failures separate.
- [ ] Produce a final handoff listing changed files, lifecycle/security decisions, exact checks/results, compatibility gaps, and deferred work. Mark only proven tasks complete.

Completion gate: acceptance criteria below are evidenced, user/developer/security documentation agrees with implemented behavior, and the packaged extension can perform the V1 workflow.

### Validation Commands

Use repository-local commands and verify prerequisites first. These are commands for the implementing model, not evidence that they have been run for this document change.

```sh
npm run compile
./rebar3 compile
./rebar3 ct --suite apps/erlangbridge/test/mcp/mcp_<actual_suite>_SUITE
npx vscode-test --run out/test/test-suite/mcp/mcp_<actual_test>.test.js
./rebar3 ct
npm test
npm run webpack
```

Replace placeholders with test files actually created or extended. Compile TypeScript before targeted VS Code tests. Use the repository's installed VSIX packaging tool for T15 and do not publish. Run focused checks immediately after edits; reserve full suites and artifact checks for relevant integration gates. Record actual exit codes and distinguish compile checks from behavior checks.

## Implementation Progress

Decisions taken with the user: own minimal Streamable HTTP MCP transport in Erlang (no maintained Erlang MCP library supports OTP 18+ and none is in the repo; zero new dependencies); client integration through the VS Code MCP provider API (`lm.registerMcpServerDefinitionProvider`, VS Code >= 1.101) plus the connection-details command.

**T01 (verified integration note).** Launch: `ErlangDebugSession.launchRequest` -> `ErlangConnection.Start` compiles `gen_connection/vscode_connection/vscode_jsone(+decode)/mcp/*` into `_build/default/lib/ebin` (separate from the LSP beams) and starts the Node HTTP receiver; `configurationDone` spawns `erl -pa ebin -s vscode_connection start`; the target posts `/listen` with its command-server port. Attach: helper node (`-hidden`, `vscode_connection:attach/0`) pushes modules to the target and runs `start_attached`. Disconnect: `Quit()` -> `debugger_exit` (launch) or `debugger_detach` (attach, node kept). Target loss: `quitEvent`/`halt_on_nodedown`. Versions: CI OTP 25.3/26.2/27.3/28.1, Node 20, VS Code `^1.136.0`; tested locally only OTP 26.2.5 (erts 14.2.5), Node 25.9, VS Code 1.139.1 headless. Baseline: no failing test before/after (`./rebar3 ct`: 225 passed).

**T02.** Protocol 2025-06-18, stateless (no Mcp-Session-Id), JSON responses only, GET 405, `notifications/cancelled` accepted but cancellation unsupported (request-scoped by timeout). Endpoint (non-secret) via DAP custom event `erlangMcp`; secret via a 0600 descriptor read+deleted by the extension host. Supported client: VS Code (provider API); others via *Show Connection Details* + fixed port. Not done: library review (superseded by the decision), a real third-party MCP client fixture, rejection of remote/container topologies (only documented).

**T03-T10.** `apps/erlangbridge/src/mcp/mcp_{policy,encoder,audit,store,tools,runtime,server,sup}.erl`; hooks in `vscode_connection.erl` (`mcp_start/mcp_renew/mcp_stop`, conditional module push via `-vscode_mcp`). Tests: `apps/erlangbridge/test/mcp/` `mcp_{policy,encoder,store,server,bridge}_SUITE` + fixtures `mcp_fixture_*`. Command: `./rebar3 ct --dir apps/erlangbridge/test/mcp`.
Deviation: `proc_lib:initial_call/1` (reads the process dictionary internally, result never returned) gates supervisor API calls so a plain gen_server is never sent `which_children`.

**T11.** No new dependency. Real MCP client interoperability and protocol-level cancellation NOT done (tests use a raw HTTP/JSON-RPC client).

**T12.** Verified end-to-end with the real adapter over DAP and a real `erl` target (launch and attach): `test/test-suite/mcp/mcp_debug_adapter.test.ts`. Only `external` adapter mode tested; target loss, inspector supervisor failure, two concurrent sessions and the live VS Code provider registration are NOT tested.

**T13.** Audit + negative tests done (auth, host/origin, hostile ids, atoms, sentinels, logs). Adapter/DAP log capture for every phase is partial.

**T14 (not done).** No real MCP client; the adapter-level e2e covers discovery/credential hand-over, policy, lease renewal, cleanup and attach survival.

**T15.** Docs: HELP.MD, mcp_security.md, DevelopersReadme.md, CHANGELOG.md; CI step added. Not done: startup from a packaged VSIX, OTP 25/27/28 runs.

Next: T11 real-client test, T12/T14 gaps above (still open; deliberately not scheduled).

### Post-V1 additions

Decided with the user after the V1 work order, all within the read-only/metadata-only guardrails (no new write, eval, mailbox, ETS-value or source access). The toolset is now twelve tools; the original seven are unchanged apart from the additive fields noted.

- `top_processes` (queue length / reductions / memory ranking), `top_ports` (queue size / traffic counters; driver name and owner only, never addresses or command lines), `ets_summary` (ETS memory per owner, no table names), `topology_overview` (one application in one collection; each part obeys its own tool's allowlist entry), `changes_since` (diff against a retained collection; baseline limited by `collection_ttl_ms` / `max_collections`).
- Additive fields: `children` counts on supervisors (`supervisor:count_children`), `pausedProcesses` in `debug_session` and a `debugger` field in `process_info` (from `int:snapshot/0`; a supervisor that is itself stopped at a breakpoint is reported as `unavailable_while_paused`), run queue / VM resource usage / connected node names in `runtime_summary`, `format: "mermaid"` on graph tools.
- New edge type `owns_port` (MAP-03 extension). New MCP prompt `map_application` (the only exception to "no prompts"; static text, argument-checked, grants nothing).
- Developer tier (opt-in, only when named in the project's `allowed_tools`; returns a bounded, best-effort-redacted sample of application data, so it is outside the metadata-only V1 guardrail by design): `process_state` (`sys:get_state` of gen_server/gen_statem/gen_event, never a supervisor or a plain process), `mailbox_sample`, `ets_sample` (approved non-private tables). `allowed_ets_tables` may be `all`. Limits can be raised above their defaults up to fixed ceilings. See [mcp_security.md](./mcp_security.md).
- Review fixes: `ets_summary` counts only approved tables, `top_ports` names only well-known drivers, `changes_since` observes with the baseline's detail, one time budget for `debug_session`, one tree walk in `topology_overview`, inspector processes excluded by module, redaction of child ids, token/secret hidden from crash reports, non-ASCII workspace paths, `\z` anchors.
- Not added: restart intensity/period (no safe API), distributed traversal, tracing, any mutation.
- Tests: `./rebar3 ct --dir apps/erlangbridge/test/mcp` (83 cases). Plain `./rebar3 ct` does not include this directory.

## Acceptance Criteria

The feature is complete when all the following are true:

- MCP is disabled by default and impossible to start through the normal LSP path.
- Enabling `erlang.mcp.enabled` in the selected folder's Erlang settings starts MCP only when an actual debug session starts, inside its target node. Normal LSP operation and `noDebug` never start it.
- Optional top-level rebar policy controls tools/tables/lower limits without overriding activation, binding, or credentials; malformed policy disables only MCP.
- A supported AI client receives the actual ephemeral endpoint and fresh credential through the documented secure integration. Static clients have a documented fixed-port setup; port conflicts never silently rebind.
- New project-owned MCP files use `mcp_` names in dedicated subfolders, with Erlang production code under `apps/erlangbridge/src/mcp/`; packaged launch/attach resolves these modules correctly.
- The complete MCP server and policy implementation is Erlang. TypeScript contains only thin existing-file VS Code/debugger/client integration hooks, with no dedicated MCP subsystem or production MCP SDK.
- The endpoint accepts connections only through loopback.
- Every request requires session-scoped authorization.
- Only documented read-only tools are advertised and callable.
- A real client reconstructs the fixture topology without prior supervisor/process names; MAP-01 through MAP-07 are verified.
- Results expose typed edges, consistent IDs, evidence, observation intervals, continuation, and explicit coverage limitations. No map claims observed message traffic.
- Topology discovery works without source access; ETS remains metadata-only.
- No generic eval, arbitrary RPC, shell, mutation, full-state dump, or unrestricted ETS/mailbox access exists.
- Results are bounded, sanitized, and safely encoded.
- MCP failure never aborts ordinary debugging; partial inspector resources are cleaned up and an actionable error is reported. No `required` configuration exists.
- The MCP server and credentials are invalidated on shutdown or target-side lease expiry under the documented runtime assumptions; retained data and descriptors follow the documented cleanup policy.
- Existing LSP and debugger behavior remains unchanged when MCP is disabled.
- Unit, integration, security, and end-to-end tests pass.
- User and security documentation is included.

## Deliverables

- Implementation following the existing Erlang/OTP project style.
- Updated Erlang settings schema, rebar project-policy reader/schema, and client connection examples.
- Automated tests.
- User documentation.
- A short security note describing threat model, controls, exclusions, and residual risks.
- Changelog entry for the new feature.

## Final Implementation Guardrails

- Inspect the repository before choosing module names, dependencies, or integration points.
- Reuse existing configuration, supervision, logging, telemetry, JSON, HTTP, DAP, and test infrastructure.
- Keep the diff focused on this feature. Do not perform unrelated refactoring.
- Prefer small modules with explicit policy boundaries.
- Do not weaken an existing security check to simplify MCP integration.
- If a requirement conflicts with existing architecture, implement the safest compatible design and document the trade-off in the final summary.
- In the final response, list changed files, describe lifecycle and security decisions, and report tests executed and their results.
