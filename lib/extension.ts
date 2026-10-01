// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';
import {
    workspace as Workspace, window as Window, ExtensionContext, TextDocument, OutputChannel, WorkspaceFolder, Uri, debug,
    languages, IndentAction, DebugAdapterDescriptorFactory, commands, lm, EventEmitter, env,
    McpHttpServerDefinition, DebugSession
} from 'vscode';

import * as Adapter from './vscodeAdapter';
import * as Rebar from './RebarRunner';
import * as Eunit from './eunitRunner';
import { ErlangDebugConfigurationProvider, configurationChanged, getElangConfigConfiguration } from './ErlangConfigurationProvider';
import { ErlangDebugAdapterDescriptorFactory, InlineErlangDebugAdapterFactory, ErlangDebugAdapterExecutableFactory } from './ErlangAdapterDescriptorFactory';
import * as erlangConnection from './erlangConnection';
import { ErlangTaskProvider } from './erlangTaskProvider';
import * as ErlangInstallation from './erlangInstallation';

import * as LspClient from './lsp/lspclientextension';

var myoutputChannel: OutputChannel;

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: ExtensionContext) {
    erlangConnection.setExtensionPath(context.extensionPath);

    myoutputChannel = Adapter.ErlangOutput();
    // Use the console to output diagnostic information (console.log) and errors (console.error)
    // This line of code will only be executed once when your extension is activated
    console.log('Congratulations, your extension "erlang" is now active!');
    myoutputChannel.appendLine("erlang extension is active");

    //configuration of erlang language -> documentation : https://code.visualstudio.com/Docs/extensionAPI/vscode-api#LanguageConfiguration
    var disposables = [];
    // The command has been defined in the package.json file
    // Now provide the implementation of the command with  registerCommand
    // The commandId parameter must match the command field in package.json
    //disposables.push(vscode.commands.registerCommand('extension.rebarBuild', () => { runRebarCommand(['compile']);}));

    var rebar = new Rebar.RebarRunner();
    rebar.activate(context);

    var eunit = new Eunit.EunitRunner();
    eunit.activate(context);

    disposables.push(ErlangTaskProvider.register(context));
    ErlangInstallation.activate(context);

    disposables.push(debug.registerDebugConfigurationProvider("erlang", new ErlangDebugConfigurationProvider()));
    disposables.push(Workspace.onDidChangeConfiguration((e) => configurationChanged()));  
    disposables.push(Workspace.onDidChangeWorkspaceFolders((e) => configurationChanged()));
    
    let runMode = getElangConfigConfiguration().debuggerRunMode;
    let factory: DebugAdapterDescriptorFactory;
    switch (runMode) {
        case 'server':
            // run the debug adapter as a server inside the extension and communicating via a socket
            factory = new ErlangDebugAdapterDescriptorFactory();
            break;

        case 'inline':
            // run the debug adapter inside the extension and directly talk to it
            factory = new InlineErlangDebugAdapterFactory();
            break;

        case 'external': default:
            // run the debug adapter as a separate process
            factory = new ErlangDebugAdapterExecutableFactory(context.extensionPath);
            break;
    }
    
    disposables.push(debug.registerDebugAdapterDescriptorFactory('erlang', factory));
    if ('dispose' in factory) {
		disposables.push(factory);
    }
    disposables.forEach((disposable => context.subscriptions.push(disposable)));
    LspClient.activate(context);
    activateMcpIntegration(context);

    languages.setLanguageConfiguration("erlang", {
        onEnterRules: [
            // Module comment: always continue comment
            {
                beforeText: /^%%% .*$/,
                action: { indentAction: IndentAction.None, appendText: "%%% " }
            },
            {
                beforeText: /^%%%.*$/,
                action: { indentAction: IndentAction.None, appendText: "%%%" }
            },
            // Comment line with double %: continue comment if needed
            {
                beforeText: /^\s*%% .*$/,
                afterText: /^.*\S.*$/,
                action: { indentAction: IndentAction.None, appendText: "%% " }
            },
            {
                beforeText: /^\s*%%.*$/,
                afterText: /^.*\S.*$/,
                action: { indentAction: IndentAction.None, appendText: "%%" }
            },
            // Comment line with single %: continue comment if needed
            {
                beforeText: /^\s*% .*$/,
                afterText: /^.*\S.*$/,
                action: { indentAction: IndentAction.None, appendText: "% " }
            },
            {
                beforeText: /^\s*%.*$/,
                afterText: /^.*\S.*$/,
                action: { indentAction: IndentAction.None, appendText: "%" }
            },
            // Any other comment line: do nothing, ignore below rules
            {
                beforeText: /^\s*%.*$/,
                afterText: /^\s*$/,
                action: { indentAction: IndentAction.None }
            },

            // Empty line: do nothing, ignore below rules
            {
                beforeText: /^\s*$/,
                action: { indentAction: IndentAction.None }
            },

            // Before guard sequence (before 'when')
            {
                beforeText: /.*/,
                afterText: /^\s*when(\s.*)?$/,
                action: { indentAction: IndentAction.None, appendText: "  " }
            },
            // After guard sequence (after 'when')
            {
                beforeText: /^\s*when(\s.*)?->\s*$/,
                action: { indentAction: IndentAction.None, appendText: "  " }
            },
            {
                beforeText: /^\s*when(\s.*)?(,|;)\s*$/,
                action: { indentAction: IndentAction.None, appendText: "     " }
            },

            // Start of clause, right hand side of an assignment, after 'after', etc.
            {
                beforeText: /^.*(->|[^=]=|\s+(after|begin|case|catch|if|maybe|of|receive|try))\s*$/,
                action: { indentAction: IndentAction.Indent }
            },

            // Not closed bracket
            {
                beforeText: /^.*[(][^)]*$/,
                action: { indentAction: IndentAction.Indent }
            },
            {
                beforeText: /^.*[{][^}]*$/,
                action: { indentAction: IndentAction.Indent }
            },
            {
                beforeText: /^.*[[][^\]]*$/,
                action: { indentAction: IndentAction.Indent }
            },

            // One liner clause but not the last
            {
                beforeText: /^.*->.+;\s*$/,
                action: { indentAction: IndentAction.None }
            },
            // End of function or attribute (e.g. export list)
            {
                beforeText: /^.*\.\s*$/,
                action: { indentAction: IndentAction.Outdent, removeText: 9000 }
            },
            // End of clause but not the last
            // FIXME: After a guard (;) it falsely outdents
            {
                beforeText: /^.*;\s*$/,
                action: { indentAction: IndentAction.Outdent }
            },
            // Last statement of a clause
            // TODO: double outdent or outdent + removeText: <tabsize>
            {
                beforeText: /^.*[^;,[({<]\s*$/,
                action: { indentAction: IndentAction.Outdent }
            }
        ]
    });
}

// ---- Embedded MCP inspector: thin VS Code hooks (all MCP logic is Erlang) ----------------------------
// The debug adapter announces the live endpoint with a custom DAP event (no secret in it) and leaves
// the session token in a 0600 descriptor that is read and deleted immediately; the token then lives
// in this process' memory only, and is handed to VS Code through the MCP provider API.
interface McpSession { label: string; url: string; sessionId: string; token: string; }
const mcpSessions = new Map<string, McpSession>();

// "Erlang MCP" output channel: one line per MCP request (metadata only), to see how many calls an
// agent makes and whether the time goes to the inspector (ms) or to the agent (+delta between calls).
let mcpChannel: OutputChannel | undefined;
const mcpJournal = new Map<string, { start: number, last?: number }>();

function mcpLog(line: string) {
    if (!mcpChannel) {
        mcpChannel = Window.createOutputChannel('Erlang MCP');
    }
    mcpChannel.appendLine(line);
}

function clock(t: number): string {
    return new Date(t).toISOString().substring(11, 23);
}

function seconds(ms: number): string {
    return ms < 1000 ? `${ms} ms` : `${(ms / 1000).toFixed(1)} s`;
}

function formatMcpCall(m: any, sinceLast: number | undefined): string {
    const parts = [`#${m.seq ?? '?'}`, m.method === 'tools/call' ? `tools/call ${m.tool}` : `${m.method}`, `${m.status}`];
    if (typeof m.durationMs === 'number') { parts.push(`${m.durationMs} ms`); }
    if (typeof m.bytes === 'number') { parts.push(m.bytes >= 1024 ? `${(m.bytes / 1024).toFixed(1)} KB` : `${m.bytes} B`); }
    if (typeof m.entities === 'number') { parts.push(`ents ${m.entities} rels ${m.relationships}`); }
    if (typeof m.offset === 'number') { parts.push(`page@${m.offset}${m.cursor ? ' (cursor)' : ''}${m.more ? ' +more' : ''}`); }
    if (m.omissions) { parts.push(`omissions ${m.omissions}`); }
    if (m.complete === false) { parts.push('partial'); }
    if (sinceLast !== undefined) { parts.push(`(+${seconds(sinceLast)} since previous)`); }
    return parts.join('  ');
}

function activateMcpIntegration(context: ExtensionContext) {
    removeStaleMcpDescriptors();
    const changed = new EventEmitter<void>();
    context.subscriptions.push(changed);

    context.subscriptions.push(debug.onDidReceiveDebugSessionCustomEvent(e => {
        if (e.event !== 'erlangMcp' && e.event !== 'erlangMcpCall') {
            return;
        }
        if (e.event === 'erlangMcpCall') {
            const journal = mcpJournal.get(e.session.id) ?? { start: Date.now() };
            const now = Date.now();
            mcpLog(`${clock(now)}  ${formatMcpCall(e.body ?? {}, journal.last === undefined ? undefined : now - journal.last)}`);
            journal.last = now;
            mcpJournal.set(e.session.id, journal);
            return;
        }
        const body = e.body ?? {};
        if (body.status === 'started') {
            mcpJournal.set(e.session.id, { start: Date.now() });
            mcpLog(`── ${clock(Date.now())} session "${e.session.name}": ${body.url}`);
            const session = readMcpDescriptor(body.descriptor, e.session);
            if (session) {
                mcpSessions.set(e.session.id, session);
            } else {
                Window.showWarningMessage('Erlang MCP: the endpoint credential could not be read; MCP is unavailable for this debug session.');
            }
        } else if (body.status === 'error') {
            Window.showWarningMessage(`Erlang MCP is disabled for this debug session: ${body.message}`);
        } else if (body.status === 'stopped') {
            mcpSessions.delete(e.session.id);
            const journal = mcpJournal.get(e.session.id);
            if (journal) {
                mcpLog(`── ${clock(Date.now())} end of "${e.session.name}": ${body.calls ?? 0} calls, `
                    + `${seconds(body.serverMs ?? 0)} in the inspector, ${seconds(Date.now() - journal.start)} of session`);
                mcpJournal.delete(e.session.id);
            }
        }
        changed.fire();
    }));
    context.subscriptions.push(debug.onDidTerminateDebugSession(s => {
        if (mcpSessions.delete(s.id)) {
            changed.fire();
        }
    }));

    // dynamic registration: needs VS Code with the MCP provider API (>= 1.101)
    if (typeof lm?.registerMcpServerDefinitionProvider === 'function') {
        context.subscriptions.push(lm.registerMcpServerDefinitionProvider('erlang.mcp', {
            onDidChangeMcpServerDefinitions: changed.event,
            provideMcpServerDefinitions: async () =>
                [...mcpSessions.values()].map(s => new McpHttpServerDefinition(s.label, Uri.parse(s.url))),
            resolveMcpServerDefinition: async (server) => {
                const session = [...mcpSessions.values()].find(s => s.label === server.label);
                if (!session || !(server instanceof McpHttpServerDefinition)) {
                    return undefined;
                }
                server.headers = { Authorization: `Bearer ${session.token}` };
                return server;
            }
        }));
    }

    context.subscriptions.push(commands.registerCommand('erlang.mcp.showConnectionDetails', async () => {
        const sessions = [...mcpSessions.entries()];
        if (sessions.length === 0) {
            Window.showInformationMessage('Erlang MCP: no debug session with an MCP inspector is running (enable erlang.mcp.enabled and start a debug session).');
            return;
        }
        let picked = sessions[0][1];
        if (sessions.length > 1) {
            const item = await Window.showQuickPick(sessions.map(([, s]) => ({ label: s.label, description: s.url, session: s })),
                { placeHolder: 'Select the debug session' });
            if (!item) {
                return;
            }
            picked = item.session;
        }
        // The token is never shown or copied automatically: only on an explicit choice.
        const choice = await Window.showInformationMessage(
            `Erlang MCP (${picked.label}): ${picked.url}\nVS Code clients are registered automatically. For a static client, `
            + `use this URL with a fixed erlang.mcp.port and an "Authorization: Bearer <token>" header (the token changes at every debug session).`,
            'Copy URL', 'Copy Authorization header');
        if (choice === 'Copy URL') {
            await env.clipboard.writeText(picked.url);
        } else if (choice === 'Copy Authorization header') {
            await env.clipboard.writeText(`Authorization: Bearer ${picked.token}`);
        }
    }));
}

function readMcpDescriptor(descriptor: string, session: DebugSession): McpSession | undefined {
    try {
        if (!descriptor || !path.basename(path.dirname(descriptor)).startsWith('erlang-mcp-')) {
            return undefined;
        }
        const data = JSON.parse(fs.readFileSync(descriptor, 'utf8'));
        return { label: `Erlang debug: ${session.name} (${session.id.substring(0, 8)})`, url: data.url, sessionId: data.sessionId, token: data.token };
    } catch {
        return undefined;
    } finally {
        try { fs.rmSync(path.dirname(descriptor), { recursive: true, force: true }); } catch { /* best effort */ }
    }
}

/** Descriptors are deleted right after being read; clean up those left by a crash. */
function removeStaleMcpDescriptors() {
    try {
        const tmp = os.tmpdir();
        for (const name of fs.readdirSync(tmp)) {
            if (name.startsWith('erlang-mcp-')) {
                const dir = path.join(tmp, name);
                if (Date.now() - fs.statSync(dir).mtimeMs > 60000) {
                    fs.rmSync(dir, { recursive: true, force: true });
                }
            }
        }
    } catch { /* best effort */ }
}

export function deactivate(): Thenable<void> {
    return LspClient.deactivate();
}
