
import * as os from 'os';
import * as fs from 'fs';
import * as path from 'path';
import {
	workspace as Workspace, window as Window, ExtensionContext, TextDocument, OutputChannel,
	LogOutputChannel, Uri, Disposable, CodeLens, FileSystemWatcher, workspace, languages, commands
} from 'vscode';

import {
	 ConfigurationParams,
	CancellationToken, DidChangeConfigurationNotification, Middleware,
	DidChangeWatchedFilesNotification, FileChangeType
} from 'vscode-languageclient';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind,
	StreamInfo,
	State
} from 'vscode-languageclient/node';

import { ErlangShellLSP } from './ErlangShellLSP';
import { erlangBridgePath } from '../erlangConnection';
import * as Net from 'net';

import * as lspcodelens from './lspcodelens';

import * as lspValue from './lsp-inlinevalues';
import * as lspTest from './lsp-testcontroller';
import { LspStatus, SHOW_OUTPUT_COMMAND, RESTART_COMMAND } from './lsp-status';


// import { ErlangShellForDebugging } from '../ErlangShellDebugger';

// import * as erlConnection from '../erlangConnection';

// import { ErlangSettings } from '../erlangSettings';
import RebarShell from '../RebarShell';
import { ErlangOutputAdapter } from '../vscodeAdapter';
import { getElangConfigConfiguration, resolveErlangSettings } from '../ErlangConfigurationProvider';
import { ErlangLanguageClient, erlangDocumentSelector } from './lsp-context';

/*
other LSP
https://github.com/rust-lang-nursery/rls-vscode/blob/master/src/extension.ts
https://github.com/tintoy/msbuild-project-tools-vscode/blob/master/src/extension/extension.ts
https://microsoft.github.io/language-server-protocol/implementors/servers/
https://microsoft.github.io/language-server-protocol/specification
https://github.com/mtsmfm/language_server-ruby/blob/master/lib/language_server.rb


exemple TS <-> TS <--> C#
https://tomassetti.me/language-server-dot-visual-studio/

*/

export let client: LanguageClient;
let clients: Map<string, LanguageClient> = new Map();
let lspOutputChannel: LogOutputChannel;

namespace Configuration {

	let configurationListener: Disposable;
	let fileSystemWatcher: FileSystemWatcher;

	// Convert VS Code specific settings to a format acceptable by the server. Since
	// both client and server do use JSON the conversion is trivial. 
	export function computeConfiguration(params: ConfigurationParams, _token: CancellationToken, _next: Function): any[] {

		if (!params.items) {
			return null;
		}
		let result: any[] = [];
		for (let item of params.items) {
			if (item.section) {
				if (item.section === "<computed>") {
					result.push({
						autosave: Workspace.getConfiguration("files").get("autoSave", "afterDelay") === "afterDelay",
						tmpdir: os.tmpdir(),
						username: os.userInfo().username
					});
				} else if (item.section === "erlang") {
					result.push(resolveErlangSettings(Workspace.getConfiguration(item.section)))
				}
				else {
					result.push(Workspace.getConfiguration(item.section));
				}
			}
			else {
				result.push(null);
			}
		}
		return result;
	}

	export function initialize() {
		//force to read configuration
		lspcodelens.configurationChanged();
		// VS Code currently doesn't sent fine grained configuration changes. So we 
		// listen to any change. However this will change in the near future.
		configurationListener = Workspace.onDidChangeConfiguration(() => {
			lspcodelens.configurationChanged();
			client.sendNotification(DidChangeConfigurationNotification.type, { settings: null });
		});
		fileSystemWatcher = workspace.createFileSystemWatcher('**/*.erl');
		fileSystemWatcher.onDidCreate(uri => {
			client.sendNotification(DidChangeWatchedFilesNotification.type,
				{ changes: [{ uri: uri.toString(), type: FileChangeType.Created }] });
		});
		fileSystemWatcher.onDidChange(uri => {
			client.sendNotification(DidChangeWatchedFilesNotification.type,
				{ changes: [{ uri: uri.toString(), type: FileChangeType.Changed }] });
		});
		fileSystemWatcher.onDidDelete(uri => {
			client.sendNotification(DidChangeWatchedFilesNotification.type,
				{ changes: [{ uri: uri.toString(), type: FileChangeType.Deleted }] });
		});
	}

	export function dispose() {
		if (configurationListener) {
			configurationListener.dispose();
		}
		if (fileSystemWatcher) {
			fileSystemWatcher.dispose();
		}
	}
}



var MAX_TRIES = 10;
var WAIT_BETWEEN_TRIES_MS = 250;

/**
 * Tries to connect to a given socket location.
 * Time between retires grows in relation to attempts (attempt * RETRY_TIMER).
 *
 *  waitForSocket({ port: 2828, maxTries: 10 }, function(err, socket) {
 *  });
 *
 * Note- there is a third argument used to recursion that should
 * never be used publicly.
 *
 * Options:
 *  - (Number) port: to connect to.
 *  - (String) host: to connect to.
 *  - (Number) tries: number of times to attempt the connect.
 *
 * @param {Object} options for connection.
 * @param {Function} callback [err, socket].
 */
function waitForSocket(options: any, callback: any, _tries: any) {
	if (!options.port)
		throw new Error('.port is a required option');

	var maxTries = options.tries || MAX_TRIES;
	var host = options.host || '127.0.0.1';
	var port = options.port;


	_tries = _tries || 0;
	if (_tries >= maxTries)
		return callback(new Error('cannot open socket'));

	function handleError() {
		// retry connection
		setTimeout(
			waitForSocket,
			// wait at least WAIT_BETWEEN_TRIES_MS or a multiplier
			// of the attempts.
			(WAIT_BETWEEN_TRIES_MS * _tries) || WAIT_BETWEEN_TRIES_MS,
			options,
			callback,
			++_tries
		);
	}

	var socket = Net.connect(port, host, () => {
		socket.removeListener('error', handleError);
		callback(null, socket);
	});
	socket.once('error', handleError);
}

/**
 * Uses the extension-provided rebar3 executable to compile the erlangbridge app.
 *
 * @param extensionPath - Path to the editor extension.
 * @returns Promise resolved or rejected when compilation is complete.
 */
async function compileErlangBridge(extensionPath: string): Promise<string> {
	const { exitCode, output } = await new RebarShell([getElangConfigConfiguration().rebarPath], extensionPath, ErlangOutputAdapter())
		.compile(extensionPath, getElangConfigConfiguration().erlangPath);
	// vscode_lsp_entry recompiles the bridge sources in memory at startup, so a
	// failed rebar3 compile only matters when no previous build exists.
	if (exitCode !== 0 && !fs.existsSync(path.join(erlangBridgePath, 'ebin', 'vscode_lsp_entry.beam'))) {
		const tail = output.trim().split('\n').slice(-5).join('\n');
		throw new Error(`compiling the Erlang bridge with rebar3 failed (exit code ${exitCode})${tail ? ':\n' + tail : ''}`);
	}
	return output;
}

function getPort(callback) {
	var server = Net.createServer(function (sock) {
		sock.end('OK\n');
	});
	server.listen(0, '127.0.0.1', function () {
		var port = (<Net.AddressInfo>server.address()).port;
		server.close(function () {
			callback(port);
		});
	});
}

export function activate(context: ExtensionContext) {
	let erlangCfg = getElangConfigConfiguration();
	if (erlangCfg.verbose)
		lspOutputChannel = Window.createOutputChannel('Erlang Language Server', { log: true });

	const status = new LspStatus();
	context.subscriptions.push(status);
	context.subscriptions.push(commands.registerCommand(SHOW_OUTPUT_COMMAND, () => client?.outputChannel.show()));
	context.subscriptions.push(commands.registerCommand(RESTART_COMMAND, () => startClient(client.restart())));
	// Set by a failed start: the Stopped state that follows must not hide it.
	let startFailure: string | undefined;
	const startClient = (starting: Promise<void>) => {
		startFailure = undefined;
		status.starting();
		starting.catch(error => {
			startFailure = error instanceof Error ? error.message : String(error);
			status.failed(startFailure);
			Window.showErrorMessage(`Erlang language server failed to start: ${startFailure}`, 'Show Output')
				.then(choice => { if (choice) { client.outputChannel.show(); } });
		});
	};

	lspValue.activate(context, lspOutputChannel);

	let middleware: Middleware = {
		workspace: {
			configuration: Configuration.computeConfiguration
		},
		provideCodeLenses: (document, token) => {
			return Promise.resolve(lspcodelens.onProvideCodeLenses(document, token)).then(x => x);
		},
		resolveCodeLens: (codeLens) => {
			return Promise.resolve(lspcodelens.onResolveCodeLenses(codeLens)).then(x => x);
		},
		didSave: async (data, next) => {
			await next(data);//call LSP
			lspcodelens.onDocumentDidSave();
		}
	};
	// Options to control the language client
	let clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'erlang' }],
		middleware: middleware,
		diagnosticCollectionName: 'Erlang Language Server',
		// An open document's diagnostics come from the push channel
		// (textDocument/publishDiagnostics); workspace/diagnostic covers the
		// project files that are *not* open (lsp_handlers reports open ones as
		// empty). Document pull would report an open file a second time, into
		// the client's own DiagnosticCollection, and two collections holding
		// the same problem means two Problems rows, two hover messages, and
		// each quick fix offered twice - lsp_codeaction:code_actions/3 emits
		// one action per diagnostic in the request context.
		// `filter` is the actual off-switch: with only the trigger flags
		// cleared, a server-sent workspace/diagnostic/refresh still makes the
		// client re-pull open documents.
		diagnosticPullOptions: {
			onChange: false,
			onSave: false,
			onFocus: false,
			onTabs: false,
			filter: () => true // true = do not pull this document
		},
		outputChannel: lspOutputChannel
	}

	// vscode-languageclient (>=10) uses this name to lazily create its own
	// fallback output channel (e.g. from handleFailedRequest/error paths)
	// whenever clientOptions.outputChannel is unset - an empty string here
	// makes that fall-back call VS Code's createOutputChannel with a falsy
	// name and throw. Always give the client a real name; erlang.verbose
	// still gates whether *our own* lspOutputChannel is created/populated.
	let clientName = 'Erlang Language Server';
	client = new ErlangLanguageClient(clientName, async () => {
		return new Promise<StreamInfo>(async (resolve, reject) => {
			try {
				await compileErlangBridge(context.extensionPath);
			} catch (error) {
				reject(error);
				return;
			}
			let erlangLsp = new ErlangShellLSP(ErlangOutputAdapter(lspOutputChannel));
			let connected = false;
			// spawnError is set when erl itself never started (GenericShell's
			// 'error' event, e.g. ENOENT on a missing cwd or missing binary);
			// exitCode alone doesn't say why the server never came up.
			erlangLsp.on('close', (exitCode, spawnError?: Error) => {
				if (!connected) {
					const reason = spawnError ? spawnError.message : `erl exited with code ${exitCode}`;
					reject(new Error(`${reason} before the language server was reachable (is erl on the PATH? see erlang.erlangPath)`));
				}
			});

			getPort(async function (port) {
				erlangLsp.Start("", erlangBridgePath, port, "src", "");
				let socket = await waitForSocket({ port: port }, 
					function (error, socket) {
						if (error) {
							reject(new Error(`cannot connect to the language server on port ${port}`));
							return;
						}
						connected = true;
						resolve({ reader: socket, writer: socket });
					}, 
					undefined);
				//
				(<ErlangLanguageClient>client).onReady();
			});
		});
	}, clientOptions, lspOutputChannel, true);
	context.subscriptions.push(client.onDidChangeState(e => {
		switch (e.newState) {
			case State.Starting:
				status.starting();
				break;
			case State.Running:
				status.ready(client.initializeResult?.serverInfo?.version);
				break;
			case State.Stopped:
				if (!startFailure) {
					status.stopped();
				}
				break;
		}
	}));
	Configuration.initialize();
	// Start the client. This will also launch the server
	startClient(client.start());
	// `client` (imported by lsp-testcontroller as a live binding) must be
	// assigned before this runs - it registers an onNotification handler
	// eagerly, unlike lspValue.activate above which only dereferences
	// `client` lazily inside request calls.
	lspTest.activate(context, lspOutputChannel);
}

export function debugLog(msg: string): void {
	if (lspOutputChannel) {
		lspOutputChannel.appendLine(msg);
	}
}

export function deactivate(): Thenable<void> {
	if (!client) {
		return undefined;
	}
	Configuration.dispose();
	return client.stop();
}
