import * as path from 'path';
import * as fs from 'fs';
import {
	workspace as Workspace, window as Window, ExtensionContext, TextDocument, OutputChannel, WorkspaceFolder,
	Uri, Disposable, WorkspaceConfiguration, ProviderResult, CodeLens, FileSystemWatcher, workspace
} from 'vscode';

import {
	LanguageClient, LanguageClientOptions, TransportKind, ConfigurationParams,
	CancellationToken, DidChangeConfigurationNotification, ServerOptions, Middleware, DidChangeWorkspaceFoldersNotification, DidChangeWatchedFilesNotification, FileChangeType
} from 'vscode-languageclient';

import * as lspcodelens from './lspcodelens';

import { ErlangShellForDebugging } from '../ErlangShellDebugger';

import * as erlConnection from '../erlangConnection';

import { ErlangSettings } from '../erlangSettings';
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
let lspOutputChannel: OutputChannel;

namespace Configuration {

    let configurationListener: Disposable;
    let fileSystemWatcher: FileSystemWatcher;

	// Convert VS Code specific settings to a format acceptable by the server. Since
	// both client and server do use JSON the conversion is trivial. 
	export function computeConfiguration(params: ConfigurationParams, _token: CancellationToken, _next: Function): any[] {

		//lspOutputChannel.appendLine("computeConfiguration :"+ JSON.stringify(params));

		if (!params.items) {
			return null;
		}
		let result: (ErlangSettings | null)[] = [];
		for (let item of params.items) {
			// The server asks the client for configuration settings without a section
			// If a section is present we return null to indicate that the configuration
			// is not supported.
			if (item.section) {
				let erlSectionConfig = JSON.parse(JSON.stringify(Workspace.getConfiguration(item.section)));
				result.push(erlSectionConfig);
				result.push(null);
				continue;
			}
			let config: WorkspaceConfiguration;
			if (item.scopeUri) {
				config = Workspace.getConfiguration('erlang', client.protocol2CodeConverter.asUri(item.scopeUri));
			} else {
				config = Workspace.getConfiguration('erlang');
			}

			let erlConfig = JSON.parse(JSON.stringify(config));
			result.push(erlConfig);
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
                {changes: [{uri: uri.fsPath, type: FileChangeType.Created}]});
        })
	}

	export function dispose() {
		if (configurationListener) {
			configurationListener.dispose();
		}
	}
}

let _sortedWorkspaceFolders: string[];
function sortedWorkspaceFolders(): string[] {
	if (_sortedWorkspaceFolders === void 0) {
		_sortedWorkspaceFolders = Workspace.workspaceFolders.map(folder => {
			let result = folder.uri.toString();
			if (result.charAt(result.length - 1) !== '/') {
				result = result + '/';
			}
			return result;
		}).sort(
			(a, b) => {
				return a.length - b.length;
			}
			);
	}
	return _sortedWorkspaceFolders;
}
Workspace.onDidChangeWorkspaceFolders(() => _sortedWorkspaceFolders = undefined);

function getOuterMostWorkspaceFolder(folder: WorkspaceFolder): WorkspaceFolder {
	let sorted = sortedWorkspaceFolders();
	for (let element of sorted) {
		let uri = folder.uri.toString();
		if (uri.charAt(uri.length - 1) !== '/') {
			uri = uri + '/';
		}
		if (uri.startsWith(element)) {
			return Workspace.getWorkspaceFolder(Uri.parse(element));
		}
	}
	return folder;
}


export function activate(context: ExtensionContext) {

	lspOutputChannel = Window.createOutputChannel('Erlang Language Server');
	let serverModule = context.asAbsolutePath(path.join('lib', 'lsp', 'lspserver.js'));
	if (!fs.existsSync(serverModule)) {
		serverModule = context.asAbsolutePath(path.join('out', 'lib', 'lsp', 'lspserver.js'));
	}
	let erlLSPPath = erlConnection.erlangBridgePath;
	//	let erlangCmd = "erl "
	// The debug options for the server
	let debugOptions = { execArgv: ["--nolazy", "--inspect=6009"] };

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	let serverOptions: ServerOptions = {
		run: { module: serverModule, transport: TransportKind.ipc },
		debug: { module: serverModule, transport: TransportKind.ipc, options: debugOptions }
	}

	let middleware: Middleware = {
		workspace: {
			configuration: Configuration.computeConfiguration
		},
		provideCodeLenses: (document, token) =>{
			return Promise.resolve(lspcodelens.onProvideCodeLenses(document, token)).then(x => x);
		},
		resolveCodeLens: (codeLens) => {
			return Promise.resolve(lspcodelens.onResolveCodeLenses(codeLens)).then(x => x);
		}
	};

	// Options to control the language client
	let clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'erlang' }],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contain in the workspace
			fileEvents: Workspace.createFileSystemWatcher('**/.clientrc'),
			// In the past this told the client to actively synchronize settings. Since the
			// client now supports 'getConfiguration' requests this active synchronization is not
			// necessary anymore. 
			// configurationSection: [ 'lspMultiRootSample' ]
		},
		middleware: middleware,
		diagnosticCollectionName: 'Erlang Language Server',
		outputChannel: lspOutputChannel
	}

	client = new LanguageClient('Erlang Language Server', 'Erlang Language Server', serverOptions, clientOptions);
	Configuration.initialize();
	// Start the client. This will also launch the server
	client.start();
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

