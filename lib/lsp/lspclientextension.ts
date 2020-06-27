import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';
import {
    workspace as Workspace, window as Window, ExtensionContext, TextDocument, OutputChannel, WorkspaceFolder,
    Uri, Disposable, WorkspaceConfiguration, ProviderResult, CodeLens, FileSystemWatcher, workspace
} from 'vscode';

import {
    LanguageClient, LanguageClientOptions, TransportKind, ConfigurationParams, StreamInfo,
    CancellationToken, DidChangeConfigurationNotification, ServerOptions, Middleware, DidChangeWorkspaceFoldersNotification, 
    DidChangeWatchedFilesNotification, FileChangeType, DidSaveTextDocumentNotification,DidSaveTextDocumentParams, TextDocumentSaveRegistrationOptions
} from 'vscode-languageclient';

import { ErlangShellLSP } from './ErlangShellLSP';
import { erlangBridgePath } from '../erlangConnection';
import * as Net from 'net';

import * as lspcodelens from './lspcodelens';

import { ErlangShellForDebugging } from '../ErlangShellDebugger';

import * as erlConnection from '../erlangConnection';

import { ErlangSettings } from '../erlangSettings';
import RebarShell from '../RebarShell';
import { ErlangOutputAdapter } from '../vscodeAdapter';
import { getElangConfigConfiguration } from '../ErlangConfigurationProvider';

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
        let result: any[] = [];
        for (let item of params.items) {
            if (item.section) {
                if (item.section === "<computed>") {
                    result.push({
                        autosave: Workspace.getConfiguration("files").get("autoSave", "afterDelay") === "afterDelay",
                        tmpdir: os.tmpdir()
                    });
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
            { changes: [{ uri: uri.fsPath, type: FileChangeType.Created }] });
        });
        fileSystemWatcher.onDidDelete(uri => {
          client.sendNotification(DidChangeWatchedFilesNotification.type,
            { changes: [{ uri: uri.fsPath, type: FileChangeType.Deleted }] });
        });
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
function waitForSocket(options:any, callback:any, _tries:any) {
  if (!options.port)
    throw new Error('.port is a required option');

  var maxTries = options.tries || MAX_TRIES;
  var host = options.host || 'localhost';
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
// TODO: convert to async function
function compileErlangBridge(extensionPath: string) : Thenable<string> {
    return new RebarShell([], extensionPath, ErlangOutputAdapter())
        .compile(extensionPath)
        .then(({ output }) => output);
    // TODO: handle failure to compile erlangbridge
}

function getPort(callback) {
  var server = Net.createServer(function (sock) {
    sock.end('OK\n');
  });
  server.listen(0, function () {
    var port = (<Net.AddressInfo>server.address()).port;
    server.close(function () {
      callback(port);
    });
  });
}

export function activate(context: ExtensionContext) {
    let erlangCfg = getElangConfigConfiguration(); 
    if (erlangCfg.verbose)
        lspOutputChannel = Window.createOutputChannel('Erlang Language Server');

    let middleware: Middleware = {
        workspace: {
            configuration: Configuration.computeConfiguration
        },
        provideCodeLenses: (document, token) =>{
            return Promise.resolve(lspcodelens.onProvideCodeLenses(document, token)).then(x => x);
        },
        resolveCodeLens: (codeLens) => {
            return Promise.resolve(lspcodelens.onResolveCodeLenses(codeLens)).then(x => x);
        },
        didSave: (data : TextDocument, next : (data : TextDocument) => void) => {
          next(data);//call LSP
          lspcodelens.onDocumentDidSave();
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

    let clientName = erlangCfg.verbose ? 'Erlang Language Server' : '';
    client = new LanguageClient(clientName, async () => {
        return new Promise<StreamInfo>(async (resolve, reject) => {
            await compileErlangBridge(context.extensionPath);
            let erlangLsp = new ErlangShellLSP(ErlangOutputAdapter(lspOutputChannel));
            erlangLsp.erlangPath = erlangCfg.erlangPath;

            getPort(async function (port) {
                erlangLsp.Start("", erlangBridgePath, port, "src", "");
                let socket = await waitForSocket({ port: port }, function (error, socket) {
                    resolve({ reader: socket, writer: socket });
                }, undefined);
            });
        });
    }, clientOptions, true);
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
