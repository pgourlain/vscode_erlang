/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict';

import {
	createConnection, TextDocuments, TextDocument, Diagnostic, DiagnosticSeverity,
	ProposedFeatures, InitializeParams, Proposed, Range, DocumentFormattingParams, CompletionItem,
	TextDocumentPositionParams, Definition, Location
} from 'vscode-languageserver';

import { ErlangLspConnection, ParsingResult } from './erlangLspConnection';
import { ErlangShellLSP } from './ErlangShellLSP';
import { IErlangShellOutput } from '../GenericShell';
import { erlangBridgePath } from '../erlangConnection';
import { ErlangSettings } from '../erlangSettings';

class ChannelWrapper implements IErlangShellOutput {
	
	show(): void {
	}
	appendLine(value: string): void {
		debugLog(value);
	}

}

// Create a connection for the server. The connection uses Node's IPC as a transport
let connection = createConnection(ProposedFeatures.all);
// erlang shell to start LSP http server
let erlangLsp = new ErlangShellLSP(new ChannelWrapper())
//local http server to send/receive command to erlang LSP
let erlangLspConnection = new ErlangLspConnection(new ChannelWrapper());

// Create a simple text document manager. The text document manager
// supports full document sync only
let documents: TextDocuments = new TextDocuments();

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
//trace for debugging 
let traceEnabled = false;

connection.onInitialize(async (params: InitializeParams) => {

	//connection.console.log("onInitialize.");
	
	await erlangLspConnection.Start(traceEnabled).then(port => {
		return erlangLsp.Start("", erlangBridgePath+"/..", port, "src", "");
	}, (reason) => {
		connection.console.log(`LspConnection Start failed : ${reason}`);		
	});

	//erlangLspConnection.on("validatedTextDocument", onValidatedTextDocument);
	let capabilities = params.capabilities;

	// Does the client support the `workspace/configuration` request? 
	// If not, we will fall back using global settings
	hasWorkspaceFolderCapability = (capabilities as Proposed.WorkspaceFoldersClientCapabilities).workspace && !!(capabilities as Proposed.WorkspaceFoldersClientCapabilities).workspace.workspaceFolders;
	hasConfigurationCapability = (capabilities as Proposed.ConfigurationClientCapabilities).workspace && !!(capabilities as Proposed.ConfigurationClientCapabilities).workspace.configuration;
	if (hasConfigurationCapability) {
		debugLog(JSON.stringify((capabilities as Proposed.ConfigurationClientCapabilities).workspace.configuration));
	}
	debugLog(`capabilities => hasWorkspaceFolderCapability:${hasWorkspaceFolderCapability}, hasConfigurationCapability:${hasConfigurationCapability}`);
	return {
		capabilities: {
			textDocumentSync: documents.syncKind,
			documentFormattingProvider : true,
			definitionProvider : true,
			// completionProvider : {
			// 	resolveProvider: true,
			// 	triggerCharacters: [ ':' ]
			// }
		}
	}
});

connection.onInitialized(async () => {
	var globalConfig = await connection.workspace.getConfiguration("erlang");
	if (globalConfig) {
		let erlangConfig = globalConfig;
		if (erlangConfig && erlangConfig.languageServerProtocol.verbose) {
			traceEnabled = true;
		}
	}

	//debugLog("connection.onInitialized");
	if (hasWorkspaceFolderCapability) {
		connection.workspace.onDidChangeWorkspaceFolders((_event) => {
			debugLog('Workspace folder change event received');
		});
	}
});

connection.onShutdown(() => {
	debugLog("connection.onShutDown");
	erlangLsp.Kill();
});

connection.onExit(() => {
	debugLog("connection.onExit");
	erlangLsp.Kill();
});

// The global settings, used when the `workspace/configuration` request is not supported by the client.
// Please note that this is not the case when using this server with the client provided in this example
// but could happen with other clients.
const defaultSettings: ErlangSettings = { erlangPath: "", rebarBuildArgs:[],  rebarPath: "",  languageServerProtocol : { verbose : false} };
let globalSettings: ErlangSettings = defaultSettings;

// Cache the settings of all open documents
let documentSettings: Map<string, Thenable<ErlangSettings>> = new Map();

connection.onDidChangeConfiguration(change => {
	if (hasConfigurationCapability) {
		// Reset all cached document settings
		documentSettings.clear();
	} else {
		globalSettings = <ErlangSettings>(change.settings.lspMultiRootSample || defaultSettings);
	}

	// Revalidate all open text documents
	documents.all().forEach(validateTextDocument);
});

function getDocumentSettings(resource: string): Thenable<ErlangSettings> {
	if (!hasConfigurationCapability) {
		return Promise.resolve(globalSettings);
	}
	let result = documentSettings.get(resource);
	if (!result) {
		result = connection.workspace.getConfiguration({ scopeUri: resource });
		documentSettings.set(resource, result);
	}
	return result;
}

// Only keep settings for open documents
documents.onDidClose(e => {
	documentSettings.delete(e.document.uri);
	erlangLspConnection.onDocumentClosed(e.document.uri);
});

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent((change) => {
	validateTextDocument(change.document);
});

async function validateTextDocument(textDocument: TextDocument): Promise<void> {
	// In this simple example we get the settings for every validate run.
	let settings = await getDocumentSettings(textDocument.uri);
	// .hrl files show incorrect errors e.g. about not used records, ddisabled for now
	if (textDocument.uri.endsWith(".hrl"))
		return;

	erlangLspConnection.validateTextDocument(textDocument.uri, 
		100,
		parsingResult => onValidatedTextDocument(parsingResult, textDocument));
}

connection.onDocumentFormatting(async (params : DocumentFormattingParams) => {
	erlangLspConnection.FormatDocument(params.textDocument.uri);
	return [];
});

connection.onDefinition(async (textDocumentPosition: TextDocumentPositionParams):Promise<Definition> => {
	let fileName = textDocumentPosition.textDocument.uri;
	let res = await erlangLspConnection.getDefinitionLocation(fileName, textDocumentPosition.position.line, 
		textDocumentPosition.position.character);
	if (res) {
		return Location.create(res.uri, Range.create(res.line, res.character, res.line, res.character));
	}
	return null;
});

//https://stackoverflow.com/questions/38378410/can-i-add-a-completions-intellisense-file-to-a-language-support-extension
connection.onCompletion(async (textDocumentPosition: TextDocumentPositionParams):Promise<CompletionItem[]> => {
	let document = documents.get(textDocumentPosition.textDocument.uri);
	if (document == null) {
		debugLog(`unable to get document '${textDocumentPosition.textDocument.uri}'`);
		return [];
	}
	let textDocument = document.getText();
	
	let offset = document.offsetAt(textDocumentPosition.position);
	let char = textDocument.substr(offset-1, 1);

	if (char == ':') {
		var items = await erlangLspConnection.GetCompletionItems(textDocumentPosition.textDocument.uri, 
			textDocumentPosition.position.line, 
			textDocumentPosition.position.character, char);
		return <CompletionItem[]>items.map(x => { return {label: x, kind:2}});	
	}
	return [];
	// return [{
	// 	label : "test",
	// 	kind : 2
	// }];
});

connection.onCompletionResolve((item: CompletionItem): CompletionItem =>{
	debugLog("resolve :" +JSON.stringify(item));
	return item;
});


function debugLog(msg : string) : void {
	if (true /*traceEnabled*/) {
		connection.console.log(msg);
	}
}

function onValidatedTextDocument(parsingResult : ParsingResult, textDocument : TextDocument) : void {

	if (parsingResult.parse_result) {	
		let diagnostics: Diagnostic[] = [];
		if (parsingResult.errors_warnings) {
			for (var i = 0; i < parsingResult.errors_warnings.length; i++) {
				let error = parsingResult.errors_warnings[i];
				var severity:DiagnosticSeverity = DiagnosticSeverity.Error;
				switch(error.type) {
					case "warning" :
						severity = DiagnosticSeverity.Warning;
					break;
					case "info" :
						severity = DiagnosticSeverity.Information;
					break;
					default :
						severity = DiagnosticSeverity.Error;
					break;
				}
				diagnostics.push({
					severity: severity,					
					range: Range.create(error.info.line-1, 0, error.info.line-1, 255),
					message: error.info.message,
					source: 'ex'
				});			
			}
		}
		connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });	
	}
}

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();