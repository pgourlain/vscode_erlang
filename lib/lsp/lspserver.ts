/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict';

import { EventEmitter } from 'events';

import {
    createConnection, TextDocuments, TextDocument, Diagnostic, DiagnosticSeverity, WorkspaceFolder,
    ProposedFeatures, InitializeParams, InitializeResult, DidChangeConfigurationNotification, Range, DocumentFormattingParams, CompletionItem,
    TextDocumentPositionParams, Definition, Hover, Location, MarkedString, CompletionItemKind,
    ReferenceParams, CodeLensParams, CodeLens, Command, ExecuteCommandParams, Position, MarkupContent, MarkupKind
} from 'vscode-languageserver';

import URI from 'vscode-uri';
 
import { ErlangLspConnection, ParsingResult } from './erlangLspConnection';
import { ErlangShellLSP } from './ErlangShellLSP';
import { IErlangShellOutput } from '../GenericShell';
import { erlangBridgePath } from '../erlangConnection';
import { ErlangSettings } from '../erlangSettings';
import * as http from 'http'; 
import * as os from 'os'; 
import * as path from 'path'; 
import * as fs from 'fs'; 
import { workspace, TextEdit } from 'vscode';

class ChannelWrapper implements IErlangShellOutput {
    
    show(): void {
    }
    appendLine(value: string): void {
        debugLog(value);
    }
}

class DocumentValidatedEvent extends EventEmitter {

    public Fire() {
        this.emit("documentValidated");
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

let lspServerConfigured = false;
let documentValidtedEvent = new DocumentValidatedEvent();
let module2helpPage: Map<string, string[]> = new Map();

//trace for debugging 
let traceEnabled = false;

connection.onInitialize(async (params: InitializeParams): Promise<InitializeResult> => {

    //connection.console.log("onInitialize.");
    await erlangLspConnection.Start(traceEnabled).then(port => {
        return erlangLsp.Start("", erlangBridgePath+"/..", port, "src", "");
    }, (reason) => {
        connection.console.log(`LspConnection Start failed : ${reason}`);       
    });

    return <InitializeResult>{
        capabilities: {
            textDocumentSync: documents.syncKind,
            documentFormattingProvider : true,
            definitionProvider: true,
            hoverProvider: true,
            codeLensProvider :  { resolveProvider : true },
            referencesProvider : true,
            completionProvider: { triggerCharacters: [":", "#", "."]}
            // executeCommandProvider: {
            //  commands : ["erlang.showReferences"]
            // },
            // completionProvider : {
            //  resolveProvider: true,
            //  triggerCharacters: [ ':' ]
            // }
        }
    }
});

connection.onInitialized(async () => {
    debugLog("onInitialized");
    var globalConfig = await connection.workspace.getConfiguration("erlang");
    if (globalConfig) {
        let erlangConfig = globalConfig;
        if (erlangConfig && erlangConfig.verbose) {
            traceEnabled = true;
        }
    }

    connection.onDidChangeWatchedFiles( event => {
        debugLog('onDidChangeWatchedFiles '+JSON.stringify(event));
    });

    var whenConnected = async function () {
        if (erlangLspConnection.isConnected) {
            setConfigInLSP(function () {
                lspServerConfigured = true;                
            });
        }
        else {
            setTimeout(function () {
                whenConnected();
            }, 100);
        }
    };
    whenConnected();    
});

async function setConfigInLSP(callback) {
    var entries = new Map<string, string>();
    entries.set("root", findRoot(await connection.workspace.getWorkspaceFolders()));
    var globalConfig = await connection.workspace.getConfiguration("erlang");
    if (globalConfig && globalConfig.includePaths.length > 0)
        entries.set("include_paths", globalConfig.includePaths.join("|"));
    erlangLspConnection.setConfig(entries, callback);
}

function uriToFile(uri: string): string {
    if (process.platform == 'win32')
        uri = uri.replace(/file:\/\/\/([A-Za-z])%3A\//, 'file://$1:/');
    if (uri.startsWith("file://"))
        return uri.substr(7);
    else
        return uri;    
}

function findRoot(folders: WorkspaceFolder[]): string {
    var root: string = "";
    folders.forEach(folder => {
        var folderPath = uriToFile(folder.uri);
        if (!root || fs.existsSync(path.join(folderPath, "rebar.config")))
            root = folderPath;
    });
    return root;
}

connection.onExecuteCommand((cmdParams: ExecuteCommandParams): any => {
    debugLog(`onExecuteCommand : ${JSON.stringify(cmdParams)}`);
    //connection.sendRequest(CommandReques)
    return null;
});

connection.onShutdown(() => {
    debugLog("connection.onShutDown");
    erlangLspConnection.Quit();
});

connection.onExit(() => {
    debugLog("connection.onExit");
    erlangLspConnection.Quit();
});

// The global settings, used when the `workspace/configuration` request is not supported by the client.
// Please note that this is not the case when using this server with the client provided in this example
// but could happen with other clients.
const defaultSettings: ErlangSettings = { erlangPath: "", rebarBuildArgs:[],  rebarPath: "", includePaths: [], linting: true, codeLensEnabled: false, verbose: false };
let globalSettings: ErlangSettings = defaultSettings;

connection.onDidChangeConfiguration(async change => {
    debugLog("connection.onDidChangeConfiguration");
    setConfigInLSP(function () {
        // Revalidate all open text documents
        documents.all().forEach(document => {
            let diagnostics: Diagnostic[] = [];
            connection.sendDiagnostics({ uri: document.uri, diagnostics });
            validateDocument(document);
        });
    });
});

function waitForServerConfigured(fun) {
    var whenReady = function () {
        if (lspServerConfigured)
            fun();
        else {
            setTimeout(function () {
                whenReady();
            }, 100);
        }
    };
    whenReady();    
}

async function isAutoSaveEnabled() {
    var filesConfig = await connection.workspace.getConfiguration("files");
    return filesConfig.autoSave === 'afterDelay';
}

documents.onDidOpen(async event => {
    debugLog("onDidOpen: " + event.document.uri);
    if (await isAutoSaveEnabled()) {
        waitForServerConfigured(function () {
            validateDocument(event.document);           
        });
    }
});

documents.onDidSave(async event => {
    debugLog("onDidSave: " + event.document.uri);
    if (await isAutoSaveEnabled()) {
        waitForServerConfigured(function () {
            validateDocument(event.document);           
        });
    }
});

documents.onDidClose(event => {
    debugLog("onDidSave: " + event.document.uri);
    let diagnostics: Diagnostic[] = [];
    connection.sendDiagnostics({ uri: event.document.uri, diagnostics });
    erlangLspConnection.onDocumentClosed(event.document.uri);
});

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent(async event => {
    if (!await isAutoSaveEnabled()) {
        waitForServerConfigured(function () {
            validateDocument(event.document, event.document.version === 1);
        });
    }
});

function saveContentsToTmpFile(document: TextDocument): string {
    var randomName:string = Math.floor(Math.random() * 10000000).toString();
    var tmpFileName = path.join(os.tmpdir(), randomName);
    fs.writeFileSync(tmpFileName, document.getText());
    return tmpFileName;
}

async function validateDocument(document: TextDocument, saved: boolean = true): Promise<void> {
    var erlangConfig = await connection.workspace.getConfiguration("erlang");
    var linting = erlangConfig && erlangConfig.linting;
    if (document.uri.endsWith(".erl")) {
        if (saved) {
            erlangLspConnection.parseSourceFile(document.uri, "", () => {
                if (linting)
                    erlangLspConnection.validateParsedSourceFile(document.uri, 
                        parsingResult => onValidatedDocument(parsingResult, document));    
            });
        }
        else {
            var tmpFileName = saveContentsToTmpFile(document);
            erlangLspConnection.parseSourceFile(document.uri, tmpFileName, () => {
                fs.unlinkSync(tmpFileName);
                if (linting)
                    erlangLspConnection.validateParsedSourceFile(document.uri, 
                        parsingResult => onValidatedDocument(parsingResult, document));    
            });
        }
    }
    else if (linting && (document.uri.endsWith(".src") || document.uri.endsWith(".config"))) {
        if (saved) {
            erlangLspConnection.validateConfigFile(document.uri, "",
                parsingResult => onValidatedDocument(parsingResult, document));        
        }
        else {
            var tmpFileName = saveContentsToTmpFile(document);
            erlangLspConnection.validateConfigFile(document.uri, tmpFileName, parsingResult => {
                fs.unlinkSync(tmpFileName);
                onValidatedDocument(parsingResult, document);
            });
        }
    }
    // fire that document is validated
    documentValidtedEvent.Fire();
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

function markdown(str: string): MarkupContent {
    str = str.trim();
    var reg = /(((< *\/[^>]+>)|(< *([a-zA-Z0-9]+)[^>]*>))|([^<]+))/g;
    var out = '';
    var result;
    var tags = [];
    var off = [];
    while ((result = reg.exec(str)) !== null) {
        if (result[4]) {
            var tagName = result[5];            
            var tag = ''
            var endTag = '';
            if (tagName === 'br') {
                if (off.length === 0)
                    out += '  \n';
                continue;
            }
            else if (tagName === 'p' || result[4].indexOf('name="') >= 0) {
                tag = '';
                endTag = '  \n';
            }
            else if (result[4].indexOf('REFTYPES') >= 0 || result[4].indexOf('func-types-title') >= 0) {
                off.push(true);
                tag = '';
                endTag = 'ON';
            }
            else if (result[4].indexOf('<dt') >= 0) {
                tag = '  \n**';
                endTag = '**  \n';
            }
            else if (result[4].indexOf('bold_code') >= 0 && off.length === 0)
                tag = endTag = ' **';
            else if (result[4].indexOf('h3') >= 0) {
                tag = '\n#### ';
                endTag = '\n';
            }
            out += tag;
            tags.push(endTag);
        }
        else if (result[3]) {
            var top = tags.pop();
            if (top === 'ON')
                off.pop();
            else
                out += top;
        }
        else if (result[6] && off.length === 0)
            out += result[6];
    }
    return {
        value: out,
        kind: MarkupKind.Markdown
    };
}

async function getModuleHelpPage(moduleName: string): Promise<string[]> {
    if (module2helpPage.has(moduleName)) {
        return module2helpPage.get(moduleName);
    }
    else {
        return new Promise<string[]>(resolve => {
            http.get('http://erlang.org/doc/man/' + moduleName + '.html', (response) => {
                let contents:string = '';
                response.on('data', (chunk) => {
                    contents += chunk;
                });
                response.on('end', () => {
                    module2helpPage.set(moduleName, contents.split('\n'));
                    resolve(module2helpPage.get(moduleName));
                });   
            }).on("error", (error) => {
                module2helpPage.set(moduleName, []);
                resolve([]);
            });       
        });
    }
};

function extractHelpForFunction(functionName: string, htmlLines: string[]): string {
    var helpText: string = '';
    var found = false;
    for (var i = 0; i < htmlLines.length; ++i) {
        var trimmed = htmlLines[i].trim();
        if (!found) {
            if (trimmed.indexOf('name="' + functionName) >= 0) {
                found = true;
                helpText = trimmed;
            }
        }
        else {
            if (!trimmed || trimmed.indexOf('name="') !== trimmed.indexOf('name="' + functionName))
                break;
            else
                helpText += '\n' + trimmed;
        }
    }
    return helpText;
}

connection.onHover(async (textDocumentPosition: TextDocumentPositionParams): Promise<Hover> => {
    var uri = textDocumentPosition.textDocument.uri;
    let res = await erlangLspConnection.getHoverInfo(uri, textDocumentPosition.position.line, textDocumentPosition.position.character);
    if (res) {
        debugLog(JSON.stringify(res))
        if (res.text) {
            return {contents: res.text};
        }
        else {
            var htmlLines = await getModuleHelpPage(res.moduleName);
            return {contents: markdown(extractHelpForFunction(res.functionName, htmlLines))};
        }
    }
    return null;
});

connection.onReferences(async (reference : ReferenceParams) : Promise<Location[]> => {
    var uri = reference.textDocument.uri;
    let res = await erlangLspConnection.getReferencesInfo(uri, reference.position.line, reference.position.character);
    if (res) {
        var Result = new Array<Location>();
        res.forEach(ref => {
            Result.push(Location.create(ref.uri, Range.create(ref.line, ref.character, ref.line, ref.character)));
        });
        return Result;
    }
    return null;
});

connection.onCodeLens(async (codeLens: CodeLensParams) : Promise<CodeLens[]>  => {
    //wait doucment validation before get codelenses
    //in order to get on last version of parsed document
    return await new Promise<CodeLens[]>(a => {
        let fn = () =>
        {
            documentValidtedEvent.removeListener("documentValidated", fn);        
            a(getCodeLenses(codeLens));        
        };
        documentValidtedEvent.addListener("documentValidated", fn);
    });
});

async function getCodeLenses(codeLens: CodeLensParams) : Promise<CodeLens[]> {
    var erlangConfig = await connection.workspace.getConfiguration("erlang");
    if (erlangConfig) {
        if (!erlangConfig.codeLensEnabled) {
            return [];
        }
    }
    var uri = codeLens.textDocument.uri;
    let res = await erlangLspConnection.getCodeLensInfo(uri);
    if (res) {      
        var Result = new Array<CodeLens>();
        res.codelens.forEach(ref => {
            if (ref.data.exported) {
                let exportedCodeLens = CodeLens.create(Range.create(ref.line, ref.character, ref.line, ref.character + ref.data.func_name.length), ref.data);
                exportedCodeLens.command = Command.create("exported", "");
                Result.push(exportedCodeLens);
            }
            if (!ref.data.exported || ref.data.count > 0) {
                let codeLens = CodeLens.create(Range.create(ref.line, ref.character, ref.line, ref.character + ref.data.func_name.length), ref.data);
                //codeLens.command = null; //set to null to invoke OnCodeLensResolve
                codeLens.command = ref.data.count == 0 ? Command.create("unused","") : 
                    Command.create(`${ref.data.count} private references`, "editor.action.findReferences", 
                                        URI.parse(res.uri), {lineNumber : ref.line+1, column:ref.character+1});
                Result.push(codeLens);  
            }
        });
        return Result;
    }
    return null;    
}

connection.onCodeLensResolve(async (codeLens : CodeLens) : Promise<CodeLens> => {   

    // let command = Command.create(`${codeLens.data.count} private references`, "erlang.showReferences", 
    //                       codeLens.data.uri, codeLens.range.start, []);
    codeLens.command = Command.create("onCodeLensResolve","");
    return codeLens;
});

connection.onCompletion(async (textDocumentPosition: TextDocumentPositionParams):Promise<CompletionItem[]> => {
    let document = documents.get(textDocumentPosition.textDocument.uri);
    if (document == null) {
        debugLog(`unable to get document '${textDocumentPosition.textDocument.uri}'`);
        return [];
    }
    let text = document.getText({
        start: Position.create(textDocumentPosition.position.line, 0),
        end: textDocumentPosition.position}
    );
    var moduleFunctionMatch = text.match(/[^a-zA-Z0-0_@]([a-z][a-zA-Z0-0_@]*):([a-z][a-zA-Z0-0_@]*)?$/);
    if (moduleFunctionMatch) {
        var prefix = moduleFunctionMatch[2] ? moduleFunctionMatch[2] : '';
        debugLog('onCompletion, module=' + moduleFunctionMatch[1] + ' function=' + prefix);
        return await completeModuleFunction(moduleFunctionMatch[1], prefix);
    }
    var recordMatch = text.match(/#([a-z][a-zA-Z0-0_@]*)?$/);
    if (recordMatch) {
        var prefix = recordMatch[1] ? recordMatch[1] : '';
        debugLog('onCompletion, record=' + prefix);
        return await completeRecord(document.uri, prefix);
    }
    var fieldMatch = text.match(/#([a-z][a-zA-Z0-0_@]*)\.([a-z][a-zA-Z0-0_@]*)?$/);
    if (fieldMatch) {
        var prefix = fieldMatch[2] ? fieldMatch[2] : '';
        debugLog('onCompletion, record=' + fieldMatch[1] + ' field=' + prefix);
        return await completeField(document.uri, fieldMatch[1], prefix);
    }
    var variableMatch = text.match(/[^a-zA-Z0-0_@]([A-Z][a-zA-Z0-0_@]*)$/);
    if (variableMatch) {
        var prefix = variableMatch[1] ? variableMatch[1] : '';
        debugLog('onCompletion, variable=' + prefix);
        return await completeVariable(document.uri, textDocumentPosition.position.line, prefix);
    }
    return [];
});

async function completeModuleFunction(moduleName: string, prefix: string): Promise<CompletionItem[]> {
    let items = await erlangLspConnection.completeModuleFunction(moduleName, prefix);
    let completionItems: CompletionItem[] = items.map(item => {
        return {
            label: item,
            kind: CompletionItemKind.Function
        };
    });
    if (completionItems.length > 0) {
        let helpPage = await getModuleHelpPage(moduleName);
        if (helpPage.length > 0) {
            completionItems = completionItems.map(function (item: CompletionItem): CompletionItem {
                item.documentation = markdown(extractHelpForFunction(item.label, helpPage));
                return item;
            });
        }
    }
    return completionItems;
}

async function completeRecord(uri: string, prefix: string): Promise<CompletionItem[]> {
    let items = await erlangLspConnection.completeRecord(uri, prefix);
    return items.map(item => {
        return {
            label: item,
            kind: CompletionItemKind.Struct
        };
    });
}

async function completeField(uri: string, record: string, prefix: string): Promise<CompletionItem[]> {
    let items = await erlangLspConnection.completeField(uri, record, prefix);
    return items.map(item => {
        return {
            label: item,
            kind: CompletionItemKind.Field
        };
    });
}

async function completeVariable(uri: string, line: number, prefix: string): Promise<CompletionItem[]> {
    let items = await erlangLspConnection.completeVariable(uri, line, prefix);
    return items.map(item => {
        return {
            label: item,
            kind: CompletionItemKind.Variable
        };
    });
}

function debugLog(msg : string) : void {
    if (true /*traceEnabled*/) {
        connection.console.log(msg);
    }
}

function onValidatedDocument(parsingResult : ParsingResult, textDocument : TextDocument) : void {
            debugLog("onValidatedDocument: " + textDocument.uri);
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
                    range: Range.create(error.info.line-1, error.info.character-1, error.info.line-1, 255),
                    message: error.info.message,
                    source: 'erl'
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
