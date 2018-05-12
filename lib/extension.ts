// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as path from 'path';
import { 
	workspace as Workspace, window as Window, ExtensionContext, TextDocument, OutputChannel, WorkspaceFolder, Uri, debug
} from 'vscode'; 

import * as Adapter from './vscodeAdapter';
import * as Rebar from './RebarRunner';
import * as Eunit from './eunitRunner';
import { ErlangDebugConfigurationProvider } from './ErlangConfigurationProvider';
import * as erlangConnection from './erlangConnection';
import * as Utils from './utils';

import * as LspClient from './lsp/lspclientextension';

var myoutputChannel : OutputChannel;
var myConsole : OutputChannel;
// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: ExtensionContext) {
	erlangConnection.setExtensionPath(context.extensionPath);
	
	myoutputChannel = Adapter.ErlangOutput();
	myConsole = Utils.pgoConsole();
	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	console.log('Congratulations, your extension "erlang" is now active!'); 
	myConsole.appendLine("erlang extension is active");

	//configuration of erlang language -> documentation : https://code.visualstudio.com/Docs/extensionAPI/vscode-api#LanguageConfiguration
	var disposables=[];
	// The command has been defined in the package.json file
	// Now provide the implementation of the command with  registerCommand
	// The commandId parameter must match the command field in package.json
	//disposables.push(vscode.commands.registerCommand('extension.rebarBuild', () => { runRebarCommand(['compile']);}));

	var rebar = new Rebar.RebarRunner();
	rebar.activate(context.subscriptions);

	var eunit = new Eunit.EunitRunner();
	eunit.activate(context);

	disposables.push(debug.registerDebugConfigurationProvider("erlang", new ErlangDebugConfigurationProvider()));

	disposables.forEach((disposable => context.subscriptions.push(disposable)));
	LspClient.activate(context);
}

export function deactivate(): Thenable<void> {

	return LspClient.deactivate();
}
