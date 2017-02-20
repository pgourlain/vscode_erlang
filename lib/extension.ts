// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
//import * as fs from 'fs'
//import * as path from 'path';

import * as Adapter from './vscodeAdapter';
import * as Rebar from './RebarRunner';
import * as Eunit from './eunitRunner';
import * as Utils from './utils';
//import * as ErlShell from './ErlangShell';
//import * as ErlDebug from './erlangDebug';

var myoutputChannel : vscode.OutputChannel;
var myConsole : vscode.OutputChannel;
// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {

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

	disposables.forEach((disposable => context.subscriptions.push(disposable)));
}
