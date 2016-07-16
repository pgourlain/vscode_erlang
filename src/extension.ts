// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as fs from 'fs'
import * as rebar from './RebarRunner';
import * as erlang from './ErlangShell';
import * as path from 'path';
import * as eunitrunner from './eunitRunner';

var myoutputChannel : vscode.OutputChannel;
var myConsole : vscode.OutputChannel;
// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {

	myoutputChannel = erlang.ErlangShell.ErlangOutput;
	myConsole = vscode.window.createOutputChannel('pgoconsole');
	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	console.log('Congratulations, your extension "erlang" is now active!'); 
	//configuration of erlang language -> documentation : https://code.visualstudio.com/Docs/extensionAPI/vscode-api#LanguageConfiguration
	var disposables=[];
	// The command has been defined in the package.json file
	// Now provide the implementation of the command with  registerCommand
	// The commandId parameter must match the command field in package.json
	disposables.push(vscode.commands.registerCommand('extension.rebarBuild', () => { runRebarCommand(['get-deps', 'compile']);}));
	disposables.push(vscode.commands.registerCommand('extension.rebarGetDeps', () => { runRebarCommand(['get-deps']);}));
	disposables.push(vscode.commands.registerCommand('extension.rebarUpdateDeps', () => { runRebarCommand(['update-deps']);}));
	disposables.push(vscode.commands.registerCommand('extension.erleunit', () => { eunitrunner.runEUnitCommand()}));
	disposables.push(vscode.commands.registerCommand('extension.rebareunit', () => { runRebarCommand(['eunit'])}));
	disposables.forEach((disposable => context.subscriptions.push(disposable)));
	eunitrunner.setExtensionPath(context.extensionPath);
}

function runRebarCommand(command: string[]) {
	var runner = new rebar.RebarRunner();
	try {
		runner.runScript(vscode.workspace.rootPath, command);	
	} catch (e) {
		vscode.window.showErrorMessage('Couldn\'t execute rebar.\n' + e);
	}	
}