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
	//disposables.push(vscode.commands.registerCommand('extension.rebarBuild', () => { runRebarCommand(['compile']);}));
	var runner = new rebar.RebarRunner();
	runner.activate(context.subscriptions);

	var erunner = new eunitrunner.EunitRunner();
	erunner.activate(context);

	//disposables.push(vscode.commands.registerCommand('extension.erleunit', () => { eunitrunner.runEUnitCommand()}));
	disposables.forEach((disposable => context.subscriptions.push(disposable)));

	//eunitrunner.setExtensionPath(context.extensionPath);
}
