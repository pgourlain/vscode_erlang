// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as fs from 'fs'
import * as path from 'path'
import * as child_process from 'child_process'

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {

	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	console.log('Congratulations, your extension "erlang" is now active!'); 

	var disposables=[];
	// The command has been defined in the package.json file
	// Now provide the implementation of the command with  registerCommand
	// The commandId parameter must match the command field in package.json
	disposables.push(vscode.commands.registerCommand('extension.rebarBuild', () => { runRebarCommand('compile');}));
	disposables.push(vscode.commands.registerCommand('extension.rebarGetDeps', () => { runRebarCommand('get-deps');}));
	disposables.push(vscode.commands.registerCommand('extension.rebarUpdateDeps', () => { runRebarCommand('update-deps');}));
	disposables.forEach((disposable => context.subscriptions.push(disposable)));
}


function runRebarCommand(command: string) {
	var runner = new RebarRunner();
	try {
		runner.runScript(vscode.workspace.rootPath, command);
	} catch (e) {
		vscode.window.showErrorMessage('Couldn\'t execute rebar.\n' + e);
	}	
}

class RebarRunner {
	public runScript(dirName: string, command: string): void {
		var rebarFileName = path.join(dirName, 'rebar');
		let args = [command];
		let rebar = child_process.spawn(rebarFileName, args, { cwd: dirName, stdio:'pipe' });
		var outputChannel = vscode.window.createOutputChannel('rebar');
		outputChannel.show();
		outputChannel.appendLine('starting rebar '+ command + ' ...');

		rebar.stdout.on('data', buffer => {
			//console.log(buffer.toString());
			outputChannel.appendLine(buffer.toString());
		});
		rebar.stderr.on('data', buffer => {
			//console.log(buffer.toString());
			outputChannel.appendLine(buffer.toString());
		});

		rebar.on('close', (exitCode) => {	
			outputChannel.appendLine('rebar exit code:'+exitCode);
		});
	}

}
