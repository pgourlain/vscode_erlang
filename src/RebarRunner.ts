import * as vscode from 'vscode';
import * as path from 'path'
import * as child_process from 'child_process'
import * as utils from './utils'

var rebarOutputChannel: vscode.OutputChannel;

/*
Rebar Compile
see : https://github.com/hoovercj/vscode-extension-tutorial

*/

export class RebarRunner implements vscode.Disposable {

	diagnosticCollection: vscode.DiagnosticCollection;
	private static commandId: string = 'extension.rebarBuild';
	private compileCommand: vscode.Disposable;
	private getDepsCommand: vscode.Disposable;
	private updateDepsCommand: vscode.Disposable;
	private eunitCommand: vscode.Disposable;

	public activate(subscriptions: vscode.Disposable[]) {
		this.compileCommand = vscode.commands.registerCommand('extension.rebarBuild', () => {this.runRebarCompile();});
		this.getDepsCommand = vscode.commands.registerCommand('extension.rebarGetDeps', () => { this.runRebarCommand(['get-deps']);});
		this.updateDepsCommand = vscode.commands.registerCommand('extension.rebarUpdateDeps', () => { this.runRebarCommand(['update-deps']);});
		this.eunitCommand = vscode.commands.registerCommand('extension.rebareunit', () => { this.runRebarCommand(['eunit'])});
		vscode.workspace.onDidCloseTextDocument(this.onCloseDocument, null, subscriptions);
		//vscode.workspace.onDidOpenTextDocument(this.onOpenDocument, null, subscriptions);
		this.diagnosticCollection = vscode.languages.createDiagnosticCollection("erlang");
		subscriptions.push(this);
	}


	public dispose(): void {
		this.diagnosticCollection.clear();
		this.diagnosticCollection.dispose();
		this.compileCommand.dispose();
		this.getDepsCommand.dispose();
		this.updateDepsCommand.dispose();
		this.eunitCommand.dispose();
	}

	private runRebarCompile(){
		try {

			this.runScript(vscode.workspace.rootPath, ['compile']).then(data => {
				this.diagnosticCollection.clear();
				this.parseCompilationResults(data);
			});
		} catch (e) {
			vscode.window.showErrorMessage('Couldn\'t execute rebar.\n' + e);
		}
	}

	private parseForDiag(data : string, diagnostics: { [id: string] : vscode.Diagnostic[]; }, regex : RegExp, severity: vscode.DiagnosticSeverity) : string
	{
		//parse data while regex return matches
		do {
			var m = regex.exec(data);
			if (m) {
				var fileName = m[1];				
				var peace = data.substring(m.index, regex.lastIndex);
				data = data.replace(peace, "");				

				let message = m[m.length-1];
				let range = new vscode.Range(Number(m[2])-1, 0, Number(m[2])-1, peace.length-1);
				let diagnostic = new vscode.Diagnostic(range, message, severity);
				regex.lastIndex = 0;
				if (!diagnostics[fileName]) {
					diagnostics[fileName] = [];
				}
				diagnostics[fileName].push(diagnostic);
			}	
		}
		while (m != null);
		return data;
	}

	private parseCompilationResults(data : string) : void {
		//how to test regexp : https://regex101.com/#javascript
		var diagnostics: { [id: string] : vscode.Diagnostic[]; } = {};
		//parsing warning at first
		var warnings = new RegExp("^(.*):(\\d+):(.*)Warning:(.*)$", "gmi");
		data = this.parseForDiag(data, diagnostics, warnings, vscode.DiagnosticSeverity.Warning);
		//then parse errors (because regex to detect errors include warnings too)
		var errors = new RegExp("^(.*):(\\d+):(.*)$", "gmi");
		data = this.parseForDiag(data, diagnostics, errors, vscode.DiagnosticSeverity.Error);
		var keys = utils.keysFromDictionary(diagnostics);
		keys.forEach(element => {
			var fileUri = vscode.Uri.file(path.join(vscode.workspace.rootPath, element));
			var diags = diagnostics[element];
			this.diagnosticCollection.set(fileUri, diags);
		});
	}

	private runRebarCommand(command: string[]): void {
		try {
			this.runScript(vscode.workspace.rootPath, command).then(data => {

			}, reject => {
			});;
		} catch (e) {
			vscode.window.showErrorMessage('Couldn\'t execute rebar.\n' + e);
		}
	}

	public runScript(dirName: string, commands: string[]): Thenable<string> {
		return new Promise<string>((a, r)=>{
			var rebarFileName = path.join(dirName, 'rebar');
			let args = commands;
			var outputChannel = RebarRunner.RebarOutput;
			var output : string = "";
			outputChannel.show();
			if (process.platform == 'win32') {
				args = [rebarFileName].concat(args);
				rebarFileName = 'escript.exe';
			}
			let rebar = child_process.spawn(rebarFileName, args, { cwd: dirName, stdio: 'pipe' });
			rebar.on('error', error => {
				outputChannel.appendLine(error);
				if (process.platform == 'win32') {
					outputChannel.appendLine("ensure 'escript.exe' is in your path.");
				}
			});
			outputChannel.appendLine('starting rebar ' + commands + ' ...');

			rebar.stdout.on('data', buffer => {
				var bufferAsString = buffer.toString();
				output += bufferAsString;
				outputChannel.appendLine(bufferAsString);
			});
			rebar.stderr.on('data', buffer => {
				outputChannel.appendLine(buffer.toString());
			});

			rebar.on('close', (exitCode) => {
				outputChannel.appendLine('rebar exit code:' + exitCode);
				a(output);
			});

		});
	}

	private onCloseDocument(doc: vscode.TextDocument): any {
		RebarRunner.RebarOutput.appendLine("doc close : " + doc.uri.toString());
		if (this.diagnosticCollection) {
			this.diagnosticCollection.delete(doc.uri);
		}
	}

	private onOpenDocument(doc : vscode.TextDocument) : any {
		RebarRunner.RebarOutput.appendLine("doc open : " + doc.uri.toString());
	}

    public static get RebarOutput(): vscode.OutputChannel {
        if (!rebarOutputChannel) {
            rebarOutputChannel = vscode.window.createOutputChannel('rebar');
        }
        return rebarOutputChannel;
    }
}
