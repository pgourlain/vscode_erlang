import * as vscode from 'vscode';
import * as path from 'path'
import * as fs from 'fs'
import * as child_process from 'child_process'
import RebarShell from './RebarShell';
import * as utils from './utils'
import { ErlangOutputAdapter } from './vscodeAdapter';
import { getElangConfigConfiguration } from './ErlangConfigurationProvider';

var rebarOutputChannel: vscode.OutputChannel;

/*
Rebar Compile
see : https://github.com/hoovercj/vscode-extension-tutorial

*/

export class RebarRunner implements vscode.Disposable {

	private extensionPath: string;
	diagnosticCollection: vscode.DiagnosticCollection;
	private static commandId: string = 'extension.rebarBuild';
	private compileCommand: vscode.Disposable;
	private getDepsCommand: vscode.Disposable;
	private updateDepsCommand: vscode.Disposable;
	private eunitCommand: vscode.Disposable;
	private dialyzerCommand: vscode.Disposable;

	public activate(context: vscode.ExtensionContext) {
		const subscriptions = context.subscriptions;
		this.extensionPath = context.extensionPath;

		this.compileCommand = vscode.commands.registerCommand('extension.rebarBuild', () => { this.runRebarCompile(); });
		this.getDepsCommand = vscode.commands.registerCommand('extension.rebarGetDeps', () => { this.runRebarCommand(['get-deps']); });
		this.updateDepsCommand = vscode.commands.registerCommand('extension.rebarUpdateDeps', () => { this.runRebarCommand(['update-deps']); });
		this.eunitCommand = vscode.commands.registerCommand('extension.rebareunit', () => { this.runRebarCommand(['eunit']) });
		this.dialyzerCommand = vscode.commands.registerCommand('extension.dialyzer', () => { this.runDialyzer() });
		vscode.workspace.onDidCloseTextDocument(this.onCloseDocument.bind(this), null, subscriptions);
		vscode.workspace.onDidOpenTextDocument(this.onOpenDocument.bind(this), null, subscriptions);
		this.diagnosticCollection = vscode.languages.createDiagnosticCollection("erlang");
		subscriptions.push(this);
	}


	public dispose(): void {
		this.diagnosticCollection.dispose();
		this.compileCommand.dispose();
		this.getDepsCommand.dispose();
		this.updateDepsCommand.dispose();
		this.eunitCommand.dispose();
		this.dialyzerCommand.dispose();
	}

	private runRebarCompile() {
		const statusBarMessage = vscode.window.setStatusBarMessage('$(loading~spin) Building');
		try {
			const buildArgs = getElangConfigConfiguration().rebarBuildArgs;
			this.runScript(buildArgs).then(data => {
				//RebarRunner.RebarOutput.appendLine(`buildArgs: ${buildArgs}`);
				this.diagnosticCollection.clear();
				this.parseCompilationResults(data);
				statusBarMessage.dispose();
			});
		} catch (e) {
			statusBarMessage.dispose();
			vscode.window.showErrorMessage('Couldn\'t execute rebar.\n' + e);
		}
	}

	private parseForDiag(data: string, diagnostics: { [id: string]: vscode.Diagnostic[]; }, regex: RegExp, severity: vscode.DiagnosticSeverity): string {
		//parse data while regex return matches
		do {
			var m = regex.exec(data);
			if (m) {
				var fileName = m[1];
				var line = Number(m[2]);
				var column = 0;
				var splittedFileName = fileName.split(':');
				if (splittedFileName.length == 2)
				{
					//filename contains ':number'
					column = line;
					fileName = splittedFileName[0];
					line = Number(splittedFileName[1]);
				}
				var peace = data.substring(m.index, regex.lastIndex);
				data = data.replace(peace, "");

				let message = m[m.length - 1];

				let range = new vscode.Range(line - 1, 0, line- 1, peace.length - 1);
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

	private parseCompilationResults(data: string): void {		
		//how to test regexp : https://regex101.com/#javascript
		var diagnostics: { [id: string]: vscode.Diagnostic[]; } = {};
		//parsing warning at first
		var warnings = new RegExp("^(.*):(\\d+):(.*)Warning:(.*)$", "gmi");
		data = this.parseForDiag(data, diagnostics, warnings, vscode.DiagnosticSeverity.Warning);
		//then parse errors (because regex to detect errors include warnings too)
		var errors = new RegExp("^(.*):(\\d+):(.*)$", "gmi");
		data = this.parseForDiag(data, diagnostics, errors, vscode.DiagnosticSeverity.Error);
		var keys = utils.keysFromDictionary(diagnostics);
		const rootPath = getElangConfigConfiguration().rootPath;
		keys.forEach(element => {
			var fileUri = vscode.Uri.file(path.join(rootPath, element));
			var diags = diagnostics[element];
			this.diagnosticCollection.set(fileUri, diags);
		});
	}

	private runRebarCommand(command: string[]): void {
		try {
			this.runScript(command).then(data => {
			}, reject => {});
		} catch (e) {
			vscode.window.showErrorMessage('Couldn\'t execute rebar.\n' + e);
		}
	}

	private lineAndMessage(input: string): [number, number, string] | null {
		var lineAndMessage1 = new RegExp("^ *Line +([0-9]+) +Column +([0-9]+): +(.+)$");
		var match1 = lineAndMessage1.exec(input);
		if (match1) {
			return [Number(match1[1]), Number(match1[2]), match1[3]];
		}
		else {
			var lineAndMessage2 = new RegExp("^ +([0-9]+): *(.+)$");
			var match2 = lineAndMessage2.exec(input);
			if (match2) {
				return [Number(match2[1]), 1, match2[2]];
			}
		}
		return null;
	}

    private runDialyzer(): void {
		try {
			const statusBarMessage = vscode.window.setStatusBarMessage('$(loading~spin) Running Dialyzer');
			this.runScript(["dialyzer"]).then(data => {
                this.diagnosticCollection.clear();
                var lines = data.split("\n");
                var currentFile = null;
                var diagnostics: { [id: string]: vscode.Diagnostic[]; } = {};
				const rootPath = getElangConfigConfiguration().rootPath;
                for (var i = 0; i < lines.length; ++i) {
                    if (lines[i]) {
                        var match = this.lineAndMessage(lines[i]);
                        if (match && currentFile) {
                            if (!diagnostics[currentFile])
                                diagnostics[currentFile] = [];
                            var range = new vscode.Range(match[0] - 1, match[1] - 1, match[0] - 1, 255);
                            diagnostics[currentFile].push(new vscode.Diagnostic(range , match[2], vscode.DiagnosticSeverity.Information));
                        }
                        else {
                            var filepath = path.join(rootPath, lines[i]);
                            if (fs.existsSync(filepath))
                                currentFile = filepath;
                        }
                    }
                    else
                        currentFile = null;
                }
                utils.keysFromDictionary(diagnostics).forEach(filepath => {
                    var fileUri = vscode.Uri.file(filepath);
                    var diags = diagnostics[filepath];
                    this.diagnosticCollection.set(fileUri, diags);
                });
                if (utils.keysFromDictionary(diagnostics).length > 0)
                    vscode.commands.executeCommand("workbench.action.problems.focus");
				else
					vscode.window.showInformationMessage('Dialyzer did not find any problems');
				statusBarMessage.dispose();
            }, reject => {});
		} catch (e) {
			vscode.window.showErrorMessage('Couldn\'t execute Dialyzer.\n' + e);
		}
	}

	/**
	 * Get search paths for the rebar executable in order of priority.
	 *
	 * @returns Directories to search for the rebar executable
	 */
	private getRebarSearchPaths(): string[] {
		
		const cfgRebarPath = getElangConfigConfiguration().rebarPath,
			rebarSearchPaths = [],
			rootPath = getElangConfigConfiguration().rootPath;
		if (cfgRebarPath) {
			rebarSearchPaths.push(cfgRebarPath);
		}
		if (cfgRebarPath !== rootPath) {
			rebarSearchPaths.push(rootPath);
		}
		return rebarSearchPaths;
	}

	/**
	 * Execute rebar on the workspace project with supplied arguments.
	 *
	 * @param commands - Arguments to rebar
	 * @returns Promise resolved or rejected when rebar exits
	 */
	public async runScript(commands: string[]): Promise<string> {
		const rootPath = getElangConfigConfiguration().rootPath;
		const { output } = await new RebarShell(this.getRebarSearchPaths(), this.extensionPath, ErlangOutputAdapter(RebarRunner.RebarOutput))
			.runScript(rootPath, commands);
		return output;
	}

	private onCloseDocument(doc: vscode.TextDocument): any {
		//RebarRunner.RebarOutput.appendLine("doc close : " + doc.uri.toString());
		if (this.diagnosticCollection) {
			this.diagnosticCollection.delete(doc.uri);
		}
	}

	private onOpenDocument(doc: vscode.TextDocument): any {
		//RebarRunner.RebarOutput.appendLine("doc open : " + doc.uri.toString());
	}

	public static get RebarOutput(): vscode.OutputChannel {
		if (!rebarOutputChannel) {
			rebarOutputChannel = vscode.window.createOutputChannel('rebar', 'erlang');
		}
		return rebarOutputChannel;
	}
}
