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
		try {
			const buildArgs = getElangConfigConfiguration().rebarBuildArgs;
			this.runScript(buildArgs).then(data => {
				this.diagnosticCollection.clear();
				this.parseCompilationResults(data);
			});
		} catch (e) {
			vscode.window.showErrorMessage('Couldn\'t execute rebar.\n' + e);
		}
	}

	private parseForDiag(data: string, diagnostics: { [id: string]: vscode.Diagnostic[]; }, regex: RegExp, severity: vscode.DiagnosticSeverity): string {
		//parse data while regex return matches
		do {
			var m = regex.exec(data);
			if (m) {
				var fileName = m[1];
				var peace = data.substring(m.index, regex.lastIndex);
				data = data.replace(peace, "");

				let message = m[m.length - 1];
				let range = new vscode.Range(Number(m[2]) - 1, 0, Number(m[2]) - 1, peace.length - 1);
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
		keys.forEach(element => {
			var fileUri = vscode.Uri.file(path.join(vscode.workspace.rootPath, element));
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

    private runDialyzer(): void {
		try {
			this.runScript(["dialyzer"]).then(data => {
                this.diagnosticCollection.clear();
                var lines = data.split("\n");
                var currentFile = null;
        		var lineAndMessage = new RegExp("^ +([0-9]+): *(.+)$");
                var diagnostics: { [id: string]: vscode.Diagnostic[]; } = {};
                for (var i = 0; i < lines.length; ++i) {
                    if (lines[i]) {
                        var match = lineAndMessage.exec(lines[i]);
                        if (match && currentFile) {
                            if (!diagnostics[currentFile])
                                diagnostics[currentFile] = [];
                            var range = new vscode.Range(Number(match[1]) - 1, 0, Number(match[1]) - 1, 255);
                            diagnostics[currentFile].push(new vscode.Diagnostic(range , match[2], vscode.DiagnosticSeverity.Information));
                        }
                        else {
                            var filepath = path.join(vscode.workspace.rootPath, lines[i]);
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
            }, reject => {});
		} catch (e) {
			vscode.window.showErrorMessage('Couldn\'t execute rebar.\n' + e);
		}
	}

	/**
	 * Get search paths for the rebar executable in order of priority.
	 *
	 * @returns Directories to search for the rebar executable
	 */
	private getRebarSearchPaths(): string[] {
		
		const cfgRebarPath = getElangConfigConfiguration().rebarPath,
			rebarSearchPaths = [];
		if (cfgRebarPath) {
			rebarSearchPaths.push(cfgRebarPath);
		}
		if (cfgRebarPath !== vscode.workspace.rootPath) {
			rebarSearchPaths.push(vscode.workspace.rootPath);
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
		const { output } = await new RebarShell(this.getRebarSearchPaths(), this.extensionPath, ErlangOutputAdapter(RebarRunner.RebarOutput))
			.runScript(vscode.workspace.rootPath, commands);
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
			rebarOutputChannel = vscode.window.createOutputChannel('rebar');
		}
		return rebarOutputChannel;
	}
}
