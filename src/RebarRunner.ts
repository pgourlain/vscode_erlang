import * as vscode from 'vscode';
import * as path from 'path'
import * as child_process from 'child_process'

export class RebarRunner {
	public runScript(dirName: string, commands: string[]): void {
		var rebarFileName = path.join(dirName, 'rebar');
		let args = commands;
		let rebar = child_process.spawn(rebarFileName, args, { cwd: dirName, stdio:'pipe' });
		var outputChannel = vscode.window.createOutputChannel('rebar');
		outputChannel.show();
		outputChannel.appendLine('starting rebar '+ commands + ' ...');

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
