import * as vscode from 'vscode';
import * as path from 'path'
import * as child_process from 'child_process'

var rebarOutputChannel : vscode.OutputChannel;

export class RebarRunner {
	public runScript(dirName: string, commands: string[]): void {
		var rebarFileName = path.join(dirName, 'rebar');
		let args = commands;
		var outputChannel = RebarRunner.RebarOutput;
		outputChannel.show();
		if (process.platform == 'win32') {
			args = [rebarFileName].concat(args);
			rebarFileName = 'escript.exe';
		}
		let rebar = child_process.spawn(rebarFileName, args, { cwd: dirName, stdio:'pipe' });
		rebar.on('error', error =>{
			outputChannel.appendLine(error);			
			if (process.platform == 'win32') {
				outputChannel.appendLine("ensure 'escript.exe' is in your path.");
			}			
		});
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

    public static get RebarOutput() : vscode.OutputChannel {
        if (!rebarOutputChannel) {
            rebarOutputChannel = vscode.window.createOutputChannel('rebar'); 
        }
        return rebarOutputChannel;
    }}
