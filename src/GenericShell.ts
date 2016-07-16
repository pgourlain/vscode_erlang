import * as vscode from 'vscode';
import * as child_process from 'child_process'

var erlangOutputChannel : vscode.OutputChannel;

export class ErlGenericShell {
    protected erlangShell : child_process.ChildProcess;
    
    protected RunProcess(processName, startDir : string, args: string[]) : Thenable<number> {

        return new Promise<number>((resolve, reject) => {
            this.erlangShell = child_process.spawn(processName, args, { cwd: startDir, stdio:'pipe'});
            var channel = ErlGenericShell.ErlangOutput;
            channel.show();
            channel.appendLine('starting '+processName + '...' + args);
            this.erlangShell.stdout.on('data', buffer => {
                channel.appendLine(buffer.toString());
            });
            this.erlangShell.stderr.on('data', buffer => {
                channel.appendLine(buffer.toString());
            });

            this.erlangShell.on('close', (exitCode) => {	
                channel.appendLine(processName + ' exit code:'+exitCode);
                if (exitCode == 0) {
                    resolve(0);
                } else {
                    reject(exitCode);
                }
            });

        });
    }
    
    public Send(what : string) {
        erlangOutputChannel.appendLine(what);
        this.erlangShell.stdin.write(what);
        this.erlangShell.stdin.end();
    }    
    
    public Kill() {
        if (this.erlangShell) {
            this.erlangShell.kill();
        }
    }

    public static get ErlangOutput() : vscode.OutputChannel {
        if (!erlangOutputChannel) {
            erlangOutputChannel = vscode.window.createOutputChannel('erlang'); 
        }
        return erlangOutputChannel;
    }
}