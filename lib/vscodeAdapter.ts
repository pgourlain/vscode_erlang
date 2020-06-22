import * as vscode from 'vscode';
import { ILogOutput } from './GenericShell';

var erlangOutputChannel : vscode.OutputChannel;


export function ErlangOutput() : vscode.OutputChannel {
        if (!erlangOutputChannel) {
            erlangOutputChannel = vscode.window.createOutputChannel('erlang'); 
        }
        return erlangOutputChannel;
    }

export function ErlangOutputAdapter(outputChannel?: vscode.OutputChannel) : ILogOutput {
    return new ErlangWrapperOutput(outputChannel || ErlangOutput());
}


class ErlangWrapperOutput implements ILogOutput {

    constructor (private channel : vscode.OutputChannel) {

    }
    public show() : void {
        this.channel.show();
    }
    
    public appendLine(value: string): void {
        this.channel.appendLine(value);
    }

    public debug(msg : string) : void {
        this.channel.appendLine("debug:" + msg);
    }


}
