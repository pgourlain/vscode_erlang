import * as vscode from 'vscode';
import {IErlangShellOutput} from './GenericShell';

var erlangOutputChannel : vscode.OutputChannel;


export function ErlangOutput() : vscode.OutputChannel {
        if (!erlangOutputChannel) {
            erlangOutputChannel = vscode.window.createOutputChannel('erlang'); 
        }
        return erlangOutputChannel;
    }

export function ErlangOutputAdapter() : IErlangShellOutput {
    return new ErlangWrapperOutput(ErlangOutput());
}


class ErlangWrapperOutput implements IErlangShellOutput {
    
    constructor (private channel : vscode.OutputChannel) {

    }
    public show() : void {
        this.channel.show();
    }
    
    public appendLine(value: string): void {
        this.channel.appendLine(value);
    }


}