import * as vscode from 'vscode';
import * as child_process from 'child_process';
import * as shell from './GenericShell';


export class ErlangShell extends shell.ErlGenericShell {
    
    public Start(startDir : string, args: string[]) : Thenable<number> {
        return this.RunProcess("erl", startDir, args);
    }    
}

export class ErlangCompilerShell extends shell.ErlGenericShell {
    
    public Start(startDir : string, args: string[]) : Thenable<number> {
        return this.RunProcess("erlc", startDir, args);
    }    
}