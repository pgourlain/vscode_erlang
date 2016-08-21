import * as vscode from 'vscode';
import * as child_process from 'child_process';
import * as shell from './GenericShell';
import * as adapt from './vscodeAdapter';


export class ErlangShell extends shell.ErlGenericShell {
    constructor(){
        super(adapt.ErlangOutputAdapter());//ErlangShell.ErlangOutput);
    }
    public Start(startDir : string, args: string[]) : Thenable<number> {
        return this.RunProcess("erl", startDir, args);
    }    
}

export class ErlangCompilerShell extends shell.ErlGenericShell {
    constructor(){
        super(adapt.ErlangOutputAdapter());//ErlangShell.ErlangOutput);
    }
    
    public Start(startDir : string, args: string[]) : Thenable<number> {
        return this.RunProcess("erlc", startDir, args);
    }    
}

export class ErlangShellForDebugging extends shell.ErlGenericShell {

    constructor () {
        super(null);
    }
    public Start(startDir : string, args: string) : Thenable<number> {
        //TODO: assumes that erlang debugger wrapper is copied and compiled into ebin directory
        //TODO: add int:start() ... int:subscribe .. and so on
        var processArgs = [args];
        return this.RunProcess("erl", startDir, processArgs);
    }
/*
	protected logNoNewLine(type: string, msg: string) : void {
    
    }

	protected log(type: string, msg: string) : void {
        
    }
    */
}