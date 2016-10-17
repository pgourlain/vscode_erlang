import {ChildProcess, spawn} from 'child_process';
import { EventEmitter } from 'events';
import { ErlGenericShell } from './GenericShell';

//inspired from https://github.com/WebFreak001/code-debug/blob/master/src/backend/mi2/mi2.ts for inspiration of an EventEmitter 
const nonOutput = /^(?:\d*|undefined)[\*\+\=]|[\~\@\&\^]/;


export interface IErlangShellOutput1 {
    show() : void;
    appendLine(value: string): void;
    append(value: string): void;
}

export class ErlangShellForDebugging extends ErlGenericShell {
    constructor(whichOutput : IErlangShellOutput1) {
        super(whichOutput);
    }    

    public Start(startDir : string, listen_port: number, bridgePath: string, args: string) : Promise<number> {
        var processArgs = [ "-pa", `'${bridgePath}'`,"-s", "int",  
            "-vscode_port", listen_port.toString(),
            "-s", "vscode_connection", "start", listen_port.toString(),  
            args];
        var result = this.RunProcess("erl", startDir, processArgs);
        return result;
    }

    public Compile(startDir : string, args: string[]) : Promise<number> {
        //if erl is used, -compile must be used
        //var processArgs = ["-compile"].concat(args);
        var processArgs = [].concat(args);    
        var result = this.RunProcess("erlc", startDir, processArgs);
        return result;
    }
}

