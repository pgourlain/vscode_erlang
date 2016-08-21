import {ChildProcess, spawn} from 'child_process';
import { EventEmitter } from 'events';
import { ErlGenericShell } from './GenericShell';

//inspired from https://github.com/WebFreak001/code-debug/blob/master/src/backend/mi2/mi2.ts for inspiration of an EventEmitter 
const nonOutput = /^(?:\d*|undefined)[\*\+\=]|[\~\@\&\^]/;


export interface IErlangShellOutput1 {
    show() : void;
    appendLine(value: string): void;
}

export class ErlangShellForDebugging extends ErlGenericShell {
    constructor(whichOutput : IErlangShellOutput1) {
        super(whichOutput);
    }    

    public Start(startDir : string, args: string) : Promise<number> {
        //TODO: assumes that erlang debugger wrapper is copied and compiled into ebin directory
        //TODO: add int:start() ... int:subscribe .. and so on
        var processArgs = [args];
        return this.RunProcess("erl", startDir, processArgs);
        /*
        return new Promise<number>((a, r) => {
            a(0);
        });
        */
    }
}

