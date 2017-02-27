import { ChildProcess, spawn } from 'child_process';
import { EventEmitter } from 'events';
import { ErlGenericShell } from './GenericShell';
import { DebugProtocol } from 'vscode-debugprotocol';
import * as path from 'path';
import * as os from 'os';

//inspired from https://github.com/WebFreak001/code-debug/blob/master/src/backend/mi2/mi2.ts for inspiration of an EventEmitter 
const nonOutput = /^(?:\d*|undefined)[\*\+\=]|[\~\@\&\^]/;


export interface IErlangShellOutputForDebugging {
    show(): void;
    appendLine(value: string): void;
    append(value: string): void;
    debug(value: string): void;
    error(value: string): void;
}

export class ErlangShellForDebugging extends ErlGenericShell {
    breakPoints: DebugProtocol.Breakpoint[];
    started : boolean;
    constructor(whichOutput: IErlangShellOutputForDebugging) {
        super(whichOutput);
        this.breakPoints = [];
    }

    public Start(erlPath:string, startDir: string, listen_port: number, bridgePath: string, args: string): Promise<boolean> {
        var debugStartArgs = ["-pa", `"${bridgePath}"`, "-s", "int",
            "-vscode_port", listen_port.toString(),
            "-s", "vscode_connection", "start", listen_port.toString()];
        var breakPointsArgs = this.breakpoints_as_startarguments();
        var processArgs = debugStartArgs.concat(breakPointsArgs).concat([args]);
        this.started = true;
        var result = this.LaunchProcess(erlPath, startDir, processArgs);
        return result;
    }

    private uniqueBy<T>(arr: Array<T>, keySelector: (v: T)=> any): Array<T> {
        var unique = {};
        var distinct:Array<T> = [];
        arr.forEach(function (x) {
            var key = keySelector(x);
            if (!unique[key]) {
                distinct.push(x);
                unique[key] = true;
            }
        });
        return distinct;
    }

    private formatPath(filePath : string) {
        if (os.platform() == 'win32') {
            if (filePath == undefined) {
                return filePath;
            }
            filePath = filePath.split("\\").join("\\\\");
            return filePath;
        }
        return filePath;
    }

    private breakpoints_as_startarguments(): string[] {
        //next step, to avoid reach the command line limits(character length), it's to store all breakpoints in a specific file, then bridge read it....
        var result: string[] = [];
        if (this.breakPoints) {
            result.push("-eval");
            var evalString = "\"int:start()";
            //first interpret source
            var bps = this.uniqueBy(this.breakPoints, bp => bp.source.path);
            bps.forEach(bp => {
                evalString += ",int:ni(\\\"" + this.formatPath(bp.source.path) + "\\\")";                
            });
            //then set break
            this.breakPoints.forEach(bp => {
                var moduleName = path.basename(bp.source.name, ".erl");
                evalString += `, int:break(${moduleName}, ${bp.line})`;
            });
            evalString += "\"";
            result.push(evalString);
        }
        return result;
    }

    /** compile specific files */
    public Compile(startDir: string, args: string[]): Promise<number> {
        //if erl is used, -compile must be used
        //var processArgs = ["-compile"].concat(args);
        var processArgs = [].concat(args);
        var result = this.RunProcess("erlc", startDir, processArgs);
        return result;
    }

    public setBreakPointsRequest(bps: DebugProtocol.Breakpoint[]): void {
        if (!this.started) {
            this.breakPoints = this.breakPoints.concat(bps);
        }
    }
}

