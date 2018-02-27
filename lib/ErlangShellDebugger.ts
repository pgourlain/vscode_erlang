import { ChildProcess, spawn } from 'child_process';
import { EventEmitter } from 'events';
import { ErlGenericShell } from './GenericShell';
import { DebugProtocol } from 'vscode-debugprotocol';
import * as path from 'path';
import * as os from 'os';
import * as fs from 'fs';

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
    argsFileName: string;
    constructor(whichOutput: IErlangShellOutputForDebugging) {
        super(whichOutput);
        this.breakPoints = [];
    }

    public Start(erlPath:string, startDir: string, listen_port: number, bridgePath: string, args: string, noDebug: boolean): Promise<boolean> {
        var randomSuffix:string = Math.floor(Math.random() * 10000000).toString();
        this.argsFileName = path.join(os.tmpdir(), path.basename(startDir) + '_' + randomSuffix);
        var debugStartArgs = ["-pa", `"${bridgePath}"`, "-s", "int",
            "-vscode_port", listen_port.toString(),
            "-s", "vscode_connection", "start", listen_port.toString()];
        var breakPointsAndModulesArgs = this.breakpointsAndModules(startDir, noDebug);
        var processArgs = debugStartArgs.concat(breakPointsAndModulesArgs).concat([args]);
        this.started = true;
        var result = this.LaunchProcess(erlPath, startDir, processArgs);
        return result;
    }

    public CleanupAfterStart() {
        if (this.argsFileName && fs.existsSync(this.argsFileName)) {
            fs.unlinkSync(this.argsFileName);
        }
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

    private findErlFiles(dir: string, fileList: string[] = []) {
        fs.readdirSync(dir).forEach(file => {
            if (file == '_build')
                return;
            const filePath = path.join(dir, file)
            if (fs.existsSync(filePath) && fs.statSync(filePath).isDirectory())
                this.findErlFiles(filePath, fileList);
            else if (path.extname(file) === '.erl')
                fileList.push(filePath);
        });
        return fileList;
    }

    private breakpointsAndModules(startDir: string, noDebug: boolean): string[] {
        var result: string[] = [];
        if (this.breakPoints) {
            var evalString = "-eval 'int:start()";
            var modulesWithoutBp: { [sourcePath: string]: boolean} = {};
            this.findErlFiles(startDir).forEach(fileName => {
                modulesWithoutBp[fileName] = true;
            });
            //first interpret source
            if (!noDebug) {
                var bps = this.uniqueBy(this.breakPoints, bp => bp.source.path);
                bps.forEach(bp => {
                    evalString += ",int:ni(\\\"" + this.formatPath(bp.source.path) + "\\\")";
                    delete modulesWithoutBp[bp.source.path];
                });
            }
            for (var fileName in modulesWithoutBp) {
                evalString += ",int:ni(\\\"" + this.formatPath(fileName) + "\\\")";
            }
            //then set break
            if (!noDebug) {
                this.breakPoints.forEach(bp => {
                    var moduleName = path.basename(bp.source.name, ".erl");
                    evalString += `, int:break(${moduleName}, ${bp.line})`;
                });
            }
            evalString += "'";
            fs.writeFileSync(this.argsFileName, evalString);
            result.push("-args_file");
            result.push("\"" + this.argsFileName + "\"");
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

