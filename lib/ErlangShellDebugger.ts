import { ChildProcess, spawn } from 'child_process';
import { EventEmitter } from 'events';
import { GenericShell, ILogOutput } from './GenericShell';
import { DebugProtocol } from 'vscode-debugprotocol';
import * as path from 'path';
import * as os from 'os';
import * as fs from 'fs';

//inspired from https://github.com/WebFreak001/code-debug/blob/master/src/backend/mi2/mi2.ts for inspiration of an EventEmitter 
const nonOutput = /^(?:\d*|undefined)[\*\+\=]|[\~\@\&\^]/;

export interface LaunchRequestArguments extends DebugProtocol.LaunchRequestArguments {
    cwd: string;
    erlpath: string;
    arguments: string;
    verbose: boolean;
    addEbinsToCodepath: boolean;
    erlangPath : string; // path of erlang if specified in configuration
}

export class FunctionBreakpoint implements DebugProtocol.Breakpoint {
    id: any;
    verified: boolean;
    name: string;
    moduleName: string;
    functionName: string;
    arity: number;
    constructor(i: string, n: string, mn:string, fn: string, a: number) {
        this.id = i;
        this.verified = false;
        this.name = n;
        this.moduleName = mn;
        this.functionName = fn;
        this.arity = a;
	}
}

// export interface IErlangShellOutputForDebugging {
//     show(): void;
//     appendLine(value: string): void;
//     append(value: string): void;
//     debug(value: string): void;
//     error(value: string): void;
// }

export class ErlangShellForDebugging extends GenericShell {
    breakPoints: DebugProtocol.Breakpoint[];
    functionBreakPoints: FunctionBreakpoint[];
    started : boolean;
    argsFileName: string;
    constructor(whichOutput: ILogOutput) {
        super(whichOutput);
        this.breakPoints = [];
        this.functionBreakPoints = [];
    }

    public Start(erlPath: string, startDir: string, listen_port: number, bridgePath: string, launchArguments: LaunchRequestArguments): Promise<boolean> {
        var randomSuffix:string = Math.floor(Math.random() * 10000000).toString();
        this.argsFileName = path.join(os.tmpdir(), path.basename(startDir) + '_' + randomSuffix);
        let argsPrecompiledFileName = this.argsFileName + ".erl";
        argsPrecompiledFileName = this.formatPath(argsPrecompiledFileName);
        var debugStartArgs = ["-noshell", "-pa", `"${bridgePath}"`, "-s", "int",
            "-vscode_port", listen_port.toString(),
            "-compiled_args_file", `"${argsPrecompiledFileName}"`,
            "-s", "vscode_connection", "start"];
        var argsFile = this.createArgsFilev1(startDir, launchArguments.noDebug, launchArguments.addEbinsToCodepath);
        var processArgs = debugStartArgs.concat(argsFile).concat([launchArguments.arguments]);
        this.started = true;
        var result = this.LaunchProcess(erlPath, startDir, processArgs, !launchArguments.verbose);
        return result;
    }

    public CleanupAfterStart() {
        if (this.argsFileName && fs.existsSync(this.argsFileName)) {
            fs.unlinkSync(this.argsFileName);
            fs.unlinkSync(this.argsFileName+".erl");
            var beamFile = this.argsFileName+".beam";
            if (fs.existsSync(beamFile)) {
                fs.unlinkSync(beamFile);
            }
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
            filePath = filePath.split("\\").join("/");
            return filePath;
        }
        return filePath;
    }

    private findEbinDirs(dir: string, dirList: string[] = []) {
        fs.readdirSync(dir).forEach(name => {
            const fullpath = path.join(dir, name)
            if (fs.existsSync(fullpath) && fs.statSync(fullpath).isDirectory()) {
                if (name === 'ebin')
                    dirList.push(fullpath);
                else
                    this.findEbinDirs(fullpath, dirList);
            }
        });
        return dirList;
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

    private createArgsFilev1(startDir: string, noDebug: boolean, addEbinsToCodepath: boolean): string[] {
        var result: string[] = [];
        if (this.breakPoints) {
            var argsFileContents = "";
            if (!noDebug) {
                let argsPrecompiledFileName = this.argsFileName+".erl";
                let argsModuleName = path.basename(this.argsFileName);
                let argsCompiledContents = `-module(${argsModuleName}).\r\n-export([configure/0]).\r\n\r\n`;
                argsCompiledContents += "configure() -> \r\n int:start()\r\n";

                argsFileContents += "-eval 'int:start()";
                var modulesWithoutBp: { [sourcePath: string]: boolean} = {};
                this.findErlFiles(startDir).forEach(fileName => {
                    modulesWithoutBp[fileName] = true;
                });

                //first interpret source
                var bps = this.uniqueBy(this.breakPoints, bp => bp.source.path);
                bps.forEach(bp => {
                    argsCompiledContents += ",int:ni(\"" + this.formatPath(bp.source.path) + "\")\r\n";
                    delete modulesWithoutBp[bp.source.path];
                });
                for (var fileName in modulesWithoutBp) {
                    argsCompiledContents += ",int:ni(\"" + this.formatPath(fileName) + "\")\r\n";
                }
                //then set break
                this.breakPoints.forEach(bp => {
                    var moduleName = path.basename(bp.source.name, ".erl");
                    argsCompiledContents += `,int:break(${moduleName}, ${bp.line})\r\n`;
                });
                this.functionBreakPoints.forEach(bp => {
                    argsCompiledContents += `,vscode_connection:set_breakpoint(${bp.moduleName}, {function, ${bp.functionName}, ${bp.arity}})\r\n`;
                });
                argsFileContents += "'";
                argsCompiledContents += ",ok.";
            
                fs.writeFileSync(argsPrecompiledFileName, argsCompiledContents);
            }
            if (addEbinsToCodepath) {
                this.findEbinDirs(path.join(startDir, "_build")).forEach(ebin => {
                    argsFileContents += " -pz \"" + this.formatPath(ebin) + "\"";
                });
            }
            fs.writeFileSync(this.argsFileName, argsFileContents);
            
            result.push("-args_file");
            result.push("\"" + this.argsFileName + "\"");
        }
        return result;
    }

    // old 
    private createArgsFilev0(startDir: string, noDebug: boolean, addEbinsToCodepath: boolean): string[] {
        var result: string[] = [];
        if (this.breakPoints) {
            var argsFileContents = "";
            if (!noDebug) {
                argsFileContents += "-eval 'int:start()";
                var modulesWithoutBp: { [sourcePath: string]: boolean} = {};
                this.findErlFiles(startDir).forEach(fileName => {
                    modulesWithoutBp[fileName] = true;
                });

                //first interpret source
                var bps = this.uniqueBy(this.breakPoints, bp => bp.source.path);
                bps.forEach(bp => {
                    argsFileContents += ",int:ni(\\\"" + this.formatPath(bp.source.path) + "\\\")";
                    delete modulesWithoutBp[bp.source.path];
                });
                for (var fileName in modulesWithoutBp) {
                    argsFileContents += ",int:ni(\\\"" + this.formatPath(fileName) + "\\\")";
                }
                //then set break
                this.breakPoints.forEach(bp => {
                    var moduleName = path.basename(bp.source.name, ".erl");
                    argsFileContents += `,int:break(${moduleName}, ${bp.line})`;
                });
                this.functionBreakPoints.forEach(bp => {
                    argsFileContents += `,vscode_connection:set_breakpoint(${bp.moduleName}, {function, ${bp.functionName}, ${bp.arity}})`;
                });
                argsFileContents += "'";            
            }
            if (addEbinsToCodepath) {
                this.findEbinDirs(path.join(startDir, "_build")).forEach(ebin => {
                    argsFileContents += " -pz \"" + this.formatPath(ebin) + "\"";
                });
            }
            fs.writeFileSync(this.argsFileName, argsFileContents);
            
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

    public setBreakPointsRequest(bps: DebugProtocol.Breakpoint[], fbps: FunctionBreakpoint[]): void {
        if (!this.started) {
            this.breakPoints = this.breakPoints.concat(bps);
            this.functionBreakPoints = this.functionBreakPoints.concat(fbps);
        }
    }
}
