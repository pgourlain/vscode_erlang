import * as vscode from 'vscode';
import { ChildProcess, spawn } from 'child_process'
import { EventEmitter } from 'events'
import * as fs from 'fs';
import * as path from 'path';
import { fstat } from 'fs';
import { ErlangSettings } from './erlangSettings';

//inspired from https://github.com/WebFreak001/code-debug/blob/master/src/backend/mi2/mi2.ts for inspiration of an EventEmitter 
const nonOutput = /^(?:\d*|undefined)[\*\+\=]|[\~\@\&\^]/;

function couldBeOutput(line: string) {
    if (nonOutput.exec(line))
        return false;
    return true;
}

/**
 * Defines support for log output from the GenericShell class.
 */
export interface ILogOutput {
    appendLine(value: string): void;
    debug(msg : string) : void;
}

/**
 * Defines support for raw shell output from processes spawned by GenericShell.
 */
export interface IShellOutput {
    append(value: string): void;
}

// Wraps a value that must survive cmd.exe re-parsing (a space, or a
// metacharacter like the `{a,b,c}` in an inet address tuple). A no-op when
// useShell is false: spawn then passes argv elements straight to the OS, so
// a literal quote character would otherwise end up inside the argument itself.
export function quoteForShell(value: string, useShell: boolean): string {
    return useShell ? `"${value}"` : value;
}

export class GenericShell extends EventEmitter {
    protected childProcess: ChildProcess;
    protected logOutput: ILogOutput;
    protected shellOutput: IShellOutput;
    protected buffer: string = "";
    protected errbuf: string = "";
    public erlangPath: string = null;
	public erlangArgs : string[] = [];
	public erlangDistributedNode: boolean = false;
    public cacheManagement: string = "memory";
    // cmd.exe is still needed on Windows to dispatch .bat/.cmd and to
    // re-parse quoted args; this default ("auto", see erlang.useShell) spawns
    // the binary directly everywhere else, since a sandboxed or minimal
    // environment (e.g. Kiro, #351) can deny/lack a shell entirely while
    // still allowing the binary itself to run. Overridable per erlang.useShell.
    public useShell: boolean = process.platform === 'win32';

    //provide IGenericShellConfiguration, in order to avoid dependencies on vscode module (it doesn't works with debugger-adpater)
    constructor(logOutput?: ILogOutput, shellOutput?: IShellOutput, erlangConfiguration?: ErlangSettings) {
        super();
        this.logOutput = logOutput;
        this.shellOutput = shellOutput;

        if (erlangConfiguration) {
            // Find Erlang 'bin' directory
            let erlangPath = erlangConfiguration.erlangPath;
            if (erlangPath) {
                if (erlangPath.match(/^[A-Za-z]:/)) {
                    // Windows absolute path (C:\...) is applicable on Windows only
                    if (process.platform == 'win32') {
                        this.erlangPath = path.win32.normalize(erlangPath);
                    }
                } else {
                    erlangPath = path.normalize(erlangPath);
                    if (! fs.existsSync(erlangPath)) {
                        erlangPath = path.join(erlangConfiguration.rootPath, erlangPath);
                    }
                    if (fs.existsSync(erlangPath)) {
                        this.erlangPath = erlangPath;
                    }
                }
            }
            this.erlangArgs = erlangConfiguration.erlangArgs;
            this.erlangDistributedNode = erlangConfiguration.erlangDistributedNode;
            this.cacheManagement = erlangConfiguration.cacheManagement;
            this.useShell = erlangConfiguration.useShell;
        }
    }

    protected shellQuote(value: string): string {
        return quoteForShell(value, this.useShell);
    }

    protected RunProcess(processName, startDir: string, args: string[]): Promise<number> {

        return new Promise<number>((resolve, reject) => {
            this.LaunchProcess(processName, startDir, args).then(started => {
                this.on('close', (exitCode) => {
                    if (exitCode == 0) {
                        resolve(0);
                    } else {
                        reject(exitCode);
                    }
                });
            })
        });
    }

    protected LaunchProcess(processName, startDir: string, args: string[], quiet: boolean = false): Promise<boolean> {
        return new Promise<boolean>((resolve, reject) => {
            try {
                if (!quiet) {
                    if (this.erlangPath) {
                        this.log("log",`using erlang binaries from path : '${this.erlangPath}'`);
                    }
                    this.log("log", `starting : ${processName} \r\n` + args.join(" "));
                }
                var childEnv = null;
                if (this.erlangPath) {
                    childEnv = process.env;
                    var separator = process.platform == 'win32' ? ";" : ":";
                    childEnv.PATH = this.erlangPath + separator + childEnv.PATH;
                }

                // A spawn that never starts (missing binary, missing cwd like a
                // never-built `_build`, no /bin/sh, ...) only fires 'error', never
                // 'exit' reliably. Before this flag, that error was logged and
                // dropped: every caller here waits on the 'close' event (see
                // RunProcess above and ErlangShellLSP.Start's caller), so it hung
                // until an unrelated layer surfaced an opaque failure later
                // (#326, #239) instead of the real spawn error.
                let launchFailed = false;
                this.childProcess = spawn(processName, args, { cwd: startDir, shell: this.useShell, stdio: 'pipe', env : childEnv });
                this.childProcess.on('error', error => {
                    launchFailed = true;
                    this.log("stderr", error.message);
                    if (process.platform == 'win32') {
                        this.log("stderr", "ensure '" + processName + "' is in your path.");
                    }
                    this.emit('close', null, error);
                });
                this.childProcess.stdout.on('data', this.stdout.bind(this));
                this.childProcess.stderr.on('data', this.stderr.bind(this));

                this.childProcess.on('exit', (exitCode: number, signal: string) => {
                    if (launchFailed) {
                        return;
                    }
                    this.log("log", processName + ' exit code:' + exitCode);
                    this.emit('close', exitCode);
                });
                resolve(true);
            }
            catch (error) {
                reject(error);
            }
        });
    }

    onOutput(lines) {
        lines = <string[]>lines.split('\n');
        lines.forEach(line => {
            this.log("stdout", line);
            this.appendToShellOutput(`${line}\n`);
        });
    }

    onOutputPartial(line) {
        if (couldBeOutput(line)) {
            this.logNoNewLine("stdout", line);
            this.appendToShellOutput(line);
            return true;
        }
        return false;
    }

    stdout(data) {
        if (typeof data == "string")
            this.buffer += data;
        else
            this.buffer += data.toString("utf8");
        let end = this.buffer.lastIndexOf('\n');
        if (end != -1) {
            this.onOutput(this.buffer.substr(0, end));
            this.buffer = this.buffer.substr(end + 1);
        }
        if (this.buffer.length) {
            if (this.onOutputPartial(this.buffer)) {
                this.buffer = "";
            }
        }
    }

    stderr(data) {
        if (typeof data == "string")
            this.errbuf += data;
        else
            this.errbuf += data.toString("utf8");
        let end = this.errbuf.lastIndexOf('\n');
        if (end != -1) {
            this.onOutputStderr(this.errbuf.substr(0, end));
            this.errbuf = this.errbuf.substr(end + 1);
        }
        if (this.errbuf.length) {
            this.logNoNewLine("stderr", this.errbuf);
            this.errbuf = "";
        }
    }

    onOutputStderr(lines) {
        lines = <string[]>lines.split('\n');
        lines.forEach(line => {
            this.log("stderr", line);
            this.appendToShellOutput(line);
        });
    }

    protected logNoNewLine(type: string, msg: string): void {
        this.logOutput && this.logOutput.appendLine(msg);
        this.emit("msg", type, msg);
    }

    protected log(type: string, msg: string): void {
        this.logOutput && this.logOutput.appendLine(msg);
        this.emit("msg", type, msg[msg.length - 1] == '\n' ? msg : (msg + "\n"));
    }

    protected debug(msg : string) : void {
        this.logOutput && this.logOutput.debug(msg);
    }

    public Send(what: string) {
        this.log("log", what);
        this.childProcess.stdin.write(what);
        this.childProcess.stdin.write('\r\n');
    }

    private appendToShellOutput(data: string) {
        this.shellOutput && this.shellOutput.append(data);
    }
}
