import * as vscode from 'vscode';
import { ChildProcess, spawn } from 'child_process'
import { EventEmitter } from 'events'
import * as fs from 'fs';
import * as path from 'path';
import { fstat } from 'fs';

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
    show(): void;
    appendLine(value: string): void;
}

/**
 * Defines support for raw shell output from processes spawned by GenericShell.
 */
export interface IShellOutput {
    append(value: string): void;
}

export class GenericShell extends EventEmitter {
    protected childProcess: ChildProcess;
    protected logOutput: ILogOutput;
    protected shellOutput: IShellOutput;
    protected buffer: string = "";
    protected errbuf: string = "";
    public erlangPath: string = null;

    constructor(logOutput?: ILogOutput, shellOutput?: IShellOutput) {
        super();
        this.logOutput = logOutput;
        this.shellOutput = shellOutput;

        // Find Erlang 'bin' directory
        let erlangPath = vscode.workspace.getConfiguration("erlang").get("erlangPath", null);
        if (erlangPath) {
            if (erlangPath.match(/^[A-Za-z]:/)) {
                // Windows absolute path (C:\...) is applicable on Windows only
                if (process.platform == 'win32') {
                    this.erlangPath = path.win32.normalize(erlangPath);
                }
            } else {
                erlangPath = path.normalize(erlangPath);
                if (! fs.existsSync(erlangPath)) {
                    erlangPath = path.join(vscode.workspace.rootPath, erlangPath);
                }
                if (fs.existsSync(erlangPath)) {
                    this.erlangPath = erlangPath;
                }
            }
        }
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
                this.logOutput && this.logOutput.show();
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

                this.childProcess = spawn(processName, args, { cwd: startDir, shell: true, stdio: 'pipe', env : childEnv });
                this.childProcess.on('error', error => {
                    this.log("stderr", error.message);
                    if (process.platform == 'win32') {
                        this.log("stderr", "ensure '" + processName + "' is in your path.");
                    }
                });
                this.childProcess.stdout.on('data', this.stdout.bind(this));
                this.childProcess.stderr.on('data', this.stderr.bind(this));

                this.childProcess.on('exit', (exitCode: number, signal: string) => {
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

    public Send(what: string) {
        this.log("log", what);
        this.childProcess.stdin.write(what);
        this.childProcess.stdin.write('\r\n');
    }

    private appendToShellOutput(data: string) {
        this.shellOutput && this.shellOutput.append(data);
    }
}
