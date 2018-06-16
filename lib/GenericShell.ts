import { ChildProcess, spawn } from 'child_process'
import { EventEmitter } from 'events'
import * as utils from './utils';

//inspired from https://github.com/WebFreak001/code-debug/blob/master/src/backend/mi2/mi2.ts for inspiration of an EventEmitter 
const nonOutput = /^(?:\d*|undefined)[\*\+\=]|[\~\@\&\^]/;

function couldBeOutput(line: string) {
    if (nonOutput.exec(line))
        return false;
    return true;
}

export interface IErlangShellOutput {
    show(): void;
    appendLine(value: string): void;
}

export class ErlGenericShell extends EventEmitter {
    protected erlangShell: ChildProcess;
    protected channelOutput: IErlangShellOutput;
    protected buffer: string = "";
    protected errbuf: string = "";
    public erlangPath: string = null;


    constructor(whichOutput: IErlangShellOutput) {
        super();
        this.channelOutput = whichOutput;
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
                var channel = this.channelOutput;
                if (this.channelOutput) {
                    channel.show();
                }
                if (!quiet) {
                    if (this.erlangPath) {
                        this.log("log",`using erlang binaries from path : '${this.erlangPath}'`);
                    }
                    this.log("log", `starting : ${processName} \r\n` + args.join(" "));
                }
                var childEnv = null;
                if (this.erlangPath) {
                    childEnv = process.env;
                    childEnv.PATH = this.erlangPath +";"+childEnv.PATH
                }

                this.erlangShell = spawn(processName, args, { cwd: startDir, shell: true, stdio: 'pipe', env : childEnv });
                this.erlangShell.on('error', error => {
                    this.log("stderr", error.message);
                    if (process.platform == 'win32') {
                        this.log("stderr", "ensure '" + processName + "' is in your path.");
                    }
                });
                this.erlangShell.stdout.on("data", this.stdout.bind(this));
                this.erlangShell.stderr.on("data", this.stderr.bind(this));

                this.erlangShell.on('close', (exitCode) => {
                    this.log("log", processName + ' exit code:' + exitCode);
                    this.emit('close', exitCode);
                });
                this.erlangShell.on('exit', (exitCode: number, signal: string) => {
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
        });
    }

    onOutputPartial(line) {
        if (couldBeOutput(line)) {
            this.logNoNewLine("stdout", line);
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
        });
    }

    protected logNoNewLine(type: string, msg: string): void {
        if (this.channelOutput) {
            this.channelOutput.appendLine(msg);
        }
        this.emit("msg", type, msg);
    }

    protected log(type: string, msg: string): void {
        if (this.channelOutput) {
            this.channelOutput.appendLine(msg);
        }
        this.emit("msg", type, msg[msg.length - 1] == '\n' ? msg : (msg + "\n"));
    }

    public Send(what: string) {
        this.log("log", what);
        this.erlangShell.stdin.write(what);
        this.erlangShell.stdin.write("\r\n");
    }
}