import {ErlangConnection} from './erlangConnection';
import { DebugProtocol } from 'vscode-debugprotocol';
import { FunctionBreakpoint } from './ErlangShellDebugger';


export class ErlangDebugConnection extends ErlangConnection {
    protected get_ErlangFiles(): string[] {
        return ["gen_connection.erl", "vscode_connection.erl", "vscode_jsone.erl"];
    }

    protected handle_erlang_event(url: string, body : any) : void {
        //this method handle every event receiver from erlang
        switch(url) {
            case "/listen":
                this.erlangbridgePort = body.port;
                this.emit("listen", "erlang bridge listen on port :" + this.erlangbridgePort.toString());
            break;
            case "/interpret":
                this.emit("new_module", body.module);
            break;
            case "/new_process":
                this.emit("new_process", body.process);
            break;
            case "/new_status":
                this.emit("new_status", body.process, body.status, body.reason, body.module, body.line);
            break;
            case "/new_break":
                this.emit("new_break", body.module, body.line);
            break;
            case "/on_break":
                this.emit("on_break", body.process, body.module, body.line, body.stacktrace);
            break;
            case "/delete_break":
            break;
            case "/fbp_verified":
                this.emit("fbp_verified", body.module, body.name, body.arity);
            break;
            default:
                this.debug("receive from erlangbridge :" + url + ", body :" + JSON.stringify(body));
            break;
        }
    }   
    
    public setBreakPointsRequest(moduleName : string, breakPoints : DebugProtocol.Breakpoint[], functionBreakpoints: FunctionBreakpoint[]) : Promise<boolean> {
        if (this.erlangbridgePort > 0) {    
            let bps = moduleName + "\r\n";
            breakPoints.forEach(bp => {
                bps += `line ${bp.line}\r\n`;
            });
            functionBreakpoints.forEach(bp => {
                bps += `function ${bp.functionName} ${bp.arity}\r\n`;
            });
            return this.post("set_bp", bps).then(res => {
                return true;
            }, err => {
                return false;
            });
        } else {
            return new Promise(() => false);
        }
    }

    public debuggerContinue(pid : string) : Promise<boolean> {
        if (this.erlangbridgePort > 0) {
            return this.post("debugger_continue", pid).then(res => {
                    return true;
                }, err => {
                    return false;
                });
        } else {
            return new Promise(() => false);
        }
        
    }

    public debuggerNext(pid : string) : Promise<boolean> {
        if (this.erlangbridgePort > 0) {
            return this.post("debugger_next", pid).then(res => {
                    return true;
                }, err => {
                    return false;
                });
        } else {
            return new Promise(() => false);
        }        
    }

    public debuggerStepIn(pid : string) : Promise<boolean> {
        if (this.erlangbridgePort > 0) {
            return this.post("debugger_stepin", pid).then(res => {
                    return true;
                }, err => {
                    return false;
                });
        } else {
            return new Promise(() => false);
        }        
    }

    public debuggerStepOut(pid : string) : Promise<boolean> {
        if (this.erlangbridgePort > 0) {
            return this.post("debugger_stepout", pid).then(res => {
                    return true;
                }, err => {
                    return false;
                });
        } else {
            return new Promise(() => false);
        }        
    }

    public debuggerBindings(pid: string, frameId : string) : Promise<any[]> { 
        if (this.erlangbridgePort > 0) {
            return this.post("debugger_bindings", pid + "\r\n" + frameId).then(res => {
                    //this.debug(`result of bindings : ${JSON.stringify(res)}`);
                    return (<Array<any>>res);
                }, err => {
                    this.debug(`debugger_bindings error : ${err}`);
                    return [];
                });
        } else {
            return new Promise(() => []);
        }        
    }

    public debuggerEval(pid: string, frameId : string, expression: string): Promise<any> {
        if (this.erlangbridgePort > 0) {
            return this.post("debugger_eval", pid + "\r\n" + frameId + "\r\n" + expression).then(res => {
                    return (<any>res);
                }, err => {
                    this.debug(`debugger_eval error : ${err}`);
                    return [];
                });
        } else {
            return new Promise(() => []);
        }  
    }
    public debuggerExit(): Promise<any> {
        if (this.erlangbridgePort > 0) {
            //this.debug('exit')
            return this.post("debugger_exit", "").then(res => {
                this.debug('exit yes')
                return (<any>res);
                }, err => {
                    this.debug('exit no')
                    this.debug(`debugger_exit error : ${err}`);
                    return [];
                });
        } else {
            return new Promise(() => []);
        }  
    }

	Quit() : void {
        this.debuggerExit().then(() => {
            this.events_receiver.close();
        });
    }
}