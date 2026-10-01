import {ErlangConnection} from './erlangConnection';
import { DebugProtocol } from '@vscode/debugprotocol';
import { FunctionBreakpoint } from './ErlangShellDebugger';


export class ErlangDebugConnection extends ErlangConnection {
    protected get_ErlangFiles(): string[] {
        // mcp/* is the embedded MCP inspector: compiled here (debugger bridge beams),
        // never loaded by the LSP node, only started on request for an enabled debug session
        return ["gen_connection.erl", "vscode_connection.erl", "vscode_jsone.erl", "vscode_jsone_decode.erl",
            "mcp/mcp_policy.erl", "mcp/mcp_encoder.erl", "mcp/mcp_audit.erl", "mcp/mcp_store.erl",
            "mcp/mcp_tools.erl", "mcp/mcp_runtime.erl", "mcp/mcp_server.erl", "mcp/mcp_sup.erl"];
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
            case "/mcp_call":
                // journal of the MCP inspector: request metadata only (no arguments, results or token)
                this.emit("mcp_call", body);
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

    public debuggerPause(pid: string): Promise<boolean> {
        if (this.erlangbridgePort > 0) {
            return this.post("debugger_pause", pid).then(res => {
                return true;
            }, err => {
                return false;
            });
        } else {
            return new Promise(() => false);
        }
    }

    public debuggerBindings(pid: string, frameId: string): Promise<any[]> { 
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

	/** Attach mode: leave the node running, without breakpoints or interpreted modules. */
	public debuggerDetach(): Promise<any> {
		return this.post("debugger_detach", "").catch(err => {
			this.debug(`debugger_detach error : ${err}`);
			return [];
		});
	}

	/** Start the MCP inspector in the target; the reply (with the session token) goes to the adapter only. */
	public mcpStart(config: any, mode: string): Promise<{ ok: boolean, error?: string, host?: string, port?: number, path?: string, sessionId?: string, token?: string }> {
		return this.post("mcp_start", JSON.stringify({ ...config, mode })).catch(err => {
			return { ok: false, error: "the MCP inspector could not be started" };
		});
	}

	/** Lease renewal: without it the target stops the inspector within ~10 seconds. */
	public mcpRenew(): Promise<boolean> {
		return this.post("mcp_renew", "").then(r => !!r?.ok, () => false);
	}

	public mcpStop(): Promise<void> {
		return this.post("mcp_stop", "").then(() => undefined, () => undefined);
	}

	public closeEventsReceiver(): void {
		this.events_receiver?.close();
	}

	Quit() : void {
        this.debuggerExit().then(() => {
            this.events_receiver.close();
        });
    }
}