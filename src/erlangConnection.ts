
import { EventEmitter } from 'events'
import * as http from 'http';
import { DebugProtocol } from 'vscode-debugprotocol';
import * as path from 'path';
import { ErlangShellForDebugging, IErlangShellOutput1 } from './ErlangShellDebugger';

var erlangBridgePath = path.join(__dirname, "..", "..", "erlangbridge");

/** this class is responsible to send/receive debug command to erlang bridge */
export class ErlangConnection extends EventEmitter {
	erlangbridgePort : number;
    events_receiver : http.Server;
    _output : IErlangShellOutput1;

    
    public get isConnected() : boolean {
        return this.erlangbridgePort > 0;
    }
    
    public constructor(output : IErlangShellOutput1) {
        super();
        this._output = output;
        this.erlangbridgePort = -1;
    }

    protected log(msg: string) : void {
        if (this._output) {
            this._output.appendLine(msg);
        }
    }

    protected logAppend(msg: string) : void {
        if (this._output) {
            this._output.append(msg);
        }
    }

    protected debug(msg : string) : void {
        if (this._output) {
            this._output.debug(msg);
        }
    }

    public async Start() : Promise<number> {
        return new Promise<number>((a,r)=> {
            this.compile_erlang_connection().then(() => {
                return this.start_events_receiver().then(res => {
                    a(res);
                }, exitCode => {
                    //this.log("reject");
                    r(exitCode);
                });    
            }, exiCode =>{
                //this.log("reject");
                r(exiCode);
            });
        });
    }

    public Quit() : void {
        this.events_receiver.close();
    }

    private compile_erlang_connection() : Promise<number> {
        return new Promise<number>((a, r) => {
			var compiler = new ErlangShellForDebugging(null);
			var erlFile = "vscode_connection.erl";	
			return compiler.Compile(erlangBridgePath, [erlFile]).then(res => {
                    this.debug("Compilation of erlang bridge...ok");
                    a(res);
				}, exitCode => {
                    this.debug("Compilation of erlang bridge...ko");
                    r(exitCode);
				});
        });		
    }

	private start_events_receiver() : Promise<number> {
        this.logAppend("Starting http listener...");
		return new Promise<number>((accept, reject) =>
		{
			this.events_receiver = http.createServer((req, res) => {
				var url = req.url;
				var body = [];
				var jsonBody = null;
				req.on('error', err => {
					this.log("request error");
				}).on('data', chunk =>{
					body.push(chunk);
				}).on('end', () => {
                    //here : receive all events from erlangBridge
					var sbody = Buffer.concat(body).toString();
                    try {
                        jsonBody = JSON.parse(sbody);
					    this.handle_erlang_event(url, jsonBody);
                    }
                    catch (err)
                    {
                        this.debug("error while receving command :" + err + "\r\n" + sbody);
                    }
    				res.statusCode = 200;
					res.setHeader('Content-Type', 'text/plain');
					res.end('ok');
				});
			});
			this.events_receiver.listen(0, '127.0.0.1', () => {
				var p = this.events_receiver.address().port;
                this.logAppend(` on http://127.0.0.1:${p}\n`);
				accept(p);
			});

		});
	}

	handle_erlang_event(url: string, body : any) {
        //this method handle every event receiver from erlang
        switch(url) {
            case "/listen" :
                this.erlangbridgePort = body.port;
                this.debug("erlang bridge listen on port :" + this.erlangbridgePort.toString());
            break;
            case "/interpret" :
                this.emit("new_module", body.module);
            break;
            case "/new_process" :
                this.emit("new_process", body.process);
            break;
            case "/new_status" :
                this.emit("new_status", body.process, body.status, body.reason, body.module, body.line);
            break;
            default:
                this.debug("receive from erlangbridge :" + url + ", body :" + JSON.stringify(body));
            break;
        }
	}    

    public setBreakPointsRequest(breakPoints : DebugProtocol.Breakpoint[]) : Promise<boolean> {
        if (this.erlangbridgePort > 0) {
            return this.post("set_bp").then(res => {
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

    private post(verb : string, body? : string) : Promise<any> {
        return new Promise<any>((a, r) => {
            if (!body) {
                body = "";
            }
            var options:http.RequestOptions = {
                host:"127.0.0.1",
                path: verb,
                port: this.erlangbridgePort,
                method:"POST",
                headers: {
                    'Content-Type': 'plain/text',
                    'Content-Length': Buffer.byteLength(body)
                } 
            }
            var postReq = http.request(options, response => {
                var body = '';
                response.on('data', buf => {
                    body += buf;
                });

                response.on('end', () => {
                    try {
                        var parsed = JSON.parse(body);
                        a(parsed);
                    } catch (err) {
                        this.log("unable to parse response as JSON:" + err)
                        //console.error('Unable to parse response as JSON', err);
                        r(err);
                    }
                });
            });
            postReq.write(body);
            postReq.end();
        });
    }


}