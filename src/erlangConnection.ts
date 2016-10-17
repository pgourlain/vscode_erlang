
import { EventEmitter } from 'events'
import * as http from 'http';
import { DebugProtocol } from 'vscode-debugprotocol';
import * as path from 'path';
import { ErlangShellForDebugging, IErlangShellOutput1 } from './ErlangShellDebugger';

var erlangBridgePath = path.join(__dirname, "..", "..", "erlangbridge");

/** this class is responsible to send/receive debug command to erlang bridge */
export class ErlangConnection extends EventEmitter {
	erlangbridgePort : number;
    command_receiver : http.Server;
    _output : IErlangShellOutput1;

    public constructor(output : IErlangShellOutput1) {
        super();
        this._output = output;
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

    public async Start() : Promise<number> {
        return new Promise<number>((a,r)=> {
            this.compile_erlang_connection().then(() => {
                return this.start_command_receiver().then(res => {
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
        this.command_receiver.close();
    }

    private compile_erlang_connection() : Promise<number> {
        return new Promise<number>((a, r) => {
			var compiler = new ErlangShellForDebugging(null);
			var erlFile = "vscode_connection.erl";	
			return compiler.Compile(erlangBridgePath, [erlFile]).then(res => {
                    this.log("Compilation of erlang bridge...ok");
                    a(res);
				}, exitCode => {
                    this.log("Compilation of erlang bridge...ko");
                    r(exitCode);
				});
        });		
    }

	private start_command_receiver() : Promise<number> {
        this.logAppend("Starting http listener...");
		return new Promise<number>((accept, reject) =>
		{
			this.command_receiver = http.createServer((req, res) => {
				var url = req.url;
				var body = [];
				var jsonBody = null;
				req.on('error', err => {
					this.log("request error");
				}).on('data', chunk =>{
					body.push(chunk);
				}).on('end', () => {
					var sbody = Buffer.concat(body).toString();
					jsonBody = JSON.parse(sbody);
					this.handle_command(url, jsonBody);
					res.statusCode = 200;
					res.setHeader('Content-Type', 'text/plain');
					res.end('ok');
				});
			});
			this.command_receiver.listen(0, '127.0.0.1', () => {
				var p = this.command_receiver.address().port;
                this.logAppend(` on http://127.0.0.1:${p}\n`);
				accept(p);
			});

		});
	}

	handle_command(url: string, body : any) {
		if (url === '/listen') {
			this.erlangbridgePort = body.port;
			this.log("erlang bridge listen on port :" + this.erlangbridgePort.toString());
		}
	}    

    public sendBreakpointSetCommand(breakPoint : DebugProtocol.Breakpoint) : boolean {
        return false;
    }


}