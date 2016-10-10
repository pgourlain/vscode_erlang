import { DebugSession, InitializedEvent, TerminatedEvent, StoppedEvent, OutputEvent, Thread, StackFrame, Scope, Source, Handles
	, Breakpoint } from 'vscode-debugadapter';
import { DebugProtocol } from 'vscode-debugprotocol';
import { ErlangShellForDebugging, IErlangShellOutput1 } from './ErlangShellDebugger';
import * as genericShell from './GenericShell';
import * as path from 'path';
import { EventEmitter } from 'events'

export interface LaunchRequestArguments {
	cwd: string;
	target: string;
	erlpath: string;
	arguments: string;
}

class ErlangDebugSession extends DebugSession implements IErlangShellOutput1 {
    protected threadID: number = 1;
	erlDebugger: ErlangShellForDebugging;
	quit: boolean;
	private _breakPoints = new Map<string, DebugProtocol.Breakpoint[]>();

    public constructor() {
		super();
		this.threadID = 1;
		this.setDebuggerLinesStartAt1(false);
		this.setDebuggerColumnsStartAt1(false);
		process.addListener('unhandledRejection', reason => {
            this.log(`******** Error in DebugAdapter - Unhandled promise rejection: ${reason}`);
        });
		process.addListener('uncaughtException', reason => {
            this.log(`******** Error in DebugAdapter - uncaughtException: ${reason}`);
		});
	}

    protected threadsRequest(response: DebugProtocol.ThreadsResponse): void {
		response.body = {
			threads: [
				new Thread(this.threadID, "Thread 1")
			]
		};
		this.sendResponse(response);
	}

    protected initializeRequest(response: DebugProtocol.InitializeResponse, args: DebugProtocol.InitializeRequestArguments): void {
        this.log("initializeRequest");

		// since this debug adapter can accept configuration requests like 'setBreakpoint' at any time,
		// we request them early by sending an 'initializeRequest' to the frontend.
		// The frontend will end the configuration sequence by calling 'configurationDone' request.
		this.sendEvent(new InitializedEvent());
		
		response.body.supportsConfigurationDoneRequest = true;
		response.body.supportsConditionalBreakpoints = true;
		response.body.supportsFunctionBreakpoints = true;
		response.body.supportsEvaluateForHovers = false;
		response.body.supportsSetVariable = true;
		response.body.supportsStepBack = false;
		this.sendResponse(response);
	}

    protected launchRequest(response: DebugProtocol.LaunchResponse, args: LaunchRequestArguments): void {
		this.log("launch");
		this.erlDebugger = new ErlangShellForDebugging(this);
		//this.erlDebugger = new ATest();

		//this.erlDebugger = null;
        //this.miDebugger = new MI2(args.gdbpath || "gdb", ["-q", "--interpreter=mi2"]);
		//this.initDebugger();
		//this.quit = false;
		//this.attached = false;
		//this.needContinue = false;
		//this.started = false;
		//this.crashed = false;
		//this.debugReady = false;

        this.erlDebugger.Start(args.cwd, args.arguments)
            .then(res => {
				this.quitEvent(res);
            }, err => {
                this.sendErrorResponse(response, 100, `erl runtime failed to run ${err}`);
            });

        this.sendResponse(response);
        //this.sendErrorResponse(response, 102, `Not yet implemented, coming soon...stay tuned !`);
    }

	protected disconnectRequest(response: DebugProtocol.DisconnectResponse, args: DebugProtocol.DisconnectArguments): void {
        this.log("disconnectRequest");
		this.erlDebugger.NormalQuit();
        super.disconnectRequest(response, args);
	}


	protected quitEvent(exitCode: number) {
		this.log(`erl exit with code ${exitCode}`);
		this.quit = true;
		this.sendEvent(new TerminatedEvent());
	}

	protected evaluateRequest(response: DebugProtocol.EvaluateResponse, args: DebugProtocol.EvaluateArguments): void {
        //this.log("evaluateRequest");
		this.erlDebugger.Send(args.expression);
		response.body = {
			result: 'sending to erlang...',
			variablesReference: 0
		};
		this.sendResponse(response);
	}

    protected setBreakPointsRequest(response: DebugProtocol.SetBreakpointsResponse, args: DebugProtocol.SetBreakpointsArguments): void {
		if (this.erlDebugger) {
			this.log("setbreakpoint after start of debugger");
		}
		var bp = new Breakpoint(true, 1, 1, new Source("coucou", "path", 1, "origin", "dtata"));
		this.log("setbreakpoints : " + JSON.stringify(<any>args));
		this.sendResponse(response);
	}

	protected configurationDoneRequest(response: DebugProtocol.ConfigurationDoneResponse, args: DebugProtocol.ConfigurationDoneArguments): void {
		this.log("configurationDoneRequest");
		super.configurationDoneRequest(response, args);
	}

	//--- set function breakpoints request ------------------------------------------------------------------------------------

	protected setFunctionBreakPointsRequest(response: DebugProtocol.SetFunctionBreakpointsResponse, args: DebugProtocol.SetFunctionBreakpointsArguments): void {
		this.log("setFunctionBreakPointsRequest");
		this.sendResponse(response);
	}

    protected log(msg: string): void {
		this.outLine(`${msg}\n`);
    }

	protected outLine(msg: string, category?: string): void {
		this.sendEvent(new OutputEvent(msg, category ? category : 'console'));
	}

	show(): void {
	}

	appendLine(value: string): void {
		this.log(value);
	}
}

DebugSession.run(ErlangDebugSession);    
