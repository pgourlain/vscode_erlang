import { DebugSession, InitializedEvent, TerminatedEvent, StoppedEvent, 
	OutputEvent, Thread, ThreadEvent, StackFrame, Scope, Source, Handles
	, Breakpoint, ModuleEvent, Module } from 'vscode-debugadapter';
import { DebugProtocol } from 'vscode-debugprotocol';
import { ErlangShellForDebugging, IErlangShellOutput1 } from './ErlangShellDebugger';
import * as genericShell from './GenericShell';
import * as path from 'path';
import { EventEmitter } from 'events'
import * as http from 'http';
import * as vscode from 'vscode';
import * as erlang from './ErlangShell';
import { ErlangConnection } from './ErlangConnection';

export interface LaunchRequestArguments {
	cwd: string;
	erlpath: string;
	arguments: string;
}

var erlangBridgePath = path.join(__dirname, "..", "..", "erlangbridge");

class ErlangDebugSession extends DebugSession implements IErlangShellOutput1 {

    protected threadIDs: {[processName: string] : number};
	erlDebugger: ErlangShellForDebugging;
	erlangConnection : ErlangConnection;
	quit: boolean;
	private _breakPoints = new Map<string, DebugProtocol.Breakpoint[]>();

    public constructor() {
		super();
		this.threadIDs = {};
		this.setDebuggerLinesStartAt1(false);
		this.setDebuggerColumnsStartAt1(false);
		process.addListener('unhandledRejection', reason => {
            this.error(`******** Error in DebugAdapter - Unhandled promise rejection: ${reason}`);
        });
		process.addListener('uncaughtException', reason => {
            this.error(`******** Error in DebugAdapter - uncaughtException: ${reason}`);
		});
	}

    protected threadsRequest(response: DebugProtocol.ThreadsResponse): void {
		//this.debug("threadsRequest");
		var ths : Thread[] = [];
		for (var key in this.threadIDs) {
			var thid = this.threadIDs[key];
			ths.push(new Thread(thid, "Thread " + key))
		}
		response.body = {
			threads: ths
		};
		this.sendResponse(response);
	}

    protected initializeRequest(response: DebugProtocol.InitializeResponse, args: DebugProtocol.InitializeRequestArguments): void {
        this.debug("initializeRequest : ");
		this.erlDebugger = new ErlangShellForDebugging(this);
		this.erlDebugger.on('close', (exitCode) => {
			this.quitEvent(exitCode);
		})
		this.erlangConnection = new ErlangConnection(this);
		this.erlangConnection.on("new_module", (arg) => this.onNewModule(arg));
		this.erlangConnection.on("new_break", (arg) => this.onNewBreak(arg));
		this.erlangConnection.on("new_process", (arg) => this.onNewProcess(arg));
		this.erlangConnection.on("new_status", (pid, status) => this.onNewStatus(pid, status));
		

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

    protected async launchRequest(response: DebugProtocol.LaunchResponse, args: LaunchRequestArguments) {
		this.debug("launch");
		try {			
			var port = await this.erlangConnection.Start();
			this.debug("listen started");
			await this.erlDebugger.Start(args.cwd, port, erlangBridgePath, args.arguments);
			this.debug("erl started");
		}
		catch(error) {
			this.sendErrorResponse(response, <DebugProtocol.Message>error);
			return;
		}
		this.sendResponse(response);
    }

	protected disconnectRequest(response: DebugProtocol.DisconnectResponse, args: DebugProtocol.DisconnectArguments): void {
        //this.log("disconnectRequest");
		if (!this.quit) {
			//send q() only if user clic on stop debugger button
			this.erlDebugger.NormalQuit();
		}
		if (this.erlangConnection) {
			this.erlangConnection.Quit();
		}
        super.disconnectRequest(response, args);
	}


	protected quitEvent(exitCode: number) {
		//this.log(`erl exit with code ${exitCode}`);
		this.quit = true;
		this.sendEvent(new TerminatedEvent());
	}

	protected evaluateRequest(response: DebugProtocol.EvaluateResponse, args: DebugProtocol.EvaluateArguments): void {
        //this.log("evaluateRequest");
		//send entire expression entered in debugger console wend
		this.erlDebugger.Send(args.expression);
		response.body = {
			result: 'sending to erlang...',
			variablesReference: 0
		};
		this.sendResponse(response);
	}

    protected async setBreakPointsRequest(response: DebugProtocol.SetBreakpointsResponse, args: DebugProtocol.SetBreakpointsArguments) {
 		// this is returned to VS Code
        let vscodeBreakpoints: Breakpoint[];
		vscodeBreakpoints = [];
		this.debug("setbreakpoints : " + JSON.stringify(<any>args));

		args.breakpoints.forEach(bp => {
			vscodeBreakpoints.push(new Breakpoint(true, bp.line, 1, new Source(args.source.name, args.source.path)))
		});
		if (this.erlangConnection.isConnected) {
			this.erlangConnection.setBreakPointsRequest(vscodeBreakpoints);
		} else if (this.erlDebugger) {
			this.erlDebugger.setBreakPointsRequest(vscodeBreakpoints);
		}
		response.body = {breakpoints: vscodeBreakpoints};
		this.sendResponse(response);
	}

	protected setExceptionBreakPointsRequest(response: DebugProtocol.SetExceptionBreakpointsResponse, args: DebugProtocol.SetExceptionBreakpointsArguments): void {
		this.debug("setExceptionBreakPointsRequest : " + JSON.stringify(<any>args));	
		this.sendResponse(response);
	}

	protected configurationDoneRequest(response: DebugProtocol.ConfigurationDoneResponse, args: DebugProtocol.ConfigurationDoneArguments): void {
		this.debug("configurationDoneRequest");
		super.configurationDoneRequest(response, args);
	}

	//--- set function breakpoints request ------------------------------------------------------------------------------------

	protected setFunctionBreakPointsRequest(response: DebugProtocol.SetFunctionBreakpointsResponse, args: DebugProtocol.SetFunctionBreakpointsArguments): void {
		this.debug("setFunctionBreakPointsRequest :" + JSON.stringify(<any>args));
		this.sendResponse(response);
	}

    protected log(msg: string): void {
		this.outLine(`${msg}\n`);
    }

	protected outLine(msg: string, category?: string): void {
		this.sendEvent(new OutputEvent(msg, category ? category : 'console'));
	}

	/** send message to console with color of debug catgeory */
	public debug(msg : string) : void {
		//other category can be 'console', 'stdout', 'stderr', 'telemetry'		
		this.outLine(`${msg}\n`, "debug");
	}

	protected error(msg :string) : void {
		this.outLine(`${msg}\n`, "stderr");
	}

	show(): void {
	}

	appendLine(value: string): void {
		this.log(value);
	}

	append(value: string) : void {
		this.outLine(`${value}`);
	}

	//----------- events from erlangConnection
	private onNewModule(moduleName : string) : void {
		this.debug("OnNewModule : " + moduleName);
		this.sendEvent(new ModuleEvent("new", new Module(moduleName, moduleName)))	
	}

	private onNewBreak(breakName : string) : void {
		this.debug("OnNewBreak : " + breakName);
		//this.sendEvent(new ModuleEvent("new", new Module(moduleName, moduleName)))	
	}

	private pid_to_number(processName : string) : number {
		var pidAsString : string = processName.substr(1, processName.length-2);
		pidAsString = pidAsString.replace(".", "");

		return Number.parseInt(pidAsString);
	}

	private onNewProcess(processName : string) : void {
		//each process in erlang is mapped to one 'thread'
		this.debug("OnNewProcess : " + processName);
		var thid = this.pid_to_number(processName);
		this.threadIDs[processName] = thid;
		this.sendEvent(new ThreadEvent("started", thid));
	}
	private onNewStatus(processName : string, status : string) {
		this.debug("OnStatus : " + processName + "," + status);
		if (status === 'exit') {
			var thid = this.pid_to_number(processName);
			delete this.threadIDs[processName];
			this.sendEvent(new ThreadEvent("exited", thid));						
		}		
	}
}

DebugSession.run(ErlangDebugSession);    
