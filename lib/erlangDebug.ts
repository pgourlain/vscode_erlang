import {
	DebugSession, InitializedEvent, TerminatedEvent, StoppedEvent,
	OutputEvent, Thread, ThreadEvent, StackFrame, Scope, Source, Handles
	, Breakpoint, ModuleEvent, Module
} from 'vscode-debugadapter';
import { DebugProtocol } from 'vscode-debugprotocol';
import { ErlangShellForDebugging, IErlangShellOutputForDebugging } from './ErlangShellDebugger';
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

class ErlangDebugSession extends DebugSession implements IErlangShellOutputForDebugging {

	protected threadIDs: { [processName: string]: {thid: number, stack:any }};
	erlDebugger: ErlangShellForDebugging;
	erlangConnection: ErlangConnection;
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
		var ths: Thread[] = [];
		for (var key in this.threadIDs) {
			var thread = this.threadIDs[key];
			ths.push(new Thread(thread.thid, "Thread " + key))
		}
		response.body = {
			threads: ths
		};
		this.sendResponse(response);
	}

	protected initializeRequest(response: DebugProtocol.InitializeResponse, args: DebugProtocol.InitializeRequestArguments): void {
		this.debug("Initializing erlang debugger");
		this.erlDebugger = new ErlangShellForDebugging(this);
		this.erlDebugger.on('close', (exitCode) => {
			this.quitEvent(exitCode);
		})
		this.erlangConnection = new ErlangConnection(this);
		this.erlangConnection.on("new_module", (arg) => this.onNewModule(arg));
		this.erlangConnection.on("new_break", (arg) => this.onNewBreak(arg));
		this.erlangConnection.on("new_process", (arg) => this.onNewProcess(arg));
		this.erlangConnection.on("new_status", (pid, status, reason, moduleName, line) => this.onNewStatus(pid, status, reason, moduleName, line));
		this.erlangConnection.on("on_break", (pid, moduleName, line, snapshot) => this.onBreak(pid, moduleName, line, snapshot));


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
		//this.debug("launch");
		try {
			var port = await this.erlangConnection.Start();
			this.debug("Local webserver for erlang is started");
			this.debug("Starting erlang");
			this.debug(`	path      : ${args.cwd}`);
			this.debug(`	arguments : ${args.arguments}`);
			await this.erlDebugger.Start(args.cwd, port, erlangBridgePath, args.arguments);
			//this.debug("erlang started");
		}
		catch (error) {
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
		response.body = { breakpoints: vscodeBreakpoints };
		this.sendResponse(response);
	}

	protected setExceptionBreakPointsRequest(response: DebugProtocol.SetExceptionBreakpointsResponse, args: DebugProtocol.SetExceptionBreakpointsArguments): void {
		//this.debug("setExceptionBreakPointsRequest : " + JSON.stringify(<any>args));	
		this.sendResponse(response);
	}

	protected configurationDoneRequest(response: DebugProtocol.ConfigurationDoneResponse, args: DebugProtocol.ConfigurationDoneArguments): void {
		//this.debug("configurationDoneRequest");
		super.configurationDoneRequest(response, args);
	}

	//--- set function breakpoints request ------------------------------------------------------------------------------------

	protected setFunctionBreakPointsRequest(response: DebugProtocol.SetFunctionBreakpointsResponse, args: DebugProtocol.SetFunctionBreakpointsArguments): void {
		//this.debug("setFunctionBreakPointsRequest :" + JSON.stringify(<any>args));
		this.sendResponse(response);
	}

	protected continueRequest(response: DebugProtocol.ContinueResponse, args: DebugProtocol.ContinueArguments): void {
		this.sendResponse(response);
		this.erlangConnection.debuggerContinue(this.thread_id_to_pid(args.threadId)).then(
			() => {
				this.sendResponse(response);
			},
			(reason) => {
				this.error("unable to continue debugging.")
				this.sendEvent(new TerminatedEvent());
				this.sendResponse(response);
			});
	}

	protected nextRequest(response: DebugProtocol.NextResponse, args: DebugProtocol.NextArguments): void {
		this.debug("nextTraceRequest");
		super.nextRequest(response, args);

	}
	protected stepInRequest(response: DebugProtocol.StepInResponse, args: DebugProtocol.StepInArguments): void {
		this.debug("stepinTraceRequest");
		super.stepInRequest(response, args);
	}
	protected stepOutRequest(response: DebugProtocol.StepOutResponse, args: DebugProtocol.StepOutArguments): void {
		this.debug("stepoOutTraceRequest");
		super.stepOutRequest(response, args);
	}

	protected stackTraceRequest(response: DebugProtocol.StackTraceResponse, args: DebugProtocol.StackTraceArguments): void {
		this.debug("stackTraceRequest");
		super.stackTraceRequest(response, args);
	}

	protected sourceRequest(response: DebugProtocol.SourceResponse, args: DebugProtocol.SourceArguments): void {
		this.debug("sourceRequest");
		super.sourceRequest(response, args);
	}

	protected convertClientLineToDebugger(line: number): number {
		this.debug("convertClientLineToDebugger");
		return super.convertClientLineToDebugger(line);
	}
	protected convertDebuggerLineToClient(line: number): number {
		this.debug("convertDebuggerLineToClient");
		return super.convertDebuggerLineToClient(line);
	}
	protected convertClientColumnToDebugger(column: number): number {
		return super.convertClientColumnToDebugger(column);

	}
	protected convertDebuggerColumnToClient(column: number): number {
		return super.convertDebuggerColumnToClient(column);

	}
	protected convertClientPathToDebugger(clientPath: string): string {
		return super.convertClientPathToDebugger(clientPath);

	}
	protected convertDebuggerPathToClient(debuggerPath: string): string {
		return super.convertDebuggerPathToClient(debuggerPath);

	}


	protected thread_id_to_pid(current_thid: number): string {
		for (var key in this.threadIDs) {
			var thread = this.threadIDs[key];
			if (thread.thid == current_thid) {
				return key;
			}
		}
		return "<0.0.0>";
	}

	private threadCount() {
		var result = 0;
		for (var key in this.threadIDs) {
			result++;
		}
		return result;
	}

	protected log(msg: string): void {
		this.outLine(`${msg}\n`);
	}

	protected outLine(msg: string, category?: string): void {
		this.sendEvent(new OutputEvent(msg, category ? category : 'console'));
	}

	/** send message to console with color of debug category */
	public debug(msg: string): void {
		//other category can be 'console', 'stdout', 'stderr', 'telemetry'		
		this.outLine(`${msg}\n`, "debug");
	}

	public error(msg: string): void {
		this.outLine(`${msg}\n`, "stderr");
	}

	show(): void {
	}

	appendLine(value: string): void {
		this.log(value);
	}

	append(value: string): void {
		this.outLine(`${value}`);
	}

	//----------- events from erlangConnection
	private onNewModule(moduleName: string): void {
		this.debug("OnNewModule : " + moduleName);
		this.sendEvent(new ModuleEvent("new", new Module(moduleName, moduleName)))
	}

	private onNewBreak(breakName: string): void {
		this.debug("OnNewBreak : " + breakName);
		//this.sendEvent(new StoppedEvent("breakpoint", ));
		//this.sendEvent(new ModuleEvent("new", new Module(moduleName, moduleName)))	
	}

	private pid_to_number(processName: string): number {
		var pidAsString: string = processName.substr(1, processName.length - 2);
		pidAsString = pidAsString.replace(".", "");

		return Number.parseInt(pidAsString);
	}

	private onNewProcess(processName: string): void {
		//each process in erlang is mapped to one 'thread'
		this.debug("OnNewProcess : " + processName);
		var thid = this.pid_to_number(processName);
		this.threadIDs[processName] = {thid:thid, stack:null};
		this.sendEvent(new ThreadEvent("started", thid));
	}

	private onBreak(processName: string, module: string, line: string, snapshot: any) {
		this.debug(`onBreak : ${processName} snapshot:${snapshot}`);
		var thid = this.pid_to_number(processName);
		if (this.threadIDs[processName]) {
			this.threadIDs[processName].stack = snapshot;
		}
		this.sendEvent(new StoppedEvent("breakpoint", thid));
	}

	private onNewStatus(processName: string, status: string, reason: string, moudleName: string, line: string) {
		this.debug("OnStatus : " + processName + "," + status);
		if (status === 'exit') {
			var thid = this.pid_to_number(processName);
			delete this.threadIDs[processName];
			this.sendEvent(new ThreadEvent("exited", thid));
			if (this.threadCount() == 0) {
				this.sendEvent(new TerminatedEvent());
			}
		}
	}
}

DebugSession.run(ErlangDebugSession);    
