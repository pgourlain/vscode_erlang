import {
	DebugSession, InitializedEvent, TerminatedEvent, StoppedEvent,
	OutputEvent, Thread, ThreadEvent, StackFrame, Scope, Source, Handles
	, Breakpoint, ModuleEvent, Module, ContinuedEvent, Variable
} from 'vscode-debugadapter';
import { DebugProtocol } from 'vscode-debugprotocol';
import { ErlangShellForDebugging, IErlangShellOutputForDebugging } from './ErlangShellDebugger';
import * as genericShell from './GenericShell';
import * as path from 'path';
import * as fs from 'fs';
import { EventEmitter } from 'events'
import * as http from 'http';
import * as vscode from 'vscode';
import * as erlang from './ErlangShell';
import { ErlangConnection, erlangBridgePath } from './ErlangConnection';

export interface LaunchRequestArguments {
	cwd: string;
	erlpath: string;
	arguments: string;
}

interface DebugVariable {
	name: string;
	value: string;
	children: Variable[];
}
/** this class is entry point of debugger  */
export class ErlangDebugSession extends DebugSession implements IErlangShellOutputForDebugging {

	protected threadIDs: { [processName: string]: {thid: number, stack:any, isBreak : boolean }};
	erlDebugger: ErlangShellForDebugging;
	erlangConnection: ErlangConnection;
	quit: boolean;
	//private _breakPoints = new Map<string, DebugProtocol.Breakpoint[]>();
    private _breakPoints : DebugProtocol.Breakpoint[];
    private _variableHandles: Handles<DebugVariable>;
    private _LaunchArguments : LaunchRequestArguments;
    private _port : number;

	public constructor() {
		super();
		this._variableHandles = new Handles<DebugVariable>();
		this.threadIDs = {};
		this.setDebuggerLinesStartAt1(true);
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
            ths.push(new Thread(thread.thid, "Process " + key));
		}
		response.body = {
			threads: ths
		};
		this.sendResponse(response);
	}
    
	protected dispatchRequest(request: DebugProtocol.Request): void {
		//uncomment to show the calling workflow of debuging session  
        //this.debug(`dispatch request: ${request.command}(${JSON.stringify(request.arguments) })`);
        super.dispatchRequest(request);
    }

	protected initializeRequest(response: DebugProtocol.InitializeResponse, args: DebugProtocol.InitializeRequestArguments): void {
		//this.debug("Initializing erlang debugger");
		this.erlDebugger = new ErlangShellForDebugging(this);
		this.erlDebugger.on('close', (exitCode) => {
			this.quitEvent(exitCode);
		})
		this.erlangConnection = new ErlangConnection(this);
		this.erlangConnection.on("new_module", (arg) => this.onNewModule(arg));
		this.erlangConnection.on("new_break", (arg) => this.onNewBreak(arg));
		this.erlangConnection.on("new_process", (arg) => this.onNewProcess(arg));
		this.erlangConnection.on("new_status", (pid, status, reason, moduleName, line) => this.onNewStatus(pid, status, reason, moduleName, line));
		this.erlangConnection.on("on_break", (pid, moduleName, line, stacktrace) => this.onBreak(pid, moduleName, line, stacktrace));

		response.body.supportsConfigurationDoneRequest = true;
		response.body.supportsConditionalBreakpoints = false;
		response.body.supportsFunctionBreakpoints = false;
		response.body.supportsEvaluateForHovers = false;
		response.body.supportsSetVariable = false;
		response.body.supportsStepBack = false;
		response.body.exceptionBreakpointFilters = [];
        /*
        */
		this.sendResponse(response);
		// since this debug adapter can accept configuration requests like 'setBreakpoint' at any time,
		// we request them early by sending an 'initializeRequest' to the frontend.
		// The frontend will end the configuration sequence by calling 'configurationDone' request.
		//this.sendEvent(new InitializedEvent());
	}

	protected launchRequest(response: DebugProtocol.LaunchResponse, args: LaunchRequestArguments): void {
        //store launch arguments in order to start erlang when configuration is done
        if (!args.erlpath) {
            args.erlpath = "erl";
        } else if (!fs.exists(args.erlpath)) {
            this.log("The specified erlPath in your launch.json is invalid. Please fix !")
            this.sendErrorResponse(response, 3000, `The specified erlPath is invalid : check your launch configuration.`);
            return;
        }
        this._LaunchArguments = args;
        this.erlangConnection.Start().then(port => {
            this.debug("Local webserver for erlang is started");
            this._port = port;
            //Initialize the workflow only when webserver is started
            this.sendEvent(new InitializedEvent());
            this.sendResponse(response);
        }).catch(reason =>{
            this.sendErrorResponse(response, 3000, `Launching debugger throw an error : ${reason}`);
        });
	}

	protected configurationDoneRequest(response: DebugProtocol.ConfigurationDoneResponse, argsConf: DebugProtocol.ConfigurationDoneArguments): void {
        var args = this._LaunchArguments;
        this.debug("Starting erlang");
        this.debug(`	path      : ${args.cwd}`);
        this.debug(`	arguments : ${args.arguments}`);
        this.erlDebugger.Start(args.erlpath, args.cwd, this._port, erlangBridgePath, args.arguments).then(r => {
            this.sendResponse(response);
        }).catch(reason =>{
            this.sendErrorResponse(response, 3000, `Launching application throw an error : ${reason}`);
        });
	}
    

	protected disconnectRequest(response: DebugProtocol.DisconnectResponse, args: DebugProtocol.DisconnectArguments): void {
		this.debug("disconnectRequest");
		if (!this.quit) {
			//send q() only if user clic on stop debugger button
			this.erlDebugger.NormalQuit();
		}
		if (this.erlangConnection) {
			this.erlangConnection.Quit();
		}
        this.sendResponse(response);
	}


	protected quitEvent(exitCode: number) {
		this.log(`erl exit with code ${exitCode}`);
		this.quit = true;
		this.sendEvent(new TerminatedEvent());
	}

	protected evaluateRequest(response: DebugProtocol.EvaluateResponse, args: DebugProtocol.EvaluateArguments): void {
		this.log("evaluateRequest");
		//send entire expression entered in debugger console wend
		this.erlDebugger.Send(args.expression);
		response.body = {
			result: 'sending to erlang...',
			variablesReference: 0
		};
		this.sendResponse(response);
	}

	protected setBreakPointsRequest(response: DebugProtocol.SetBreakpointsResponse, args: DebugProtocol.SetBreakpointsArguments) {
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
        this._breakPoints = vscodeBreakpoints;
		this.sendResponse(response);
	}

	protected setExceptionBreakPointsRequest(response: DebugProtocol.SetExceptionBreakpointsResponse, args: DebugProtocol.SetExceptionBreakpointsArguments): void {
		//this.debug("setExceptionBreakPointsRequest : " + JSON.stringify(<any>args));	
		this.sendResponse(response);
	}

	//--- set function breakpoints request ------------------------------------------------------------------------------------

	protected setFunctionBreakPointsRequest(response: DebugProtocol.SetFunctionBreakpointsResponse, args: DebugProtocol.SetFunctionBreakpointsArguments): void {
		//this.debug("setFunctionBreakPointsRequest :" + JSON.stringify(<any>args));
		this.sendResponse(response);
	}

    private doProcessUserRequest(threadId : number, response: DebugProtocol.Response, fn: (pid : string) => Promise<boolean>) {
        this.sendResponse(response);
        fn(this.thread_id_to_pid(threadId)).then(
            () => {
                this.sendEvent(new ContinuedEvent(threadId));                
            },
            (reason) => {
				this.error("unable to continue debugging.")
				this.sendEvent(new TerminatedEvent());
                this.sendErrorResponse(response, 3000, `Unable to continue debugging : ${reason}`);
            });
    }

	protected continueRequest(response: DebugProtocol.ContinueResponse, args: DebugProtocol.ContinueArguments): void {
        this.doProcessUserRequest(args.threadId, response, (pid:string) => this.erlangConnection.debuggerContinue(pid));
	}

	protected nextRequest(response: DebugProtocol.NextResponse, args: DebugProtocol.NextArguments): void {
        this.doProcessUserRequest(args.threadId, response, (pid:string) => this.erlangConnection.debuggerNext(pid));
	}
	protected stepInRequest(response: DebugProtocol.StepInResponse, args: DebugProtocol.StepInArguments): void {
        this.doProcessUserRequest(args.threadId, response, (pid:string) => this.erlangConnection.debuggerStepIn(pid));
	}
	protected stepOutRequest(response: DebugProtocol.StepOutResponse, args: DebugProtocol.StepOutArguments): void {
		//this.debug("stepoOutTraceRequest");
		super.stepOutRequest(response, args);
	}

    protected scopesRequest(response: DebugProtocol.ScopesResponse, args: DebugProtocol.ScopesArguments): void {
        var vars = [];
        var frameId = Math.floor(args.frameId / 100000);
        var threadId = (args.frameId - frameId * 100000);
        //this.debug(`${threadId} : ${frameId}`);
        var processName = this.thread_id_to_pid(threadId);
		var that = this;
        this.erlangConnection.debuggerBindings(processName, frameId.toString()).then( v => {
            //this.debug(`get bindings ok : ${JSON.stringify(v)}`);
			vars = v;
			var scopes = new Array<Scope>();
			var localVariables  : DebugVariable = {
				name: 'Local',
				value: '',
				children: vars
			};
			scopes.push(new Scope('Local', this._variableHandles.create(localVariables), false));
			response.body = { scopes };
			this.sendResponse(response);
        });
    }

    protected variablesRequest(response: DebugProtocol.VariablesResponse, args: DebugProtocol.VariablesArguments): void {
		const variables = [];
		const id = this._variableHandles.get(args.variablesReference);
		if (id != null) {
			for (var i =0; i< id.children.length; i++) {
				var v = id.children[i];
				variables.push({
					name: v.name,
					type: "string",
					value: v.value,
					variablesReference: 0
				});
				
			}
		}
		response.body = {
			variables: variables
		};
		this.sendResponse(response);		
	}


	protected stackTraceRequest(response: DebugProtocol.StackTraceResponse, args: DebugProtocol.StackTraceArguments): void {
		//this.debug("stackTraceRequest :" + JSON.stringify(args));
		var processName = this.thread_id_to_pid(args.threadId);
		var thread = this.threadIDs[processName];
		var stackAsObject = <Array<any>>thread.stack;
		if (thread && stackAsObject) {
			//"{\"process\":\"<0.97.0>\", \"initial_call\":\"zcmailchecker_app/start/2\", \"break_module\":\"zcmailchecker_app\", \"line\":10}"
			//first step -> just show one frame
			const frames = new Array<StackFrame>();
			const startFrame = typeof args.startFrame === 'number' ? args.startFrame : 0;
			const maxLevels = args.levels;
			const endFrame = Math.min(startFrame + maxLevels, stackAsObject.length);

			for (let i= startFrame; i < endFrame; i++) {
                var frame = stackAsObject[i];
				const name = frame.module;
                const sourceFile = frame.source;
                const line = frame.line;
				frames.push(new StackFrame(frame.sp*100000+thread.thid, `${name}(${line})`, new Source(path.basename(sourceFile),
					this.convertDebuggerPathToClient(sourceFile)),
					this.convertDebuggerLineToClient(line), 0));
                
            }
			response.body = {
				stackFrames: frames,
				totalFrames: endFrame-startFrame
			};
			this.sendResponse(response);
		}
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
		var result = super.convertDebuggerLineToClient(line);
		//this.debug(`convertDebuggerLineToClient ${line}:${result}`);
		return result;
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
		//this.debug("OnNewModule : " + moduleName);
		this.sendEvent(new ModuleEvent("new", new Module(moduleName, moduleName)))
	}

	private onNewBreak(breakName: string): void {
		//this.debug("OnNewBreak : " + breakName);
	}

	private pid_to_number(processName: string): number {
		var pidAsString: string = processName.substr(1, processName.length - 2);
		pidAsString = pidAsString.replace(".", "");

		return Number.parseInt(pidAsString);
	}

	private onNewProcess(processName: string): void {
		//each process in erlang is mapped to one 'thread'
		//this.debug("OnNewProcess : " + processName);
		var thid = this.pid_to_number(processName);
		this.threadIDs[processName] = {thid:thid, stack:null, isBreak:false};
		this.sendEvent(new ThreadEvent("started", thid));
	}

	private onBreak(processName: string, module: string, line: string, stacktrace: any) {
		//this.debug(`onBreak : ${processName} stacktrace:${stacktrace}`);
		var currentThread = this.threadIDs[processName];
		if (currentThread) {
			currentThread.stack = stacktrace;
            currentThread.isBreak = true;
            var breakReason = "breakpoint";
            if (!this.isOnBreakPoint(module, line)) {
                breakReason = "step";
            }
            //this._breakPoints.forEach()
			this.sendEvent(new StoppedEvent(breakReason, currentThread.thid));
		}
	}

    private isOnBreakPoint(module: string, line : string) : boolean {
        var nLine = Number(line);
        var candidates = this._breakPoints.filter(bp => {
            return bp.line == nLine && bp.source.name == module;
        });
        return candidates.length > 0;
    }

	private onNewStatus(processName: string, status: string, reason: string, moudleName: string, line: string) {
		//this.debug("OnStatus : " + processName + "," + status);
		if (status === 'exit') {
			var currentThread = this.threadIDs[processName];
			delete this.threadIDs[processName];
			this.sendEvent(new ThreadEvent("exited", currentThread.thid));
			var thCount = this.threadCount(); 
			if (thCount == 0) {
				this.sendEvent(new TerminatedEvent());
			} else {
				this.debug(`thcount:${thCount}, ${JSON.stringify(this.threadIDs)}`);
			}
		}
	}
}
