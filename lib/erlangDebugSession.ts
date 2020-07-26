import {
	DebugSession, InitializedEvent, TerminatedEvent, StoppedEvent,
	OutputEvent, Thread, ThreadEvent, StackFrame, Scope, Source, Handles
	, Breakpoint, ModuleEvent, Module, ContinuedEvent, Variable, BreakpointEvent
} from 'vscode-debugadapter';
import { DebugProtocol } from 'vscode-debugprotocol';
import { ErlangShellForDebugging, LaunchRequestArguments, FunctionBreakpoint } from './ErlangShellDebugger';
import { ILogOutput } from './GenericShell';
import * as path from 'path';
import * as fs from 'fs';
import { EventEmitter } from 'events'
import * as http from 'http';
import * as erlang from './ErlangShell';
import { erlangBridgePath } from './erlangConnection';
import { ErlangDebugConnection } from './erlangDebugConnection';

interface DebugVariable {
	name: string;
	value: string;
	type?: string;
	variablesReference: number,
	children: Variable[];
}

class ConditionalBreakpoint {
	condition: string;
	hitCount: number;
	actualHitCount: number;
	constructor(condition: string, hitCondition: string) {
		this.condition = condition;
		this.hitCount = parseInt(hitCondition);
		this.actualHitCount = 0;
	}
}

/** this class is entry point of debugger  */
export class ErlangDebugSession extends DebugSession implements ILogOutput {

	protected threadIDs: { [processName: string]: {thid: number, stack:any, vscode: boolean}};
	erlDebugger: ErlangShellForDebugging;
	erlangConnection: ErlangDebugConnection;
	quit: boolean;
	//private _breakPoints = new Map<string, DebugProtocol.Breakpoint[]>();
	private _rebarBuildPath = path.join("_build", "default", "lib");
	private _breakPoints: DebugProtocol.Breakpoint[];
	private _functionBreakPoints: Map<string, FunctionBreakpoint[]> = new Map();
	private _conditionalBreakPoints: Map<string, Map<number, ConditionalBreakpoint>> = new Map();
	private _variableHandles: Handles<DebugVariable>;
	private _LaunchArguments: LaunchRequestArguments;
	private _port: number;

	public constructor(verbose: boolean) {
		super();
		this._breakPoints = [];
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
			if (thread.vscode) {
				ths.push(new Thread(thread.thid, "Process " + key));
			}
		}
		response.body = {
			threads: ths
		};
		this.sendResponse(response);
	}
		
	protected dispatchRequest(request: DebugProtocol.Request): void {
		//uncomment to show the calling workflow of debuging session  
		//this.debug(`dispatch request: ${request.command}(${JSON.stringify(request.arguments) })\r\n`);
		super.dispatchRequest(request);
	}

	protected initializeRequest(response: DebugProtocol.InitializeResponse, args: DebugProtocol.InitializeRequestArguments): void {
		//this.debug("Initializing erlang debugger");		
		this.erlDebugger = new ErlangShellForDebugging(this);
		this.erlDebugger.on('close', (exitCode) => {
			this.quitEvent(exitCode);
		})
		this.erlangConnection = new ErlangDebugConnection(this);
		this.erlangConnection.on("listen", (msg) => this.onStartListening(msg));
		this.erlangConnection.on("new_module", (arg) => this.onNewModule(arg));
		this.erlangConnection.on("new_break", (arg) => this.onNewBreak(arg));
		this.erlangConnection.on("new_process", (arg) => this.onNewProcess(arg));
		this.erlangConnection.on("new_status", (pid, status, reason, moduleName, line) => this.onNewStatus(pid, status, reason, moduleName, line));
		this.erlangConnection.on("on_break", (pid, moduleName, line, stacktrace) => this.onBreak(pid, moduleName, line, stacktrace));
		this.erlangConnection.on("fbp_verified", (moduleName, functionName, arity) => this.onFbpVerified(moduleName, functionName, arity));

		response.body.supportsConfigurationDoneRequest = true;
		response.body.supportsConditionalBreakpoints = true;
		response.body.supportsHitConditionalBreakpoints = true;
		response.body.supportsFunctionBreakpoints = true;
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
		} else if (!fs.existsSync(args.erlpath)) {
				this.log("The specified erlPath in your launch.json is invalid. Please fix !")
				this.sendErrorResponse(response, 3000, `The specified erlPath is invalid : check your launch configuration.`);
				return;
		}
		if (typeof args.addEbinsToCodepath === "undefined") {
			args.addEbinsToCodepath = true;
		}
		this._LaunchArguments = args;
		if (this._LaunchArguments.verbose) {
			this.log(`debugger launchRequest arguments : ${JSON.stringify(args)}`);
		}
		this.erlangConnection.Start(this._LaunchArguments.verbose).then(port => {
			//this.debug("Local webserver for erlang is started");
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
		if (args.verbose) {
			this.debug("Starting erlang");
			this.debug(`	path      : ${args.cwd}`);
			this.debug(`	arguments : ${args.arguments}`);
		}
		this.erlDebugger.erlangPath = args.erlangPath;
		//path of erl_connection.beam not compiled with lsp, because we don't that the target access to lsp_xx.beam
		var bridgeBinPath = path.normalize(path.join(erlangBridgePath, "..", "ebin"))
		this.erlDebugger.Start(args.erlpath, args.cwd, this._port, bridgeBinPath, args).then(r => {
			this.sendResponse(response);
		}).catch(reason =>{
			this.sendErrorResponse(response, 3000, `Launching application throw an error : ${reason}`);
		});
	}
		

	protected disconnectRequest(response: DebugProtocol.DisconnectResponse, args: DebugProtocol.DisconnectArguments): void {
		//this.debug("disconnectRequest");
		if (this.erlangConnection) {
			this.erlangConnection.Quit();
		}
		this.sendResponse(response);
		this.erlDebugger.CleanupAfterStart();
	}


	protected quitEvent(exitCode: number) {
		this.log(`erl exit with code ${exitCode}`);
		this.quit = true;
		this.sendEvent(new TerminatedEvent());
		this.erlDebugger.CleanupAfterStart();
	}

	protected evaluateRequest(response: DebugProtocol.EvaluateResponse, args: DebugProtocol.EvaluateArguments): void {
		//this.log("evaluateRequest");
		//send entire expression entered in debugger console wend
		if (this.erlangConnection.isConnected) {
			var frameId = Math.floor(args.frameId / 100000);
			var threadId = (args.frameId - frameId * 100000);
			var processName = this.thread_id_to_pid(threadId);
			this.erlangConnection.debuggerEval(processName, frameId.toString(), args.expression).then((res) => {
				var result = this.mapRawVariables(res);
				response.body = {
					result: result.value,
					type: result.type,
					variablesReference: result.variablesReference
				};
				this.sendResponse(response);
			});
		}
	}

	protected setBreakPointsRequest(response: DebugProtocol.SetBreakpointsResponse, args: DebugProtocol.SetBreakpointsArguments) {
		//it's called 1 per source file
		// this is returned to VS Code
		let vscodeBreakpoints: Breakpoint[];
		vscodeBreakpoints = [];
		//this.debug("setbreakpoints : " + JSON.stringify(<any>args));
		let targetModuleName = path.basename(args.source.path, ".erl");
		this._conditionalBreakPoints.delete(targetModuleName);
		args.breakpoints.forEach(bp => {
			vscodeBreakpoints.push(new Breakpoint(true, bp.line, 1, new Source(targetModuleName, args.source.path)));
			if (bp.condition || bp.hitCondition) {
				if (!this._conditionalBreakPoints.has(targetModuleName)) {
					this._conditionalBreakPoints.set(targetModuleName, new Map());
				}
				var cbp = new ConditionalBreakpoint(bp.condition, bp.hitCondition);
				this._conditionalBreakPoints.get(targetModuleName).set(bp.line, cbp);
			}
		});
		this._updateBreakPoints(targetModuleName, vscodeBreakpoints);
		this._setAllBreakpoints(targetModuleName);
		response.body = { breakpoints: vscodeBreakpoints };
		this.sendResponse(response);
	}

	private _setAllBreakpoints(moduleName : string) {
		let modulebreakpoints = this._breakPoints.filter((bp) => bp.source.name == moduleName);
		let moduleFunctionBreakpoints = this._functionBreakPoints.has(moduleName) ? this._functionBreakPoints.get(moduleName) : [];
		if (this.erlangConnection.isConnected) {
			if (!this._LaunchArguments.noDebug) {
				this.erlangConnection.setBreakPointsRequest(moduleName, modulebreakpoints, moduleFunctionBreakpoints);
			}
		} else if (this.erlDebugger) {
			this.erlDebugger.setBreakPointsRequest(modulebreakpoints, moduleFunctionBreakpoints);
		}
	}

	private _updateBreakPoints(moduleName : string, bps : Breakpoint[]) {
		let newBps = this._breakPoints.filter((bp) => bp.source.name != moduleName);
		this._breakPoints = newBps.concat(bps);
	}

	protected setExceptionBreakPointsRequest(response: DebugProtocol.SetExceptionBreakpointsResponse, args: DebugProtocol.SetExceptionBreakpointsArguments): void {
		//this.debug("setExceptionBreakPointsRequest : " + JSON.stringify(<any>args));	
		this.sendResponse(response);
	}

	protected setFunctionBreakPointsRequest(response: DebugProtocol.SetFunctionBreakpointsResponse, args: DebugProtocol.SetFunctionBreakpointsArguments): void {
		let vscodeBreakpoints: Breakpoint[] = [];
		let modulesToSetBreakpoints = new Set<string>();
		let re = new RegExp("^(.+):(.+)/([0-9]+)$");
		this._functionBreakPoints.forEach((breakpoints, moduleName) => modulesToSetBreakpoints.add(moduleName));
		this._functionBreakPoints = new Map();
		args.breakpoints.forEach(bp => {
			let parsed = re.exec(bp.name);
			if (parsed) {
				let moduleName = parsed[1];
				let breakpoint = new FunctionBreakpoint((<any>bp).id, bp.name, moduleName, parsed[2], +parsed[3]);
				vscodeBreakpoints.push(breakpoint);
				if (this._functionBreakPoints.has(moduleName))
					this._functionBreakPoints.get(moduleName).push(breakpoint);
				else
					this._functionBreakPoints.set(moduleName, [breakpoint]);
			}
			else {
				let name = bp.name;
				if (!name.endsWith(" (Invalid format)")) {
					name += " (Invalid format)";
				}
				let breakpoint = new FunctionBreakpoint("", name, "", "", 0);
				vscodeBreakpoints.push(breakpoint);
			}
		});
		this._functionBreakPoints.forEach((breakpoints, moduleName) => modulesToSetBreakpoints.add(moduleName));
		modulesToSetBreakpoints.forEach((moduleName) => this._setAllBreakpoints(moduleName));
		response.body = { breakpoints: vscodeBreakpoints };
		this.sendResponse(response);
	}

	private doProcessUserRequest(threadId : number, response: DebugProtocol.Response, fn: (pid : string) => Promise<boolean>) {
		this.sendResponse(response);
		fn(this.thread_id_to_pid(threadId)).then(
			() => {
				this.sendEvent(new ContinuedEvent(threadId, false));
			},
			(reason) => {
				this.error("unable to continue debugging.")
				this.sendEvent(new TerminatedEvent());
				this.sendErrorResponse(response, 3000, `Unable to continue debugging : ${reason}`);
			});
	}

	protected continueRequest(response: DebugProtocol.ContinueResponse, args: DebugProtocol.ContinueArguments): void {
		response.body = { allThreadsContinued: false };
		this.doProcessUserRequest(args.threadId, response, (pid:string) => this.erlangConnection.debuggerContinue(pid));
	}

	protected nextRequest(response: DebugProtocol.NextResponse, args: DebugProtocol.NextArguments): void {
		this.doProcessUserRequest(args.threadId, response, (pid:string) => this.erlangConnection.debuggerNext(pid));
	}
	protected stepInRequest(response: DebugProtocol.StepInResponse, args: DebugProtocol.StepInArguments): void {
		this.doProcessUserRequest(args.threadId, response, (pid:string) => this.erlangConnection.debuggerStepIn(pid));
	}
	protected stepOutRequest(response: DebugProtocol.StepOutResponse, args: DebugProtocol.StepOutArguments): void {
		var processName = this.thread_id_to_pid(args.threadId);
		var thread = this.threadIDs[processName];
		if (thread && thread.stack && thread.stack.length == 1) {
			this.doProcessUserRequest(args.threadId, response, (pid:string) => this.erlangConnection.debuggerContinue(pid));	
		}
		else {
			this.doProcessUserRequest(args.threadId, response, (pid:string) => this.erlangConnection.debuggerStepOut(pid));
		}
	}

	protected pauseRequest(response: DebugProtocol.PauseResponse, args: DebugProtocol.PauseArguments): void {
		//this.debug("pauseRequest :" + JSON.stringify(args));
		this.sendResponse(response);
		this.erlangConnection.debuggerPause(this.thread_id_to_pid(args.threadId)).then(
			() => {
			},
			(reason) => {
				this.error("unable to pause.")
				this.sendErrorResponse(response, 3000, `Unable to pause : ${reason}`);
			});
	}

	protected scopesRequest(response: DebugProtocol.ScopesResponse, args: DebugProtocol.ScopesArguments): void {
		//this.debug("scopesRequest :" + JSON.stringify(args));
		var vars = [];
		var frameId = Math.floor(args.frameId / 100000);
		var threadId = (args.frameId - frameId * 100000);
		//this.debug(`${threadId} : ${frameId}`);
		var processName = this.thread_id_to_pid(threadId);
		var that = this;
		this.erlangConnection.debuggerBindings(processName, frameId.toString()).then( v => {
		vars = v.map((el: any) => this.mapRawVariables(el));
			var scopes = new Array<Scope>();
			var localVariables: DebugVariable = {
				name: 'Local',
				value: '',
				variablesReference: 0,
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
			id.children.forEach((el) => variables.push(el));
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

			for (let i = startFrame; i < endFrame; i++) {
				var frame = stackAsObject[i];
				const name = frame.func;
				const sourceFile = frame.source;
				const line = frame.line;
				if (fs.existsSync(sourceFile)) {
					frames.push(new StackFrame(frame.sp * 100000 + thread.thid, name,
						new Source(frame.module, this.convertDebuggerPathToClient(sourceFile)),
						this.convertDebuggerLineToClient(line), 0));
				}
				else {
					frames.push(new StackFrame(frame.sp * 100000 + thread.thid, path.basename(sourceFile), null, <any>""));
				}
			}
			response.body = {
				stackFrames: frames,
				totalFrames: endFrame - startFrame
			};
			this.sendResponse(response);
		}
		else {
			response.body = {
				stackFrames: [],
				totalFrames: 0
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
		var relative = path.relative(this._LaunchArguments.cwd, debuggerPath);
		var ret = super.convertDebuggerPathToClient(debuggerPath);
		if (relative.startsWith(this._rebarBuildPath)) {
			relative = path.relative(path.join(this._LaunchArguments.cwd, this._rebarBuildPath), debuggerPath);
			if (fs.existsSync(path.join(this._LaunchArguments.cwd, "apps", relative))) {
				ret = path.join(this._LaunchArguments.cwd, "apps", relative);
			}
			else {
				var basedirname = path.parse(this._LaunchArguments.cwd).base;
				if (relative.startsWith(basedirname + path.sep)) {
					var converted = path.join(this._LaunchArguments.cwd, "..", relative);
					if (fs.existsSync(converted)) {
						ret = converted;
					}
				}
			}
		}
		return ret;
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

	private mapRawVariables(v: any): DebugVariable {
		var handler: number = 0;
		if (v.children) { 
			handler = this._variableHandles.create({
				name: v.name,
				value: v.value,
				type: v["type"],
				variablesReference: 0,
				children: <Array<Variable>> v.children.map((el: any) => this.mapRawVariables(el))
			})
		}
		return <DebugVariable> {
			name: v.name,
			type: v["type"],
			value: v.value,
			variablesReference: handler
		};
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
	private onStartListening(message: string): void {
		if (this._LaunchArguments.verbose)
			this.debug(message);
	}
	
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
		this.threadIDs[processName] = {thid:thid, stack:null, vscode: false};
		var that = this;
		setTimeout(function () {
			that.sendThreadStartedEventIfNeeded(processName);
		}, 250);
	}

	private sendThreadStartedEventIfNeeded(processName: string) {
		var thread = this.threadIDs[processName];
		if (thread && !thread.vscode) {
			thread.vscode = true;
			this.sendEvent(new ThreadEvent("started", thread.thid));
		}
	}

	private breakReason(module: string, line: string) {
		if (this.isOnBreakPoint(module, line)) {
			return "breakpoint";
		}
		return "";
	}

	private findConditionalBreakpoint(module: string, line: string) : ConditionalBreakpoint {
		if (this._conditionalBreakPoints.has(module)) {
			var moduleConditionalBreakpoints = this._conditionalBreakPoints.get(module);
			var lineNo = parseInt(line);
			if (moduleConditionalBreakpoints.has(lineNo)) {
				return moduleConditionalBreakpoints.get(lineNo);
			}
		}
		return null;
	}

	private conditionalBreakpointHit(cbp: ConditionalBreakpoint, processName: string, module: string, line: string, thid: number) {
		if (isNaN(cbp.hitCount)) {
			this.sendEvent(new StoppedEvent(this.breakReason(module, line), thid));
		}
		else {
			if (++cbp.actualHitCount == cbp.hitCount) {
				this.sendEvent(new StoppedEvent(this.breakReason(module, line), thid));
			}
			else {
				this.erlangConnection.debuggerContinue(processName);				
			}
		}
	}

	private onBreak(processName: string, module: string, line: string, stacktrace: any) {
		//this.debug(`onBreak : ${processName} stacktrace:${JSON.stringify(stacktrace)}`);
		this.sendThreadStartedEventIfNeeded(processName);
		var currentThread = this.threadIDs[processName];
		if (currentThread) {
			currentThread.stack = stacktrace;
			var cbp = this.findConditionalBreakpoint(module, line);
			if (cbp) {
				if (cbp.condition) {
					var sp = stacktrace && stacktrace.length > 0 ? stacktrace[0].sp : -1;
					this.erlangConnection.debuggerEval(processName, sp, cbp.condition).then((res) => {
						if (res.value == "true") {
							this.conditionalBreakpointHit(cbp, processName, module, line, currentThread.thid);
						}
						else {
							this.erlangConnection.debuggerContinue(processName);
						}
					});
				}
				else {
					this.conditionalBreakpointHit(cbp, processName, module, line, currentThread.thid);					
				}
			}
			else {
				this.sendEvent(new StoppedEvent(this.breakReason(module, line), currentThread.thid));				
			}
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
			var that = this;
			//Use 125ms delay to mitigate case when a process spawns another one and exits
			//It is then possible to receive onNewStatus('exit') before onNewProcess for the spawned process
			setTimeout(function () {
				var currentThread = that.threadIDs[processName];
				delete that.threadIDs[processName];
				if (currentThread.vscode) {
					that.sendEvent(new ThreadEvent("exited", currentThread.thid));
				}
				var thCount = that.threadCount(); 
				if (thCount == 0) {
					that.sendEvent(new TerminatedEvent());
				} else {
					//that.debug(`thcount:${thCount}, ${JSON.stringify(that.threadIDs)}`);
				}
			}, 125);
		}
	}

	private onFbpVerified(moduleName: string, functionName:string, arity:number) {
		if (this._functionBreakPoints.has(moduleName)) {
			this._functionBreakPoints.get(moduleName).forEach((fbp) => {
					if (fbp.functionName === functionName && fbp.arity === arity) {
						fbp.verified = true;
						this.sendEvent(new BreakpointEvent("changed", fbp));
					}
				}
			);
		}
	}
}
