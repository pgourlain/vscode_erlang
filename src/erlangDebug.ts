import { DebugSession, InitializedEvent, TerminatedEvent, StoppedEvent, OutputEvent, Thread, StackFrame, Scope, Source, Handles } from 'vscode-debugadapter';
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
	erlDebugger : ErlangShellForDebugging;
	quit : boolean;

    public constructor(debuggerLinesStartAt1: boolean, isServer: boolean = false, threadID: number = 1) {
		super(debuggerLinesStartAt1, isServer);
		this.threadID = threadID;
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
		response.body.supportsConfigurationDoneRequest = true;
		response.body.supportsConditionalBreakpoints = true;
		response.body.supportsFunctionBreakpoints = true;
		response.body.supportsEvaluateForHovers = true;
		response.body.supportsSetVariable = true;
		response.body.supportsStepBack = true;
		this.sendResponse(response);
	}

    protected launchRequest(response: DebugProtocol.LaunchResponse, args: LaunchRequestArguments): void {
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
            
        this.log("launchrequest");
        this.erlDebugger.Send("q().");
        this.sendResponse(response);
        //this.sendErrorResponse(response, 102, `Not yet implemented, coming soon...stay tuned !`);
    }

	protected quitEvent(exitCode : number) {
		this.log(`erl exit with code ${exitCode}`);
		this.quit = true;
		this.sendEvent(new TerminatedEvent());
	}

    protected log(msg : string) : void {
        //const outputCategory = level === logger.LogLevel.Error ? 'stderr' : undefined;
		const outputCategory = undefined;
        this.sendEvent(new OutputEvent(`  ›${msg}\n`, outputCategory));
    }

	show() : void {

	}

	appendLine(value: string): void {
		this.log(value);
	}
}

DebugSession.run(ErlangDebugSession);    
