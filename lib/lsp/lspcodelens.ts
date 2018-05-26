import {
	workspace as Workspace,
	Uri, Disposable, WorkspaceConfiguration, CodeLens as VSCodeLens, Command as VSCommand,
	ProviderResult, TextDocument, CancellationToken, Range as VSCodeRange, Position as VSCodePosition
} from 'vscode';

import {
	CodeLensRequest, CodeLensRegistrationOptions, CodeLensParams, CodeLens,
	TextDocumentIdentifier, Command, Range, Position
} from 'vscode-languageclient';

import { debugLog, client } from './lspclientextension';
import URI from 'vscode-uri';
import { EventEmitter } from 'events';

class DocumentValidatedEvent extends EventEmitter {

    public Fire() {
        this.emit("documentValidated");
    }
}

let codeLensEnabled = false;
let autosave = Workspace.getConfiguration("files").get<string>("autoSave")=="afterDelay";
let documentValidatedEvent = new DocumentValidatedEvent();

export function configurationChanged() : void {
	codeLensEnabled = Workspace.getConfiguration("erlang").get<boolean>("codeLensEnabled");
	autosave = Workspace.getConfiguration("files").get<string>("autoSave")=="afterDelay";
}
export async function onProvideCodeLenses(document: TextDocument, token: CancellationToken): Promise<ProviderResult<VSCodeLens[]>> {
	if (!codeLensEnabled) {
		return Promise.resolve([]);
	}
	if (autosave && document.isDirty) {
		//codeLens event is fire after didChange and before DidSave
		//So, when autoSave is on, Erlang document is validated on didSaved		
		return await new Promise<ProviderResult<VSCodeLens[]>>(a => {
			let fn = () =>
			{
				documentValidatedEvent.removeListener("documentValidated", fn);        
				a(internalProvideCodeLenses(document, token));        
			};
			documentValidatedEvent.addListener("documentValidated", fn);
		});
	}
	return await internalProvideCodeLenses(document, token);
}

async function internalProvideCodeLenses(document: TextDocument, token: CancellationToken): Promise<ProviderResult<VSCodeLens[]>> {
    //Send request for codeLens
	return await client.sendRequest<CodeLensParams, CodeLens[], void, CodeLensRegistrationOptions>(CodeLensRequest.type,
		<CodeLensParams>{
			textDocument: <TextDocumentIdentifier>{ uri: document.uri.toString() }
		},
		token).then(async (codelenses) => await codeLensToVSCodeLens(codelenses));
}

export function onResolveCodeLenses(codeLens: VSCodeLens): ProviderResult<VSCodeLens> {
	debugLog("onResolveCodeLenses");
	return codeLens;
}

export function onDocumentDidSave() : void {
	documentValidatedEvent.Fire();
}

function delay(ms: number) {
	return new Promise(resolve => setTimeout(resolve, ms));
}



async function codeLensToVSCodeLens(codelenses: CodeLens[]): Promise<VSCodeLens[]> {
    //debugLog(`convert codelens : ${JSON.stringify(codelenses)}`);
	return Promise.resolve(codelenses.map(V => asCodeLens(V)));
}

function asCommand(item: Command): VSCommand {
	let result: VSCommand = {
		title: item.title,
        command: item.command
	}

	if (item.arguments) { result.arguments = item.arguments.map(function (arg:any): any {
        if (arg.indexOf && arg.indexOf("file:") === 0)
            return URI.parse(arg);
        else
            return arg;
    }); }
	return result;
}

function asCodeLens(item: CodeLens): VSCodeLens {
	let result = new VSCodeLens(asRange(item.range));
	if (item.command) { result.command = asCommand(item.command); }
	return result;
}

function asRange(item: Range): VSCodeRange {
	return new VSCodeRange(asPosition(item.start), asPosition(item.end));
}

function asPosition(item: Position): VSCodePosition {
	return new VSCodePosition(item.line, item.character);
}