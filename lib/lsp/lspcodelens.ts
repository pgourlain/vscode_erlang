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

let codeLensEnabled = false;

export function configurationChanged() : void {
	codeLensEnabled = Workspace.getConfiguration("erlang").get<boolean>("codeLensEnabled");
}
export async function onProvideCodeLenses(document: TextDocument, token: CancellationToken): Promise<ProviderResult<VSCodeLens[]>> {
	if (!codeLensEnabled) {
		return Promise.resolve([]);
	}
	return await internalProvideCodeLenses(document, token);
}

async function internalProvideCodeLenses(document: TextDocument, token: CancellationToken): Promise<ProviderResult<VSCodeLens[]>> {
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

	if (item.arguments) { result.arguments = item.arguments; }
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