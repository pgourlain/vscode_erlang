
import {
    CancellationToken, Event, ExtensionContext, InlayHint, InlayHintsProvider,
    OutputChannel, ProviderResult, TextDocument, languages,
    workspace as Workspace,
    InlayHintKind, Range, Position
} from 'vscode';
import { clientIsReady, erlangDocumentSelector } from './lsp-context';
import { RequestType, TextDocumentIdentifier } from 'vscode-languageclient';
import { client } from './lspclientextension';
import * as proto from 'vscode-languageserver-protocol';

export function activate(context: ExtensionContext, lspOutputChannel: OutputChannel) {
    //inlay should be registered only if config enabled
    // const enabledSetting = 'editor.inlayHints.enabled';
    const inlay = languages.registerInlayHintsProvider(erlangDocumentSelector,
        new ErlangInlayHintsProvider(lspOutputChannel));
    context.subscriptions.push(inlay);
}

namespace protocol {
    export interface ErlangInlayHint {
        range: proto.Range;
        position?: proto.Position;
        kind: string;
        label: string;
    }

    export interface InlayHintsParams {
        textDocument: TextDocumentIdentifier;
        range?: proto.Range;
    }
    export namespace InlayHintsRequest {
        export const type =
            new RequestType<InlayHintsParams, ErlangInlayHint[], void>(
                'textDocument/inlayHints');
    }
}

//global activation
let inlayHintsEnabled = false;
//TODO: like C#, it can be useful to have several inlayHints type configuration
// let inlayHintsinLoop, ....
let autosave = Workspace.getConfiguration("files").get<string>("autoSave") == "afterDelay";

export function configurationChanged(): void {
    inlayHintsEnabled = Workspace.getConfiguration("erlang").get<boolean>("inlayHintsEnabled");
    autosave = Workspace.getConfiguration("files").get<string>("autoSave") == "afterDelay";
}

export class ErlangInlayHintsProvider implements InlayHintsProvider {

    constructor(private lspOutputChannel: OutputChannel) {
    }

    onDidChangeInlayHints?: Event<void>;

    provideInlayHints(document: TextDocument, range: Range, token: CancellationToken):
        ProviderResult<InlayHint[]> {

        if (!inlayHintsEnabled) {
            this.lspOutputChannel.appendLine("inlayHints feature is disabled");
            return Promise.resolve([]);
        }
        if (!clientIsReady) {
            //client not ready => not connected to language server, so return empty
            return Promise.resolve([]);
        }
        const request: protocol.InlayHintsParams = {
            textDocument: { uri: document.uri.toString() },
            range: client.code2ProtocolConverter.asRange(range),
        };
        return this.sendRequest(request, token);
    }

    async sendRequest(request: protocol.InlayHintsParams, token: CancellationToken): Promise<InlayHint[]> {
        //send requiest to lsp textDocument/inlayHints
        let result = await client.sendRequest(protocol.InlayHintsRequest.type, request, token);
        //this.lspOutputChannel.appendLine(`receive from lsp => ${JSON.stringify(result)}`);
        return result.map(this.decode, this);
    }

    decodeKind(kind: string): InlayHintKind | undefined {
        if (kind == 'type')
            return InlayHintKind.Type;
        if (kind == 'parameter')
            return InlayHintKind.Parameter;
        return undefined;
    }

    decode(hint: protocol.ErlangInlayHint): InlayHint {
        return {
            position: client.protocol2CodeConverter.asPosition(hint.position!),
            kind: this.decodeKind(hint.kind),
            label: hint.label.trim(),
            paddingLeft: hint.label.startsWith(' '),
            paddingRight: hint.label.endsWith(' '),
        };
    }

    resolveInlayHint?(hint: InlayHint, token: CancellationToken): ProviderResult<InlayHint> {
        //TODO: add tooltip,...
        //called when mouse is over the InlayHint
        //this.lspOutputChannel.appendLine(`resolveInlayHint : ${JSON.stringify(hint)}`);
        return hint;
    }
}