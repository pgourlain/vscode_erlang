import {
    CancellationToken, Event, ExtensionContext, InlineValue,InlineValueText, 
    InlineValueVariableLookup, InlineValueEvaluatableExpression, InlineValuesProvider,
    OutputChannel, ProviderResult, TextDocument, languages,
    workspace as Workspace, Range, Position, InlineValueContext
} from 'vscode';
import { clientIsReady, erlangDocumentSelector } from './lsp-context';
import { RequestType, TextDocumentIdentifier } from 'vscode-languageclient';
import { client } from './lspclientextension';
import * as proto from 'vscode-languageserver-protocol';

export function activate(context: ExtensionContext, lspOutputChannel: OutputChannel) {
    //inlay should be registered only if config enabled
    // const enabledSetting = 'editor.inlayHints.enabled';
    const values = languages.registerInlineValuesProvider(erlangDocumentSelector,
        new ErlangInlineValueProvider(lspOutputChannel));
    context.subscriptions.push(values);
}

namespace protocol {
    export interface ErlangInlineValue {
        range: proto.Range;
        position?: proto.Position;
        kind: string;
        label: string;
    }

    export interface InlineValuesParams {
        textDocument: TextDocumentIdentifier;
        range?: proto.Range;
        stoppedLocation?: proto.Range;
        frameId?: number;
        threadId?: number;
    }
    export namespace InlineValueRequest {
        export const type =
            new RequestType<InlineValuesParams, ErlangInlineValue[], void>(
                'textDocument/inlineValues');
    }
}

export class ErlangInlineValueProvider implements InlineValuesProvider
{
    constructor(private lspOutputChannel: OutputChannel) {
    }
    
    onDidChangeInlineValues?: Event<void>;
    provideInlineValues(document: TextDocument, viewPort: Range, context: InlineValueContext, token: CancellationToken): ProviderResult<InlineValue[]> {
        
        if (!clientIsReady) {
            //client not ready => not connected to language server, so return empty
            return Promise.resolve([]);
        }
        var frameId = Math.floor(context.frameId / 100000);
        var threadId = (context.frameId - frameId * 100000);
        //var processName = this.thread_id_to_pid(threadId);

        const request: protocol.InlineValuesParams = {
            textDocument: { uri: document.uri.toString() },
            range: client.code2ProtocolConverter.asRange(viewPort), 
            frameId: frameId,
            threadId: threadId,
            stoppedLocation: client.code2ProtocolConverter.asRange(context.stoppedLocation)
        };
        return this.sendRequest(request, token);
    }

    async sendRequest(request: protocol.InlineValuesParams, token: CancellationToken): Promise<InlineValue[]> {
        //send requiest to lsp textDocument/inlineValues
        let result = await client.sendRequest(protocol.InlineValueRequest.type, request, token);
        //this.lspOutputChannel.appendLine(`inlineValues params => ${JSON.stringify(request)}`);
        return result.map(this.decode, this);
    }

    decode(value: protocol.ErlangInlineValue): InlineValue {
        const P = client.protocol2CodeConverter.asPosition(value.position!);        
        const R = new Range(P, P);
        switch (value.kind) 
        {
            case 'text':
                return new InlineValueText(R, value.label);
            case 'var':
                return new InlineValueVariableLookup(R, value.label, true);
            case 'expression':
                return new InlineValueEvaluatableExpression(R, value.label);       
            default:
                return new InlineValueText(R, "unknown kind of inlinevalue");
        }
    }

}