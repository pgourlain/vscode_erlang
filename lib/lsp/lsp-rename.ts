
import {
    CancellationToken, Event, ExtensionContext, RenameProvider,
    OutputChannel, ProviderResult, TextDocument, languages,
    workspace as Workspace, Range, Position,
    WorkspaceEdit
} from 'vscode';
import { clientIsReady, erlangDocumentSelector } from './lsp-context';
import { RequestType, TextDocumentIdentifier, PrepareRenameSignature } from 'vscode-languageclient';
import { client, debugLog } from './lspclientextension';
import * as proto from 'vscode-languageserver-protocol';

export function activate(context: ExtensionContext, lspOutputChannel: OutputChannel) {
    //inlay should be registered only if config enabled
    // const enabledSetting = 'editor.inlayHints.enabled';
    // const inlay = languages.registerRenameProvider(erlangDocumentSelector,
    //     new ErlangRenameProvider(lspOutputChannel));
    // context.subscriptions.push(inlay);
}

export class ErlangRenameProvider implements RenameProvider {
    
    constructor(private lspOutputChannel: OutputChannel) {
    }

    provideRenameEdits(document: TextDocument, position: Position, newName: string, token: CancellationToken): ProviderResult<WorkspaceEdit> {
        throw new Error('Method provideRenameEdits not implemented.');
    }
    prepareRename?(document: TextDocument, position: Position, token: CancellationToken): ProviderResult<Range | { range: Range; placeholder: string; }> {
        debugLog(`onPrepareRename :`);
        throw new Error('Method prepareRename not implemented.');
        //return null;
    }

}