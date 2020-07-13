// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as path from 'path';
import {
    workspace as Workspace, window as Window, ExtensionContext, TextDocument, OutputChannel, WorkspaceFolder, Uri, debug,
    languages, IndentAction, DebugAdapterDescriptorFactory
} from 'vscode';

import * as Adapter from './vscodeAdapter';
import * as Rebar from './RebarRunner';
import * as Eunit from './eunitRunner';
import { ErlangDebugConfigurationProvider, configurationChanged, getElangConfigConfiguration } from './ErlangConfigurationProvider';
import { ErlangDebugAdapterDescriptorFactory, InlineErlangDebugAdapterFactory, ErlangDebugAdapterExecutableFactory } from './ErlangAdapterDescriptorFactory';
import * as erlangConnection from './erlangConnection';

import * as LspClient from './lsp/lspclientextension';

var myoutputChannel: OutputChannel;

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: ExtensionContext) {
    erlangConnection.setExtensionPath(context.extensionPath);

    myoutputChannel = Adapter.ErlangOutput();
    // Use the console to output diagnostic information (console.log) and errors (console.error)
    // This line of code will only be executed once when your extension is activated
    console.log('Congratulations, your extension "erlang" is now active!');
    myoutputChannel.appendLine("erlang extension is active");

    //configuration of erlang language -> documentation : https://code.visualstudio.com/Docs/extensionAPI/vscode-api#LanguageConfiguration
    var disposables = [];
    // The command has been defined in the package.json file
    // Now provide the implementation of the command with  registerCommand
    // The commandId parameter must match the command field in package.json
    //disposables.push(vscode.commands.registerCommand('extension.rebarBuild', () => { runRebarCommand(['compile']);}));

    var rebar = new Rebar.RebarRunner();
    rebar.activate(context);

    var eunit = new Eunit.EunitRunner();
    eunit.activate(context);

    disposables.push(debug.registerDebugConfigurationProvider("erlang", new ErlangDebugConfigurationProvider()));
    disposables.push(Workspace.onDidChangeConfiguration((e) => configurationChanged()));    
    let runMode = getElangConfigConfiguration().debuggerRunMode;
    let factory: DebugAdapterDescriptorFactory;
    switch (runMode) {
        case 'server':
            // run the debug adapter as a server inside the extension and communicating via a socket
            factory = new ErlangDebugAdapterDescriptorFactory();
            break;

        case 'inline':
            // run the debug adapter inside the extension and directly talk to it
            factory = new InlineErlangDebugAdapterFactory();
            break;

        case 'external': default:
            // run the debug adapter as a separate process
            factory = new ErlangDebugAdapterExecutableFactory(context.extensionPath);
            break;
    }
    
    disposables.push(debug.registerDebugAdapterDescriptorFactory('erlang', factory));
    if ('dispose' in factory) {
		disposables.push(factory);
    }
    disposables.forEach((disposable => context.subscriptions.push(disposable)));
    LspClient.activate(context);

    languages.setLanguageConfiguration("erlang", {
        onEnterRules: [
            // Module comment: always continue comment
            {
                beforeText: /^%%% .*$/,
                action: { indentAction: IndentAction.None, appendText: "%%% " }
            },
            {
                beforeText: /^%%%.*$/,
                action: { indentAction: IndentAction.None, appendText: "%%%" }
            },
            // Comment line with double %: continue comment if needed
            {
                beforeText: /^\s*%% .*$/,
                afterText: /^.*\S.*$/,
                action: { indentAction: IndentAction.None, appendText: "%% " }
            },
            {
                beforeText: /^\s*%%.*$/,
                afterText: /^.*\S.*$/,
                action: { indentAction: IndentAction.None, appendText: "%%" }
            },
            // Comment line with single %: continue comment if needed
            {
                beforeText: /^\s*% .*$/,
                afterText: /^.*\S.*$/,
                action: { indentAction: IndentAction.None, appendText: "% " }
            },
            {
                beforeText: /^\s*%.*$/,
                afterText: /^.*\S.*$/,
                action: { indentAction: IndentAction.None, appendText: "%" }
            },
            // Any other comment line: do nothing, ignore below rules
            {
                beforeText: /^\s*%.*$/,
                afterText: /^\s*$/,
                action: { indentAction: IndentAction.None }
            },

            // Empty line: do nothing, ignore below rules
            {
                beforeText: /^\s*$/,
                action: { indentAction: IndentAction.None }
            },

            // Before guard sequence (before 'when')
            {
                beforeText: /.*/,
                afterText: /^\s*when(\s.*)?$/,
                action: { indentAction: IndentAction.None, appendText: "  " }
            },
            // After guard sequence (after 'when')
            {
                beforeText: /^\s*when(\s.*)?->\s*$/,
                action: { indentAction: IndentAction.None, appendText: "  " }
            },
            {
                beforeText: /^\s*when(\s.*)?(,|;)\s*$/,
                action: { indentAction: IndentAction.None, appendText: "     " }
            },

            // Start of clause, right hand side of an assignment, after 'after', etc.
            {
                beforeText: /^.*(->|[^=]=|\s+(after|case|catch|if|of|receive|try))\s*$/,
                action: { indentAction: IndentAction.Indent }
            },

            // Not closed bracket
            {
                beforeText: /^.*[(][^)]*$/,
                action: { indentAction: IndentAction.Indent }
            },
            {
                beforeText: /^.*[{][^}]*$/,
                action: { indentAction: IndentAction.Indent }
            },
            {
                beforeText: /^.*[[][^\]]*$/,
                action: { indentAction: IndentAction.Indent }
            },

            // One liner clause but not the last
            {
                beforeText: /^.*->.+;\s*$/,
                action: { indentAction: IndentAction.None }
            },
            // End of function or attribute (e.g. export list)
            {
                beforeText: /^.*\.\s*$/,
                action: { indentAction: IndentAction.Outdent, removeText: 9000 }
            },
            // End of clause but not the last
            // FIXME: After a guard (;) it falsely outdents
            {
                beforeText: /^.*;\s*$/,
                action: { indentAction: IndentAction.Outdent }
            },
            // Last statement of a clause
            // TODO: double outdent or outdent + removeText: <tabsize>
            {
                beforeText: /^.*[^;,[({<]\s*$/,
                action: { indentAction: IndentAction.Outdent }
            }
        ]
    });
}

export function deactivate(): Thenable<void> {
    return LspClient.deactivate();
}
