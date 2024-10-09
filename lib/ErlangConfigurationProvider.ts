import {
    workspace, WorkspaceFolder, DebugConfiguration, DebugConfigurationProvider, CancellationToken, ProviderResult, WorkspaceConfiguration
} from 'vscode';
import { ErlangSettings } from './erlangSettings';
import { stringify } from 'querystring';
//import { ErlangOutput } from './vscodeAdapter';

export class ErlangDebugConfigurationProvider implements DebugConfigurationProvider {
    provideDebugConfigurations?(folder: WorkspaceFolder | undefined, token?: CancellationToken): ProviderResult<DebugConfiguration[]> {
        if (folder) {
            return [];
        }
        return undefined;
    }

    resolveDebugConfiguration?(folder: WorkspaceFolder, debugConfiguration: DebugConfiguration, token?: CancellationToken): ProviderResult<DebugConfiguration> {
        let cfg = getElangConfigConfiguration();
        debugConfiguration.verbose = cfg.verbose;
        debugConfiguration.erlangPath = cfg.erlangPath;
        return debugConfiguration;
    }
};

let currentSettings: ErlangSettings = null;

export function configurationChanged(): void {
    let erlangConf = workspace.getConfiguration("erlang");
    let settings: ErlangSettings = {
        erlangPath: resolveVariables(erlangConf.get<string>("erlangPath", null)),
        erlangArgs: erlangConf.get("erlangArgs", []),
        erlangDistributedNode: erlangConf.get("erlangDistributedNode", false),
        rebarPath: resolveVariables(erlangConf.get<string>("rebarPath", null)),
        codeLensEnabled: erlangConf.get<boolean>('codeLensEnabled', false),
        compressLargeEtsTables: erlangConf.get("compressLargeEtsTables", false),
        inlayHintsEnabled: erlangConf.get<boolean>('inlayHintsEnabled', false),
        debuggerRunMode: erlangConf.get<string>("debuggerRunMode", "Server"),
        includePaths: erlangConf.get("includePaths", []),
        linting: erlangConf.get<boolean>('linting', false),
        rebarBuildArgs: erlangConf.get("rebarBuildArgs", ['compile']),
        rootPath: extractRootPath(),
        verbose: erlangConf.get("verbose", false)
    };
    currentSettings = settings;
}

export function resolveErlangSettings(erlangSection : WorkspaceConfiguration): any {    
    const erlangconfigAsJson = JSON.stringify(erlangSection);
    const erlangConfiguration = JSON.parse(erlangconfigAsJson);
    if (erlangConfiguration) {
        erlangConfiguration.erlangPath = resolveVariables(erlangConfiguration.erlangPath);
        erlangConfiguration.rebarPath = resolveVariables(erlangConfiguration.rebarPath);
    }
    return erlangConfiguration;
}

function getFirstWorkspaceFolderPath(): string {
    let folders = workspace.workspaceFolders;
    if (folders && folders.length > 0) {
        return folders[0].uri.fsPath;
    }
    return "";
}

function extractRootPath(): string {
    const res = getFirstWorkspaceFolderPath();
    return res != "" ? res : undefined;
}

function resolveVariables(value: string) : string {
    //https://code.visualstudio.com/docs/editor/variables-reference#_predefined-variables
    if (!value) return value;
    value = value.replace('${workspaceFolder}', getFirstWorkspaceFolderPath);
    return value;
}


export function getElangConfigConfiguration(): ErlangSettings {
    if (!currentSettings) {
        configurationChanged();
        // in order to debug
        // let output = ErlangOutput();
        // output.appendLine("workspace information");
        // output.appendLine(`rootPath: ${workspace.rootPath}`);
        // output.appendLine(`workspaceFolders: ${JSON.stringify(workspace.workspaceFolders)}`);
    }
    return currentSettings;
}
