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
        if (!debugConfiguration.cwd && (folder || cfg.rootPath)) {
            debugConfiguration.cwd = folder ? folder.uri.fsPath : cfg.rootPath;
        }
        debugConfiguration.verbose = cfg.verbose;
        debugConfiguration.erlangPath = cfg.erlangPath;
        debugConfiguration.useShell = cfg.useShell;
        return debugConfiguration;
    }
};

let currentSettings: ErlangSettings = null;

// "auto" matches prior (unconfigurable) behavior: a shell is only needed on
// Windows to dispatch .bat/.cmd and re-parse quoted args. "always"/"never"
// are the escape hatch - e.g. for a sandboxed POSIX environment that blocks
// or lacks a shell (#351), or for erlangArgs/erlangPath/rebarPath values that
// rely on shell expansion and would break under "auto" on non-Windows.
export function resolveUseShell(setting: string): boolean {
    if (setting === 'always') return true;
    if (setting === 'never') return false;
    return process.platform === 'win32';
}

export function configurationChanged(): void {
    let erlangConf = workspace.getConfiguration("erlang");
    let settings: ErlangSettings = {
        erlangPath: resolveVariables(erlangConf.get<string>("erlangPath", null)),
        erlangArgs: erlangConf.get("erlangArgs", []),
        erlangDistributedNode: erlangConf.get("erlangDistributedNode", false),
        rebarPath: resolveVariables(erlangConf.get<string>("rebarPath", null)),
        codeLensEnabled: erlangConf.get<boolean>('codeLensEnabled', false),
        cacheManagement: erlangConf.get("cacheManagement", "memory"),
        inlayHintsEnabled: erlangConf.get<boolean>('inlayHintsEnabled', false),
        semanticTokensEnabled: erlangConf.get<boolean>('semanticTokensEnabled', true),
        debuggerRunMode: erlangConf.get<string>("debuggerRunMode", "Server"),
        includePaths: erlangConf.get("includePaths", []),
        linting: erlangConf.get<boolean>('linting', false),
        rebarBuildArgs: erlangConf.get("rebarBuildArgs", ['compile']),
        rootPath: extractRootPath(),
        useShell: resolveUseShell(erlangConf.get<string>("useShell", "auto")),
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
