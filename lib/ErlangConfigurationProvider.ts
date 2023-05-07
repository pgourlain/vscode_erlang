import { 
	workspace, WorkspaceFolder, DebugConfiguration, DebugConfigurationProvider, CancellationToken, ProviderResult
} from 'vscode'; 
import { ErlangSettings } from './erlangSettings';
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

let currentSettings : ErlangSettings = null;

export function configurationChanged() : void {
    let erlangConf = workspace.getConfiguration("erlang");
    let settings : ErlangSettings = {
        erlangPath: erlangConf.get<string>("erlangPath", null),
        rebarPath: erlangConf.get<string>("rebarPath", null),
        codeLensEnabled: erlangConf.get<boolean>('codeLensEnabled', false),
        debuggerRunMode: erlangConf.get<string>("debuggerRunMode", "Server"),
        includePaths: erlangConf.get("includePaths", []),
        linting: erlangConf.get<boolean>('linting', false),
        rebarBuildArgs: erlangConf.get("rebarBuildArgs", ['compile']),
        rootPath : extractRootPath(),
        verbose: erlangConf.get("verbose", false)
    };
    currentSettings = settings;
}

function extractRootPath() : string {
    //workspace.rootPath is deprecated, and documentation notice that value is store in first workspace folder
    let folders = workspace.workspaceFolders;
    if (folders && folders.length > 0) {
        return folders[0].uri.fsPath;
    }
    return undefined;
}

export function getElangConfigConfiguration() : ErlangSettings {
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
