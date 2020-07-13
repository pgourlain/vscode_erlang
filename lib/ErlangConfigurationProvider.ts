import { 
	workspace, WorkspaceFolder, DebugConfiguration, DebugConfigurationProvider, CancellationToken, ProviderResult
} from 'vscode'; 
import { ErlangSettings } from './erlangSettings';

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
        rootPath : workspace.rootPath,
        verbose: erlangConf.get("verbose", false)
    };
    currentSettings = settings;
}

export function getElangConfigConfiguration() : ErlangSettings {
    if (!currentSettings) {
        configurationChanged();
    }
    return currentSettings;
}
