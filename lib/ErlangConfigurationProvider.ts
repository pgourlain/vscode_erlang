import { 
	workspace, WorkspaceFolder, DebugConfiguration, DebugConfigurationProvider, CancellationToken, ProviderResult
} from 'vscode'; 

export class ErlangDebugConfigurationProvider implements DebugConfigurationProvider {
    resolveDebugConfiguration?(folder: WorkspaceFolder, debugConfiguration: DebugConfiguration, token?: CancellationToken): ProviderResult<DebugConfiguration> {
        debugConfiguration.verbose = workspace.getConfiguration("erlang").get("verbose", false);
        debugConfiguration.erlangPath = <string>workspace.getConfiguration("erlang").get("erlangPath", null);
        return debugConfiguration;
    }
};
