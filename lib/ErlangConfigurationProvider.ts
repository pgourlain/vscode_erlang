import { 
	workspace, WorkspaceFolder, DebugConfiguration, DebugConfigurationProvider, CancellationToken, ProviderResult
} from 'vscode'; 

export class ErlangDebugConfigurationProvider implements DebugConfigurationProvider {
    resolveDebugConfiguration?(folder: WorkspaceFolder, debugConfiguration: DebugConfiguration, token?: CancellationToken): ProviderResult<DebugConfiguration> {
        debugConfiguration.verbose = workspace.getConfiguration("erlang").get("verbose", false);
        return debugConfiguration;
    }
};
