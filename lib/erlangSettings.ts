/**
 * Internal, folder-resolved MCP settings handed to the debug adapter (never a
 * user-facing launch/attach block). Validation is authoritative in Erlang (mcp_policy).
 */
export interface ErlangMcpSettings {
	enabled: boolean;
	host: string;
	port: number;
	/** optional fixed development token (erlang.mcp.authToken); validated in Erlang */
	authToken?: string;
	/** Workspace Trust status of the folder */
	trusted: boolean;
	/** selected workspace folder; the project policy is never read outside it */
	root: string;
}

export interface ErlangSettings {
	erlangPath : string;
	erlangArgs : string[];
	erlangDistributedNode: boolean;
	rebarPath : string;
	rebarBuildArgs : string[];
	includePaths : string[];
	linting: boolean;
	codeLensEnabled : boolean;
	cacheManagement: string;
	inlayHintsEnabled: boolean;
	semanticTokensEnabled: boolean;
	verbose: boolean;
	debuggerRunMode : string;
	// resolved from erlang.useShell ("auto"/"always"/"never") - see ErlangConfigurationProvider.resolveUseShell
	useShell: boolean;
	/// workspace.rootPath, since VSCode 1.78 workspace.workspaceFolders[0].Uri.path 
	rootPath: string;
}
