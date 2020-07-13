export interface ErlangSettings {
	erlangPath : string;
	rebarPath : string;
	rebarBuildArgs : string[];
	includePaths : string[];
	linting: boolean;
	codeLensEnabled : boolean;
	verbose: boolean;
	debuggerRunMode : string;
	/// workspace.rootPath
	rootPath: string;
}
