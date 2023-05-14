export interface ErlangSettings {
	erlangPath : string;
	rebarPath : string;
	rebarBuildArgs : string[];
	includePaths : string[];
	linting: boolean;
	codeLensEnabled : boolean;
	inlayHintsEnabled: boolean;
	verbose: boolean;
	debuggerRunMode : string;
	/// workspace.rootPath, since VSCode 1.78 workspace.workspaceFolders[0].Uri.path 
	rootPath: string;
}
