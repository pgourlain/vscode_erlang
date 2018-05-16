export interface ErlangSettings {
	erlangPath : string;
	rebarPath : string;
	rebarBuildArgs : string[];
	includePaths : string[];
	linting: true;
	codeLensEnabled : boolean;
	verbose: boolean;
}
