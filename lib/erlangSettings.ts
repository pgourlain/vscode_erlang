

export interface ErlangSettings {
	erlangPath : string;
	rebarPath : string;
	rebarBuildArgs : string[];
    languageServerProtocol : LanguageServerProtocol;
}

export interface LanguageServerProtocol {
	verbose : boolean;
	codeLensEnabled : boolean;
}