import {ErlangConnection} from '../erlangConnection';

export interface ParsingResult {
    parse_result : string;
    errors_warnings : ErrorOrWarning[]
}

export interface ErrorOrWarning {
    type : string;    
    file : string;
    info : ErrorInfo;
}

export interface ErrorInfo {
    line : number;
    character : number;
    message : string;
}

export interface ReferenceLocation {
    uri : string;
    line : number;
    character : number;
}

export interface CodeLensInfoResult {
    uri: string;
    codelens : CodeLensInfo[]
}
export interface CodeLensInfo {
    line : number;
    character : number;
    data : CodeLensInfoData;
}

export interface CodeLensInfoData {
    count : number;
    func_name : string;
    exported : boolean;
}

export interface HoverInfo {
    text: string;
    moduleName: string;
    functionName: string;
}

export class ErlangLspConnection extends ErlangConnection {
    
    protected get_ErlangFiles() : string[] {
        return ["vscode_lsp_entry.erl"];
        //return [];
    }

    protected handle_erlang_event(url: string, body: any) {
        //console.log(`handle_erlang_event url:${url}, body:${body}`);
        //throw new Error("Method not implemented.");
        switch(url) {
            case "/listen":
            this.erlangbridgePort = body.port;
            this.debug("erlang lsp listen on port :" + this.erlangbridgePort.toString());
            break;
            default :
            this.debug(`url command '${url}' is not handled`);            
            break;
        }
    }

    public validateTextDocument(uri : string, numberOfProblems : number, callback: (parsingResult : ParsingResult) => void): void {
        //this.debug("begin validateTextDocument");
        this.post("validate_text_document", this.toErlangUri(uri)).then(
            res => {
                if (res.error) {
                    this.debug(`validateTextDocument error:${res.error}`);                    
                } else {
                    callback(res);
                }
                return true;
            },
            err =>  {return false;}
        );
    }

    public FormatDocument(uri : string) : void {
        this.post("format_document", this.toErlangUri(uri)).then(
            res => {
                if (res.error) {
                    this.debug(`validateTextDocument error:${res.error}`);                    
                } else {
                    this.debug(`FormatDocument result : ${JSON.stringify(res)}`)
                }
                return true;
            },
            err =>  {return false;}
        );        
    }

    public async GetCompletionItems(uri : string, line : number, character : number, lastEnterChar : string) : Promise<string[]> {
        return await this.post("completion_items", this.toErlangUri(uri) + "\r\n" + line.toString() +  "\r\n" + (character-1).toString()+"\r\n" + lastEnterChar).then(
            res => {
                if (res.error) {
                    this.debug(`completion_items error:${res.error}`);                    
                    return [];
                } else {
                    this.debug(`completion_items result : ${JSON.stringify(res)}`)
                }
                return [];
            },
            err =>  {return [];}
        );     
    }

    public GetModuleExports(uri : string) {

    }

    public onDocumentClosed(uri : string) : void {
        this.post("document_closed", this.toErlangUri(uri)).then( 
            res => { return true;}, 
            err => {return false;} 
        );
    }

    public async getDefinitionLocation(uri: string, line: number, character: number): Promise<ReferenceLocation> {
        return await this.post("goto_definition", this.toErlangUri(uri) + "\r\n" + (line + 1).toString() +  "\r\n" + (character + 1).toString()).then(
            res => {
                //this.debug(`goto_definition result : ${JSON.stringify(res)}`);
                if (res.result == "ok") {
                    return {
                        uri : this.fromErlangUri(res.uri),
                        line : res.line - 1,
                        character : res.character - 1
                    };
                }
                return null;
            },
            err =>  {return null;}
        );
    }

    public async getHoverInfo(uri: string, line: number, character: number): Promise<HoverInfo> {
        return await this.post("hover_info", this.toErlangUri(uri) + "\r\n" + (line + 1).toString() +  "\r\n" + (character + 1).toString()).then(
            res => {
                if (res.result == "ok") {
                    return {
                        text: res.text,
                        moduleName: res.moduleName,
                        functionName: res.functionName
                    };
                }
                return null;
            },
            err =>  {return null;}
        );
    }
    
    public async getReferencesInfo(uri: string, line: number, character: number): Promise<ReferenceLocation[]> {
        return await this.post("references_info", this.toErlangUri(uri) + "\r\n" + (line + 1).toString() +  "\r\n" + (character + 1).toString()).then(
            res => {
                //this.debug(`references_info result : ${JSON.stringify(res)}`);
                if (res.result == "ok") {
                    let refs = (<ReferenceLocation[]>res.references);
                    let self = this;
                    refs.map(x => {
                        x.uri = self.fromErlangUri(x.uri);
                        x.line = x.line-1;
                        x.character = x.character-1;
                        return x;
                    })
                    return refs;
                }
                return null;
            },
            err =>  {return null;}
        );
    }
    
    public async getCodeLensInfo(uri: string): Promise<CodeLensInfoResult> {
        return await this.post("codelens_info", this.toErlangUri(uri)).then(
            res => {
                //this.debug(`getCodeLensInfo result : ${JSON.stringify(res)}`);
                if (res.result == "ok") {
                    let result = <CodeLensInfoResult>{};
                    result.uri = this.fromErlangUri(res.uri);
                    let codelens = (<CodeLensInfo[]>res.codelens);
                    let self = this;
                    codelens.map(x => {
                        x.line = x.line-1;
                        x.character = x.character-1;
                        return x;
                    });
                    return <CodeLensInfoResult>{
                        uri : res.uri,
                        codelens : codelens
                    };
                }
                return null;
            },
            err =>  {return null;}
        );
    }

    public Quit() : void {
        this.post("stop_server");
        this.events_receiver.close();
    }

    private toErlangUri(uri: string): string {
        if (process.platform == 'win32')
            return uri.replace(/file:\/\/\/([A-Za-z])%3A\//, 'file://$1:/');
        else
            return uri;    
    }

    private fromErlangUri(uri: string): string {
        if (process.platform == 'win32')
            return uri.replace(/file:\/\/([A-Za-z]):/, 'file:///$1%3A');
        else
            return uri;
    }
}

