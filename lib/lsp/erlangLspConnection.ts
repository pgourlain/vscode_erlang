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
    message : string;
}

export interface ReferenceLocation {
    uri : string;
    line : number;
    character : number;
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
        this.post("validate_text_document", uri).then(
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
        this.post("format_document", uri).then(
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
        return await this.post("completion_items", uri + "\r\n" + line.toString() +  "\r\n" + (character-1).toString()+"\r\n" + lastEnterChar).then(
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
        this.post("document_closed", uri).then( 
            res => { return true;}, 
            err => {return false;} 
        );
    }

    public async getDefinitionLocation(uri : string, line : number, character : number ) : Promise<ReferenceLocation> {
        return await this.post("goto_definition", uri + "\r\n" + line.toString() +  "\r\n" + (character-1).toString()).then(
            res => {
                //this.debug(`goto_definition result : ${JSON.stringify(res)}`);
                if (res.result == "ok") {
                    return {
                        uri : res.uri,
                        line : res.line,
                        character : res.character
                    };
                }
                return null;
            },
            err =>  {return null;}
        );
    }

    public Quit() : void {
        this.events_receiver.close();
    }
}