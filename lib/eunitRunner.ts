
import * as vscode from 'vscode';
import * as fs from 'fs'
import * as rebar from './RebarRunner';
import * as erlang from './ErlangShell';
import * as path from 'path';
import * as utils from './utils';
import * as adapter from './vscodeAdapter';
import { erlangBridgePath } from './erlangConnection';


export class EunitRunner implements vscode.Disposable {

    diagnosticCollection: vscode.DiagnosticCollection;
    private eunitCommand: vscode.Disposable;

    public activate(context: vscode.ExtensionContext) {
        this.eunitCommand = vscode.commands.registerCommand('extension.erleunit', () => { this.runEUnitCommand() });
        this.diagnosticCollection = vscode.languages.createDiagnosticCollection("euniterlang");
        setExtensionPath(context.extensionPath);
        context.subscriptions.push(this);
    }

    public dispose(): void {
        this.diagnosticCollection.clear();
        this.diagnosticCollection.dispose();
        this.eunitCommand.dispose();
    }

    private runEUnitCommand() {
        let that = this;
        runEUnitRequirements().then(_ => {
            myoutputChannel.clear();
            this.diagnosticCollection.clear();
            logTitle("Read configuration...");
            readRebarConfigWithErlangShell().then(v => {
                //add file type to compile
                v.TestDirs = v.TestDirs.map(x => joinPath(x, "*.erl"));
                return v;
            }).then(x => {
                logTitle("Compile units tests...");
                compile(x).then(v => {
                    logTitle("Run units tests...");
                    runTests(v).then(testResults => {
                        that.parseTestsResults(testResults).then(ok =>ok,
                            reason => {
                            myoutputChannel.appendLine('eunit command failed :' + reason + '\n');
                        });
                    });
                });
            });
        }, reason => {
            myoutputChannel.appendLine('rebar eunit command failed :' + reason + '\n');
        });
    }

    private parseTestsResults(results: TestResults): Thenable<boolean> {
        return new Promise<boolean>((a, r) => {
            if (results.failed > 0 || results.aborted > 0) {
                var diagnostics: { [id: string]: vscode.Diagnostic[]; } = {};

                this.parseForDiag(results.testcases, diagnostics);
                var keys = utils.keysFromDictionary(diagnostics);
                keys.forEach(element => {
                    var fileUri = vscode.Uri.file(path.join(vscode.workspace.rootPath, element));
                    var diags = diagnostics[element];
                    this.diagnosticCollection.set(fileUri, diags);
                });
                var failed = Number(results.failed) + Number(results.aborted);
                r((failed) + " unittest(s) failed.");
            }
            a(true);
        });
    }

    private getFile(stacktrace: any[]): string {
        if (stacktrace && stacktrace.length > 0) {
            return stacktrace[0].file;
        }
        return "";
    }

    private getExpected(location: any): string {
        if (location.expected) {
            return location.expected;
        } else if (location.pattern) {
            return location.pattern;
        }
        return JSON.stringify(location);
    }

    private parseForDiag(testcases: TestCase[], diagnostics: { [id: string]: vscode.Diagnostic[]; }) {
        testcases.forEach(testcase => {
            if (testcase.result && testcase.result != "ok") {
                /*
                let message = m[m.length-1];
                let range = new vscode.Range(Number(m[2])-1, 0, Number(m[2])-1, peace.length-1);
                let diagnostic = new vscode.Diagnostic(range, message, vscode.DiagnosticSeverity.Error);
                */
                let message = "";
                var failed = testcase.result.failed;
                var diagnostic: vscode.Diagnostic = null;
                if (failed) {
                    message = failed.assertion + "/" + failed.location.module + ", expected :" + this.getExpected(failed.location) + ", value :" + failed.location.value;
                    var file = this.getFile(failed.stacktrace);
                    var line = Number(failed.location.line) - 1;
                    let range = new vscode.Range(line, 0, line, 80);
                    diagnostic = new vscode.Diagnostic(range, message, vscode.DiagnosticSeverity.Error);
                } else if (testcase.result.aborted) {
                    //TODO: when test is aborted
                    var aborted = testcase.result.aborted;
                    message = aborted.error;

                    var file = this.getFile(aborted.stacktrace);
                    var line = Number(aborted.stacktrace[0].line) - 1;
                    let range = new vscode.Range(line, 0, line, 80);
                    diagnostic = new vscode.Diagnostic(range, message, vscode.DiagnosticSeverity.Error);
                }
                if (!diagnostics[file]) {
                    diagnostics[file] = [];
                }
                diagnostics[file].push(diagnostic);
            }
        });
    }
}



var myoutputChannel = adapter.ErlangOutput();
var eunitDirectory = ".eunit";
var myExtensionPath = "";


function setExtensionPath(extensionPath: string) {
    myExtensionPath = extensionPath;
}

function joinPath(x: String, y: String): string {
    return x + "/" + y;
}

function logTitle(title: string) {
    myoutputChannel.appendLine("------------------------------------------");
    myoutputChannel.appendLine(title);
    myoutputChannel.appendLine("------------------------------------------");
}

function runEUnitRequirements(): Thenable<boolean> {
    return new Promise<boolean>((a, r) => {
        var rebarConfig = path.join(vscode.workspace.rootPath, "rebar.config");
        if (fs.existsSync(rebarConfig)) {
            a(true);
        }
        else {
            r("rebar.config is missing !");
        }
    });
}

function readRebarConfigWithErlangShell(): Thenable<CompileArgs> {
    return new Promise<CompileArgs>((a, r) => {

        var erlangShell = new erlang.ErlangShell();
        erlangShell.erlangPath = vscode.workspace.getConfiguration("erlang").get("erlangPath", null);

        erlangShell.Start(vscode.workspace.rootPath, []).then(
            _ => {
                var compileArgs = new CompileArgs();
                var content = fs.readFileSync(path.join(vscode.workspace.rootPath, "rebarconfig.json"), "utf-8");
                var o = JSON.parse(content);
                compileArgs.IncludeDirs = o.IncludeDirs;
                if (compileArgs.IncludeDirs) {
                    //ADD -I before each élément
                    insertBeforeEachElement(compileArgs.IncludeDirs, "-I");
                }
                compileArgs.TestDirs = o.TestDirs;
                //todo: test if TestDirs is not empty
                a(compileArgs);
            },
            exitCode => {
                r("Erlang shell that get rebar config failed with exitcode :" + exitCode);
            });
        var cmd = '{ok, Config}=file:consult("./rebar.config"),';
        cmd += 'Undef = fun (E) -> case E of (undefined) -> []; (_) -> E end end,';
        //read erl_opts
        cmd += 'E=Undef(proplists:get_value(erl_opts, Config)),';
        //get includes dirs
        cmd += 'I=Undef(proplists:get_value(i, E)),';
        //read eunit_compile_opts
        cmd += 'EunitOpts=Undef(proplists:get_value(eunit_compile_opts, Config)),';
        //get src_dirs
        cmd += 'SrcDirs=Undef(proplists:get_value(src_dirs, EunitOpts)),';
        //get erlang tuples as printable chars
        cmd += 'IR=lists:flatten(io_lib:print(case io_lib:printable_list(I) and (string:length(I)>0) of true -> [I]; false -> I end)),';
        //json representation
        cmd += 'R = "{\\"TestDirs\\":"++lists:flatten(io_lib:print(SrcDirs))++", \\"IncludeDirs\\":"++IR++"}",';
        cmd += 'file:write_file("./rebarconfig.json", R),'
        cmd += 'q().';
        //send command to current erlang shell  
        erlangShell.Send(cmd);
    });
}

function to_modulename(moduleFileName: string): string {
    var parsedModuleFileName = path.parse(moduleFileName);
    return parsedModuleFileName.name;
}

function relativeTo(ref: string, value: string): string {
    var parsedValue = path.parse(value);
    if (parsedValue.dir.startsWith(ref)) {
        return path.join(".", parsedValue.dir.substring(ref.length), parsedValue.base);
    }
    return value;
}

function relativePathTo(ref: string, value: string): string {
    var parsedValue = path.parse(value);
    if (parsedValue.dir.startsWith(ref)) {
        return path.join(".", parsedValue.dir.substring(ref.length));
    }
    return value;
}

function findErlangFiles(dirAndPattern: string): Thenable<string[]> {
    //find file from the root of current workspace
    return vscode.workspace.findFiles(dirAndPattern, "").then((files: vscode.Uri[]) => {
        return files.map((v, i, a) => relativeTo(vscode.workspace.rootPath, v.fsPath));
    });
}

function mapToFirstDirLevel(x: vscode.Uri): string {
    var y = relativeTo(vscode.workspace.rootPath, x.fsPath);
    return y.split(path.sep)[0];
}

function findIncludeDirectories(): Thenable<string[]> {
    return vscode.workspace.findFiles("**/*.hrl", "").then((files: vscode.Uri[]) => {
        var iDirs = files.map(x => mapToFirstDirLevel(x));
        insertBeforeEachElement(iDirs, "-I");
        return iDirs;
    });
}

function insertBeforeEachElement(A: string[], value: string) {
    var startIndex = A.length - 1;
    var count = A.length;
    for (var index = 0; index < count; index++) {
        A.splice(startIndex - index, 0, value);
    }
}

function cleanDirectory(dir: string) {
    fs.readdirSync(dir).forEach(element => {
        var file = path.resolve(dir, element);
        var stats = fs.statSync(file);
        if (stats && stats.isFile()) {
            fs.unlinkSync(file);
        }
    });
}

function compile(compileArgs: CompileArgs): Thenable<string[]> {
    var eunitDir = path.join(vscode.workspace.rootPath, eunitDirectory);
    if (fs.existsSync(eunitDir)) {
        cleanDirectory(eunitDir);
    }
    if (!fs.existsSync(eunitDir)) {
        fs.mkdirSync(eunitDir);
    }
    fs.createReadStream(path.resolve(erlangBridgePath, 'eunit_jsonreport.erl'))
        .pipe(fs.createWriteStream(path.resolve(eunitDir, 'eunit_jsonreport.erl')));
    return findIncludeDirectories()
        .then(iDirs => {
            compileArgs.IncludeDirs = compileArgs.IncludeDirs.concat(iDirs);
            return compileArgs;
        }).then(args => {
            return findErlangFiles("{" + compileArgs.TestDirs.join(",") + "}").then(erlFiles => {
                args.ErlangFiles = erlFiles.concat(['./.eunit/eunit_jsonreport.erl']);
                return args;
            });
        }).then(args => {
            var argsCmd = args.IncludeDirs.concat(["-o", eunitDirectory]).concat(args.ErlangFiles);
            var erlc = new erlang.ErlangCompilerShell();
            erlc.erlangPath = vscode.workspace.getConfiguration("erlang").get("erlangPath", null);

            return erlc.Start(vscode.workspace.rootPath, argsCmd.map<string>(x => x.toString()))
                .then(exitCode => {
                    return args.ErlangFiles;
                });
        });
}

function walkdir(dir: string, done: (err: NodeJS.ErrnoException, files: string[]) => void, accept: (dirName: string, fullPath: string) => boolean) {
    //custom function, because 'vscode.workspace.findFiles' use files.exclude from .vscode/settings.json
    //so some files are hidden (i.e *.beam) 
    var results = [];
    fs.readdir(dir, (err, list) => {
        if (err) return done(err, null);
        var pending = list.length;
        if (!pending) return done(null, results);
        list.forEach((fileName) => {
            var file = path.resolve(dir, fileName);
            fs.stat(file, (err, stat) => {
                if (stat && stat.isDirectory()) {
                    if (accept(fileName, file)) {
                        results.push(file);
                    }
                    walkdir(file, (err, res) => {
                        results = results.concat(res);
                        if (!--pending) done(null, results);
                    }, accept);
                } else {
                    //results.push(file);
                    if (!--pending) done(null, results);
                }
            });
        });
    });
}

function findebinDirs(): Thenable<string[]> {
    return new Promise<string[]>((a, r) => {
        walkdir(vscode.workspace.rootPath, (err, files) => {
            if (err) r(err);
            a(files.map(x => relativePathTo(vscode.workspace.rootPath, path.resolve(x, "dummy.txt"))));
        },
            //accept only directory that contains ebin 
            (dirName, fullPath) => dirName.match(/ebin/gi) != null)
    });
}

function runTests(filenames: string[]): Thenable<TestResults> {
    return new Promise<TestResults>((a, r) => {
        findebinDirs().then(pzDirs => {
            var erlangShell = new erlang.ErlangShell();
            var moduleNames = filenames.map((v, i, a) => to_modulename(v));
            insertBeforeEachElement(pzDirs, "-pz");
            var args = pzDirs.concat(["-pz", "./" + eunitDirectory]);
            erlangShell.Start(vscode.workspace.rootPath, args).then(
                _ => {
                    var jsonResults = fs.readFileSync(path.resolve(vscode.workspace.rootPath, ".eunit", "testsuite_results.json"), "utf-8")
                    var typedResults = (<TestResults>JSON.parse(jsonResults));
                    a(typedResults);
                },
                exitCode => {
                    r("Erlang shell that run tests failed with exitcode :" + exitCode);
                });
            //send command to current erlang shell  
            erlangShell.Send('eunit:test([' + moduleNames.join(',') + '],[{report,{eunit_jsonreport,[{dir,"' + eunitDirectory + '"}]}}]),q().');
        });
    });
}

class CompileArgs {
    TestDirs: string[];
    IncludeDirs: string[];
    ErlangFiles: string[];
}

class TestResults {
    name: string;
    time: number;
    output: any;
    succeeded: number;
    failed: number;
    aborted: number;
    skipped: number;
    testcases: TestCase[];
}

class TestCase {
    displayname: string;
    description: string;
    module: string;
    function: string;
    arity: number;
    line: number;
    result: any;
    time: string;
    output: any;
}
