import { ErlGenericShell, IErlangShellOutput } from '../GenericShell';

export class ErlangShellLSP extends ErlGenericShell {
    constructor(whichOutput: IErlangShellOutput) {
        super(whichOutput);
    }
    public Start(erlPath:string, startDir: string, listen_port: number, bridgePath: string, args: string): Promise<boolean> {
        //var debugStartArgs = ["-pa", `"${bridgePath}"`, "-pa", "ebin", "-s", "int",
        var debugStartArgs = ["-pa", "src", "-pa", "ebin", "-s", "int",
            "-vscode_port", listen_port.toString(),
            "-s", "vscode_lsp_entry", "start", listen_port.toString()];
        var processArgs = debugStartArgs.concat([args]);

        var result = this.LaunchProcess("erl", startDir, processArgs);
        return result;
    }
}