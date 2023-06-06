import { GenericShell, ILogOutput } from '../GenericShell';
import { getElangConfigConfiguration } from '../ErlangConfigurationProvider';

export class ErlangShellLSP extends GenericShell {
    constructor(whichOutput: ILogOutput) {
        super(whichOutput, null, getElangConfigConfiguration());
    }
    public Start(erlPath:string, startDir: string, listen_port: number, bridgePath: string, args: string): Promise<boolean> {
        var debugStartArgs = [];
        // Start as distributed node to be able to connect to the Erlang VM for investigation
        if (this.erlangDistributedNode) {
            debugStartArgs.push(
                "-sname", "vscode_" + listen_port.toString(),
                "-setcookie", "vscode_" + listen_port.toString());
        }
        // Use special command line arguments
        if (this.erlangArgs) {
            debugStartArgs = debugStartArgs.concat(this.erlangArgs)
        }
        debugStartArgs.push(
            "-noshell",
            "-pa", "src",
            "-pa", "ebin",
            "-s", "int",
            "-vscode_port", listen_port.toString(),
            "-s", "vscode_lsp_entry", "start", listen_port.toString());
        var processArgs = debugStartArgs.concat([args]);

        var result = this.LaunchProcess("erl", startDir, processArgs);
        return result;
    }
}
