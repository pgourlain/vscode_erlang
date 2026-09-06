import * as os from 'os';
import { GenericShell, ILogOutput } from '../GenericShell';
import { getElangConfigConfiguration } from '../ErlangConfigurationProvider';

export class ErlangShellLSP extends GenericShell {
    constructor(whichOutput: ILogOutput) {
        super(whichOutput, null, getElangConfigConfiguration());
    }
    public Start(erlPath:string, startDir: string, listen_port: number, bridgePath: string, args: string): Promise<boolean> {
        var debugStartArgs = [];
        // Start as distributed node to be able to connect to the Erlang VM for investigation.
        // Bind distribution to loopback: a network-reachable distribution port plus the
        // predictable cookie below is the classic Erlang RCE (rpc:call/4 os:cmd/1). Use
        // -name <node>@127.0.0.1 so local remsh still connects over loopback (a short name
        // resolves to the LAN address, which the loopback-bound listener refuses), and
        // confine both the distribution listener (inet_dist_use_interface) and epmd
        // (ERL_EPMD_ADDRESS) to 127.0.0.1. The {127,0,0,1} tuple is double-quoted so the
        // shell (GenericShell spawns with shell:true) passes it as a single argument.
        if (this.erlangDistributedNode) {
            debugStartArgs.push(
                "-name", "vscode_" + listen_port.toString() + "@127.0.0.1",
                "-setcookie", "vscode_" + listen_port.toString(),
                "-kernel", "inet_dist_use_interface", '"{127,0,0,1}"',
                "-env", "ERL_EPMD_ADDRESS", "127.0.0.1");
        }
        // Set management mode for large caches
        switch (this.cacheManagement) {
            case 'file':
                debugStartArgs.push("-vscode_cache_mgmt", "file", os.userInfo().username, os.tmpdir());
                break;

            case 'compressed memory':
                debugStartArgs.push("-vscode_cache_mgmt", "memory", "compressed");
                break;

            case 'memory':
            default:
                debugStartArgs.push("-vscode_cache_mgmt", "memory");
                break;
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
