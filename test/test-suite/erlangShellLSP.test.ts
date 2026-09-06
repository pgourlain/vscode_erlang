import * as assert from 'assert';
import { ErlangShellLSP } from '../../lib/lsp/ErlangShellLSP';

// Capture the argument list ErlangShellLSP.Start would pass to `erl`, without
// actually spawning a process, by overriding the protected LaunchProcess.
function captureStartArgs(distributed: boolean): Promise<string[]> {
    const log = { appendLine: (_: string) => {}, debug: (_: string) => {} };
    const shell = new ErlangShellLSP(log);
    shell.erlangDistributedNode = distributed;
    let captured: string[] = [];
    (shell as any).LaunchProcess = (_name: string, _dir: string, args: string[]) => {
        captured = args;
        return Promise.resolve(true);
    };
    return shell.Start('', '.', 12345, 'src', '').then(() => captured);
}

suite('ErlangShellLSP distribution hardening', () => {
    test('distributed node binds distribution and epmd to loopback', async () => {
        const args = await captureStartArgs(true);
        // -name <node>@127.0.0.1 (not -sname) so local remsh still reaches the
        // loopback-bound listener.
        assert.ok(args.includes('-name'), '-name flag present');
        assert.ok(args.includes('vscode_12345@127.0.0.1'), 'node named on loopback host');
        assert.ok(!args.includes('-sname'), '-sname must not be used (resolves to LAN host)');
        // Distribution listener and epmd confined to loopback.
        assert.ok(args.includes('inet_dist_use_interface'), 'inet_dist_use_interface present');
        assert.ok(args.includes('"{127,0,0,1}"'), 'loopback tuple quoted for the shell');
        assert.ok(args.includes('ERL_EPMD_ADDRESS'), 'ERL_EPMD_ADDRESS present');
        assert.ok(args.includes('127.0.0.1'), 'epmd bound to loopback');
    });

    test('non-distributed node adds no distribution flags', async () => {
        const args = await captureStartArgs(false);
        assert.ok(!args.includes('-name'), 'no -name when not distributed');
        assert.ok(!args.includes('-sname'), 'no -sname when not distributed');
        assert.ok(!args.includes('inet_dist_use_interface'), 'no distribution binding when not distributed');
        assert.ok(!args.includes('ERL_EPMD_ADDRESS'), 'no epmd binding when not distributed');
    });
});
