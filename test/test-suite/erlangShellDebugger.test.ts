import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { DebugProtocol } from '@vscode/debugprotocol';
import { ErlangShellForDebugging } from '../../lib/ErlangShellDebugger';

function newShell(): ErlangShellForDebugging {
    const log = { appendLine: (_: string) => {}, debug: (_: string) => {} };
    return new ErlangShellForDebugging(log);
}

function breakpointFor(sourcePath: string): DebugProtocol.Breakpoint {
    return { verified: true, source: { path: sourcePath } };
}

suite('ErlangShellForDebugging', () => {
    // #172, #173: F5 on a project that was never `rebar3 compile`d has no
    // `_build` directory yet. findEbinDirs used to call fs.readdirSync on it
    // unconditionally and throw ENOENT, aborting the whole launch.
    test('findEbinDirs returns no ebin dirs when _build does not exist', () => {
        const missing = path.join(os.tmpdir(), 'does-not-exist-' + Math.random().toString(36).slice(2), '_build');
        const shell = newShell() as any;
        assert.deepStrictEqual(shell.findEbinDirs(missing), []);
    });

    test('findEbinDirs still finds ebin directories when _build exists', () => {
        const root = fs.mkdtempSync(path.join(os.tmpdir(), 'ebin-scan-'));
        try {
            const ebin = path.join(root, 'lib', 'myapp', 'ebin');
            fs.mkdirSync(ebin, { recursive: true });
            const shell = newShell() as any;
            assert.deepStrictEqual(shell.findEbinDirs(root), [ebin]);
        } finally {
            fs.rmSync(root, { recursive: true, force: true });
        }
    });

    test('findErlFiles returns no files when the directory does not exist', () => {
        const missing = path.join(os.tmpdir(), 'does-not-exist-' + Math.random().toString(36).slice(2));
        const shell = newShell() as any;
        assert.deepStrictEqual(shell.findErlFiles(missing), []);
    });

    // #140: a breakpoint set in any non-.erl file (rebar.config, .app.src,
    // .config, ...) must never reach int:ni/1 - erl_parse can't parse those
    // and the debuggee crashes. Allowlist, not a denylist of one extension.
    test('excludeUnwantedFiles only keeps breakpoints in .erl files', () => {
        const shell = newShell() as any;
        assert.strictEqual(shell.excludeUnwantedFiles(breakpointFor('/proj/src/foo.erl')), true);
        assert.strictEqual(shell.excludeUnwantedFiles(breakpointFor('/proj/src/foo.app.src')), false);
        assert.strictEqual(shell.excludeUnwantedFiles(breakpointFor('/proj/rebar.config')), false);
        assert.strictEqual(shell.excludeUnwantedFiles(breakpointFor('/proj/README.md')), false);
    });
});
