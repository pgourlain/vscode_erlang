import * as assert from 'assert';
import { GenericShell, ILogOutput } from '../../lib/GenericShell';

class TestShell extends GenericShell {
    public launch(processName: string, startDir: string, args: string[]) {
        return (this as any).LaunchProcess(processName, startDir, args);
    }
}

// #326, #239: a spawn that never starts (here: a cwd that does not exist,
// exactly what a never-`rebar3 compile`d _build/default/lib/vscode_lsp
// produces) used to be logged and dropped. Every caller of LaunchProcess
// waits on the 'close' event, which plain spawn-error alone never used to
// fire, so callers hung until some unrelated layer surfaced an opaque
// failure instead of the real one.
suite('GenericShell spawn error handling', () => {
    test('a spawn error that never starts the process still emits close with the error', function (done) {
        this.timeout(5000);
        const log: ILogOutput = { appendLine: (_: string) => {}, debug: (_: string) => {} };
        const shell = new TestShell(log);
        const missingDir = '/no/such/directory/at/all/' + Math.random().toString(36).slice(2);

        shell.on('close', (exitCode: number | null, error?: Error) => {
            assert.strictEqual(exitCode, null);
            assert.ok(error, 'expected the spawn error to be forwarded on close');
            assert.match(error!.message, /ENOENT/);
            done();
        });

        shell.launch('erl', missingDir, ['-eval', 'ok']);
    });
});
