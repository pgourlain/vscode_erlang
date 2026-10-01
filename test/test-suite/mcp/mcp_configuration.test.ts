import * as assert from 'assert';
import { workspace, WorkspaceFolder, DebugConfiguration } from 'vscode';
import { applyMcpSettings } from '../../../lib/ErlangConfigurationProvider';
import { redactArgs } from '../../../lib/GenericShell';
import { sanitizeArguments } from '../../../lib/erlangDebugSession';
import { ErlangDebugConnection } from '../../../lib/erlangDebugConnection';

// erlang.mcp.* is the only activation switch of the MCP inspector; these tests cover the
// TypeScript hooks (Erlang validates authoritatively: see mcp_policy_SUITE / mcp_server_SUITE).
suite('MCP: debug configuration hook', () => {
    const folder: WorkspaceFolder = workspace.workspaceFolders![0];

    async function setMcp(key: string, value: unknown) {
        await workspace.getConfiguration('erlang', folder.uri).update(key, value, false);
    }

    teardown(async () => {
        await setMcp('mcp.enabled', undefined);
        await setMcp('mcp.host', undefined);
        await setMcp('mcp.port', undefined);
    });

    test('disabled by default: no internal mcp settings reach the adapter', () => {
        const cfg: DebugConfiguration = { type: 'erlang', request: 'launch', name: 'x' };
        applyMcpSettings(folder, cfg);
        assert.strictEqual(cfg.mcpSettings, undefined);
        assert.strictEqual(cfg.mcp, undefined);
    });

    test('enabled for the debug folder: settings are resolved for that folder', async () => {
        await setMcp('mcp.enabled', true);
        await setMcp('mcp.port', 4321);
        const cfg: DebugConfiguration = { type: 'erlang', request: 'launch', name: 'x' };
        applyMcpSettings(folder, cfg);
        assert.strictEqual(cfg.mcpSettings.enabled, true);
        assert.strictEqual(cfg.mcpSettings.port, 4321);
        assert.strictEqual(cfg.mcpSettings.host, '127.0.0.1');
        assert.strictEqual(cfg.mcpSettings.root, folder.uri.fsPath);
        assert.strictEqual(typeof cfg.mcpSettings.trusted, 'boolean');
    });

    test('noDebug never gets MCP settings, even when enabled', async () => {
        await setMcp('mcp.enabled', true);
        const cfg: DebugConfiguration = { type: 'erlang', request: 'launch', name: 'x', noDebug: true };
        applyMcpSettings(folder, cfg);
        assert.strictEqual(cfg.mcpSettings, undefined);
    });

    test('a user-authored mcp block cannot enable or tune the inspector', () => {
        const cfg: DebugConfiguration = {
            type: 'erlang', request: 'launch', name: 'x',
            mcp: { enabled: true, host: '0.0.0.0', authToken: 'secret', required: true },
            mcpSettings: { enabled: true, host: '0.0.0.0', port: 1, trusted: true, root: '/' }
        };
        applyMcpSettings(folder, cfg);
        assert.strictEqual(cfg.mcp, undefined);
        assert.strictEqual(cfg.mcpSettings, undefined);
    });

    test('package.json declares the four settings with the documented defaults', () => {
        const pkg = require('../../../../package.json');
        const props = pkg.contributes.configuration.properties;
        assert.strictEqual(props['erlang.mcp.enabled'].default, false);
        assert.strictEqual(props['erlang.mcp.host'].default, '127.0.0.1');
        assert.strictEqual(props['erlang.mcp.port'].default, 0);
        // empty = random token per session; machine scope keeps it out of workspace settings
        assert.strictEqual(props['erlang.mcp.authToken'].default, '');
        assert.strictEqual(props['erlang.mcp.authToken'].scope, 'machine');
        assert.deepStrictEqual(Object.keys(props).filter(k => k.startsWith('erlang.mcp.')).sort(),
            ['erlang.mcp.authToken', 'erlang.mcp.enabled', 'erlang.mcp.host', 'erlang.mcp.port']);
        // no `required` option
        assert.ok(!('erlang.mcp.required' in props));
        assert.ok(pkg.contributes.commands.some((c: any) => c.command === 'erlang.mcp.showConnectionDetails'));
    });
});

suite('MCP: secrets never reach logs', () => {
    test('verbose launch/attach argument logging redacts secret-like keys', () => {
        const out = sanitizeArguments({ node: 'a@b', cookie: 'SENTINEL_COOKIE', authToken: 'SENTINEL_TOKEN', cwd: '/x', password: 'p' });
        assert.deepStrictEqual(out, { node: 'a@b', cookie: '<redacted>', authToken: '<redacted>', cwd: '/x', password: '<redacted>' });
        assert.ok(!JSON.stringify(out).includes('SENTINEL'));
    });

    test('the command line log redacts the Erlang cookie', () => {
        const out = redactArgs(['-noshell', '-setcookie', 'SENTINEL_COOKIE', '-s', 'x']);
        assert.deepStrictEqual(out, ['-noshell', '-setcookie', '<redacted>', '-s', 'x']);
    });

    test('the debugger bridge compiles the MCP inspector modules, all prefixed mcp_', () => {
        const files: string[] = (<any>ErlangDebugConnection.prototype).get_ErlangFiles.call({});
        const mcp = files.filter(f => f.startsWith('mcp/'));
        assert.strictEqual(mcp.length, 8);
        assert.ok(mcp.every(f => /^mcp\/mcp_[a-z_]+\.erl$/.test(f)));
    });
});
