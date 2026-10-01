import * as assert from 'assert';
import * as fs from 'fs';
import * as http from 'http';
import * as net from 'net';
import * as os from 'os';
import * as path from 'path';
import { ChildProcess, spawn } from 'child_process';

// End-to-end: the real debug adapter (out/lib/erlangDebug.js) over DAP/stdio launches a real
// `erl` target. Covers the enablement gate, the secret hand-over, the rebar.config policy, fixed
// port conflicts, lease renewal and cleanup. Requires `erl` on the PATH (as the other debugger tests).
const repoRoot = path.join(__dirname, '..', '..', '..', '..');

class DapClient {
    private buf = Buffer.alloc(0);
    private seq = 1;
    private listeners: Array<(m: any) => void> = [];
    readonly events: any[] = [];
    readonly child: ChildProcess;

    constructor() {
        this.child = spawn(process.execPath, [path.join(repoRoot, 'out', 'lib', 'erlangDebug.js')],
            { stdio: 'pipe', env: { ...process.env, ELECTRON_RUN_AS_NODE: '1' } });
        this.child.stdout!.on('data', d => this.onData(d));
    }

    private onData(d: Buffer) {
        this.buf = Buffer.concat([this.buf, d]);
        for (;;) {
            const end = this.buf.indexOf('\r\n\r\n');
            if (end < 0) { return; }
            const len = parseInt(/Content-Length: (\d+)/.exec(this.buf.slice(0, end).toString())![1]);
            if (this.buf.length < end + 4 + len) { return; }
            const msg = JSON.parse(this.buf.slice(end + 4, end + 4 + len).toString());
            this.buf = this.buf.slice(end + 4 + len);
            if (msg.type === 'event') { this.events.push(msg); }
            this.listeners.slice().forEach(l => l(msg));
        }
    }

    request(command: string, args: any): Promise<any> {
        const seq = this.seq++;
        const body = JSON.stringify({ seq, type: 'request', command, arguments: args });
        this.child.stdin!.write(`Content-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}`);
        return new Promise(resolve => {
            const l = (m: any) => {
                if (m.type === 'response' && m.request_seq === seq) {
                    this.listeners.splice(this.listeners.indexOf(l), 1);
                    resolve(m);
                }
            };
            this.listeners.push(l);
        });
    }

    waitEvent(name: string, pred: (b: any) => boolean = () => true, timeout = 40000): Promise<any> {
        const found = this.events.find(e => e.event === name && pred(e.body));
        if (found) { return Promise.resolve(found.body); }
        return new Promise((resolve, reject) => {
            const timer = setTimeout(() => reject(new Error(`timeout waiting for '${name}'`)), timeout);
            const l = (m: any) => {
                if (m.type === 'event' && m.event === name && pred(m.body)) {
                    clearTimeout(timer);
                    this.listeners.splice(this.listeners.indexOf(l), 1);
                    resolve(m.body);
                }
            };
            this.listeners.push(l);
        });
    }

    async attach(project: string, extra: any): Promise<void> {
        const init = await this.request('initialize', { adapterID: 'erlang', pathFormat: 'path', linesStartAt1: true, columnsStartAt1: true });
        assert.ok(init.success, init.message);
        const attach = this.request('attach', { cwd: project, verbose: false, useShell: false, erlangPath: '', ...extra });
        await this.waitEvent('initialized');
        assert.ok((await attach).success);
        assert.ok((await this.request('configurationDone', {})).success);
    }

    async launch(project: string, extra: any): Promise<void> {
        const init = await this.request('initialize', { adapterID: 'erlang', pathFormat: 'path', linesStartAt1: true, columnsStartAt1: true });
        assert.ok(init.success, init.message);
        const launch = this.request('launch', { cwd: project, verbose: false, useShell: false, erlangPath: '', ...extra });
        await this.waitEvent('initialized');
        assert.ok((await launch).success);
        assert.ok((await this.request('configurationDone', {})).success);
    }

    async close(): Promise<void> {
        await this.request('disconnect', { terminateDebuggee: true }).catch(() => undefined);
        await new Promise(r => setTimeout(r, 500));
        this.child.kill();
    }
}

function post(url: string, token: string, body: any): Promise<{ status: number, json: any }> {
    return new Promise((resolve, reject) => {
        const u = new URL(url);
        const req = http.request({ host: u.hostname, port: u.port, path: u.pathname, method: 'POST',
            headers: { 'Content-Type': 'application/json', Authorization: 'Bearer ' + token } }, res => {
            let data = '';
            res.on('data', d => data += d);
            res.on('end', () => resolve({ status: res.statusCode!, json: data ? JSON.parse(data) : undefined }));
        });
        req.on('error', reject);
        req.end(JSON.stringify(body));
    });
}

function refused(url: string): Promise<boolean> {
    const u = new URL(url);
    return new Promise(resolve => {
        const s = net.connect(Number(u.port), u.hostname);
        s.on('connect', () => { s.destroy(); resolve(false); });
        s.on('error', () => resolve(true));
    });
}

const call = (url: string, token: string, name: string, args: any = {}) =>
    post(url, token, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } });

suite('MCP: debug adapter end-to-end (real erl target)', function () {
    this.timeout(90000);
    let project: string;
    let client: DapClient;
    let attachTarget: ChildProcess | undefined;

    setup(() => {
        project = fs.mkdtempSync(path.join(os.tmpdir(), 'mcp-e2e-'));
    });

    teardown(async () => {
        attachTarget?.kill();
        attachTarget = undefined;
        await client?.close();
        fs.rmSync(project, { recursive: true, force: true });
    });

    const settings = (over: any = {}) => ({ mcpSettings: { enabled: true, host: '127.0.0.1', port: 0, trusted: true, root: project, ...over } });

    test('enabled: endpoint announced without the secret, policy honoured, lease renewed, cleaned up', async () => {
        fs.writeFileSync(path.join(project, 'rebar.config'),
            '{mcp, [{allowed_tools, [<<"runtime_summary">>, <<"debug_session">>]}]}.\n');
        client = new DapClient();
        await client.launch(project, settings());
        const started = await client.waitEvent('erlangMcp', b => b.status !== undefined);
        assert.strictEqual(started.status, 'started', JSON.stringify(started));
        assert.match(started.url, /^http:\/\/127\.0\.0\.1:\d+\/mcp$/);

        // the credential is in a 0600 descriptor, never in a DAP event or output
        const descriptor = JSON.parse(fs.readFileSync(started.descriptor, 'utf8'));
        if (process.platform !== 'win32') {
            assert.strictEqual(fs.statSync(started.descriptor).mode & 0o777, 0o600);
        }
        assert.ok(descriptor.token.length >= 43);
        assert.ok(!JSON.stringify(client.events).includes(descriptor.token), 'token leaked in a DAP message');

        assert.strictEqual((await post(started.url, 'wrong-token', { jsonrpc: '2.0', id: 1, method: 'ping' })).status, 401);
        const list = await post(started.url, descriptor.token, { jsonrpc: '2.0', id: 1, method: 'tools/list' });
        assert.deepStrictEqual(list.json.result.tools.map((t: any) => t.name), ['runtime_summary', 'debug_session']);
        const dbg = await call(started.url, descriptor.token, 'debug_session');
        assert.strictEqual(dbg.json.result.structuredContent.mode, 'launch');
        assert.strictEqual((await call(started.url, descriptor.token, 'process_info', { name: 'init' })).json.error.code, -32602);

        // the adapter renews the lease (target lease: 8 s)
        await new Promise(r => setTimeout(r, 10000));
        assert.strictEqual((await post(started.url, descriptor.token, { jsonrpc: '2.0', id: 1, method: 'ping' })).status, 200);

        // request journal: one erlangMcpCall event per request, metadata only
        const journal = await client.waitEvent('erlangMcpCall', b => b.method === 'tools/call' && b.tool === 'debug_session');
        assert.strictEqual(journal.status, 'ok');
        assert.strictEqual(typeof journal.durationMs, 'number');
        assert.ok(!JSON.stringify(client.events.filter(e => e.event === 'erlangMcpCall')).includes(descriptor.token));

        const dir = path.dirname(started.descriptor);
        await client.request('disconnect', { terminateDebuggee: true });
        const stopped = await client.waitEvent('erlangMcp', b => b.status === 'stopped', 15000);
        assert.ok(stopped.calls >= 4, JSON.stringify(stopped));
        assert.strictEqual(typeof stopped.serverMs, 'number');
        assert.ok(!fs.existsSync(dir), 'descriptor directory must be removed');
        await new Promise(r => setTimeout(r, 1000));
        assert.ok(await refused(started.url), 'endpoint must be closed');
    });

    test('all tools and the prompt answer in a real target (default policy)', async () => {
        client = new DapClient();
        await client.launch(project, settings());
        const started = await client.waitEvent('erlangMcp', b => b.status !== undefined);
        assert.strictEqual(started.status, 'started', JSON.stringify(started));
        const token = JSON.parse(fs.readFileSync(started.descriptor, 'utf8')).token;
        const tools = (await post(started.url, token, { jsonrpc: '2.0', id: 1, method: 'tools/list' })).json.result.tools.map((t: any) => t.name);
        assert.strictEqual(tools.length, 12, tools.join(','));
        const ok = async (name: string, args: any = {}) => {
            const r = await call(started.url, token, name, args);
            assert.ok(r.json.result && r.json.result.isError === false, name + ': ' + JSON.stringify(r.json));
            return r.json.result.structuredContent;
        };
        const rt = await ok('runtime_summary');
        assert.ok(rt.resources.processes.count > 0 && rt.resources.atoms.limit > 0);
        const top = await ok('top_processes', { limit: 3, format: 'mermaid' });
        assert.strictEqual(top.entities.length, 3);
        assert.ok(top.mermaid.startsWith('graph TD'));
        assert.ok((await ok('top_ports', { limit: 3 })).entities.length > 0);
        assert.ok((await ok('ets_summary', { limit: 3 })).entities.length > 0);
        const topo = await ok('topology_overview', { name: 'kernel', format: 'mermaid' });
        assert.ok(topo.relationships.some((r: any) => r.type === 'supervises'), 'kernel supervision tree');
        assert.ok(topo.mermaid.includes('-->'));
        const changes = await ok('changes_since', { collectionId: topo.collectionId });
        assert.strictEqual(changes.scope.baselineTool, 'topology_overview');
        const prompt = await post(started.url, token, { jsonrpc: '2.0', id: 1, method: 'prompts/get',
            params: { name: 'map_application', arguments: { application: 'kernel' } } });
        assert.ok(prompt.json.result.messages[0].content.text.includes('kernel'));
        assert.ok(!JSON.stringify(client.events).includes(token), 'token leaked in a DAP message');
        await client.request('disconnect', { terminateDebuggee: true });
    });

    test('disabled (default): no listener, no event, debugging works', async () => {
        client = new DapClient();
        await client.launch(project, {});
        await new Promise(r => setTimeout(r, 6000));
        assert.strictEqual(client.events.filter(e => e.event === 'erlangMcp').length, 0);
    });

    test('noDebug never starts the inspector, even when enabled', async () => {
        client = new DapClient();
        await client.launch(project, { noDebug: true, ...settings() });
        await new Promise(r => setTimeout(r, 6000));
        assert.strictEqual(client.events.filter(e => e.event === 'erlangMcp').length, 0);
    });

    test('untrusted workspace disables only MCP', async () => {
        client = new DapClient();
        await client.launch(project, settings({ trusted: false }));
        const err = await client.waitEvent('erlangMcp', b => b.status === 'error', 60000);
        assert.match(err.message, /not trusted/);
    });

    test('non-loopback host disables only MCP', async () => {
        client = new DapClient();
        await client.launch(project, settings({ host: '0.0.0.0' }));
        const err = await client.waitEvent('erlangMcp', b => b.status === 'error', 60000);
        assert.match(err.message, /loopback/);
    });

    test('malformed or profile-only rebar.config policy disables MCP with an actionable error', async () => {
        fs.writeFileSync(path.join(project, 'rebar.config'), '{mcp, [{allowed_tools, [}.\n');
        client = new DapClient();
        await client.launch(project, settings());
        const err = await client.waitEvent('erlangMcp', b => b.status === 'error');
        assert.match(err.message, /malformed/);
        assert.ok(!/token|Bearer/i.test(err.message));
    });

    test('attach: the inspector runs in the attached node, and a detach leaves that node running', async function () {
        const nodeName = `mcp_e2e_${process.pid}`;
        const target = spawn('erl', ['-noshell', '-sname', nodeName, '-eval', 'receive stop -> ok end'], { stdio: 'ignore' });
        attachTarget = target;
        try {
            await new Promise(r => setTimeout(r, 2500));
            const host = os.hostname().split('.')[0];
            client = new DapClient();
            await client.attach(project, { node: `${nodeName}@${host}`, ...settings() });
            const started = await client.waitEvent('erlangMcp', b => b.status !== undefined, 60000);
            assert.strictEqual(started.status, 'started', JSON.stringify(started));
            const descriptor = JSON.parse(fs.readFileSync(started.descriptor, 'utf8'));
            const summary = await call(started.url, descriptor.token, 'runtime_summary', { redactNodeHost: false });
            assert.strictEqual(summary.json.result.structuredContent.node, `${nodeName}@${host}`);
            const dbg = await call(started.url, descriptor.token, 'debug_session');
            assert.strictEqual(dbg.json.result.structuredContent.mode, 'attach');

            await client.request('disconnect', { terminateDebuggee: false });
            await client.waitEvent('erlangMcp', b => b.status === 'stopped', 15000);
            await new Promise(r => setTimeout(r, 1500));
            assert.ok(await refused(started.url), 'endpoint must be closed after the detach');
            assert.strictEqual(target.exitCode, null, 'the attached node must keep running');
        } finally {
            target.kill();
        }
    });

    test('fixed port already in use: MCP disabled, no rebinding', async () => {
        const busy = net.createServer();
        await new Promise<void>(r => busy.listen(0, '127.0.0.1', () => r()));
        const port = (busy.address() as net.AddressInfo).port;
        try {
            client = new DapClient();
            await client.launch(project, settings({ port }));
            const err = await client.waitEvent('erlangMcp', b => b.status === 'error');
            assert.match(err.message, /already in use/);
        } finally {
            busy.close();
        }
    });
});
