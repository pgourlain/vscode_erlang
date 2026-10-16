import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import {
	buildHints, BridgeDiagnosticsInfo, classifyRebarFile, findUnknownSettings, formatReport, pickSettings
} from '../../lib/lsp/bridgeDiagnostics';
import RebarShell from '../../lib/RebarShell';
import { ILogOutput } from '../../lib/GenericShell';

const noLog: ILogOutput = { appendLine: () => { }, debug: () => { } };

function baseInfo(overrides: Partial<BridgeDiagnosticsInfo> = {}): BridgeDiagnosticsInfo {
	return {
		extensionVersion: '1.2.7', vscodeVersion: '1.136.0', platform: 'linux', arch: 'x64',
		erlangPath: '', erlPath: '/usr/bin/erl', escriptCommand: 'escript', escriptPath: '/usr/bin/escript',
		otpVersion: '26.2.5', rebarPath: '/ext/rebar3', rebarSource: 'default-dir',
		rebarCandidates: [{ path: '/ext/rebar3', exists: true, origin: 'default-dir' }],
		rebarFile: { kind: 'escript', firstLine: '#!/usr/bin/env escript' },
		settings: {}, unknownSettings: [],
		...overrides
	};
}

suite('bridge diagnostics', () => {
	let dir: string;
	setup(() => { dir = fs.mkdtempSync(path.join(os.tmpdir(), 'bridge-diag-')); });
	teardown(() => { fs.rmSync(dir, { recursive: true, force: true }); });

	const write = (name: string, content: string | Buffer) => {
		const file = path.join(dir, name);
		fs.writeFileSync(file, content);
		return file;
	};

	suite('classifyRebarFile', () => {
		test('escript shebang', () => {
			assert.deepStrictEqual(classifyRebarFile(write('rebar3', '#!/usr/bin/env escript\n%% x')),
				{ kind: 'escript', firstLine: '#!/usr/bin/env escript' });
		});

		test('shell wrapper (#364)', () => {
			assert.strictEqual(classifyRebarFile(write('rebar3', '#!/usr/bin/sh\nexec erl -s rebar3 main')).kind, 'script');
		});

		test('no shebang, binary content gives no first line', () => {
			const info = classifyRebarFile(write('rebar3', Buffer.from([0x50, 0x4b, 0x03, 0x04, 0x00, 0xff])));
			assert.strictEqual(info.kind, 'other');
			assert.strictEqual(info.firstLine, undefined);
		});

		test('missing file', () => {
			assert.strictEqual(classifyRebarFile(path.join(dir, 'nope')).kind, 'missing');
		});
	});

	suite('findUnknownSettings', () => {
		const known = ['rebarPath', 'erlangPath', 'verbose', 'mcp'];

		test('suggests the close known key for a typo (#364: rebar3Path)', () => {
			assert.deepStrictEqual(findUnknownSettings(['rebar3Path', 'verbose'], known),
				[{ key: 'rebar3Path', suggestion: 'rebarPath' }]);
		});

		test('no suggestion when nothing is close', () => {
			assert.deepStrictEqual(findUnknownSettings(['somethingElse'], known), [{ key: 'somethingElse' }]);
		});
	});

	suite('pickSettings', () => {
		test('keeps only reported settings and hides the cookie', () => {
			const picked = pickSettings({
				rebarPath: '/x', erlangArgs: ['-setcookie', 'secret', '-pa', 'a'], mcp: { authToken: 'tok' }
			});
			assert.deepStrictEqual(picked, { rebarPath: '/x', erlangArgs: ['-setcookie', '<redacted>', '-pa', 'a'] });
		});
	});

	suite('hints', () => {
		test('no hint when everything looks fine', () => {
			assert.deepStrictEqual(buildHints(baseInfo()), []);
		});

		test('shell script rebar3 is explained', () => {
			const hints = buildHints(baseInfo({ rebarFile: { kind: 'script', firstLine: '#!/usr/bin/sh' } }));
			assert.strictEqual(hints.length, 1);
			assert.match(hints[0], /shell script/);
			assert.match(hints[0], /erlang\.rebarPath/);
		});

		test('unknown setting, missing escript and erl', () => {
			const hints = buildHints(baseInfo({
				unknownSettings: [{ key: 'rebar3Path', suggestion: 'rebarPath' }],
				escriptPath: undefined, erlPath: undefined
			}));
			assert.strictEqual(hints.length, 3);
			assert.match(hints[0], /Did you mean `erlang\.rebarPath`/);
		});

		test('rebarPath set but not honoured', () => {
			const hints = buildHints(baseInfo({ settings: { rebarPath: '/home/me/rebar3' }, rebarSource: 'PATH' }));
			assert.strictEqual(hints.length, 1);
			assert.match(hints[0], /must be a directory/);
		});
	});

	suite('formatReport', () => {
		test('contains the failure output and the lookup, hints first', () => {
			const report = formatReport(baseInfo({
				failure: { exitCode: 127, output: 'syntax error before: #' },
				rebarFile: { kind: 'script', firstLine: '#!/usr/bin/sh' }
			}));
			assert.match(report.details, /exit code: 127/);
			assert.match(report.details, /syntax error before: #/);
			assert.match(report.details, /\[found\] \/ext\/rebar3/);
			assert.ok(report.markdown.indexOf('Probable causes') < report.markdown.indexOf('## Details'));
			assert.ok(!report.details.includes('Probable causes'));
		});
	});

	suite('RebarShell.getRebarResolution', () => {
		test('rebarPath directory wins and is reported', async () => {
			const file = write('rebar3', '#!/usr/bin/env escript\n');
			const resolution = await new RebarShell([dir], path.join(dir, 'other'), noLog).getRebarResolution();
			assert.strictEqual(resolution.source, 'search-path');
			assert.strictEqual(resolution.path, file);
		});

		test('an empty rebarPath is skipped, the default directory is the last resort', async () => {
			const resolution = await new RebarShell([''], dir, noLog).getRebarResolution();
			const tried = resolution.candidates.map(c => c.origin);
			assert.ok(!tried.includes('search-path'));
			if (resolution.source !== 'PATH') {
				assert.strictEqual(resolution.source, 'fallback');
			}
		});

		test('a file given as rebarPath is not found, and the miss is listed', async () => {
			const file = write('rebar3_escript', '#!/usr/bin/env escript\n');
			const resolution = await new RebarShell([file], path.join(dir, 'empty'), noLog).getRebarResolution();
			assert.notStrictEqual(resolution.source, 'search-path');
			assert.ok(resolution.candidates.some(c => c.origin === 'search-path' && !c.exists));
		});
	});
});
