import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import * as vscode from 'vscode';
import { detectTasks, rebarArgs } from '../../lib/erlangTaskProvider';

function withRebarConfig(contents: string, fn: (file: string) => void) {
	const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'rebar3-tasks-'));
	const file = path.join(dir, 'rebar.config');
	fs.writeFileSync(file, contents);
	try {
		fn(file);
	} finally {
		fs.rmSync(dir, { recursive: true, force: true });
	}
}

const commands = (file: string) => detectTasks(file).map(t => t.command);

suite('rebar3 task provider', () => {
	test('offers the standard rebar3 tasks', () => {
		withRebarConfig('{erl_opts, [debug_info]}.\n', file => {
			assert.deepStrictEqual(commands(file), ['compile', 'eunit', 'ct', 'dialyzer', 'shell', 'clean']);
		});
	});

	test('offers release only when relx is configured', () => {
		withRebarConfig('{relx, [{release, {myapp, "0.1.0"}, [myapp]}]}.\n', file => {
			assert.ok(commands(file).includes('release'));
		});
		withRebarConfig('%% {relx, [{release, {myapp, "0.1.0"}, [myapp]}]}.\n', file => {
			assert.ok(!commands(file).includes('release'));
		});
	});

	// #19: `rebar3 escriptize` was unreachable from the task provider even
	// though it needs nothing beyond what release/relx detection already does.
	test('offers escriptize only when the project builds one', () => {
		withRebarConfig('{escript_main_app, myapp}.\n', file => {
			assert.ok(commands(file).includes('escriptize'));
		});
		withRebarConfig('{erl_opts, [debug_info]}.\n', file => {
			assert.ok(!commands(file).includes('escriptize'));
		});
	});

	test('compile is the build task, eunit and ct are test tasks', () => {
		withRebarConfig('', file => {
			const tasks = detectTasks(file);
			assert.strictEqual(tasks.find(t => t.command === 'compile').group, vscode.TaskGroup.Build);
			assert.strictEqual(tasks.find(t => t.command === 'eunit').group, vscode.TaskGroup.Test);
			assert.strictEqual(tasks.find(t => t.command === 'ct').group, vscode.TaskGroup.Test);
		});
	});

	test('profile and args', () => {
		assert.deepStrictEqual(rebarArgs({ type: 'rebar3', command: 'ct', profile: 'test', args: ['--suite', 'a_SUITE'] }),
			['as', 'test', 'ct', '--suite', 'a_SUITE']);
		assert.deepStrictEqual(rebarArgs({ type: 'rebar3', command: 'compile' }), ['compile']);
	});
});
