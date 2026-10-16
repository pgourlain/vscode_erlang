import * as fs from 'fs';
import * as path from 'path';
import { commands, env, extensions, version as vscodeVersion, window as Window, workspace } from 'vscode';
import RebarShell, { RebarCandidate, RebarSource } from '../RebarShell';
import { redactArgs } from '../GenericShell';
import { findErl, otpVersion } from '../erlangInstallation';

export const BRIDGE_DIAGNOSTICS_COMMAND = 'erlang.showBridgeDiagnostics';
const EXTENSION_ID = 'pgourlain.erlang';
const OUTPUT_TAIL_CHARS = 6000;

/** `erlang.*` settings copied into the report. Anything else may hold secrets (MCP token...). */
const REPORTED_SETTINGS = ['erlangPath', 'erlangArgs', 'rebarPath', 'rebarBuildArgs',
	'useShell', 'debuggerRunMode', 'cacheManagement', 'verbose'];

export type RebarFileKind = 'missing' | 'escript' | 'script' | 'other';

export interface RebarFileInfo {
	kind: RebarFileKind;
	/** first line of the file when it is a shebang or plain text */
	firstLine?: string;
}

export interface UnknownSetting {
	key: string;
	suggestion?: string;
}

export interface BridgeDiagnosticsInfo {
	extensionVersion: string;
	vscodeVersion: string;
	platform: string;
	arch: string;
	remoteName?: string;
	erlangPath: string;
	erlPath?: string;
	escriptCommand?: string;
	escriptPath?: string;
	otpVersion?: string;
	otpError?: string;
	rebarPath: string;
	rebarSource: RebarSource;
	rebarCandidates: RebarCandidate[];
	rebarFile: RebarFileInfo;
	command?: string;
	cwd?: string;
	failure?: { exitCode: number | null, output: string };
	settings: Record<string, unknown>;
	unknownSettings: UnknownSetting[];
}

export interface BridgeReport {
	hints: string[];
	/** the facts, ready to paste into an issue */
	details: string;
	/** hints + details, to display */
	markdown: string;
}

/** Thrown when the bridge cannot be compiled; carries the diagnostics for the UI. */
export class BridgeCompileError extends Error {
	constructor(message: string, public readonly report: BridgeReport) {
		super(message);
		this.name = 'BridgeCompileError';
	}
}

/** Tells an escript from a shell wrapper (#364): `escript` fails on the latter with "syntax error before: '#'". */
export function classifyRebarFile(filePath: string): RebarFileInfo {
	let head: string;
	try {
		const fd = fs.openSync(filePath, 'r');
		try {
			const buffer = Buffer.alloc(256);
			const read = fs.readSync(fd, buffer, 0, buffer.length, 0);
			head = buffer.subarray(0, read).toString('latin1');
		} finally {
			fs.closeSync(fd);
		}
	} catch {
		return { kind: 'missing' };
	}
	const line = head.split(/\r?\n/, 1)[0];
	const firstLine = /^[\x20-\x7e\t]*$/.test(line) ? line.substring(0, 200) : undefined;
	if (line.startsWith('#!')) {
		return { kind: /escript/.test(line) ? 'escript' : 'script', firstLine };
	}
	return { kind: 'other', firstLine };
}

function editDistance(a: string, b: string): number {
	const row = Array.from({ length: b.length + 1 }, (_, j) => j);
	for (let i = 1; i <= a.length; i++) {
		let diagonal = row[0];
		row[0] = i;
		for (let j = 1; j <= b.length; j++) {
			const above = row[j];
			row[j] = Math.min(row[j] + 1, row[j - 1] + 1, diagonal + (a[i - 1] === b[j - 1] ? 0 : 1));
			diagonal = above;
		}
	}
	return row[b.length];
}

const normalizeKey = (key: string) => key.toLowerCase().replace(/[^a-z0-9]/g, '');

/** `erlang.*` keys set by the user that the extension doesn't define (typos such as `rebar3Path`), with a close known key when there is one. */
export function findUnknownSettings(userKeys: string[], knownKeys: string[]): UnknownSetting[] {
	const known = new Set(knownKeys);
	return userKeys.filter(key => !known.has(key)).map(key => {
		const wanted = normalizeKey(key);
		const suggestion = knownKeys.find(k => editDistance(normalizeKey(k), wanted) <= 2);
		return suggestion ? { key, suggestion } : { key };
	});
}

function redactValue(value: unknown): unknown {
	if (Array.isArray(value)) {
		return redactArgs(value.map(v => typeof v === 'string' ? redactString(v) : v));
	}
	return typeof value === 'string' ? redactString(value) : value;
}

const redactString = (value: string) => value.replace(/(-setcookie\s+)\S+/g, '$1<redacted>');

/** Keeps only the reported settings, with the Erlang cookie removed. */
export function pickSettings(all: Record<string, unknown>): Record<string, unknown> {
	const picked: Record<string, unknown> = {};
	for (const key of REPORTED_SETTINGS) {
		if (key in all) {
			picked[key] = redactValue(all[key]);
		}
	}
	return picked;
}

export function buildHints(info: BridgeDiagnosticsInfo): string[] {
	const hints: string[] = [];
	for (const { key, suggestion } of info.unknownSettings) {
		hints.push(`Setting \`erlang.${key}\` does not exist and is ignored.`
			+ (suggestion ? ` Did you mean \`erlang.${suggestion}\`?` : ''));
	}
	if (!info.erlPath) {
		hints.push('`erl` was not found: install Erlang/OTP, or set `erlang.erlangPath` to its `bin` directory.');
	} else if (info.otpError) {
		hints.push(`\`erl\` does not run (${info.otpError}): check the Erlang/OTP installation, or \`erlang.erlangPath\`.`);
	}
	if (info.escriptCommand && !info.escriptPath) {
		hints.push(`\`${info.escriptCommand}\` was not found: it ships with Erlang/OTP, check \`erlang.erlangPath\` or the PATH.`);
	}
	if (info.rebarFile.kind === 'script') {
		hints.push(`\`${info.rebarPath}\` is a shell script (\`${info.rebarFile.firstLine ?? '#!...'}\`), not an escript: `
			+ 'the extension runs rebar3 with `escript`, which cannot read it ("syntax error before: \'#\'"). '
			+ 'Point `erlang.rebarPath` to a directory holding a real `rebar3` escript (rebar3 from rebar3.org), '
			+ 'or leave it empty to use the one bundled with the extension.');
	} else if (info.rebarFile.kind === 'missing') {
		hints.push(`rebar3 was not found (\`${info.rebarPath}\`). Leave \`erlang.rebarPath\` empty to use the one bundled with the extension.`);
	}
	const configured = info.settings['rebarPath'];
	if (typeof configured === 'string' && configured !== '' && info.rebarSource !== 'search-path') {
		hints.push(`\`erlang.rebarPath\` is "${configured}" but no \`rebar3\`/\`rebar\` file was found in it `
			+ `(it must be a directory, not the file itself): fell back to \`${info.rebarPath}\`.`);
	}
	return hints;
}

const SOURCE_LABEL: Record<RebarSource, string> = {
	'search-path': 'erlang.rebarPath setting',
	'PATH': 'PATH',
	'default-dir': 'bundled with the extension',
	'fallback': 'not found, plain "rebar3" command'
};

export function formatReport(info: BridgeDiagnosticsInfo): BridgeReport {
	const hints = buildHints(info);
	const lines: string[] = [
		`extension: ${EXTENSION_ID} ${info.extensionVersion}`,
		`VS Code: ${info.vscodeVersion}${info.remoteName ? ` (remote: ${info.remoteName})` : ''}`,
		`platform: ${info.platform} ${info.arch}`,
		'',
		`erlang.erlangPath: ${info.erlangPath || '(empty)'}`,
		`erl: ${info.erlPath ?? 'NOT FOUND'}`,
		`OTP: ${info.otpVersion ?? (info.otpError ? `error: ${info.otpError}` : 'unknown')}`,
		`escript: ${info.escriptPath ?? (info.escriptCommand ? `NOT FOUND (${info.escriptCommand})` : 'unknown')}`,
		'',
		`rebar3: ${info.rebarPath} (${SOURCE_LABEL[info.rebarSource]})`,
		`rebar3 file: ${info.rebarFile.kind}${info.rebarFile.firstLine !== undefined ? `, first line: ${info.rebarFile.firstLine}` : ''}`,
		'rebar3 lookup:',
		...info.rebarCandidates.map(c => `  - [${c.exists ? 'found' : 'missing'}] ${c.path} (${c.origin})`),
	];
	if (info.command) {
		lines.push('', `command: ${info.command}`, `cwd: ${info.cwd}`);
	}
	lines.push('', 'settings:', ...Object.entries(info.settings).map(([k, v]) => `  erlang.${k}: ${JSON.stringify(v)}`));
	if (info.failure) {
		const output = info.failure.output.trim();
		lines.push('', `exit code: ${info.failure.exitCode ?? 'none (the process did not start)'}`, 'output:',
			output.length > OUTPUT_TAIL_CHARS ? '[...]\n' + output.slice(-OUTPUT_TAIL_CHARS) : output);
	}
	const details = lines.join('\n');
	const title = info.failure ? 'Erlang bridge: compilation failed' : 'Erlang bridge: diagnostics';
	const markdown = [`# ${title}`, '',
		...(hints.length ? ['## Probable causes', '', ...hints.map(h => `- ${h}`), ''] : []),
		'## Details', '',
		'Paths and the user name appear below: check them before sharing.', '',
		'```text', details, '```', ''].join('\n');
	return { hints, details, markdown };
}

function locateExecutable(command: string): string | undefined {
	const dirs = path.dirname(command) !== '.' ? [path.dirname(command)] : (process.env.PATH ?? '').split(path.delimiter);
	return dirs.filter(d => !!d).map(d => path.join(d, path.basename(command))).find(f => fs.existsSync(f));
}

/** Gathers the facts about the environment `shell` ran in. `failure` is the outcome of the failed rebar3 run, if any. */
export async function collectBridgeDiagnostics(shell: RebarShell,
	failure?: { exitCode: number | null, output: string }): Promise<BridgeReport> {
	const extension = extensions.getExtension(EXTENSION_ID);
	const section = workspace.getConfiguration('erlang');
	const userSettings: Record<string, unknown> = JSON.parse(JSON.stringify(section));
	const contributed = extension?.packageJSON?.contributes?.configuration;
	const properties: Record<string, unknown> = Object.assign({},
		...[].concat(contributed ?? []).map((c: any) => c.properties));
	const knownKeys = Object.keys(properties).filter(k => k.startsWith('erlang.')).map(k => k.split('.')[1]);

	const resolution = await shell.getRebarResolution();
	const run = shell.lastRun;
	const escriptCommand = run?.command ?? (shell.erlangPath
		? path.join(shell.erlangPath, process.platform === 'win32' ? 'escript.exe' : 'escript')
		: process.platform === 'win32' ? 'escript.exe' : 'escript');
	const erlPath = findErl();
	let otp: string | undefined, otpError: string | undefined;
	if (erlPath) {
		try {
			otp = await otpVersion(erlPath);
		} catch (error) {
			otpError = error instanceof Error ? error.message : String(error);
		}
	}

	return formatReport({
		extensionVersion: extension?.packageJSON?.version ?? 'unknown',
		vscodeVersion,
		platform: process.platform,
		arch: process.arch,
		remoteName: env.remoteName,
		erlangPath: String(userSettings['erlangPath'] ?? ''),
		erlPath,
		escriptCommand,
		escriptPath: locateExecutable(escriptCommand),
		otpVersion: otp,
		otpError,
		rebarPath: resolution.path,
		rebarSource: resolution.source,
		rebarCandidates: resolution.candidates,
		rebarFile: classifyRebarFile(resolution.path),
		command: run ? [run.command, ...run.args].join(' ') : undefined,
		cwd: run?.cwd,
		failure,
		settings: pickSettings(userSettings),
		unknownSettings: findUnknownSettings(Object.keys(userSettings), knownKeys)
	});
}

export async function showBridgeReport(report: BridgeReport): Promise<void> {
	const document = await workspace.openTextDocument({ language: 'markdown', content: report.markdown });
	await Window.showTextDocument(document, { preview: false });
}

/** Opens VS Code's issue reporter for this extension, prefilled with the report for the user to review. */
export function reportBridgeIssue(report: BridgeReport): Thenable<unknown> {
	return commands.executeCommand('workbench.action.openIssueReporter', {
		extensionId: EXTENSION_ID,
		issueTitle: 'Erlang bridge cannot compile',
		issueBody: `<!-- What happened? -->\n\n\n## Diagnostics\n\n\`\`\`text\n${report.details}\n\`\`\`\n`
	});
}
