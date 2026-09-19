import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import { execFile } from 'child_process';
import RebarShell from './RebarShell';
import { getElangConfigConfiguration } from './ErlangConfigurationProvider';

export const CHECK_INSTALLATION_COMMAND = 'erlang.checkInstallation';
/** Backs the `${command:erlpath}` debug configuration variable. */
export const GET_ERL_PATH_COMMAND = 'erlang.getErlPath';

const exe = (name: string) => process.platform == 'win32' ? name + '.exe' : name;

/** `erlang.erlangPath`, made absolute against the workspace root. */
function erlangBinDir(): string | undefined {
    const cfg = getElangConfigConfiguration();
    if (!cfg.erlangPath) {
        return undefined;
    }
    return path.isAbsolute(cfg.erlangPath) || !cfg.rootPath ? cfg.erlangPath : path.join(cfg.rootPath, cfg.erlangPath);
}

/**
 * Absolute path of the `erl` executable the extension uses: the one in
 * `erlang.erlangPath` when set, otherwise the first one on the PATH.
 */
export function findErl(): string | undefined {
    const binDir = erlangBinDir();
    const dirs = binDir ? [binDir] : (process.env.PATH ?? '').split(path.delimiter);
    for (const dir of dirs.filter(d => !!d)) {
        const candidate = path.join(dir, exe('erl'));
        if (fs.existsSync(candidate)) {
            return candidate;
        }
    }
    return undefined;
}

function run(file: string, args: string[], cwd?: string): Promise<string> {
    return new Promise((resolve, reject) => {
        execFile(file, args, { cwd, timeout: 30000 }, (error, stdout, stderr) => {
            if (error) {
                reject(new Error((stderr || error.message).toString().trim()));
            } else {
                resolve(stdout.toString().trim());
            }
        });
    });
}

const OTP_VERSION_EVAL =
    'R = erlang:system_info(otp_release), ' +
    'F = filename:join([code:root_dir(), "releases", R, "OTP_VERSION"]), ' +
    'V = case file:read_file(F) of {ok, B} -> string:trim(B); _ -> R end, ' +
    'io:put_chars(V), halt().';

export async function otpVersion(erl: string): Promise<string> {
    return run(erl, ['-noshell', '-eval', OTP_VERSION_EVAL]);
}

async function rebarVersion(extensionPath: string): Promise<{ rebar: string, version: string }> {
    const cfg = getElangConfigConfiguration();
    const rebar = await new RebarShell([cfg.rebarPath, cfg.rootPath].filter(p => !!p), extensionPath, undefined)
        .getRebarFullPath();
    const binDir = erlangBinDir();
    const escript = binDir ? path.join(binDir, exe('escript')) : exe('escript');
    return { rebar, version: await run(escript, [rebar, 'version'], cfg.rootPath) };
}

/** Walkthrough step "Find erl": report the OTP and rebar3 the extension will use. */
async function checkInstallation(extensionPath: string): Promise<void> {
    const erl = findErl();
    if (!erl) {
        const choice = await vscode.window.showErrorMessage(
            'Erlang: `erl` was not found on the PATH. Install Erlang/OTP, or set `erlang.erlangPath` to the directory holding erl.',
            'Open Settings', 'Download Erlang/OTP');
        if (choice === 'Open Settings') {
            vscode.commands.executeCommand('workbench.action.openSettings', 'erlang.erlangPath');
        } else if (choice) {
            vscode.env.openExternal(vscode.Uri.parse('https://www.erlang.org/downloads'));
        }
        return;
    }
    let otp: string;
    try {
        otp = await otpVersion(erl);
    } catch (error) {
        vscode.window.showErrorMessage(`Erlang: ${erl} does not run: ${error.message}`);
        return;
    }
    let rebarInfo: string;
    try {
        const { rebar, version } = await rebarVersion(extensionPath);
        const bundled = path.dirname(rebar) === path.normalize(extensionPath);
        rebarInfo = `${version} (${bundled ? 'bundled with the extension' : rebar})`;
    } catch (error) {
        rebarInfo = `not working: ${error.message}`;
    }
    vscode.window.showInformationMessage(`Erlang/OTP ${otp} found at ${erl}. rebar3: ${rebarInfo}.`);
}

export function activate(context: vscode.ExtensionContext): void {
    context.subscriptions.push(vscode.commands.registerCommand(CHECK_INSTALLATION_COMMAND,
        () => checkInstallation(context.extensionPath)));
    context.subscriptions.push(vscode.commands.registerCommand(GET_ERL_PATH_COMMAND, () => {
        const erl = findErl();
        if (!erl) {
            throw new Error('erl was not found: set erlang.erlangPath or add Erlang/OTP to the PATH');
        }
        return erl;
    }));
}
