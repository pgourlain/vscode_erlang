import * as vscode from 'vscode';
import { CHECK_INSTALLATION_COMMAND } from '../erlangInstallation';

export const SHOW_OUTPUT_COMMAND = 'erlang.showLanguageServerOutput';
export const RESTART_COMMAND = 'erlang.restartLanguageServer';
const ERROR_BACKGROUND = 'statusBarItem.errorBackground';

/**
 * Status bar item for the language server: starting / ready (with the OTP
 * version the bridge runs on) / stopped / failed / Erlang not found. A start failure used to be
 * visible only in the output channel with `erlang.verbose` on.
 */
export class LspStatus implements vscode.Disposable {
    private item: vscode.StatusBarItem;

    constructor() {
        this.item = vscode.window.createStatusBarItem('erlang.languageServer', vscode.StatusBarAlignment.Left, 1);
        this.item.name = 'Erlang Language Server';
        this.item.command = SHOW_OUTPUT_COMMAND;
        this.item.show();
    }

    public starting(): void {
        this.set('$(sync~spin) Erlang', 'Erlang language server: starting (compiling the bridge, then starting erl)');
    }

    public ready(otpVersion: string | undefined): void {
        const otp = otpVersion ? ` · OTP ${otpVersion}` : '';
        this.set(`$(check) Erlang${otp}`, `Erlang language server: ready${otp ? `\nRunning on OTP ${otpVersion}` : ''}\nClick to show the output`);
    }

    public stopped(): void {
        this.set('$(debug-stop) Erlang', 'Erlang language server: stopped\nClick to show the output', ERROR_BACKGROUND);
    }

    public failed(reason: string): void {
        this.set('$(error) Erlang', `Erlang language server failed to start:\n${reason}\nClick to show the output`, ERROR_BACKGROUND);
    }

    public erlangNotFound(): void {
        this.set('$(warning) Erlang', 'Erlang/OTP not found: the language server is not started.\n'
            + 'Install Erlang/OTP or set erlang.erlangPath.\nClick to check the installation',
            'statusBarItem.warningBackground', CHECK_INSTALLATION_COMMAND);
    }

    private set(text: string, tooltip: string, background?: string, command = SHOW_OUTPUT_COMMAND): void {
        this.item.text = text;
        this.item.tooltip = tooltip;
        this.item.command = command;
        this.item.backgroundColor = background ? new vscode.ThemeColor(background) : undefined;
    }

    public dispose(): void {
        this.item.dispose();
    }
}
