import * as vscode from 'vscode';

export const SHOW_OUTPUT_COMMAND = 'erlang.showLanguageServerOutput';
export const RESTART_COMMAND = 'erlang.restartLanguageServer';

/**
 * Status bar item for the language server: starting / ready (with the OTP
 * version the bridge runs on) / stopped / failed. A start failure used to be
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
        this.set('$(debug-stop) Erlang', 'Erlang language server: stopped\nClick to show the output', true);
    }

    public failed(reason: string): void {
        this.set('$(error) Erlang', `Erlang language server failed to start:\n${reason}\nClick to show the output`, true);
    }

    private set(text: string, tooltip: string, error = false): void {
        this.item.text = text;
        this.item.tooltip = tooltip;
        this.item.backgroundColor = error ? new vscode.ThemeColor('statusBarItem.errorBackground') : undefined;
    }

    public dispose(): void {
        this.item.dispose();
    }
}
