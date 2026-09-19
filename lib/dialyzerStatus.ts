import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import { getElangConfigConfiguration } from './ErlangConfigurationProvider';
import { REBAR3_TASK_TYPE } from './erlangTaskProvider';

export type PltState =
    { kind: 'none' } |
    { kind: 'missing' } |
    { kind: 'stale', plt: string, mtime: Date, newerThan: string } |
    { kind: 'ready', plt: string, mtime: Date };

/**
 * Status of the project's rebar3 dialyzer PLT, read from disk.
 *
 * rebar3 keeps the project PLT at `_build/<profile>/<prefix>_<otp>_plt` and
 * updates it incrementally: a run only rebuilds it for the apps whose beams
 * changed. The PLT covers OTP and the dependencies, not the project's own
 * modules, so it goes stale when the dependencies change - rebar.lock or
 * rebar.config newer than the PLT.
 */
export function pltState(rootPath: string | undefined): PltState {
    if (!rootPath || !fs.existsSync(path.join(rootPath, 'rebar.config'))) {
        return { kind: 'none' };
    }
    const plt = newestPlt(path.join(rootPath, '_build'));
    if (!plt) {
        return { kind: 'missing' };
    }
    for (const input of ['rebar.lock', 'rebar.config']) {
        const inputMtime = mtimeOf(path.join(rootPath, input));
        if (inputMtime && inputMtime > plt.mtime) {
            return { kind: 'stale', plt: plt.file, mtime: plt.mtime, newerThan: input };
        }
    }
    return { kind: 'ready', plt: plt.file, mtime: plt.mtime };
}

function newestPlt(buildDir: string): { file: string, mtime: Date } | undefined {
    let newest: { file: string, mtime: Date } | undefined;
    let profiles: string[] = [];
    try {
        profiles = fs.readdirSync(buildDir);
    } catch {
        return undefined;
    }
    for (const profile of profiles) {
        let entries: string[] = [];
        try {
            entries = fs.readdirSync(path.join(buildDir, profile));
        } catch {
            continue;
        }
        for (const entry of entries.filter(e => e.endsWith('_plt'))) {
            const file = path.join(buildDir, profile, entry);
            const mtime = mtimeOf(file);
            if (mtime && (!newest || mtime > newest.mtime)) {
                newest = { file, mtime };
            }
        }
    }
    return newest;
}

function mtimeOf(file: string): Date | undefined {
    try {
        return fs.statSync(file).mtime;
    } catch {
        return undefined;
    }
}

/** Status bar item showing the PLT state; a click runs dialyzer. */
export class DialyzerStatus implements vscode.Disposable {
    private item: vscode.StatusBarItem;
    private running = 0;
    private disposables: vscode.Disposable[] = [];

    constructor() {
        this.item = vscode.window.createStatusBarItem('erlang.dialyzer', vscode.StatusBarAlignment.Left, 0);
        this.item.name = 'Erlang Dialyzer';
        this.item.command = 'extension.dialyzer';
        this.watch();
        this.disposables.push(vscode.tasks.onDidStartTask(e => {
            if (isDialyzerTask(e.execution.task)) { this.setRunning(true); }
        }));
        this.disposables.push(vscode.tasks.onDidEndTask(e => {
            if (isDialyzerTask(e.execution.task)) { this.setRunning(false); }
        }));
        this.disposables.push(vscode.workspace.onDidChangeWorkspaceFolders(() => {
            this.watch();
            this.refresh();
        }));
        this.refresh();
    }

    public setRunning(running: boolean): void {
        this.running = Math.max(0, this.running + (running ? 1 : -1));
        this.refresh();
    }

    public refresh(): void {
        const state = pltState(getElangConfigConfiguration().rootPath);
        if (state.kind === 'none') {
            this.item.hide();
            return;
        }
        if (this.running > 0) {
            this.item.text = '$(sync~spin) Dialyzer';
            this.item.tooltip = 'Dialyzer is running (the PLT is updated incrementally first)';
        } else {
            switch (state.kind) {
                case 'missing':
                    this.item.text = '$(shield) PLT: none';
                    this.item.tooltip = 'No dialyzer PLT yet: the first run builds it and can take several minutes. Click to run dialyzer.';
                    break;
                case 'stale':
                    this.item.text = '$(shield) PLT: stale';
                    this.item.tooltip = `${state.newerThan} changed after the PLT was built (${state.mtime.toLocaleString()}); the next run updates it incrementally. Click to run dialyzer.\n${state.plt}`;
                    break;
                case 'ready':
                    this.item.text = '$(shield) PLT: ready';
                    this.item.tooltip = `Dialyzer PLT up to date (${state.mtime.toLocaleString()}). Click to run dialyzer.\n${state.plt}`;
                    break;
            }
        }
        this.item.show();
    }

    private watcher: vscode.FileSystemWatcher | undefined;

    private watch(): void {
        this.watcher?.dispose();
        const rootPath = getElangConfigConfiguration().rootPath;
        if (!rootPath) {
            this.watcher = undefined;
            return;
        }
        this.watcher = vscode.workspace.createFileSystemWatcher(
            new vscode.RelativePattern(rootPath, '{rebar.config,rebar.lock,_build/*/*_plt}'));
        const refresh = () => this.refresh();
        this.watcher.onDidCreate(refresh);
        this.watcher.onDidChange(refresh);
        this.watcher.onDidDelete(refresh);
    }

    public dispose(): void {
        this.watcher?.dispose();
        this.disposables.forEach(d => d.dispose());
        this.item.dispose();
    }
}

function isDialyzerTask(task: vscode.Task): boolean {
    return task.definition.type === REBAR3_TASK_TYPE && task.definition.command === 'dialyzer';
}
