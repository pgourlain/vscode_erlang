import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import RebarShell from './RebarShell';
import { getElangConfigConfiguration } from './ErlangConfigurationProvider';

/** `contributes.taskDefinitions` entry with type `rebar3` (see package.json). */
export interface Rebar3TaskDefinition extends vscode.TaskDefinition {
    command: string;
    args?: string[];
    profile?: string;
}

export const REBAR3_TASK_TYPE = 'rebar3';

interface DetectedTask {
    command: string;
    group?: vscode.TaskGroup;
    problemMatchers: string[];
}

/**
 * Provides rebar3 tasks (compile, eunit, ct, dialyzer, release, escriptize, shell, clean)
 * for every workspace folder holding a rebar.config. Complements the ad-hoc
 * commands of RebarRunner: tasks can be bound to keys, chained with
 * `dependsOn`, and customized in tasks.json.
 */
export class ErlangTaskProvider implements vscode.TaskProvider {

    constructor(private extensionPath: string) {
    }

    public static register(context: vscode.ExtensionContext): vscode.Disposable {
        return vscode.tasks.registerTaskProvider(REBAR3_TASK_TYPE, new ErlangTaskProvider(context.extensionPath));
    }

    public async provideTasks(_token: vscode.CancellationToken): Promise<vscode.Task[]> {
        const tasks: vscode.Task[] = [];
        for (const folder of vscode.workspace.workspaceFolders ?? []) {
            const rebarConfig = path.join(folder.uri.fsPath, 'rebar.config');
            if (!fs.existsSync(rebarConfig)) {
                continue;
            }
            for (const detected of detectTasks(rebarConfig)) {
                const definition: Rebar3TaskDefinition = { type: REBAR3_TASK_TYPE, command: detected.command };
                const task = await this.createTask(folder, definition, detected.problemMatchers);
                task.group = detected.group;
                tasks.push(task);
            }
        }
        return tasks;
    }

    public async resolveTask(task: vscode.Task, _token: vscode.CancellationToken): Promise<vscode.Task | undefined> {
        const definition = <Rebar3TaskDefinition>task.definition;
        if (!definition.command) {
            return undefined;
        }
        const folder = typeof task.scope === 'object' ? task.scope : vscode.workspace.workspaceFolders?.[0];
        if (!folder) {
            return undefined;
        }
        // A task from tasks.json keeps its own problemMatcher; the resolved
        // task must reuse the same definition object, as VS Code requires.
        return this.createTask(folder, definition, task.problemMatchers);
    }

    private async createTask(folder: vscode.WorkspaceFolder, definition: Rebar3TaskDefinition, problemMatchers: string[]): Promise<vscode.Task> {
        const cfg = getElangConfigConfiguration();
        const rebar = await new RebarShell([cfg.rebarPath, folder.uri.fsPath].filter(p => !!p), this.extensionPath, undefined)
            .getRebarFullPath();
        // Same as RebarShell.runScript: always go through escript, rebar may
        // lack the execution bit.
        let escript = process.platform == 'win32' ? 'escript.exe' : 'escript';
        const options: vscode.ShellExecutionOptions = { cwd: folder.uri.fsPath };
        if (cfg.erlangPath) {
            escript = path.join(cfg.erlangPath, escript);
            options.env = { PATH: cfg.erlangPath + path.delimiter + process.env.PATH };
        }
        const execution = new vscode.ShellExecution(escript, [rebar, ...rebarArgs(definition)], options);
        return new vscode.Task(definition, folder, taskLabel(definition), REBAR3_TASK_TYPE, execution, problemMatchers);
    }
}

export function rebarArgs(definition: Rebar3TaskDefinition): string[] {
    const args = definition.profile ? ['as', definition.profile, definition.command] : [definition.command];
    return args.concat(definition.args ?? []);
}

function taskLabel(definition: Rebar3TaskDefinition): string {
    return rebarArgs(definition).join(' ');
}

/**
 * Tasks worth offering for the project described by `rebarConfigPath`.
 * `release` is only offered when rebar.config configures relx; `escriptize`
 * only when it configures escript_main_app/escript_name/escript_*.
 */
export function detectTasks(rebarConfigPath: string): DetectedTask[] {
    let contents = '';
    try {
        contents = fs.readFileSync(rebarConfigPath, 'utf8');
    } catch {
        // unreadable rebar.config: still offer the tasks that need nothing from it
    }
    const tasks: DetectedTask[] = [
        { command: 'compile', group: vscode.TaskGroup.Build, problemMatchers: ['$rebar3'] },
        { command: 'eunit', group: vscode.TaskGroup.Test, problemMatchers: ['$rebar3'] },
        { command: 'ct', group: vscode.TaskGroup.Test, problemMatchers: ['$rebar3'] },
        { command: 'dialyzer', problemMatchers: ['$rebar3-dialyzer'] },
    ];
    if (/\{\s*relx\s*,/.test(stripComments(contents))) {
        tasks.push({ command: 'release', group: vscode.TaskGroup.Build, problemMatchers: ['$rebar3'] });
    }
    // escript_main_app / escript_name / escript_* options configure `rebar3
    // escriptize` (#19); offer the task only for projects that use them.
    if (/\{\s*escript_(main_app|name|[a-z_]+)\s*,/.test(stripComments(contents))) {
        tasks.push({ command: 'escriptize', group: vscode.TaskGroup.Build, problemMatchers: ['$rebar3'] });
    }
    tasks.push({ command: 'shell', problemMatchers: [] });
    tasks.push({ command: 'clean', group: vscode.TaskGroup.Clean, problemMatchers: [] });
    return tasks;
}

function stripComments(erlangTerms: string): string {
    // good enough for rebar.config: `%` never appears inside the atoms and
    // strings that matter here
    return erlangTerms.replace(/%.*$/gm, '');
}
