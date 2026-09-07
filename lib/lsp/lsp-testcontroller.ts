import * as vscode from 'vscode';
import { CancellationToken, NotificationType, RequestType } from 'vscode-languageclient';
import * as proto from 'vscode-languageserver-protocol';
import { clientIsReady, isErlangDocument } from './lsp-context';
import { client } from './lspclientextension';

// Task 6.1-6.5 (Phase 6, tasks.md): a thin TS shell over three custom LSP
// requests/notifications - all test discovery and execution happens in
// Erlang (lsp_testing.erl, lsp_testing_eunit_report.erl,
// lsp_testing_ct_hook.erl), matching this codebase's rule that Erlang does
// the analysis and TS is only a wrapper.
namespace protocol {
    export interface ErlangTestCase {
        name: string;
        arity: number;
        range: proto.Range;
    }

    export interface ErlangTestModule {
        module: string;
        kind: 'eunit' | 'ct';
        uri: string;
        tests: ErlangTestCase[];
    }

    export interface DiscoverTestsResult {
        modules: ErlangTestModule[];
    }

    export namespace DiscoverTestsRequest {
        export const type =
            new RequestType<{}, DiscoverTestsResult, void>('erlang/discoverTests');
    }

    export interface RunTestsTarget {
        module: string;
        function: string;
        kind: 'eunit' | 'ct';
    }

    export interface RunTestsParams {
        tests: RunTestsTarget[];
        coverage?: boolean;
    }

    export interface ErlangFileCoverage {
        uri: string;
        statements: { line: number; executed: number }[];
    }

    export interface RunTestsResult {
        summary: { passed: number; failed: number; skipped: number };
        coverage: ErlangFileCoverage[];
    }

    export namespace RunTestsRequest {
        export const type =
            new RequestType<RunTestsParams, RunTestsResult, void>('erlang/runTests');
    }

    export interface TestProgressParams {
        kind: 'eunit' | 'ct';
        module: string;
        function: string;
        arity: number;
        status: 'running' | 'passed' | 'failed' | 'skipped';
        message: string | null;
        line: number | null;
    }

    export namespace TestProgressNotification {
        export const type =
            new NotificationType<TestProgressParams>('erlang/testRunProgress');
    }
}

interface TestTarget {
    module: string;
    function: string;
    kind: 'eunit' | 'ct';
}

let testTargets = new Map<string, TestTarget>();
let currentRun: vscode.TestRun | undefined;
let currentItemsByKey: Map<string, vscode.TestItem> | undefined;
// Task 6.6: `erlang/runTests` returns every statement's hit count in one
// shot (there is no separate "fetch detail for this file" request), so
// `loadDetailedCoverage` below just replays what's already been received
// rather than asking the server again.
let coverageDetailsByUri = new Map<string, vscode.StatementCoverage[]>();

function targetKey(module: string, functionName: string): string {
    return `${module}::${functionName}`;
}

export function activate(context: vscode.ExtensionContext, lspOutputChannel: vscode.OutputChannel) {
    const controller = vscode.tests.createTestController('erlangTests', 'Erlang Tests');
    context.subscriptions.push(controller);

    controller.resolveHandler = async (item) => {
        if (!item) {
            await discoverTests(controller, lspOutputChannel);
        }
    };

    const watcher = vscode.workspace.createFileSystemWatcher('**/*.erl');
    const refresh = () => discoverTests(controller, lspOutputChannel);
    watcher.onDidChange(refresh);
    watcher.onDidCreate(refresh);
    watcher.onDidDelete(refresh);
    context.subscriptions.push(watcher);

    context.subscriptions.push(vscode.workspace.onDidSaveTextDocument((document) => {
        if (isErlangDocument(document)) {
            discoverTests(controller, lspOutputChannel);
        }
    }));

    const runProfile = controller.createRunProfile(
        'Run', vscode.TestRunProfileKind.Run,
        (request, token) => runTests(controller, request, token, false), true);
    const debugProfile = controller.createRunProfile(
        'Debug', vscode.TestRunProfileKind.Debug,
        (request, token) => debugTests(controller, request, token), false);
    const coverageProfile = controller.createRunProfile(
        'Coverage', vscode.TestRunProfileKind.Coverage,
        (request, token) => runTests(controller, request, token, true), false);
    coverageProfile.loadDetailedCoverage = async (_testRun, fileCoverage) =>
        coverageDetailsByUri.get(fileCoverage.uri.toString()) ?? [];
    context.subscriptions.push(runProfile, debugProfile, coverageProfile);

    client.onNotification(protocol.TestProgressNotification.type, (params) => {
        handleProgress(params);
    });

    if (clientIsReady) {
        discoverTests(controller, lspOutputChannel);
    }
}

async function discoverTests(controller: vscode.TestController, lspOutputChannel: vscode.OutputChannel) {
    if (!clientIsReady) {
        return;
    }
    try {
        const result = await client.sendRequest(protocol.DiscoverTestsRequest.type, {});
        const newTargets = new Map<string, TestTarget>();
        const moduleItems: vscode.TestItem[] = [];
        for (const testModule of result.modules) {
            const moduleUri = vscode.Uri.parse(testModule.uri);
            const moduleItem = controller.createTestItem(testModule.module, testModule.module, moduleUri);
            const testItems: vscode.TestItem[] = [];
            for (const testCase of testModule.tests) {
                const id = targetKey(testModule.module, testCase.name);
                const testItem = controller.createTestItem(id, testCase.name, moduleUri);
                testItem.range = client.protocol2CodeConverter.asRange(testCase.range);
                testItems.push(testItem);
                newTargets.set(id, { module: testModule.module, function: testCase.name, kind: testModule.kind });
            }
            moduleItem.children.replace(testItems);
            moduleItems.push(moduleItem);
        }
        controller.items.replace(moduleItems);
        testTargets = newTargets;
    } catch (e) {
        lspOutputChannel?.appendLine(`erlang/discoverTests failed: ${e}`);
    }
}

function collectLeaves(item: vscode.TestItem, into: vscode.TestItem[]) {
    if (item.children.size > 0) {
        item.children.forEach((child) => collectLeaves(child, into));
    } else {
        into.push(item);
    }
}

function itemsToRun(controller: vscode.TestController, request: vscode.TestRunRequest): vscode.TestItem[] {
    const roots: vscode.TestItem[] = [];
    if (request.include) {
        request.include.forEach((item) => roots.push(item));
    } else {
        controller.items.forEach((item) => roots.push(item));
    }
    const excluded = new Set((request.exclude ?? []).map((item) => item.id));
    const leaves: vscode.TestItem[] = [];
    roots.forEach((item) => collectLeaves(item, leaves));
    return leaves.filter((item) => !excluded.has(item.id));
}

async function runTests(controller: vscode.TestController, request: vscode.TestRunRequest, token: CancellationToken, withCoverage: boolean) {
    const run = controller.createTestRun(request);
    const itemsByKey = new Map<string, vscode.TestItem>();
    const tests: protocol.RunTestsTarget[] = [];

    for (const item of itemsToRun(controller, request)) {
        const target = testTargets.get(item.id);
        if (!target) {
            continue;
        }
        run.enqueued(item);
        itemsByKey.set(targetKey(target.module, target.function), item);
        tests.push({ module: target.module, function: target.function, kind: target.kind });
    }

    currentRun = run;
    currentItemsByKey = itemsByKey;
    try {
        const result = await client.sendRequest(protocol.RunTestsRequest.type, { tests, coverage: withCoverage }, token);
        if (withCoverage) {
            applyCoverage(run, result.coverage);
        }
    } catch (e) {
        // erlang/runTests failed outright (e.g. LSP request error) - leave
        // whatever state the `erlang/testRunProgress` notifications already
        // applied to individual items, there is nothing more specific to
        // report at the run level.
    } finally {
        currentRun = undefined;
        currentItemsByKey = undefined;
        run.end();
    }
}

// Task 6.6: turn the per-line call counts lsp_testing.erl's `cover:analyse`
// call returned into a vscode.FileCoverage (the file-level summary shown
// directly in the Test Coverage view) plus the vscode.StatementCoverage[]
// detail `loadDetailedCoverage` above hands back on demand (the per-line
// gutter highlights, only computed when a file is actually opened there).
function applyCoverage(run: vscode.TestRun, coverage: protocol.ErlangFileCoverage[]) {
    coverageDetailsByUri.clear();
    for (const fileCoverage of coverage) {
        const uri = vscode.Uri.parse(fileCoverage.uri);
        const statements = fileCoverage.statements.map((statement) =>
            new vscode.StatementCoverage(statement.executed, new vscode.Position(Math.max(0, statement.line - 1), 0)));
        coverageDetailsByUri.set(uri.toString(), statements);
        const covered = statements.filter((s) => Number(s.executed) > 0).length;
        run.addCoverage(new vscode.FileCoverage(uri, new vscode.TestCoverageCount(covered, statements.length)));
    }
}

function handleProgress(params: protocol.TestProgressParams) {
    if (!currentRun || !currentItemsByKey) {
        return;
    }
    const item = currentItemsByKey.get(targetKey(params.module, params.function));
    if (!item) {
        return;
    }
    switch (params.status) {
        case 'running':
            currentRun.started(item);
            break;
        case 'passed':
            currentRun.passed(item);
            break;
        case 'failed': {
            const message = new vscode.TestMessage(params.message ?? 'Test failed');
            if (item.uri && params.line !== null) {
                message.location = new vscode.Location(item.uri, new vscode.Position(Math.max(0, params.line - 1), 0));
            }
            currentRun.failed(item, message);
            break;
        }
        case 'skipped':
            currentRun.skipped(item);
            break;
    }
}

// Task 6.5: launch the existing DAP session (see lib/ErlangShellDebugger.ts
// / lib/erlangDebugSession.ts) with the test as entry point. There is no
// dedicated "module/function entry point" field on the debug configuration
// - `arguments` is appended verbatim to the `erl` command line, and
// createArgsFilev1 already does `int:start()` + `int:ni(...)` for every
// project file first, so an extra `-eval` here just calls into code that's
// already interpreted.
async function debugTests(controller: vscode.TestController, request: vscode.TestRunRequest, token: CancellationToken) {
    const run = controller.createTestRun(request);
    try {
        for (const item of itemsToRun(controller, request)) {
            const target = testTargets.get(item.id);
            if (!target || !item.uri) {
                continue;
            }
            const folder = vscode.workspace.getWorkspaceFolder(item.uri) ?? vscode.workspace.workspaceFolders?.[0];
            if (!folder) {
                continue;
            }
            run.started(item);
            const debugConfig: vscode.DebugConfiguration = {
                type: 'erlang',
                name: `Debug ${target.module}:${target.function}`,
                request: 'launch',
                cwd: folder.uri.fsPath,
                arguments: debugEvalArgument(target)
            };
            await vscode.debug.startDebugging(folder, debugConfig);
        }
    } finally {
        run.end();
    }
}

function debugEvalArgument(target: TestTarget): string {
    return target.kind === 'eunit'
        ? `-eval 'eunit:test({${target.module}, ${target.function}})'`
        : `-eval 'ct:run_test([{suite, ${target.module}}, {testcase, ${target.function}}])'`;
}
