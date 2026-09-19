// Drives a real VS Code (Electron) with Playwright to capture the README screenshots.
//
// Prereqs: erl on PATH, `npm run compile`, `./rebar3 compile`, `npm i -D playwright`.
// Usage:   node scripts/screenshots/take-screenshots.mjs [shotName ...]
import { _electron as electron } from 'playwright';
import { downloadAndUnzipVSCode } from '@vscode/test-electron';
import { execFileSync } from 'node:child_process';
import * as fs from 'node:fs';
import * as os from 'node:os';
import * as path from 'node:path';
import { fileURLToPath } from 'node:url';

const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '..', '..');
const workspace = path.join(here, 'workspace');
const imagesDir = path.join(root, 'images');
const isMac = process.platform === 'darwin';
const mod = isMac ? 'Meta' : 'Control';

const userDataDir = fs.mkdtempSync(path.join(os.tmpdir(), 'vscode-erlang-shots-'));
fs.mkdirSync(path.join(userDataDir, 'User'), { recursive: true });
fs.writeFileSync(path.join(userDataDir, 'User', 'settings.json'), JSON.stringify({
    'workbench.colorTheme': 'Default Dark Modern',
    'workbench.startupEditor': 'none',
    'workbench.tips.enabled': false,
    'workbench.secondarySideBar.defaultVisibility': 'hidden',
    'chat.disableAIFeatures': true,
    'security.workspace.trust.enabled': false,
    'update.mode': 'none',
    'telemetry.telemetryLevel': 'off',
    'editor.fontSize': 14,
    'editor.minimap.enabled': false,
    'editor.showFoldingControls': 'always',
    'testing.openTesting': 'neverOpen',
    'erlang.inlayHintsEnabled': true,
    'git.enabled': false,
    'git.openRepositoryInParentFolders': 'never',
}, null, 2));

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

async function command(page, text) {
    await page.keyboard.press('F1');
    await page.waitForSelector('.quick-input-widget', { state: 'visible' });
    await page.keyboard.type(text);
    await sleep(400);
    await page.keyboard.press('Enter');
    await sleep(500);
}

async function closeAll(page) {
    await page.keyboard.press('Escape');
    await command(page, 'View: Close All Editors');
}

async function openFile(page, name, line, col = 1) {
    await page.keyboard.press(`${mod}+P`);
    await page.waitForSelector('.quick-input-widget', { state: 'visible' });
    await page.keyboard.type(name);
    await sleep(600);
    await page.keyboard.press('Enter');
    await page.waitForSelector('.monaco-editor .view-lines');
    await sleep(500);
    if (line) {
        await page.keyboard.press('Control+G');
        await page.keyboard.type(`${line}:${col}`);
        await page.keyboard.press('Enter');
        await sleep(300);
    }
}

async function shot(page, name) {
    const file = path.join(imagesDir, `vscode-erlang-${name}.png`);
    await page.screenshot({ path: file });
    console.log(`  saved ${path.relative(root, file)}`);
}

async function codeActionMenu(page) {
    const bulb = page.locator('.lightBulbWidget').first();
    await bulb.waitFor({ state: 'visible', timeout: 15000 });
    await bulb.click();
    await page.waitForSelector('.action-widget', { state: 'visible', timeout: 15000 });
    await sleep(500);
}

// Expand every node of the focused tree: Right expands a collapsed node, Down walks on.
async function expandTree(page, steps = 20) {
    for (let i = 0; i < steps; i++) {
        await page.keyboard.press('ArrowRight');
        await page.keyboard.press('ArrowDown');
    }
    await page.keyboard.press('Home');
}

const shots = {
    'semantic-tokens': async (page) => {
        await openFile(page, 'demo.erl', 1);
        await sleep(1500);
        await shot(page, 'semantic-tokens');
    },
    'codeactions': async (page) => {
        await openFile(page, 'demo.erl', 14, 6);
        await codeActionMenu(page);
        await shot(page, 'codeactions');
    },
    'implement-callbacks': async (page) => {
        await openFile(page, 'demo_server.erl', 2, 3);
        await sleep(3000);
        await codeActionMenu(page);
        await shot(page, 'implement-callbacks');
    },
    'workspace-symbol': async (page) => {
        await page.keyboard.press(`${mod}+T`);
        await page.waitForSelector('.quick-input-widget', { state: 'visible' });
        await page.keyboard.type('area');
        await page.waitForSelector('.quick-input-list .monaco-list-row', { timeout: 15000 });
        await sleep(1000);
        await shot(page, 'workspace-symbol');
    },
    'call-hierarchy': async (page) => {
        await openFile(page, 'demo.erl', 22, 3);
        await page.keyboard.press('Shift+Alt+H');
        await page.waitForSelector('.monaco-list-row:has-text("start/1")', { timeout: 15000 });
        await sleep(1500);
        await shot(page, 'call-hierarchy');
    },
    'folding': async (page) => {
        await openFile(page, 'demo.erl', 1);
        // Chevrons on lines 10, 12, 19, 22: fold start/1 (12), then area/1 (19),
        // which becomes the 2nd still-expanded chevron.
        const expanded = page.locator('.margin-view-overlays .codicon-folding-expanded');
        await expanded.nth(1).click();
        await sleep(300);
        await expanded.nth(1).click();
        await page.mouse.move(900, 700);
        await sleep(800);
        await shot(page, 'folding');
    },
    'test-explorer': async (page) => {
        await openFile(page, 'demo_tests.erl', 1);
        await command(page, 'Testing: Focus on Test Explorer View');
        await page.waitForSelector('.test-explorer .monaco-list-row', { timeout: 30000 });
        await command(page, 'Test: Run All Tests');
        await sleep(20000);
        await command(page, 'Testing: Focus on Test Explorer View');
        await page.locator('.test-explorer .monaco-list-row').first().click();
        await expandTree(page);
        await sleep(800);
        await shot(page, 'test-explorer');
    },
    'test-coverage': async (page) => {
        await command(page, 'Test: Run All Tests with Coverage');
        await sleep(25000);
        await openFile(page, 'demo_tests.erl', 1);
        await command(page, 'Testing: Focus on Test Explorer View');
        await page.waitForSelector('.monaco-list-row:has-text("%")', { timeout: 15000 });
        await sleep(1500);
        await shot(page, 'test-coverage');
    },
};

async function main() {
    const wanted = process.argv.slice(2);
    const names = wanted.length ? wanted : Object.keys(shots);
    // The test runner loads project code from _build/**/ebin, it does not compile src/.
    execFileSync(path.join(root, 'rebar3'), ['compile'], { cwd: workspace, stdio: 'inherit' });
    const executablePath = await downloadAndUnzipVSCode('stable');
    const extensionsDir = fs.mkdtempSync(path.join(os.tmpdir(), 'vscode-erlang-ext-'));

    // Inherited when run from a VS Code terminal: it would start Code as plain Node.
    const env = { ...process.env };
    delete env.ELECTRON_RUN_AS_NODE;

    const app = await electron.launch({
        executablePath,
        env,
        args: [
            workspace,
            `--extensionDevelopmentPath=${root}`,
            `--user-data-dir=${userDataDir}`,
            `--extensions-dir=${extensionsDir}`,
            '--disable-workspace-trust',
            '--skip-welcome',
            '--skip-release-notes',
            '--no-sandbox',
        ],
    });
    try {
        const page = await app.firstWindow();
        await app.evaluate(({ BrowserWindow }) => {
            const w = BrowserWindow.getAllWindows()[0];
            w.setSize(1280, 800);
            w.center();
        });
        await page.waitForSelector('.monaco-workbench', { timeout: 60000 });

        // Wait for the language server: the unused variable warning proves it is up.
        console.log('waiting for the Erlang language server...');
        await openFile(page, 'demo.erl', 1);
        await page.waitForSelector('.squiggly-warning', { timeout: 180000 });
        await sleep(2000);

        for (const name of names) {
            console.log(`shot: ${name}`);
            try {
                await command(page, 'View: Show Explorer');
                await shots[name](page);
            } catch (e) {
                console.error(`  FAILED ${name}: ${e.message}`);
                await page.screenshot({ path: path.join(os.tmpdir(), `failed-${name}.png`) });
            }
            if (name !== 'test-explorer') {
                await closeAll(page);
            }
        }
    } finally {
        await app.close();
    }
}

main().catch((e) => {
    console.error(e);
    process.exit(1);
});
