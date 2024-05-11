import * as assert from 'assert';
import { after } from 'mocha';
import * as fs from 'fs';
import * as path from 'path';
import { env } from 'process';
import * as vscode from 'vscode';
import * as erlExtension from '../../lib/extension';

suite('Erlang Language Extension', () => {
	after(() => {
		vscode.window.showInformationMessage('All tests done!');
	  });
	test('Extension should be present', () => {
		const myExtension = vscode.extensions.getExtension('pgourlain.erlang');
		assert.ok(myExtension);
	});

	test('Diagnostics should be generated', async () => {
		const extension = vscode.extensions.getExtension('pgourlain.erlang');
		await extension.activate();

		//use console.info('...') to write on output during test
		const wk = vscode.workspace.workspaceFolders[0];
		const filepath = path.join(wk.uri.fsPath, "/fixture1/fixture1.erl");
		const document = await vscode.workspace.openTextDocument(filepath);	
		assert.ok(document != null);
		assert.equal('erlang', document.languageId);

		const waitForDiags = new Promise<readonly vscode.Uri[]>((resolve, reject) => {
			const disposeToken = vscode.languages.onDidChangeDiagnostics(
				async (ev) => {
					disposeToken.dispose();
					resolve(ev.uris);
				}
			) 
		});
		const uris = await waitForDiags;
		assert.equal(true, uris.length > 0);
		const diags = vscode.languages.getDiagnostics(uris[0]);
		assert.equal(1, diags.length);
	});
});