import * as assert from 'assert';
import { after } from 'mocha';
import * as fs from 'fs';
import * as path from 'path';
import { env } from 'process';
import * as vscode from 'vscode';
import * as vscodeclient from 'vscode-languageclient'
import * as erlExtension from '../../lib/extension';



function openTextDocument(fileRelativePath: string ) : Thenable<vscode.TextDocument>
{
	const wk = vscode.workspace.workspaceFolders[0];
	if (!fileRelativePath.startsWith("/")) {
		fileRelativePath = "/" + fileRelativePath;
 	}
	const filepath = path.join(wk.uri.fsPath, fileRelativePath);
	return vscode.workspace.openTextDocument(filepath);	
}

suite('Erlang Language Extension', () => {
	after(() => {
		vscode.window.showInformationMessage('All tests done!');
	  });
	test('Extension should be present', async () => {
		const myExtension = vscode.extensions.getExtension('pgourlain.erlang');
		await myExtension.activate();
		assert.ok(myExtension);
	});

	test('Diagnostics should be generated', async () => {
		//use console.info('...') to write on output during test

		const document = await openTextDocument("/fixture1/fixture1.erl");	
		assert.ok(document != null);
		assert.equal('erlang', document.languageId);

		const waitForDiags = new Promise<vscode.Diagnostic[]>((resolve) => {
			const disposeToken = vscode.languages.onDidChangeDiagnostics(
				(ev) => {
					const docUri = document.uri.toString();
					if (ev.uris.some(u => u.toString() === docUri)) {
						const diags = vscode.languages.getDiagnostics(document.uri);
						if (diags.length > 0) {
							disposeToken.dispose();
							resolve(diags);
						}
					}
				}
			)
		});
		const diags = await waitForDiags;
		assert.equal(1, diags.length);
	});
});