import * as assert from 'assert';
import { after } from 'mocha';
import * as fs from 'fs';
import * as path from 'path';
import { env } from 'process';
import * as vscode from 'vscode';

suite('Erlang Language Extension', () => {
	after(() => {
		vscode.window.showInformationMessage('All tests done!');
	  });
	test('Extension should be present', () => {
		assert.ok(vscode.extensions.getExtension('pgourlain.erlang'));
	});
});