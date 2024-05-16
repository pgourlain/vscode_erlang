import { defineConfig } from '@vscode/test-cli';

export default defineConfig({
	files: 'out/test/**/*.test.js',
	workspaceFolder: './test/test-fixtures',
	mocha: {
		ui: 'tdd',
		timeout: 60000
	  }
});