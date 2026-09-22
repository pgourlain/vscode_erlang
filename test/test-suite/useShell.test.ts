import * as assert from 'assert';
import { quoteForShell } from '../../lib/GenericShell';
import { resolveUseShell } from '../../lib/ErlangConfigurationProvider';

suite('erlang.useShell resolution', () => {
    test('"always" forces a shell regardless of platform', () => {
        assert.strictEqual(resolveUseShell('always'), true);
    });

    test('"never" forces no shell regardless of platform', () => {
        assert.strictEqual(resolveUseShell('never'), false);
    });

    test('"auto" (and any unrecognized value) matches the platform default', () => {
        const expected = process.platform === 'win32';
        assert.strictEqual(resolveUseShell('auto'), expected);
        assert.strictEqual(resolveUseShell('anything-else'), expected);
    });
});

suite('quoteForShell', () => {
    test('wraps the value in double quotes when useShell is true', () => {
        assert.strictEqual(quoteForShell('{127,0,0,1}', true), '"{127,0,0,1}"');
    });

    test('leaves the value untouched when useShell is false', () => {
        assert.strictEqual(quoteForShell('{127,0,0,1}', false), '{127,0,0,1}');
    });
});
