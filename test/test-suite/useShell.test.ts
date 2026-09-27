import * as assert from 'assert';
import { quoteForShell, splitCommandLine, commandLineArgs } from '../../lib/GenericShell';
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

suite('splitCommandLine', () => {
    test('splits on whitespace like /bin/sh', () => {
        assert.deepStrictEqual(splitCommandLine('  -s myapp   start '), ['-s', 'myapp', 'start']);
    });

    test('keeps quoted words together and strips the quotes', () => {
        assert.deepStrictEqual(splitCommandLine(`-eval 'application:start(x), ok.' -pa "/a b/ebin"`),
            ['-eval', 'application:start(x), ok.', '-pa', '/a b/ebin']);
    });

    test('handles escapes inside double quotes and bare backslashes', () => {
        assert.deepStrictEqual(splitCommandLine(`-eval "io:format(\\"hi\\")" a\\ b`),
            ['-eval', 'io:format("hi")', 'a b']);
    });

    test('keeps an explicitly quoted empty word', () => {
        assert.deepStrictEqual(splitCommandLine(`-x ''`), ['-x', '']);
    });
});

suite('commandLineArgs', () => {
    test('missing or blank value yields no argv word (#358)', () => {
        for (const useShell of [true, false]) {
            assert.deepStrictEqual(commandLineArgs(undefined, useShell), []);
            assert.deepStrictEqual(commandLineArgs('', useShell), []);
            assert.deepStrictEqual(commandLineArgs('   ', useShell), []);
        }
    });

    test('passes the value as-is through a shell, splits it otherwise', () => {
        assert.deepStrictEqual(commandLineArgs('-s myapp', true), ['-s myapp']);
        assert.deepStrictEqual(commandLineArgs('-s myapp', false), ['-s', 'myapp']);
    });
});
