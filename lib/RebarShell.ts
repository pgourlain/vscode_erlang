import * as fs from 'fs';
import * as path from 'path';
import { GenericShell, ILogOutput, IShellOutput } from './GenericShell';

/**
 * Provides rebar shell commands. Locates appropriate rebar executable based on provided settings.
 * The exit codes and stdout/stderr outputs are returned.
 */
export default class RebarShell extends GenericShell {
    protected shellOutput: RebarShellOutput;

    constructor(private rebarSearchPaths: string[], private defaultRebarSearchPath: string, outputChannel: ILogOutput) {
        super(outputChannel, new RebarShellOutput());
    }

    /**
     * Compile the Erlang apps located at `cwd`.
     *
     * @param cwd - The working directory where compilation will take place
     * @returns Promise resolved or rejected when rebar exits
     */
    public compile(cwd: string) : Promise<RebarShellResult> {
        return this.runScript(cwd, ['compile']);
    }

    /**
     * Execute rebar with supplied arguments.
     *
     * @param cwd - The working directory where rebar will be executed
     * @param commands - Arguments to rebar
     * @returns Promise resolved or rejected when rebar exits
     */
    public async runScript(cwd: string, commands: string[]): Promise<RebarShellResult> {
        // Rebar may not have execution permission (e.g. if extension is built
        // on Windows but installed on Linux). Let's always run rebar by escript.
        let escript = (process.platform == 'win32' ? 'escript.exe' : 'escript');
        let rebarFileName = this.getRebarFullPath();
        let args = [rebarFileName].concat(commands);

        this.shellOutput.clear();

        let result: number;
        try {
            result = await this.RunProcess(escript, cwd, args);
        } catch (exitCode) {
            result = exitCode;
        }
        return wrapProcessExit(result, this.shellOutput.output);
    }

    /**
     * Get the full path to the rebar executable that will be used.
     *
     * @returns Full path to rebar executable
     */
    private getRebarFullPath(): string {
        const rebarSearchPaths = this.rebarSearchPaths.slice();
        if (!rebarSearchPaths.includes(this.defaultRebarSearchPath)) {
            rebarSearchPaths.push(this.defaultRebarSearchPath);
        }
        return this.findBestFile(rebarSearchPaths, ['rebar3', 'rebar'], 'rebar3');
    }

    /**
     * Find the rebar executable to be used based on the order of `dirs` and `filenames` provided.
     * The order defines the priority. `defaultResult` will be used if no executable could be found.
     *
     * @param dirs - Directories to search for one of `fileNames`, in order of priority
     * @param fileNames - Filenames to search in each directory, in order of priority
     * @param defaultResult - Fallback executable path or command if rebar not found
     * @returns Preferred rebar executable path or command
     */
    private findBestFile(dirs : string[], fileNames : string[], defaultResult : string) : string
    {
        var result = defaultResult;
        for (var i=0; i < dirs.length; i++)
        {
            for (var j=0; j < fileNames.length; j++)
            {
                var fullPath = path.normalize(path.join(dirs[i], fileNames[j]));
                if (fs.existsSync(fullPath)) {
                    return fullPath;
                }
            }
        }
        return result;
    }
}

export interface RebarShellResult {
    exitCode: number,
    output: string
}

/**
 * Accumulates shell output from processes executed by GenericShell.
 */
class RebarShellOutput implements IShellOutput {
    public output: string = '';

    append(value: string) {
        this.output += value;
    }

    clear() {
        this.output = '';
    }
}

const wrapProcessExit = (exitCode: number, output: string) => ({
    exitCode,
    output
});
