import * as fs from 'fs';
import * as fsPromises from 'node:fs/promises'; 
import * as path from 'path';
import { GenericShell, ILogOutput, IShellOutput } from './GenericShell';
import { getElangConfigConfiguration } from './ErlangConfigurationProvider';

/**
 * Provides rebar shell commands. Locates appropriate rebar executable based on provided settings.
 * The exit codes and stdout/stderr outputs are returned.
 */
export default class RebarShell extends GenericShell {
    protected shellOutput: RebarShellOutput;
    private _lastRun: RebarRun | undefined;

    /** The command line of the last `runScript`, for diagnostics. */
    public get lastRun(): RebarRun | undefined {
        return this._lastRun;
    }

    constructor(private rebarSearchPaths: string[], private defaultRebarSearchPath: string, outputChannel: ILogOutput) {
        super(outputChannel, new RebarShellOutput(), getElangConfigConfiguration());
    }

    /**
     * Compile the Erlang apps located at `cwd`.
     *
     * @param cwd - The working directory where compilation will take place
     * @returns Promise resolved or rejected when rebar exits
     */
    public compile(cwd: string, erlPath: string) : Promise<RebarShellResult> {
        return this.runScript(cwd, ['compile'], erlPath);
    }

    /**
     * Execute rebar with supplied arguments.
     *
     * @param cwd - The working directory where rebar will be executed
     * @param commands - Arguments to rebar
     * @returns Promise resolved or rejected when rebar exits
     */
    public async runScript(cwd: string, commands: string[], erlPath: string): Promise<RebarShellResult> {
        // Rebar may not have execution permission (e.g. if extension is built
        // on Windows but installed on Linux). Let's always run rebar by escript.
        let escript = (process.platform == 'win32' ? 'escript.exe' : 'escript');
        if (erlPath !== '') {
            escript = path.join(erlPath, escript);
        }
        let rebarFileName = await this.getRebarFullPath();
        if (this.useShell && rebarFileName.search(' ') > -1) {
            // There is at least one space in rebarPath. cmd.exe (the only
            // shell this still goes through - see erlang.useShell) needs it
            // quoted to keep it one argument.
            rebarFileName = ('"' + rebarFileName + '"');
        }
        let args = [rebarFileName].concat(commands);
        this._lastRun = { command: escript, args: args.slice(), cwd };

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
    public async getRebarFullPath(): Promise<string> {
        return (await this.getRebarResolution()).path;
    }

    /**
     * Same lookup as `getRebarFullPath`, but also reports where the executable
     * came from and every location that was tried (for diagnostics).
     */
    public async getRebarResolution(): Promise<RebarResolution> {
        const candidates: RebarCandidate[] = [];
        const names = ['rebar3', 'rebar'];

        const onSearchPaths = this.findBestFile(this.rebarSearchPaths, names, 'search-path', candidates);
        if (onSearchPaths !== '') {
            return { path: onSearchPaths, source: 'search-path', candidates };
        }
        const onPATH = await this.findExecutable('rebar3');
        candidates.push({ path: onPATH ?? 'rebar3', exists: !!onPATH, origin: 'PATH' });
        if (onPATH) {
            return { path: onPATH, source: 'PATH', candidates };
        }
        const inDefaultDir = this.findBestFile([this.defaultRebarSearchPath], names, 'default-dir', candidates);
        if (inDefaultDir !== '') {
            return { path: inDefaultDir, source: 'default-dir', candidates };
        }
        return { path: 'rebar3', source: 'fallback', candidates };
    }

    /**
     * Find the first existing one of `fileNames` in `dirs`, in order of priority
     * (empty entries are skipped). Returns '' when none exists; every location
     * looked at is appended to `candidates`.
     *
     * @param dirs - Directories to search for one of `fileNames`, in order of priority
     * @param fileNames - Filenames to search in each directory, in order of priority
     * @param origin - Where `dirs` come from, recorded in `candidates`
     * @param candidates - Collects the locations that were tried
     */
    private findBestFile(dirs : string[], fileNames : string[], origin : string, candidates : RebarCandidate[]) : string
    {
        for (const dir of dirs.filter(d => !!d))
        {
            for (const fileName of fileNames)
            {
                const fullPath = path.normalize(path.join(dir, fileName));
                const exists = fs.existsSync(fullPath);
                candidates.push({ path: fullPath, exists, origin });
                if (exists) {
                    return fullPath;
                }
            }
        }
        return '';
    }

    private async findExecutable(name: string): Promise<string | null> {
        const envPath = process.env.PATH ?? '';
        const dirs = envPath.split(path.delimiter);
        const exts = process.platform === 'win32'
            ? (process.env.PATHEXT ?? '.EXE;.CMD;.BAT').split(';')
            : [''];

        for (const dir of dirs) {
            for (const ext of exts) {
            const full = path.join(dir, name + ext);
            try {
                await fsPromises.access(full, fsPromises.constants.X_OK);
                return full;
            } catch { /* keep looking */ }
            }
        }
        return null;
    }
}

/** Where `getRebarResolution` found the rebar executable. */
export type RebarSource = 'search-path' | 'PATH' | 'default-dir' | 'fallback';

export interface RebarCandidate {
    path: string,
    exists: boolean,
    origin: string
}

export interface RebarResolution {
    path: string,
    source: RebarSource,
    candidates: RebarCandidate[]
}

export interface RebarRun {
    command: string,
    args: string[],
    cwd: string
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
