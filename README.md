# Erlang for Visual Studio Code

[![Visual Studio Marketplace](https://img.shields.io/visual-studio-marketplace/v/pgourlain.erlang?style=for-the-badge&label=VS%20Marketplace&logo=visual-studio-code)](https://marketplace.visualstudio.com/items?itemName=pgourlain.erlang)
[![Installs](https://img.shields.io/visual-studio-marketplace/i/pgourlain.erlang?style=for-the-badge)](https://marketplace.visualstudio.com/items?itemName=pgourlain.erlang)
[![Build Status](https://img.shields.io/github/actions/workflow/status/pgourlain/vscode_erlang/pr-verify.yml?branch=master&style=for-the-badge&logo=github)](https://github.com/pgourlain/vscode_erlang/actions?query=workflow:pr-verify)
[![License](https://img.shields.io/github/license/pgourlain/vscode_erlang?style=for-the-badge&logo=erlang)](https://github.com/pgourlain/vscode_erlang/blob/master/LICENSE)

This extension adds support for the Erlang language to Visual Studio Code, including editing, building and debugging.


## Editing support

- Syntax highlighting
- Automatic indentation
- Erlang IntelliSense
- Shows errors and warnings dynamically while you type
- Go To Definition/Peek Definition
- Hover help for standard functions
- Hover for project functions showing head clauses
- CodeLens showing exported functions and references
- InlayHints showing parameters name in function calls
  - disable by default : enable in configuration settings
  - limits : only works with locals calls

![editing](images/vscode-erlang-editing.gif)

InlayHints in function calls

![inlayHints](images/vscode-erlang-inlayhints.png)
- showing parameter name when it doesn't match with caller var name

### Semantic highlighting

Modules, functions, macros, variables, parameters, types, records and record fields are colored from the real syntax tree, on top of the TextMate grammar. OTP modules are flagged as `defaultLibrary` and functions listed in `-deprecated` as `deprecated`.

- disable with `erlang.semanticTokensEnabled`

![semanticTokens](images/vscode-erlang-semantic-tokens.png)

### Quick fixes and refactorings

Press `Ctrl+.` / `Cmd+.` on a diagnostic or on a selection:

- Quick fixes driven by compiler warnings/errors:
  - Prefix unused variable with `_`
  - Export an unused function
  - Create a stub for an undefined function
  - Add a missing field to a record definition
  - Remove include of a missing file
  - Add missing `-module(...)`
- Export / unexport a function, generate `-spec` from the function clauses
- Implement missing callbacks for a `-behaviour(...)`
- Refactorings: extract function from selection, inline variable, convert `if` ↔ `case`
- Source actions: sort the `-export` list

![codeActions](images/vscode-erlang-codeactions.png)

![behaviourCallbacks](images/vscode-erlang-implement-callbacks.png)

### Navigation

- Go to Symbol in Workspace (`Ctrl+T` / `Cmd+T`): functions, records, types, macros across the project
- Go to Declaration, Go to Type Definition (from a `-spec`/type usage to its `-type`/`-opaque`)
- Go to Implementations: from a `-callback` to every module implementing the behaviour
- Call Hierarchy (incoming/outgoing calls) and Type Hierarchy (behaviour ↔ implementors)
- Highlight all occurrences of the variable/function/record under the cursor
- Clickable `-include` / `-include_lib` paths and URLs in comments

![workspaceSymbol](images/vscode-erlang-workspace-symbol.png)

![callHierarchy](images/vscode-erlang-call-hierarchy.png)

### Formatting and folding

- Format document, format selection and format on type (`.`, `;`, `,`, newline), powered by erlfmt
  - disable with `erlang.formatterEnabled`, line length with `erlang.formattingLineLength`
- Folding of functions, clauses, `case`/`receive`/`try`, `-export` lists, comment blocks and `%% region` / `%% endregion` markers
- Expand/shrink selection (`Shift+Alt+→` / `Shift+Alt+←`)
- Completion with snippets for function arguments, `?` for macros, `-` for attributes

![folding](images/vscode-erlang-folding.png)

## Testing

EUnit and Common Test tests are discovered by the language server and shown in the VS Code **Testing** sidebar:

- EUnit: `*_test/0`, `*_test_/0` functions (including inside `-ifdef(TEST)`)
- Common Test: `*_SUITE.erl` modules and their test cases
- Run, Debug (breakpoints in tests) and Run with Coverage profiles
- Per-test results with failure message and location, line coverage of the code under test shown in the editor gutter

![testExplorer](images/vscode-erlang-test-explorer.png)

![testCoverage](images/vscode-erlang-test-coverage.png)

## Build

![build](images/vscode-erlang-build.png)

- Standard rebar3 is the default build tool, also rebar is supported. The rebar.config file should be placed in the root directory.
- Build arguments are configurable, by default "compile" is used
- You can override the default in configuration file (i.e. workspace settings)

![build](images/vscode-erlang-build-args.png)

## Debugger

- Variables List shows variables from the current scope
- Call Stack shows Erlang processes and allows to control them with e.g. Pause and Continue
- Standard commands Step Over, Step Into, Step Out supported
- Full breakpoints support:
  - Regular breakpoints
  - Function Breakpoints: use format module:function/arity
  - Conditional Breakpoints
  - Hit-Count Breakpoints

![debug-inlinevalues](images/vscode-erlang-inlinevalues.png)

## Running debugger

You can provide a specific command line to 'erl' in launch.json configuration file in "arguments" entry.

![debug1](images/vscode-erlang-debug-args.png)

The modified code may be automatically build before debugger is started. To set automatic build up you need to:

1. Add to launch.json file the entry "preLaunchTask": "rebar3 compile"
1. Select **Configure Task** in the alert, choose **Create tasks.json file from template** and then **Others: Example to run an arbitrary command**
1. This will create tasks.json for you. Change both label and command to "rebar3 compile".
1. Add entry "problemMatcher": "$erlang"

![debug](images/vscode-erlang-build-task.png)

Then, before debugging is started, modified files will be recompiled automatically.

## Using this extension in Erlang Docker instance

Clone this repo, and try it :
- [vscode-remote-try-erlang](https://github.com/pgourlain/vscode-remote-try-erlang)

For more information about vscode and containers :
- [setup vscode for containers](https://code.visualstudio.com/docs/containers/overview) and [remote container documentation](https://code.visualstudio.com/docs/remote/containers)

## Available commands

Support for Erlang tools, including rebar3, EUnit and Dialyzer

![commands](images/vscode-erlang-commands.png)

- Dialyzer warnings displayed in Problems tab for easy navigation

## Settings

- `erlang.erlangPath` - Directory location of erl/escript
- `erlang.erlangArgs` - Arguments passed to Erlang backend
- `erlang.erlangDistributedNode` - Start the Erlang backend in a distributed Erlang node for extension development
- `erlang.rebarPath` - Directory location of rebar/rebar3
- `erlang.rebarBuildArgs` - Arguments to provide to rebar/rebar3 build command
- `erlang.includePaths` - Include paths are read from rebar.config, and also standard set of paths is used. This setting is for special cases when the default behaviour is not enough
- `erlang.linting` - Enable/disable dynamic validation of opened Erlang source files
- `erlang.codeLensEnabled` - Enable/Disable CodeLens
- `erlang.cacheManagement` - Specify where and how to store large cache tables
- `erlang.inlayHintsEnabled` - Enable/Disable InlayHints
- `erlang.semanticTokensEnabled` - Enable/Disable semantic highlighting
- `erlang.formatterEnabled` - Enable/Disable the formatter (document, selection and on-type)
- `erlang.formattingLineLength` - Maximum line length for formatting
- `erlang.verbose` - Activate technical traces for use in the extension development
- `erlang.verboseExcludeFilter` - LSP methods excluded from technical traces
- `erlang.debuggerRunMode` - How the debug adapter is run (`external`, `server`, `inline`)

## Help

[Some configuration tricks](./HELP.MD)

## Credits

File 'Erlang.tmLanguage' is inspired from <https://github.com/textmate/erlang.tmbundle/blob/master/Syntaxes/Erlang.plist>
