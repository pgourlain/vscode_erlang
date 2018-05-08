# Erlang for Visual Studio Code
This extension adds support for the Erlang language to Visual Studio Code, including editing, building and debugging.

## Editing support
- Syntax highlighting
- Shows errors and warnings dynamically while you type (requires File/Auto Save to be on)
- Go To Definition/Peek Definition
- Hover help for standard functions
- Hover for project functions showing head clauses

![editing](images/vscode-editing.gif)

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
  * Regular breakpoints
  * Function Breakpoints: use format module:function/arity
  * Conditional Breakpoints
  * Hit-Count Breakpoints

![debug](images/vscode-erlang-debug.png)

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

## Available commands
![commands](images/vscode-erlang-commands.png)

## Credits
File 'Erlang.tmLanguage' is inspired from https://github.com/textmate/erlang.tmbundle/blob/master/Syntaxes/Erlang.plist
